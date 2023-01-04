{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Best current practice library for UDP clients and servers.
--
--   * Efficient receiving function without memory copy.
--   * Proper buffer size.
--   * Type-safe APIs.
--   * TCP-like APIs (creating a UDP connection from a listing socket).
--
--   The 'Network.Socket.ByteString.recv' family in
--   "Network.Socket.ByteString" uses 'createAndTrim'
--   internaly.  So, one buffer is allocated before corresponding
--   system calls are called. Then another buffer is allocated
--   according to the input size and the input is copied.
--   Receiving functions provided by this library uses 'createUptoN'
--   to avoid the memory copy.
--
--   Recent application protocols are designed to avoid IP
--   fragmentation. So, the UDP payload size is never over 1,500.
--   This library uses 2,048 for the buffer size. This size ensures
--   no global locking when allocating 'ByteString' (i.e. a buffer).
--
--   To know the background of TCP-like API in the server side, see:
--
--   * https://kazu-yamamoto.hatenablog.jp/entry/2022/02/25/153122
--
--   To know the background of auto migration in the client side, see:
--
--   * https://kazu-yamamoto.hatenablog.jp/entry/2021/06/29/134930
--   * https://www.iij.ad.jp/en/dev/iir/pdf/iir_vol52_focus2_EN.pdf (Sec 3.9)
module Network.UDP (
  -- * Sockets used by clients and servers after accept
    UDPSocket(..)
  , clientSocket
  , recv
  , recvBuf
  , send
  , sendBuf
  -- * Server's wildcard socket
  , ListenSocket(..)
  , serverSocket
  , ClientSockAddr(..)
  , recvFrom
  , sendTo
  -- * Server's connected socket
  , accept
  -- * Closing
  , stop
  , close
  -- * Misc
  , natRebinding
  ) where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import Data.IP hiding (addr)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import qualified GHC.IO.Exception as E
import qualified Network.Socket as NS
import Network.Socket hiding (accept, close, sendBuf, recvBuf)
import qualified Network.Socket.ByteString as NSB
import qualified System.IO.Error as E

import Network.UDP.Types
import qualified Network.UDP.Recv as R

----------------------------------------------------------------

anySockAddr :: SockAddr -> SockAddr
anySockAddr (SockAddrInet p _)      = SockAddrInet  p 0
anySockAddr (SockAddrInet6 p f _ s) = SockAddrInet6 p f (0,0,0,0) s
anySockAddr _                       = error "anySockAddr"

isAnySockAddr :: SockAddr -> Bool
isAnySockAddr (SockAddrInet _ 0)              = True
isAnySockAddr (SockAddrInet6 _ _ (0,0,0,0) _) = True
isAnySockAddr _                               = False

----------------------------------------------------------------

-- | A listening socket for UDP which can be used
--   for 'recvFrom' and 'sendTo'.
--   Optionally, a connected UDP socket can be created
--   with 'accept' as an emulation of TCP.
data ListenSocket = ListenSocket {
    listenSocket :: Socket
  , mySockAddr   :: SockAddr
  , wildcard     :: Bool -- ^ 'True' for wildcard. 'False' for interface-specific.
  } deriving (Eq, Show)

-- | A UDP socket which are used with 'recv' and 'send'.
data UDPSocket = UDPSocket {
    udpSocket    :: Socket
  , peerSockAddr :: SockAddr -- ^ Used for a unconnected socket naturally. Used for a connected sockdet for checking
  , connected    :: Bool
  } deriving (Eq, Show)

-- | A client socket address from the server point of view.
data ClientSockAddr = ClientSockAddr SockAddr [Cmsg] deriving (Eq, Show)

----------------------------------------------------------------

-- | Creating a listening UDP socket.
serverSocket :: (IP, PortNumber) -> IO ListenSocket
serverSocket ip = E.bracketOnError open NS.close $ \s -> do
    setSocketOption s ReuseAddr 1
    withFdSocket s setCloseOnExecIfNeeded
#if !defined(openbsd_HOST_OS)
    when (family == AF_INET6) $ setSocketOption s IPv6Only 1
#endif
    bind s sa
    let wild = isAnySockAddr sa
    when wild $ do
        let opt = decideOption sa
        setSocketOption s opt 1
    return $ ListenSocket s sa wild
  where
    sa     = toSockAddr ip
    family = sockAddrFamily sa
    open   = socket family Datagram defaultProtocol

----------------------------------------------------------------

-- | Receiving data with a listening UDP socket.
--   For a wildcard socket, recvmsg() is called.
--   For an interface specific socket, recvfrom() is called.
recvFrom :: ListenSocket -> IO (ByteString, ClientSockAddr)
recvFrom ListenSocket{..}
  | wildcard = do
        (bs,sa,cmsg,_) <- R.recvMsg listenSocket properUDPSize properCMSGSize 0
        return (bs, ClientSockAddr sa cmsg)
  | otherwise = do
        (bs,sa) <- R.recvFrom listenSocket properUDPSize
        return (bs, ClientSockAddr sa [])

-- | Sending data with a listening UDP socket.
--   For a wildcard socket, sendmsg() is called.
--   For an interface specific socket, sento() is called.
sendTo :: ListenSocket -> ByteString -> ClientSockAddr -> IO ()
sendTo ListenSocket{..} bs (ClientSockAddr sa cmsgs)
  | wildcard  = void $ NSB.sendMsg listenSocket sa [bs] cmsgs 0
  | otherwise = void $ NSB.sendTo  listenSocket bs sa

----------------------------------------------------------------

-- | Creating a connected UDP socket like TCP's accept().
accept :: ListenSocket -> ClientSockAddr -> IO UDPSocket
accept ListenSocket{..} (ClientSockAddr peersa cmsgs) = E.bracketOnError open NS.close $ \s -> do
    setSocketOption s ReuseAddr 1
    withFdSocket s setCloseOnExecIfNeeded
    let mysa' | wildcard  = getMySockAddr mySockAddr cmsgs
              | otherwise = anySockAddr mySockAddr
    -- wildcard:  (UDP, *.443, *:*) -> (UDP, 127.0.0.1:443, *:*)
    -- otherwise: (UDP, 127.0.0.1:443, *:*) -> (UDP, *:443, *:*)
    bind s mysa'
    -- bind and connect is not atomic
    -- So, bind may results in EADDRINUSE
       `E.catch` postphone (bind s mysa')
    connect s peersa  -- (UDP, 127.0.0.1:443, pa:pp)
    return $ UDPSocket s peersa True
  where
    postphone action e
      | E.ioeGetErrorType e == E.ResourceBusy = threadDelay 10000 >> action
      | otherwise                             = E.throwIO e
    family = sockAddrFamily mySockAddr
    open   = socket family Datagram defaultProtocol

----------------------------------------------------------------

-- | Creating a unconnected UDP socket.
clientSocket :: HostName -> ServiceName -> Bool -> IO UDPSocket
clientSocket host port conn = do
    addr <- head <$> NS.getAddrInfo (Just hints) (Just host) (Just port)
    E.bracketOnError (NS.openSocket addr) NS.close $ \s -> do
        let sa = addrAddress addr
        when conn $ NS.connect s sa
        return $ UDPSocket s sa conn
 where
    hints = NS.defaultHints { addrSocketType = Datagram }

-- | Sending data with a UDP socket.
--   If the socket is connected, send() is called.
--   Otherwise, sento() is called.
send :: UDPSocket -> (ByteString -> IO ())
send UDPSocket{..}
  | connected = \bs -> void $ NSB.send   udpSocket bs
  | otherwise = \bs -> void $ NSB.sendTo udpSocket bs peerSockAddr

-- | Receiving data with a UDP socket.
--   If the socket is connected, recv() is called.
--   Otherwise, recvfrom() is called.
recv :: UDPSocket -> IO ByteString
recv UDPSocket{..}
  | connected = R.recv udpSocket properUDPSize
  | otherwise = go
  where
    go = do
        (bs, sa) <- R.recvFrom udpSocket properUDPSize
        if sa == peerSockAddr then return bs else go

-- | Sending data in a buffer with a UDP socket.
--   If the socket is connected, send() is called.
--   Otherwise, sento() is called.
sendBuf :: UDPSocket -> (Ptr Word8 -> Int -> IO ())
sendBuf UDPSocket{..} ptr siz
  | connected = void $ NS.sendBuf   udpSocket ptr siz
  | otherwise = void $ NS.sendBufTo udpSocket ptr siz peerSockAddr

-- | Receiving data in a buffer with a UDP socket.
--   If the socket is connected, recv() is called.
--   Otherwise, recvfrom() is called.
recvBuf :: UDPSocket -> (Ptr Word8 -> Int -> IO Int)
recvBuf UDPSocket{..} ptr siz
  | connected = NS.recvBuf   udpSocket ptr siz
  | otherwise = go
  where
    go = do
        (len,sa) <- NS.recvBufFrom udpSocket ptr siz
        if sa == peerSockAddr then return len else go

-- | Closing a socket.
stop :: ListenSocket -> IO ()
stop (ListenSocket s _ _) = NS.close s

-- | Closing a socket.
close :: UDPSocket -> IO ()
close (UDPSocket s _ _) = NS.close s

-- | Emulation of NAT rebiding in the client side.
--   This is mainly used for test purposes.
natRebinding :: UDPSocket -> IO UDPSocket
natRebinding (UDPSocket _ sa conn) = E.bracketOnError open NS.close $ \s -> do
    when conn $ NS.connect s sa
    return $ UDPSocket s sa conn
  where
    family = sockAddrFamily sa
    open = NS.socket family Datagram NS.defaultProtocol

----------------------------------------------------------------

decideOption :: SockAddr -> SocketOption
decideOption SockAddrInet{}  = RecvIPv4PktInfo
decideOption SockAddrInet6{} = RecvIPv6PktInfo
decideOption _               = error "decideOption"

-- | Obtaining my sockaddr for a wildcard socket from cmsgs.
getMySockAddr :: SockAddr -> [Cmsg] -> SockAddr
getMySockAddr (SockAddrInet p _) cmsgs = SockAddrInet p addr
  where
    pktinfo = fromJust $ lookupCmsg CmsgIdIPv4PktInfo cmsgs
    IPv4PktInfo _ _ addr = fromJust $ decodeCmsg pktinfo
getMySockAddr (SockAddrInet6 p f _ sc) cmsgs = SockAddrInet6 p f addr sc
  where
    pktinfo = fromJust $ lookupCmsg CmsgIdIPv6PktInfo cmsgs
    IPv6PktInfo _ addr = fromJust $ decodeCmsg pktinfo
getMySockAddr _ _ = error "getMySockAddr"
