{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Best current practice library for UDP servers.
--
--   * Efficient receiving function without memory copy.
--   * Proper buffer size.
--   * Type-safe APIs.
--   * TCP-like APIs (creating a UDP connection from a listing socket).
--
--   'Network.Socket.ByteString.recv' families use 'createAndTrim'
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
--   To know the background of TCP-like APIs, see:
--
--   * https://kazu-yamamoto.hatenablog.jp/entry/2022/02/25/153122
module Network.UDP.Server (
  -- * Wildcard socket
    ServerSocket(..)
  , serverSocket
  , ClientSockAddr(..)
  , recvMsg
  , sendMsg
  -- * Connected socket
  , ConnectedSocket(..)
  , accept
  , recv
  , send
  ) where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import Data.IP hiding (addr)
import qualified GHC.IO.Exception as E
import Network.Socket hiding (accept)
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
--   for 'recvMsg' and 'sendMsg'.
--   Optionally, a connected UDP socket can be created
--   with 'connectedSocket' as an emulation of TCP's accept().
data ServerSocket = ServerSocket Socket SockAddr Bool -- wildcard or not
                  deriving (Eq, Show)

-- | A connected UDP socket which are used with 'recv' and 'send'.
newtype ConnectedSocket = ConnectedSocket Socket deriving (Eq, Show)

-- | A client socket address from the server point of view.
data ClientSockAddr = ClientSockAddr SockAddr [Cmsg] deriving (Eq, Show)

----------------------------------------------------------------

-- | Creating a listening UDP socket.
serverSocket :: (IP, PortNumber) -> IO ServerSocket
serverSocket ip = E.bracketOnError open close $ \s -> do
    setSocketOption s ReuseAddr 1
    withFdSocket s setCloseOnExecIfNeeded
#if !defined(openbsd_HOST_OS)
    when (family == AF_INET6) $ setSocketOption s IPv6Only 1
#endif
    bind s sa
    let wildcard = isAnySockAddr sa
    return $ ServerSocket s sa wildcard
  where
    sa     = toSockAddr ip
    family = sockAddrFamily sa
    open   = socket family Datagram defaultProtocol

----------------------------------------------------------------

-- | Receiving data with a listening UDP socket.
--   For a wildcard socket, recvmsg() is called.
--   For an interface specific socket, recvfrom() is called.
recvMsg :: ServerSocket -> IO (ByteString, ClientSockAddr)
recvMsg (ServerSocket s _ wildcard)
  | wildcard = do
        (bs,sa,cmsg,_) <- R.recvMsg s properUDPSize properCMSGSize 0
        return (bs,ClientSockAddr sa cmsg)
  | otherwise = do
        (bs,sa) <- R.recvFrom s properUDPSize
        return (bs,ClientSockAddr sa [])

-- | Sending data with a listening UDP socket.
--   For a wildcard socket, sendmsg() is called.
--   For an interface specific socket, sento() is called.
sendMsg :: ServerSocket -> ByteString -> ClientSockAddr -> IO ()
sendMsg (ServerSocket s _ wildcard) bs (ClientSockAddr sa cmsgs)
  | wildcard  = void $ NSB.sendMsg s sa [bs] cmsgs 0
  | otherwise = void $ NSB.sendTo s bs sa

----------------------------------------------------------------

-- | Creating a connected UDP socket like TCP's accept().
accept :: ServerSocket -> ClientSockAddr -> IO ConnectedSocket
accept (ServerSocket _ mysa _) (ClientSockAddr peersa _) = E.bracketOnError open close $ \s -> do
    setSocketOption s ReuseAddr 1
    withFdSocket s setCloseOnExecIfNeeded
    let mysa' | isAnySockAddr mysa = mysa
              | otherwise          = anySockAddr mysa
    -- wildcard:  (UDP, *.443, *:*) -> (UDP, 127.0.0.1:443, *:*)
    -- otherwise: (UDP, 127.0.0.1:443, *:*) -> (UDP, *:443, *:*)
    bind s mysa'
    -- bind and connect is not atomic
    -- So, bind may results in EADDRINUSE
       `E.catch` postphone (bind s mysa')
    connect s peersa  -- (UDP, 127.0.0.1:443, pa:pp)
    return $ ConnectedSocket s
  where
    postphone action e
      | E.ioeGetErrorType e == E.ResourceBusy = threadDelay 10000 >> action
      | otherwise                             = E.throwIO e
    family = sockAddrFamily mysa
    open   = socket family Datagram defaultProtocol

----------------------------------------------------------------

-- | Sending data with a connected UDP socket.
--   Faster than other the send functions since
--   the socket is connected.
send :: ConnectedSocket -> ByteString -> IO ()
send (ConnectedSocket s) bs = void $ NSB.send s bs

-- | Receiving data with a connected UDP socket.
recv :: ConnectedSocket -> Int -> IO ByteString
recv (ConnectedSocket s) = R.recv s
