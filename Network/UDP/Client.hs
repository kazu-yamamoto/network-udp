-- | Best current practice library for UDP clients.
--
--   For more information, see:
--
--   * https://kazu-yamamoto.hatenablog.jp/entry/2021/06/29/134930
--   * https://www.iij.ad.jp/en/dev/iir/pdf/iir_vol52_focus2_EN.pdf (Sec 3.9)
module Network.UDP.Client (
  -- * Unconnected socket
    UnconnectedSocket
  , ServerSockAddr(..)
  , unconnectedSocket
  , sendTo
  , recvFrom
  -- * Connected socket
  , ConnectedSocket
  , ServerSockAddrC
  , connect
  , send
  , recv
  -- * NAT rebinding
  , natRebindingUnconnectedSocket
  , natRebindingConnectedSocket
  ) where

import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import qualified Network.Socket as NS
import Network.Socket hiding (connect)
import qualified Network.Socket.ByteString as NSB

import Network.UDP.Types
import qualified Network.UDP.Recv as R

-- | Creating a unconnected UDP socket.
unconnectedSocket :: HostName -> ServiceName -> IO (UnconnectedSocket,SockAddr)
unconnectedSocket host port = do
    addr <- head <$> getAddrInfo (Just hints) (Just host) (Just port)
    E.bracketOnError (openSocket addr) close $ \s -> do
        let sa = addrAddress addr
        return (UnconnectedSocket s,sa)
 where
    hints = defaultHints { addrSocketType = Datagram }

-- | Sending data with a unconnected UDP socket.
sendTo :: UnconnectedSocket -> ByteString -> ServerSockAddr -> IO ()
sendTo (UnconnectedSocket s) bs (ServerSockAddr sa) = void $ NSB.sendTo s bs sa

-- | Receiving data with a unconnected UDP socket.
recvFrom :: UnconnectedSocket -> IO (ByteString, ServerSockAddr)
recvFrom (UnconnectedSocket s) = do
    (bs, sa) <- R.recvFrom s properUDPSize
    return (bs, ServerSockAddr sa)

-- | Creating a connected UDP socket.
connect :: UnconnectedSocket -> ServerSockAddr -> IO (ConnectedSocket, ServerSockAddrC)
connect (UnconnectedSocket s) (ServerSockAddr sa) = do
    NS.connect s sa
    return (ConnectedSocket s, ServerSockAddrC sa)

-- | Emulation of NAT rebiding in the client side.
--   This is mainly used for test purposes.
natRebindingUnconnectedSocket :: ServerSockAddr -> IO UnconnectedSocket
natRebindingUnconnectedSocket (ServerSockAddr sa) = E.bracketOnError open close $ \s ->
    return $ UnconnectedSocket s
  where
    family = sockAddrFamily sa
    open = socket family Datagram defaultProtocol

-- | Emulation of NAT rebiding in the client side.
--   This is mainly used for test purposes.
natRebindingConnectedSocket :: ServerSockAddrC -> IO ConnectedSocket
natRebindingConnectedSocket (ServerSockAddrC sa) = E.bracketOnError open close $ \s -> do
    NS.connect s sa
    return $ ConnectedSocket s
  where
    family = sockAddrFamily sa
    open = socket family Datagram defaultProtocol
