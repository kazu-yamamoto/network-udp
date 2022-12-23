-- | Best current practice library for UDP clients.
--
--   For more information, see:
--
--   * https://kazu-yamamoto.hatenablog.jp/entry/2021/06/29/134930
--   * https://www.iij.ad.jp/en/dev/iir/pdf/iir_vol52_focus2_EN.pdf (Sec 3.9)
module Network.UDP.Client (
  -- * Client socket
    ClientSocket(..)
  , clientSocket
  -- * Sending and receiving
  , send
  , sendBuf
  , recv
  , recvBuf
  -- * Closing
  , close
  -- * NAT rebinding
  , natRebinding
  ) where

import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import Network.Socket (Socket, SockAddr, HostName, ServiceName, SocketType(..),AddrInfo(..))
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import Foreign.Ptr (Ptr)

import Network.UDP.Types
import qualified Network.UDP.Recv as R

data ClientSocket = ClientSocket Socket SockAddr Bool -- connected or not
                  deriving (Eq, Show)

-- | Creating a unconnected UDP socket.
clientSocket :: HostName -> ServiceName -> Bool -> IO ClientSocket
clientSocket host port conn = do
    addr <- head <$> NS.getAddrInfo (Just hints) (Just host) (Just port)
    E.bracketOnError (NS.openSocket addr) NS.close $ \s -> do
        let sa = addrAddress addr
        when conn $ NS.connect s sa
        return $ ClientSocket s sa conn
 where
    hints = NS.defaultHints { addrSocketType = Datagram }

send :: ClientSocket -> (ByteString -> IO ())
send (ClientSocket s sa conn)
  | conn      = \bs -> void $ NSB.send s bs
  | otherwise = \bs -> void $ NSB.sendTo s bs sa

recv :: ClientSocket -> IO ByteString
recv (ClientSocket s sa conn)
  | conn      = R.recv s properUDPSize
  | otherwise = go
  where
    go = do
        (bs, sa') <- R.recvFrom s properUDPSize
        if sa == sa' then return bs else go

sendBuf :: ClientSocket -> (Ptr a -> Int -> IO ())
sendBuf = undefined

recvBuf :: ClientSocket -> (Ptr a -> Int -> IO Int)
recvBuf = undefined

close :: ClientSocket -> IO ()
close (ClientSocket s _ _) = NS.close s

-- | Emulation of NAT rebiding in the client side.
--   This is mainly used for test purposes.
natRebinding :: ClientSocket -> IO ClientSocket
natRebinding (ClientSocket _ sa conn) = E.bracketOnError open NS.close $ \s -> do
    when conn $ NS.connect s sa
    return $ ClientSocket s sa conn
  where
    family = sockAddrFamily sa
    open = NS.socket family Datagram NS.defaultProtocol
