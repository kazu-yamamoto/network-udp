module Network.UDP.Types where

import Control.Monad
import Data.ByteString (ByteString)
import Network.Socket
import qualified Network.Socket.ByteString as NSB

import qualified Network.UDP.Recv as R

properUDPSize :: Int
properUDPSize = 2048

properCMSGSize :: Int
properCMSGSize = 64

sockAddrFamily :: SockAddr -> Family
sockAddrFamily SockAddrInet{}  = AF_INET
sockAddrFamily SockAddrInet6{} = AF_INET6
sockAddrFamily _               = error "sockAddrFamily"

-- | A connected UDP socket which are used with 'recv' and 'send'.
newtype ConnectedSocket = ConnectedSocket Socket deriving (Eq, Show)

-- | A unconnected UDP socket for clients.
newtype UnconnectedSocket = UnconnectedSocket Socket deriving (Eq, Show)

-- | A server socket address from the client point of view.
newtype ServerSockAddr = ServerSockAddr SockAddr deriving (Eq, Show)

-- | A server socket address from the client point of view.
--   This is for a connected socket.
newtype ServerSockAddrC = ServerSockAddrC SockAddr deriving (Eq, Show)

-- | A client socket address from the server point of view.
newtype ClientSockAddr = ClientSockAddr SockAddr deriving (Eq, Show)

-- | Sending data with a connected UDP socket.
--   Faster than other the send functions since
--   the socket is connected.
send :: ConnectedSocket -> ByteString -> IO ()
send (ConnectedSocket s) bs = void $ NSB.send s bs

-- | Receiving data with a connected UDP socket.
recv :: ConnectedSocket -> Int -> IO ByteString
recv (ConnectedSocket s) = R.recv s
