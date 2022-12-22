module Network.UDP.Types where

import Network.Socket

properUDPSize :: Int
properUDPSize = 2048

properCMSGSize :: Int
properCMSGSize = 64

sockAddrFamily :: SockAddr -> Family
sockAddrFamily SockAddrInet{}  = AF_INET
sockAddrFamily SockAddrInet6{} = AF_INET6
sockAddrFamily _               = error "sockAddrFamily"
