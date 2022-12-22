{-# LANGUAGE OverloadedStrings #-}

module Network.UDP.Recv (
    recv
  , recvFrom
  , recvMsg
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Internal (create, ByteString(..), createUptoN)
import Foreign.ForeignPtr (withForeignPtr)
import Network.Socket (Socket, SockAddr, Cmsg, MsgFlag, recvBuf, recvBufFrom, recvBufMsg)
import Network.Socket.Internal (zeroMemory)
import System.IO.Error (ioeSetErrorString, mkIOError)
import GHC.IO.Exception (IOErrorType(..))

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = ioeSetErrorString err "non-positive length"
  where
    err = mkIOError InvalidArgument loc Nothing Nothing

recv :: Socket -> Int -> IO ByteString
recv s siz
  | siz < 0   = ioError (mkInvalidRecvArgError "Network.UDP.Recv.recv")
  | otherwise = createUptoN siz $ \ptr -> recvBuf s ptr siz

recvFrom :: Socket -> Int -> IO (ByteString, SockAddr)
recvFrom s siz = do
    bs@(PS fptr _ _) <- create siz $ \ptr -> zeroMemory ptr (fromIntegral siz)
    withForeignPtr fptr $ \ptr -> do
        (len, sa) <- recvBufFrom s ptr siz
        let bs' | len < siz = PS fptr 0 len
                | otherwise = bs
        return (bs', sa)

recvMsg :: Socket -> Int -> Int -> MsgFlag
        -> IO (ByteString, SockAddr, [Cmsg], MsgFlag)
recvMsg s siz clen flags = do
    bs@(PS fptr _ _) <- create siz $ \ptr -> zeroMemory ptr (fromIntegral siz)
    withForeignPtr fptr $ \ptr -> do
        (addr,len,cmsgs,flags') <- recvBufMsg s [(ptr,siz)] clen flags
        let bs' | len < siz = PS fptr 0 len
                | otherwise = bs
        return (bs', addr, cmsgs, flags')
