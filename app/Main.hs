module Main where

import Network.Pcap
import Numeric (showHex)
import Data.List (foldl)
import Data.Bits
import Data.Word
import qualified Data.ByteString as BS
import Control.Monad (replicateM, replicateM_)
import Foreign.Ptr
import Foreign.Storable (peek, peekByteOff)
import Control.Monad.State.Lazy (StateT, evalStateT, get, put, state, liftIO, lift)
import Text.Hex
import Text.Printf
import qualified Data.Text as T
import qualified Parser.Main as P

ethernetHeaderLength = 14
ipHeaderLength = 20
tpcHeaderLength = 24

-- dump = "64eeb7ef19d00800"

main :: IO ()
main = do
  devices <- findAllDevs
  let deviceName = ifName . head $ devices
  handle <- openLive deviceName 4096 True 0
  loop handle (-1) handlePacket
  putStrLn "Capturing started"

handlePacket :: PktHdr -> Ptr Word8 -> IO ()
handlePacket (PktHdr _ _ _ hdrCaptureLength) startPtr= do
  putStrLn "START OF THE PACKET\n"
  rawPacket <- evalStateT getPacketByteString startPtr
  parsedPacket <- P.parsePacket rawPacket
  putStrLn $ show parsedPacket
  putStrLn "END OF THE PACKET\n"
  return ()

getPacketByteString :: StateT (Ptr Word8) IO ByteString
getPacketByteString = do
  bs <- readNBytes $ ethernetHeaderLength + ipHeaderLength
  return $ BS.pack bs

readNBytes :: Int -> StateT (Ptr Word8) IO [Word8]
readNBytes n = do
  ptr <- get
  words <- lift $ evalStateT (replicateM n readByte) ptr
  return words

readByte :: StateT (Ptr Word8) IO Word8
readByte = do
  ptr <- get
  byte <- lift $ peek (ptr :: Ptr Word8)
  put $ ptr `plusPtr` 1
  return byte
