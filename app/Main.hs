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
import Parser.Ipv4

ethernetHeaderLength = 14
ipHeaderLength = 24

data EthernetHdr = EthernetHdr {
    ethernetDst :: [Word8],
    ethernetSrc :: [Word8],
    ethernetType :: Word16
}

data IpHdr = IpHdr {
    version :: Word8,
    hdrLen :: Word8,
    typeOfService :: Word8,
    totalLen :: Word16,
    id :: Word16,
    flags :: Word8,
    fragOffset :: Word16,
    ttl :: Word8,
    protocol :: Word8,
    checksum :: Word16,
    src :: [Word8],
    dst :: [Word8]
}

-- data TcpHdr = TcpHdr {
--     srcPort :: Word16,
--     dstPort :: Word16,
--     seqNum :: Word32,
--     ackNum :: Word32,
--     urg :: Bool,
--     ack :: Bool,
--     psh :: Bool,
--     pst :: Bool,
--     syn :: Bool,
--     fin :: Bool,
--     window :: Word16,
--     checksum :: Word16,
--     urgPointer :: Word16,
--     data :: [Word8]
-- }

data RawPacket = RawPacket {
  rawEth :: [Word8],
  rawIp :: [Word8]
} deriving (Show)

data Packet = Packet {
  eth :: EthernetHdr,
  ip :: IpHdr
}

dump = "64eeb7ef19d00800"

instance Show EthernetHdr where
  show (EthernetHdr dst src type_) =
    let (_:dstStr) = foldl (\acc x -> acc ++ ":" ++ showHex x "") "" dst
        (_:srcStr) = foldl (\acc x -> acc ++ ":" ++ showHex x "") "" src
        typeStr = showHex type_ ""
     in
    "EthernetHdr \n" ++ "Destination MAC: " ++ dstStr ++ "\n" ++ "Source MAC: " ++ srcStr ++ "\n" ++ "Type: " ++ typeStr

instance Show IpHdr where
  show (IpHdr version hdrLen typeOfService totalLen id flags offset ttl protocol checksum src dst) =
    let (_:dstStr) = foldl (\acc x -> acc ++ "." ++ show x) "" dst
        (_:srcStr) = foldl (\acc x -> acc ++ "." ++ show x) "" src
        len = showHex totalLen ""
     in
    "IpHdr \n" ++ "Destination IP: " ++ dstStr ++ "\n" ++ "Source IP: " ++ srcStr ++ " Total len: " ++ len ++ "\n"

main :: IO ()
main = do
  devices <- findAllDevs
  let deviceName = ifName . head $ devices
  handle <- openLive deviceName 4096 True 0
  loop handle (-1) handlePacket
  putStrLn "Capturing started"

handlePacket :: PktHdr -> Ptr Word8 -> IO ()
handlePacket (PktHdr _ _ _ hdrCaptureLength) startPtr= do
  putStrLn "START OF THE PACKET"
  rawPacket <- evalStateT getRawPacket startPtr
  putStrLn $ show (BS.pack (rawEth rawPacket))
  putStrLn $ show (BS.pack (rawIp rawPacket))
  putStrLn "END OF THE PACKET"
  return ()

getRawPacket :: StateT (Ptr Word8) IO RawPacket
getRawPacket = do
  ethernetHdr <- readNBytes ethernetHeaderLength
  ipHdr <- readNBytes ipHeaderLength
  return $ RawPacket ethernetHdr ipHdr

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
