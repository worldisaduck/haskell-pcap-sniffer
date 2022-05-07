module Main where

import Network.Pcap
import Numeric (showHex)
import Data.List (foldl)
import Data.Bits
import Data.Word
import Control.Monad (replicateM, replicateM_)
import Foreign.Ptr
import Foreign.Storable (peek, peekByteOff)
import Control.Monad.State.Lazy (StateT, evalStateT, get, put, state, liftIO, lift)

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
--

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
  loop handle (1) handlePacket
  putStrLn "Capturing started"

getWord8S :: StateT (Ptr Word8) IO Word8
getWord8S = do
  ptr <- get
  byte <- lift $ peek ptr
  put $ plusPtr ptr 1
  return $ byte

getWord16S :: StateT (Ptr Word8) IO Word16
getWord16S = do
  ptr <- get
  byte1 <- lift $ peek (ptr :: Ptr Word8)
  byte2 <- lift $ peek (plusPtr ptr 2 :: Ptr Word8)
  put $ plusPtr ptr 2
  let word16 = (fromIntegral byte1) + (shift (fromIntegral byte2 :: Word16) 8)
  return word16

getNWord8S :: Int -> StateT (Ptr Word8) IO [Word8]
getNWord8S n = do
  bytes <- replicateM n getWord8S
  return bytes

handlePacket :: PktHdr -> Ptr Word8 -> IO ()
handlePacket (PktHdr _ _ _ hdrCaptureLength) startPtr= do
  evalStateT (replicateM_ 14 printByte) startPtr
  -- firstByte <- peek startPtr
  -- putStrLn $ "Packet captured of length " ++ show hdrCaptureLength
  -- ethernetHdr <- evalStateT getEthernetHdrS startPtr
  -- ipHdr <- evalStateT getIpHdrS (plusPtr startPtr ethernetHeaderLength)
  -- putStrLn $ show ethernetHdr
  -- putStrLn $ "\n"
  -- putStrLn $ show ipHdr
  -- putStrLn $ "\n"

printByte :: StateT (Ptr Word8) IO ()
printByte = do
  ptr <- get
  byte1 <- lift $ peek (ptr :: Ptr Word8)
  put $ plusPtr ptr 1
  liftIO $ (putStr $ (showHex byte1 "") ++ " ")
  return ()

getEthernetHdrS :: StateT (Ptr Word8) IO EthernetHdr
getEthernetHdrS = do
  dst <- getNWord8S 6
  src <- getNWord8S 6
  type_ <- getWord16S
  return $ EthernetHdr dst src type_

getIpHdrS :: StateT (Ptr Word8) IO IpHdr
getIpHdrS = do
  versionAndHdrLen <- getWord8S
  typeOfService <- getWord8S
  totalLen <- getWord16S
  id <- getWord16S
  flagsAndOffset <- getWord16S
  ttl <- getWord8S
  protocol <- getWord8S
  checksum <- getWord16S
  src <- getNWord8S 4
  dst <- getNWord8S 4
  let version = versionAndHdrLen `shiftR` 4
      hdrLen = versionAndHdrLen .&. 0x0F
      flags = fromIntegral $ flagsAndOffset `shiftR` 13 :: Word8
      offset = flagsAndOffset .&. 0x1FFF
  return $ IpHdr version hdrLen typeOfService totalLen id flags offset ttl protocol checksum src dst
