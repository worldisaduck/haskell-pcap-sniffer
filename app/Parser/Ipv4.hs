module Parser.Ipv4 (IpHdr, parser) where

import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString
import Control.Monad (replicateM)
import Data.Word (Word8, Word16)
import Data.Bits
import Numeric (showHex)
import qualified Parser.Utils as P

data IpHdr = IpHdr {
  version :: Word8,
  ihl :: Word8,
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

instance Show IpHdr where
  show (IpHdr version hdrLen typeOfService totalLen id flags offset ttl protocol checksum src dst) =
    let (_:dstStr) = foldl (\acc x -> acc ++ "." ++ show x) "" dst
        (_:srcStr) = foldl (\acc x -> acc ++ "." ++ show x) "" src
        len = show totalLen
     in
    "IpHdr \n" ++ "Destination IP: " ++ dstStr ++ "\n" ++ "Source IP: " ++ srcStr ++ " Total len: " ++ len ++ "\n"

parser :: Parser IpHdr
parser = do
  versionAndIHL <- anyWord8
  let version = versionAndIHL `shiftR` 4
  let ihl = versionAndIHL .&. 0x0F
  typeOfService <- anyWord8
  totalLen <- P.anyWord16
  id <- P.anyWord16
  flagsAndFragOffset <- P.anyWord16
  let flags = fromIntegral $ flagsAndFragOffset `shiftR` 13 :: Word8
  let fragOffset = flagsAndFragOffset .&. 0x1FFF
  ttl <- anyWord8
  protocol <- anyWord8
  checksum <- P.anyWord16
  src <- replicateM 4 anyWord8
  dst <- replicateM 4 anyWord8
  return $ (IpHdr version ihl typeOfService totalLen id flags fragOffset ttl protocol checksum src dst)
