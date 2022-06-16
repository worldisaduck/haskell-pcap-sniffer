module Parser.Ipv4 where

import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString
import Data.Word (Word8, Word16)
import qualified ExtendedParser as P

data IpHdr = IpHdr {
  version :: Word8,
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

parser :: Parser IpHdr
parser = do
  version <- anyWord8
  hdrLen <- anyWord8
  typeOfService <- anyWord8
  totalLen <- P.anyWord16
  id <- P.anyWord16
  flags <- anyWord8
  fragOffset <- P.anyWord16
  ttl <- anyWord8
  protocol <- anyWord8
  checksum <- P.anyWord16
  src <- replicateM 4 anyWord8
  dst <- replicateM 4 anyWord8
  return $ IpHdr version hdrLen typeOfService totalLen id flags fragOffset ttl protocol checksum src dst
