module Parser.Ipv4 where

import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString (Parser)

-- data IpHdr = IpHdr {
--   version :: Word8,
--   hdrLen :: Word8,
--   typeOfService :: Word8,
--   totalLen :: Word16,
--   id :: Word16,
--   flags :: Word8,
--   fragOffset :: Word16,
--   ttl :: Word8,
--   protocol :: Word8,
--   checksum :: Word16,
--   src :: [Word8],
--   dst :: [Word8]
-- }

-- parser :: ByteString -> Parser
-- parser rawData =

