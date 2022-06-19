module Parser.Main where

import Data.Attoparsec.ByteString (word8, anyWord8, Parser, parseOnly)
import Data.ByteString (ByteString)
import qualified Parser.Ipv4 as Ipv4 (IpHdr, parser)
import qualified Parser.Ethernet as Ethernet (EthernetHdr, parser)

data Packet = Packet {
  eth :: Ethernet.EthernetHdr,
  ip :: Ipv4.IpHdr
}

instance Show Packet where
  show (Packet eth ip) =
    show eth ++ "\n\n" ++ show ip

parsePacket :: ByteString -> IO (Either String Packet)
parsePacket rawBytes = return $ parseOnly mainParser rawBytes

mainParser :: Parser Packet
mainParser = do
  eth <- Ethernet.parser
  ip <- Ipv4.parser
  return $ Packet eth ip
