module Parser.Main where

import Data.Attoparsec.ByteString (word8, anyWord8, Parser, parseOnly)
import Data.ByteString (ByteString)
import qualified Parser.Ipv4 as Ipv4 (IpHdr, parser)
import qualified Parser.Ethernet as Ethernet (EthernetHdr, parser)
import qualified Parser.Tcp as Tcp (TcpHdr, parser)

data Packet = Packet {
  eth :: Ethernet.EthernetHdr,
  ip :: Ipv4.IpHdr,
  tcp :: Tcp.TcpHdr
}

instance Show Packet where
  show (Packet eth ip tcp) =
    show eth ++ "\n\n" ++ show ip ++ "\n\n" ++ show tcp

parsePacket :: ByteString -> IO (Either String Packet)
parsePacket rawBytes = return $ parseOnly mainParser rawBytes

mainParser :: Parser Packet
mainParser = do
  eth <- Ethernet.parser
  ip <- Ipv4.parser
  tcp <- Tcp.parser 0
  return $ Packet eth ip tcp
