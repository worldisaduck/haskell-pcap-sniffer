module Parser.Ethernet where

import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString
import Control.Monad (replicateM)
import Data.Word (Word8, Word16)
import Numeric (showHex)
import Control.Applicative ((<|>))
import Parser.Utils (anyWord16, word16, satisfyWithWord16)

data EtherType = IPv4 | ARP | WoL | AVTP | Other deriving (Show, Eq)

data EthernetHdr = EthernetHdr {
  ethernetDst :: [Word8],
  ethernetSrc :: [Word8],
  ethernetType :: EtherType
}

instance Show EthernetHdr where
  show (EthernetHdr dst src type_) =
    let (_:dstStr) = foldl (\acc x -> acc ++ ":" ++ showHex x "") "" dst
        (_:srcStr) = foldl (\acc x -> acc ++ ":" ++ showHex x "") "" src
        typeStr = show type_
    in
    "EthernetHdr \n" ++ "Destination MAC: " ++ dstStr ++ "\n" ++ "Source MAC: " ++ srcStr ++ "\n" ++ "Type: " ++ typeStr

parser :: Parser EthernetHdr
parser = do
  dst <- replicateM 6 anyWord8
  src <- replicateM 6 anyWord8
  etherType <- etherType
  return $ EthernetHdr dst src etherType

etherType :: Parser EtherType
etherType = do
  anyWord16 >>= return . matchEtherType

matchEtherType :: Word16 -> EtherType
matchEtherType 0x0800 = IPv4
matchEtherType 0x0806 = ARP
matchEtherType _ = Other
