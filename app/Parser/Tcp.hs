module Parser.Tcp where

import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString
import Control.Monad (replicateM)
import Data.Word (Word8, Word16, Word32)
import Data.Bits
import Numeric (showHex)
import Parser.Utils

data TcpHdr = TcpHdr {
  srcPort :: Word16,
  dstPort :: Word16,
  seqNum :: Word32,
  ackNum :: Word32,
  urg :: Bool,
  ack :: Bool,
  psh :: Bool,
  pst :: Bool,
  syn :: Bool,
  fin :: Bool,
  window :: Word16,
  checksum :: Word16,
  urgPointer :: Word16,
  data :: [Word8]
}

parser :: Int -> Parser TcpHdr
parser = dataLength do
  srcPort <- anyWord16
  dstPort <- anyWord16
  seqNum <- anyWord32
  ackNum <- anyWord32
  offsetReservedFlags <- anyWord16
  let flags = fromIntegral $ offsetReservedFlags .&. 0x3F :: Word8
  let (urg, ack, psh, pst, syn, fin) = parseFlags flags
  window <- anyWord16
  checksum <- anyWord16
  urgPointer <- anyWord16
  optionsPadding <- anyWord32
  _data <- replicateM dataLength anyWord8

parseFlags :: Word8 -> (Bool, Bool, Bool, Bool, Bool, Bool)
parseFlags flagsBits =
  let urg = flagsBits .&. 0x20 == 0x20
      ack = flagsBits .&. 0x10 == 0x10
      psh = flagsBits .&. 0x08 == 0x08
      pst = flagsBits .&. 0x04 == 0x04
      syn = flagsBits .&. 0x02 == 0x02
      fin = flagsBits .&. 0x01 == 0x01
  in
  (urg, ack, psh, pst, syn, fin)
