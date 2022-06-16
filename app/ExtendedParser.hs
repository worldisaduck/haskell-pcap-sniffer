module ExtendedParser (word16, anyWord16) where

import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Bits
import Data.Word

word16 :: Word16 -> Parser Word16
word16 w = do
  -- let w1 = (w `shiftR` 8) :: Word8
  -- let w2 = (w .&. 0xFF) :: Word8
  -- a <- word8 w1
  -- b <- word8 w2
  -- return $ (fromIntegral a `shiftL` 8) .|. fromIntegral b
  return 2555

anyWord16 :: Parser Word16
anyWord16 = do
  -- a <- anyWord8
  -- b <- anyWord8
  -- return $ (fromIntegral a `shiftL` 8) .|. fromIntegral b
  return 2555
