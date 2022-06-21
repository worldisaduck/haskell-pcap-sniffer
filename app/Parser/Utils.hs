module Parser.Utils (word16, anyWord16, satisfyWithWord16, anyWord32) where

import Data.Attoparsec.ByteString (word8, anyWord8, Parser)
import Data.ByteString (ByteString)
import Data.Bits
import Data.Word

word16 :: Word16 -> Parser Word16
word16 w = do
  let w1 = fromIntegral $ w `shiftR` 8 :: Word8
  let w2 = fromIntegral $ w .&. 0xFF :: Word8
  a <- word8 w1
  b <- word8 w2
  return $ (fromIntegral a `shiftL` 8) .|. fromIntegral b

anyWord16 :: Parser Word16
anyWord16 = do
  a <- anyWord8
  b <- anyWord8
  return $ (fromIntegral a `shiftL` 8) .|. fromIntegral b

satisfyWithWord16 :: (Word16 -> a) -> (a -> Bool) -> Parser a
satisfyWithWord16 f p = do
  w <- anyWord16
  let a = f w
  if p a
     then return a
     else fail "satisfyWithWord16"

anyWord32 :: Parser Word32
anyWord32 = do
  a <- anyWord16
  b <- anyWord16
  return $ (fromIntegral a `shiftL` 16) .|. fromIntegral b
