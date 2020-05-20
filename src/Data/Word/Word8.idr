module Data.Word.Word8

-- in Idris2 currently mod is really rem 
private
%foreign "scheme:modulo"
mod' : Int -> Int -> Int

private
mod256 : Int -> Int
mod256 x = mod' x 256

export
data Word8 : Type where
  W8 : Int -> Word8

private 
fromInt : Int -> Word8 
fromInt = W8 . mod256

export
implementation
Num Word8 where
  W8 x + W8 y = W8 $ mod256 (x + y)
  W8 x * W8 y = W8 $ mod256 (x * y)
  -- Careful, silently overflows.
  fromInteger x = W8 $ mod256 (cast x)

export
implementation
Integral Word8 where
  W8 x `mod` W8 y = W8 $ mod256 (x `mod` y)
  W8 x `div` W8 y = W8 $ mod256 (x `div` y)

export
implementation
Neg Word8 where
  negate (W8 x) = W8 $ mod256 (256 - x)
  W8 x - W8 y   = W8 $ mod256 (x - y)
  
export
implementation
Eq Word8 where
  W8 x == W8 y = x == y

export
implementation
Ord Word8 where
    W8 x `compare` W8 y = x `compare` y

export
implementation
Cast Int Word8 where
  cast = fromInt

export
implementation
Cast Word8 Int where
  cast (W8 i) = i