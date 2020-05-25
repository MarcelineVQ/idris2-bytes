module Data.Word.Word8

-- Not yet, kinks to work out with totality checking
-- %default total

-- TODO: Add modulo to base to incentivise backend support.
-- In Idris2 currently mod is really rem!
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

%tcinline
private
fromInteger' : Integer -> Word8 
fromInteger' = W8 . mod256 . cast

 -- might need to %tcinline this, depends how private affects things
private
%inline -- %spec or %inline? TODO: test each
word8Over : (f : Int -> Int -> Int) -> Word8 -> Word8 -> Word8
word8Over f (W8 x) (W8 y) = W8 $ mod256 (x `f` y)

export
implementation
Num Word8 where
  (+) = word8Over (+)
  (*) = word8Over (*)
  -- Careful, silently overflows as a literal.
  fromInteger x = if x >= 0 && x < 256
                    then W8 $ cast x
                    else fromInteger' x

export
implementation
Integral Word8 where
  mod = word8Over mod
  div = word8Over div

export
implementation
Neg Word8 where
  negate = word8Over (-) 0
  (-)    = word8Over (-)

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

export
Show Word8 where
  show (W8 i) = show i

