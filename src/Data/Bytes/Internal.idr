module Data.Bytes.Internal

import Data.Bytes.Util
import Data.Bytes.Prim

import Data.Word.Word8

import Data.So

-- Unfortunately neccesary addition atm, no idea why. For some reason things
-- from Data.Buffer are in scope.
-- For instance I shouldn't be able to define Bytes here as it uses Buffer
-- from Data.Bytes which is not re-exported from Data.Bytes.Prim
-- Supposedly this is also occuring in contrib:Data/List/TailRec.idr
%hide Data.Buffer.getByte

private
moduleName : String
moduleName = "Data.Bytes.Internal"

-- Our Bytes type, a Ptr to a `block` of memory, its size in bytes,
-- and the current 0-based offset into that memory.
public export
data Bytes : Type where
     MkB : (b : Buffer) -> (pos : Int) -> (len : Int) -> Bytes

-- It'd be nice to be able to provide NonEmpty based on `len` but it's quite a
-- hassle to have a Nat here with the various castings around the codebase and
-- other concerns.

public export
data NonEmpty : Bytes -> Type where
  IsNonEmpty : So (len > 0) -> NonEmpty (MkB _ _ len)

public export
data NonEmpty' : Bytes -> Type where
  IsNonEmpty' : So (len > 0) => NonEmpty' (MkB _ _ len)

-- ^ how can I provide this proof without making the user need access
-- to MkB? Simply by having my own functions imlpicity prove that when
-- used? Things like cons and singleton and append? And if a
-- ByteString comes from elsewhere we use Dec?

export -- Does this need public export or is being Dec enough for proofs?
nonEmpty : (b : Bytes) -> Dec (NonEmpty b)
nonEmpty (MkB _ _ len) with (choose (len > 0))
  nonEmpty (MkB _ _ len) | (Left t) = Yes (IsNonEmpty t)
  nonEmpty (MkB _ _ len) | (Right f) = No (\(IsNonEmpty t) => soNotSonot t f)
  where
    soNotSonot : So x -> So (not x) -> Void -- So x -> Not (So (not x))
    soNotSonot Oh Oh impossible

----------------------------------------------------------------------




{- NB: In the bytestring lib of haskell they use unsafePerformIO
because of how the strictness analzer and sharing works, they note:
The use of unsafePerformIO in allocating functions (unsafeCreate) is critical!

Otherwise:
 singleton 255 `compare` singleton 127
is compiled to:
 case mallocByteString 2 of
     ForeignPtr f internals ->
          case writeWord8OffAddr# f 0 255 of _ ->
          case writeWord8OffAddr# f 0 127 of _ ->
          case eqAddr# f f of
                 False -> case compare (GHC.Prim.plusAddr# f 0)
                                       (GHC.Prim.plusAddr# f 0)
TODO: Find out if this distinction is true in idris.
-}

export
unsafeCreateBytes : Int -> (MutBuffer -> IO ()) -> Bytes
unsafeCreateBytes len f = MkB (unsafePerformIO $ allocateAndFill len f) 0 len

export
unsafeCreateNBytes : Int -> (MutBuffer -> IO Int) -> Bytes
unsafeCreateNBytes len f = unsafePerformIO $ do
    (b, i) <- allocateAndFillToN len f
    pure (MkB b 0 i)

export
unsafeCreateNBytes' : Int -> (MutBuffer -> IO (Int,a)) -> (Bytes, a)
unsafeCreateNBytes' len f = unsafePerformIO $ do
    (b, i, a) <- allocateAndFillToN' len f
    pure ((MkB b 0 i), a)

export
unsafeCreateAndTrim : Int -> (MutBuffer -> IO Int) -> Bytes
unsafeCreateAndTrim len0 f = unsafePerformIO $ do
    (b, len) <- allocateAndTrim len0 f
    pure (MkB b 0 len)


-- This is internal so NonEmpty is used to keep the library maker honest.
-- TODO: Why is NonEmpty for List available here? I have not imported
-- Data.List!
packUpToNBytes : Int -> (l : List Word8) -> NonEmpty l => (Bytes, List Word8)
packUpToNBytes len xs0 = unsafeCreateNBytes' len $ \p => go p len 0 xs0
  where
    go : MutBuffer -> Int -> Int -> List Word8 -> IO (Int, List Word8)
    go b n pos [] = pure (len - n, [])
    go b 0 pos xs = pure (len, xs)
    go b n pos (x :: xs) = setByte b pos x *> go b (n-1) (pos+1) xs


-- Unpacking bytestrings into lists effeciently is a tradeoff: on the one hand
-- we would like to write a tight loop that just blats the list into memory, on
-- the other hand we want it to be unpacked lazily so we don't end up with a
-- massive list data structure in memory.
--
-- Our strategy is to combine both: we will unpack lazily in reasonable sized
-- chunks, where each chunk is unpacked strictly.
--
-- unpackBytes and unpackChars do the lazy loop, while unpackAppendBytes and
-- unpackAppendChars do the chunks strictly.

-- unpackBytes : Bytes -> List Word8



-- Intended complexity: O(n+m)
-- No need to export, simply use <+>
private
append : Bytes -> Bytes -> Bytes
append (MkB xs xpos xlen) (MkB ys ypos ylen)
  = unsafeCreateBytes (xlen + ylen) $ \p => do
      copyBuffer xs xpos xlen p 0
      copyBuffer ys ypos ylen p xlen

private
show_buffer : Buffer -> String
show_buffer buf
    = "[" ++ fastAppend (intersperse "," (map show (unsafePerformIO (bufferData buf)))) ++ "]"


-- Debug use ONLY, **don't ever use this**, bytes are not characters!
-- TODO: Remove this down the road, or  otherwise prevent it from escaping this
-- package. If a person wants to Show/Read/OverloadedString Bytes they should
-- create a principled package that does this.
export
Show Bytes where
  show (MkB b pos len) = "MkB " ++ show_buffer b ++ " " ++ show pos ++ " " ++ show len

private
compareBytes : Bytes -> Bytes -> Ordering
compareBytes (MkB _   _    0)    (MkB _   _    0)    = EQ
compareBytes (MkB xb xpos xlen) (MkB yb ypos ylen) =
    unsafePerformIO $ go (xpos+xlen-1) (ypos+ylen-1) xb yb
  where
    go : Int -> Int -> Buffer -> Buffer -> IO Ordering
    go 0 0 xb yb = [| getByte xb xpos `compare` getByte yb ypos |]
    go 0 _ _ _   = pure LT
    go _ 0 _ _   = pure GT
    go k j xb yb
      = do EQ <- [| getByte xb k `compare` getByte yb j |]
             | res => pure res
           go (k-1) (j-1) xb yb


-- Offset will tend to be 0 so we first compare length, then buffer object
-- equality, then offset. The buffer should be a simple ptr comparison so it's
-- worth doing before trying for a full compareBytes. This order is partly
-- because it makes sense and partly because ByteString does it this way.
-- However I'm not sure why length is checked before buffer object equality,
-- perhaps in ghc length has a cheaper comparison than than the extra
-- foreignPtr prodding that ptr equality needs.

-- To ask if we are nominally equal.
infixl 9 `basicEq`
private
basicEq : Bytes -> Bytes -> Bool
MkB xb xpos xlen `basicEq` MkB yb ypos ylen
    = xlen == ylen && (unsafePerformIO (exactEqBuff xb yb) && xpos == ypos)

export
implementation
Eq Bytes where
  x == y = x `basicEq` y || compareBytes x y == EQ

-- basicEq is much speedier than comparison, use it when possible!
export
Ord Bytes where
  compare = compareBytes
  x >= y = x `basicEq` y || compareBytes x y == GT
  x <= y = x `basicEq` y || compareBytes x y == LT

export
Semigroup Bytes where
  (<+>) = append

-- Is there any way to make this a unique object? There's no real reason to
-- have this be anything other than the null ptr. Or at the very least there's
-- no reason to ever create more than one.
-- TODO: Investigate this as backends develop ^
export
Monoid Bytes where
  neutral = unsafeCreateBytes 0 (\_ => pure ())

-- Other things down the road that might need to concat, e.g.
-- sconcat/mconcat/foldMap should use this since it avoids a ton of copying by checking the required size upfront.

-- The idea here is to first compute the size of the Bytes we need to make and
-- then fill it. This makes it significantly faster than piece-wise methods
-- that must copy to build up.
concat : List Bytes -> Bytes
concat bss = goLen0 bss bss
  where
    mutual
      -- Check if we're a '0', meaning if we can skip
      goLen0 : List Bytes -> List Bytes -> Bytes
      goLen0 bs0 [] = neutral
      goLen0 bs0 (MkB _ _ 0 :: bs) = goLen0 bs0 bs -- skip empty
      goLen0 bs0 (b :: bs) = goLen1 bs0 b bs

      -- We have at least one, so carry on from there
      goLen1 : List Bytes -> Bytes -> List Bytes -> Bytes
      goLen1 bs0 b [] = b -- there was only one Bytes in the list
      goLen1 bs0 b (MkB _ _ 0 :: bs) = goLen1 bs0 b bs -- skip empty
      goLen1 bs0 b (MkB _ _ len :: xs) = ?dsfsfd_33

      goLen  : List Bytes -> Int -> List Bytes -> Bytes
      goLen bss0 len (x :: xs) = ?dsfsfdef_2
      goLen bss0 len [] -- [] means we're done counting, time to copy!
        = ?dsfsfdef_1

      goCopy : Bytes -> Bytes -> IO ()
      
      
