module Data.Bytes.Strict.Internal

import Data.Bytes.Util
import Data.Bytes.Prim

import Data.Word.Word8

import Data.Strings -- fastAppend
import Data.List    -- intersperse
import Data.So      -- for our NonEmpty


private
moduleName : String
moduleName = "Data.Bytes.Strict.Internal"

-- Our Bytes type, a Ptr to a `block` of memory, the current 0-based offset
-- into that memory, its size in bytes.
public export
data Bytes : Type where
  MkB : (b : Block) -> (pos : Int) -> (len : Int) -> Bytes

-- It'd be nice to be able to provide NonEmpty based on `len` but it's quite a
-- hassle to have a Nat here with the various castings around the codebase and
-- other concerns.

public export
data NonEmpty : Bytes -> Type where
  IsNonEmpty : So (len > 0) -> NonEmpty (MkB _ _ len)

-- ^ how can I provide this proof without making the user need access
-- to MkB? Simply by having my own functions imlpicity prove that when
-- used? Things like cons and singleton and append? And if a
-- ByteString comes from elsewhere we use Dec?

-- This is here instead of in a 'where' due to a current coverage checking bug
private
soNotSonot' : So x -> So (not x) -> Void -- So x -> Not (So (not x))
soNotSonot' Oh Oh impossible

export -- Does this need public export or is being Dec enough for proofs?
isNonEmpty : (b : Bytes) -> Dec (NonEmpty b)
isNonEmpty (MkB _ _ len) with (choose (len > 0))
  isNonEmpty (MkB _ _ len) | (Left t) = Yes (IsNonEmpty t)
  isNonEmpty (MkB _ _ len) | (Right f) = No (\(IsNonEmpty t) => soNotSonot' t f)

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
unsafeCreateBytes : Int -> (MutBlock -> IO ()) -> Bytes
unsafeCreateBytes len f = unsafePerformIO $ do
    b <- allocateAndFill len f
    pure (MkB b 0 len)

export
unsafeCreateNBytes : Int -> (MutBlock -> IO Int) -> Bytes
unsafeCreateNBytes len f = unsafePerformIO $ do
    (b, i) <- allocateAndFillToN len f
    pure (MkB b 0 i)

export
unsafeCreateNBytes' : Int -> (MutBlock -> IO (Int,a)) -> (Bytes, a)
unsafeCreateNBytes' len f = unsafePerformIO $ do
    (b, i, a) <- allocateAndFillToN' len f
    pure ((MkB b 0 i), a)

export
unsafeCreateAndTrim : Int -> (MutBlock -> IO Int) -> Bytes
unsafeCreateAndTrim len0 f = unsafePerformIO $ do
    (b, len) <- allocateAndTrim len0 f
    pure (MkB b 0 len)


-- This is internal so NonEmpty is used to keep the library maker honest.
-- TODO: Why is NonEmpty for List available here? I have not imported
-- Data.List!
export
packUpToNBytes : Int -> (l : List Word8) -> (Bytes, List Word8)
packUpToNBytes len xs0 = unsafeCreateNBytes' len $ \p => go p len 0 xs0
  where
    go : MutBlock -> Int -> Int -> List Word8 -> IO (Int, List Word8)
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
      copyBlock xs xpos xlen p 0
      copyBlock ys ypos ylen p xlen

-- Debug use ONLY, **don't ever use this**, bytes are not characters!
-- Further, this shows the whole block, not just the live part.
-- TODO: Remove this down the road, or  otherwise prevent it from escaping this
-- package. If a person wants to Show/Read/OverloadedString Bytes they should
-- create a principled package that does this. Bytes are not Strings.
export
Show Bytes where
  show (MkB b pos len) = "MkB " ++ show_block b ++ " " ++ show pos ++ " " ++ show len
    where
      show_block : Block -> String
      show_block buf
        = "[" ++ fastAppend (intersperse ","
            (map show . unsafePerformIO $ blockData buf)) ++ "]"

private
compareBytes : Bytes -> Bytes -> Ordering
compareBytes (MkB _  _    0   ) (MkB _  _    0   ) = EQ
compareBytes (MkB xb xpos xlen) (MkB yb ypos ylen) =
    unsafePerformIO $ go (xlen-1) (ylen-1) xb yb
  where
    go : Int -> Int -> Block -> Block -> IO Ordering
    go 0 0 xb yb = [| getByte xb xpos `compare` getByte yb ypos |]
    go 0 _ _ _   = pure LT
    go _ 0 _ _   = pure GT
    go k j xb yb
      = do EQ <- [| getByte xb (xpos + k) `compare` getByte yb (ypos + j) |]
             | res => pure res
           go (k-1) (j-1) xb yb


-- Offset will tend to be 0 so we first compare length, then block object
-- equality, then offset. The block should be a simple ptr comparison so it's
-- worth doing before trying for a full compareBytes. This order is partly
-- because it makes sense and partly because ByteString does it this way.
-- However I'm not sure why length is checked before block object equality,
-- perhaps in ghc length has a cheaper comparison than than the extra
-- foreignPtr prodding that ptr equality needs.

-- To ask if we are nominally equal.
infixl 9 `basicEq`
private
basicEq : Bytes -> Bytes -> Bool
MkB xb xpos xlen `basicEq` MkB yb ypos ylen
    = xlen == ylen && (unsafePerformIO (exactEqBlock xb yb) && xpos == ypos)

export
implementation
Eq Bytes where
  x == y = x `basicEq` y || compareBytes x y == EQ

-- basicEq is much speedier than compareBytes, use it when possible!
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
-- sconcat/mconcat/foldMap should use this since it avoids a ton of copying by
-- checking the required size upfront. The idea here is to first compute the
-- size of the Bytes we need to make and then fill it. Despite going over the
-- list twice this makes it significantly faster than piece-wise methods that
-- must copy over and over to build up.
-- TODO: test this!
export
concat : List Bytes -> Bytes
concat bs = let maxlen = getLen bs
            in  unsafeCreateBytes maxlen (go 0 (maxlen-1) bs)
  where
    getLen : List Bytes -> Int
    getLen [] = 0             -- Check overflow of Int, which would be bad.
    getLen (MkB _ _ len :: bs) = checkedAdd moduleName "concat" len (getLen bs)
    
    go : (buf_pos : Int) -> (end : Int) -> List Bytes -> MutBlock -> IO ()
    go n_pos end [] buf = pure ()
    go n_pos end (MkB b pos len :: bs) buf
      = if n_pos > end then pure ()
                       else do copyBlock b pos len buf n_pos
                               go (n_pos + len) end bs buf


