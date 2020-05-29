module Data.Bytes.Strict.API

import public Data.Bytes.Strict.Internal
import public Data.Bytes.Prim

import Data.Word.Word8

import Data.Bytes.Util

import Data.List -- drop

moduleName : String
moduleName = "Data.Bytes.Strict.API"

---------------------------------------------------------------------
-- Documentation
---------------------------------------------------------------------
{-

Pretty standard (strict) ByteString kind of stuff. Some operations have `'`
(prime) versions with slightly different behavior for convenience. In
particular operations which require `NonEmpty` proof have their `'` versions
give runtime errors on bad inputs instead. And things like replicate' use Int
instead of Nat.

This is to support what I consider the Idris community spirit: be provably safe
when it can be made convenient, but don't force anyone to jump through hoops.
I'm sure there's better wording for that though.

Included are some intended complexity values. They might be a lie since I don't
understand complexity very well. Hopefully they're in the ballpark, which is
all big-o is anyway.

Open Design Questions:

Is there a reason to allow negative position values in a Bytes? If we were
using pointers there might be some cute things we could do with that but
overall it doesn't really seem useful.
If we can get Nat or Fin to compile to Int, instead of the Integer that Nat
compiles to, then Nat for both fields of Bytes makes sense.

https://hackage.haskell.org/package/bytestring-0.10.10.0/docs/src/Data.ByteString.Internal.html#concat
Should helpers take all their arguments explicitly to avoid capturing as much
in closures? Does this even happen in idris?

Should things like replicate and unfoldrN simply cast their Nat and then call
to replicate'/unfoldrN' ? If that's the case is there any advantage to
providing them, aside from reducing cast noise for users?

Should functions that return List return lazy Lists?



If you feel something is missing from the API it might not be! Internal modules
implement common interfaces, be sure to check there first. e.g. <+>
-}


-- All of this will get fancier as Idris2 gains support for documentation
-- generation.

---------------------------------------------------------------------
-- API
---------------------------------------------------------------------



-------------------------------------------------
-- Barebones basic Bytes
-------------------------------------------------


-- Intended complexity: O(1)
export
empty : Bytes
empty = neutral

-- Intended complexity: O(1)
-- Provides NonEmpty
export
singleton : Word8 -> Bytes
singleton w = unsafeCreateBytes 1 $ \p => setByte p 0 w

private
packLenBytes : Int -> List Word8 -> Bytes
packLenBytes len xs0
    = unsafeCreateBytes len $ \b => go 0 b xs0
  where
    go : Int -> MutBlock -> List Word8 -> IO ()
    go p buf [] = pure ()
    go p buf (x::xs) = setByte buf p x *> go (p+1) buf xs

-- Intended complexity: O(n)
export
pack : List Word8 -> Bytes
pack xs = packLenBytes (cast (length xs)) xs

-- Intended complexity: O(n)
export
unpack : Bytes -> List Word8
unpack (MkB fp pos len) =
  if pos < 0 then errorCall moduleName "unpack" "position was negative"
             else take (intToNat len) . drop (intToNat pos)
                           . unsafePerformIO . blockData $ fp

-------------------------------------------------
-- Basic interface
-------------------------------------------------

-- Intended complexity: O(1)
total
length : Bytes -> Int
length (MkB _ _ len) = len

-- Intended complexity: O(1)
export
total
null : Bytes -> Bool
null buf = 0 >= length buf

-- Intended complexity: O(1)
export
cons : Word8 -> Bytes -> Bytes
cons x (MkB p0 pos len)
    = do unsafeCreateBytes (1 + len) $ \p => do
         setByte p 0 x
         copyBlock p0 pos len p 1

-- Intended complexity: O(1)
export
snoc : Bytes -> Word8 -> Bytes
snoc (MkB p0 pos len) x
  = unsafeCreateBytes (1 + len) $ \p => do
      copyBlock p0 pos len p 0
      setByte p len x

-- Intended complexity: O(1)
export
head : (b : Bytes) -> NonEmpty b => Word8
head (MkB b pos _) = unsafePerformIO (getByte b pos)

export
head' : (b : Bytes) -> Word8
head' (MkB b pos len) = if 0 >= len
  then errorCall moduleName "head'" "block empty"
  else unsafePerformIO (getByte b pos)

-- Intended complexity: O(1)
export
tail : (b : Bytes) -> NonEmpty b => Bytes
tail (MkB p pos len) = MkB p (1 + pos) (len - 1)

export
tail' : Bytes -> Bytes
tail' (MkB p pos len) = if len > 0
                          then MkB p (1 + pos) (len - 1)
                          else errorCall moduleName "tail'" "block empty"

-- Intended complexity: O(1)
export
uncons : (b : Bytes) -> NonEmpty b => (Word8, Bytes)
uncons bs = (head bs, tail bs)

export
uncons' : Bytes -> Maybe (Word8, Bytes)
uncons' bs@(MkB _ _ len) = if len > 0
                             then Just (head' bs, tail' bs)
                             else Nothing

export
uncons'' : Bytes -> (Word8, Bytes)
uncons'' bs@(MkB _ _ len)
  = if len > 0
      then (head' bs, tail' bs)
      else errorCall moduleName "uncons''" "block empty"

-- Intended complexity: O(1)
export
last : (b : Bytes) -> NonEmpty b => Word8
last (MkB p pos len) = unsafePerformIO (getByte p (len + pos - 1))

export
last' : Bytes -> Word8
last' (MkB p pos len) = if len > 0
                          then unsafePerformIO (getByte p (len + pos - 1))
                          else errorCall moduleName "last'" "block empty"

-- Intended complexity: O(1)
export
init : (b: Bytes) -> NonEmpty b => Bytes
init (MkB p pos len) = MkB p pos (len - 1)

export
init' : Bytes -> Bytes
init' (MkB p pos len) = if len > 0
                        then MkB p pos (len - 1)
                        else errorCall moduleName "init'" "block empty"

-- Intended complexity: O(1)
export
unsnoc : (b : Bytes) -> NonEmpty b => (Bytes, Word8)
unsnoc bs = (init bs, last bs)

export
unsnoc' : Bytes -> Maybe (Bytes, Word8)
unsnoc' bs@(MkB _ _ len) = if len > 0
                             then Just (init' bs, last' bs)
                             else Nothing

export
unsnoc'' : Bytes -> (Bytes, Word8)
unsnoc'' bs@(MkB _ _ len) = if len > 0
                              then (init' bs, last' bs)
                              else errorCall moduleName "unsnoc''" "block empty"

-------------------------------------------------
-- Transforms
-------------------------------------------------

-- Intended complexity: O(n)
export
reverse : Bytes -> Bytes
reverse b@(MkB buf pos len) = unsafeCreateBytes len $ \new =>
    rev_ 0 (len - 1) new
  where
    rev_ : Int -> Int -> MutBlock -> IO ()
    rev_ p q new = if 0 > q
      then pure ()
      else do
        x <- getByte buf (pos + p)
        setByte new q x
        rev_ (p+1) (q-1) new

sort : Bytes -> Bytes

intersperse : Word8 -> Bytes -> Bytes

intercalate : Bytes -> List Bytes -> Bytes


transpose : List Bytes -> List Bytes

-------------------------------------------------
-- Folding
-------------------------------------------------

-- Intended complexity: O(n)
export
foldl : (a -> Word8 -> a) -> a -> Bytes -> a
foldl f v (MkB b pos len)
    = unsafePerformIO $ go v pos (len + pos)
  where
    go : a -> (pos1 : Int) -> (pos2 : Int) -> IO a
    go z p q
      = if p >= q then pure z
                  else go (f z !(getByte b p)) (p+1) q

-- TODO: consider foldl', foldl with a lazy accumulator

foldl1 : (Word8 -> Word8 -> Word8) -> (b : Bytes) -> NonEmpty b => Word8
foldl1 f b = foldl f (head b) (tail b)


foldl1' : (Word8 -> Word8 -> Word8) -> (b : Bytes) -> Word8
foldl1' f b = foldl f (head' b) (tail'
 b)

-- Intended complexity: O(n)
export
foldr : (Word8 -> a -> a) -> a -> Bytes -> a
foldr f v (MkB bs pos len)
    = unsafePerformIO $ go v (len + pos) pos
  where
    go : a -> (pos1 : Int) -> (pos1 : Int) -> IO a
    go z p q
      = if p <= q then pure z
                  else go (f !(getByte bs p) z) (p-1) q

-- TODO: consider foldr', foldr with a lazy accumulator

foldr1 : (Word8 -> Word8 -> Word8) -> (b : Bytes) -> NonEmpty b => Word8
foldr1 f b = foldr f (last b) (init b)

foldr1' : (Word8 -> Word8 -> Word8) -> Bytes -> Word8
foldr1' f b = foldr f (last' b) (init' b)

concat : List Bytes -> Bytes

concatMap : (Word8 -> Bytes) -> Bytes -> Bytes

-------------------------------------------------
-- Mapping
-------------------------------------------------

-- Intended complexity: O(n)
export
map : (Word8 -> Word8) -> Bytes -> Bytes
map f (MkB b0 pos len)
    = unsafeCreateBytes len $ \new => map_ 0 len new
  where
    map_ : (pos1 : Int) -> (pos2 : Int) -> MutBlock -> IO ()
    map_ p q buf = if p >= q then pure ()
                             else do
                               x <- getByte b0 (pos + p)
                               setByte buf p (f x)
                               map_ (p+1) q buf

-- foldl with a state passed along
mapAccumL : (s -> Word8 -> (s, Word8)) -> s -> Bytes -> (s, Bytes)

-- foldr with a state passed along
mapAccumR : (s -> Word8 -> (s, Word8)) -> s -> Bytes -> (s, Bytes)

-------------------------------------------------
-- Scans
-------------------------------------------------

scanl : (Word8 -> Word8 -> Word8) -> Word8 -> Bytes -> Bytes
scanl1 : (Word8 -> Word8 -> Word8) -> (b : Bytes) -> NonEmpty b => Bytes
scanl1' : (Word8 -> Word8 -> Word8) -> Bytes -> Bytes

scanr : (Word8 -> Word8 -> Word8) -> Word8 -> Bytes -> Bytes
scanr1 : (Word8 -> Word8 -> Word8) -> (b : Bytes) -> NonEmpty b => Bytes
scanr1' : (Word8 -> Word8 -> Word8) -> Bytes -> Bytes

-------------------------------------------------
-- Generate Bytes
-------------------------------------------------

-- This can be written faster with a primitive like memset, should backends be
-- inclined to provide one?
-- Like replicate for List and elsewhere negative values are treated as 0.
replicate' : Int -> Word8 -> Bytes
replicate' n w = if n > 0
                   then map (const w) $ unsafeCreateBytes n $ (\_ => pure ())
                   else neutral

-- Why does a Nat version exist you ask? To make safety painless. People are
-- hopefully going to be using Nat all over their programs for safety reasons
-- so we want to support that easily. The user littering their code with casts
-- can get tedious so we'll support that for them here. That being said,
-- replicate' exists for when you're using Int anyway and especially if that's
-- due to performance, note that Nat compiles to Integer which (depending on
-- the backend) can be slower than Int.
-- TODO: should this case on Nat or is a cast fine? What does casing gain us?
replicate : Nat -> Word8 -> Bytes
replicate n w = replicate' (cast n) w

unfoldr : (a -> Maybe (Word8, a)) -> a -> Bytes

unfoldrN : Nat -> (a -> Maybe (Word8, a)) -> a -> (Bytes, Maybe a)

unfoldrN' : Int -> (a -> Maybe (Word8, a)) -> a -> (Bytes, Maybe a)

-------------------------------------------------
-- Splitting Bytes
-------------------------------------------------

take' : Int -> Bytes -> Bytes
take  : Nat -> Bytes -> Bytes

drop' : Int -> Bytes -> Bytes
drop  : Nat -> Bytes -> Bytes

splitAt' : Int -> Bytes -> (Bytes, Bytes)
splitAt  : Nat -> Bytes -> (Bytes, Bytes)

takeWhile : (Word8 -> Bool) -> Bytes -> Bytes
dropWhile : (Word8 -> Bool) -> Bytes -> Bytes

span  : (Word8 -> Bool) -> Bytes -> (Bytes, Bytes)
spanEnd  : (Word8 -> Bool) -> Bytes -> (Bytes, Bytes) -- starts from end

-- Should I really provide break when a person can just write not?
-- Might as well, for the same reason that we provide Nat and Int versions.
break : (Word8 -> Bool) -> Bytes -> (Bytes, Bytes)
breakEnd  : (Word8 -> Bool) -> Bytes -> (Bytes, Bytes) -- starts from end

-- potentially faster than groupBy (==), depends how we write this
group : Bytes -> List Bytes

groupBy : (Word8 -> Word8 -> Bool) -> Bytes -> List Bytes

inits : Bytes -> List Bytes
tails : Bytes -> List Bytes

-- TODO: Is this really common enough a need to include?
-- returns 2nd Bytes stripped of 1st Bytes iff 1st is a prefix of 2nd
stripPrefix : Bytes -> Bytes -> Maybe Bytes

-- Same, but from the other end.
stripSuffix : Bytes -> Bytes -> Maybe Bytes

-- split on, and consume, each occurence of Word8
split : Word8 -> Bytes -> List Bytes

-- same as above with predicate to determine delimiter
splitWith : (Word8 -> Bool) -> Bytes -> List Bytes

-------------------------------------------------
-- Predicates
-------------------------------------------------

any : (Word8 -> Bool) -> Bytes -> Bool
all : (Word8 -> Bool) -> Bytes -> Bool

isPrefixOf : Bytes -> Bytes -> Bool
isSuffixOf : Bytes -> Bytes -> Bool
isInfixOf  : Bytes -> Bytes -> Bool


-------------------------------------------------
-- Searches
-------------------------------------------------

-- TODO: needs more fitting name, even though yes we have a string of bytes in
-- memory.
-- Like break but we're breaking on a Bytes instead of a Word8. Examples:
{-
breakSubstring [3,4]     [1,2,3,4,5] ~ ([1,2],[3,4,5])
breakSubstring [3]       [1,2,3,4,5] ~ ([1,2],[3,4,5])
breakSubstring [3,4,5,6] [1,2,3,4,5] ~ ([1,2,3,4,5],[]) -- NB
breakSubstring [6]       [1,2,3,4,5] ~ ([1,2,3,4,5],[])
-}
-- This is likely to be a big player when you go to write parsers for Bytes
breakSubstring : Bytes -> Bytes -> (Bytes, Bytes)


elem : Word8 -> Bytes -> Bool

-- notElem is a common enough need to provide it for users so they don't have
-- to compose `not`
notElem : Word8 -> Bytes -> Bool

maximum :  Bytes -> Word8
minimum :  Bytes -> Word8

find : (Word8 -> Bool) -> Bytes -> Maybe Word8

filter : (Word8 -> Bool) -> Bytes -> Bytes

partition : (Word8 -> Bool) -> Bytes -> (Bytes, Bytes)

-------------------------------------------------
-- Indexing
-------------------------------------------------

-- NB: Best to avoid the Nat returning options until we have a primitive cast
-- for it to Int.

index : Bytes -> Nat -> Word8

index' : Bytes -> Int -> Word8

elemIndex  : Word8 -> Bytes -> Maybe Nat
elemIndex' : Word8 -> Bytes -> Maybe Int

elemIndices  : Word8 -> Bytes -> List Nat
elemIndices' : Word8 -> Bytes -> List Int

elemIndexEnd  : Word8 -> Bytes -> Maybe Nat
elemIndexEnd' : Word8 -> Bytes -> Maybe Int

findIndex  : (Word8 -> Bool) -> Bytes -> Maybe Nat
findIndex' : (Word8 -> Bool) -> Bytes -> Maybe Int

count  : Word8 -> Bytes -> Nat
count' : Word8 -> Bytes -> Int

-------------------------------------------------
-- Zips
-------------------------------------------------

-- analogous to zip for List
zip : Bytes -> Bytes -> List (Word8, Word8)

zipWith : (Word8 -> Word8 -> a) -> Bytes -> Bytes -> List a

unzip : List (Word8, Word8) -> (Bytes, Bytes)

