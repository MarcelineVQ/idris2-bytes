module Data.Bytes

import public Data.Bytes.Internal

import Data.Word.Word8

-- Unfortunately neccesary addition atm, no idea why. For some reason things
-- from Data.Buffer are in scope.
%hide Data.Buffer.getByte

moduleName : String
moduleName = "Data.Bytes"

---------------------------------------------------------------------
-- Documentation
---------------------------------------------------------------------
{-

Pretty standard (strict) ByteString kind of stuff. Some operations have `'`
(prime) versions with slightly different behavior for convenience. In
particular operations which require `NonEmpty` proof have their `'` versions
give runtime errors on bad inputs instead.

This hopefully demonstrates the Idris community spirit: be provably safe when
it can be made convenient, but don't force anyone to jump through hoops.
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


If you feel something is missing from the API it might not be! Bytes.Internal
implements common interfaces, be sure to check there first. e.g. <+>

-}

---------------------------------------------------------------------
-- API
---------------------------------------------------------------------

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
    go : Int -> MutBuffer -> List Word8 -> IO ()
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
             else map cast . take (intToNat len) . drop (intToNat pos)
                           . unsafePerformIO . bufferData $ fp

-- Intended complexity: O(1)
total
length : Bytes -> Int
length (MkB _ _ len) = len

total
-- Intended complexity: O(1)
null : Bytes -> Bool
null buf = 0 >= length buf

-- Intended complexity: O(1)
-- Provides NonEmpty
export
cons : Word8 -> Bytes -> Bytes
cons x (MkB p0 pos len)
    = do unsafeCreateBytes (1 + len) $ \p => do
         setByte p 0 x
         copyBuffer p0 pos len p 1

-- Intended complexity: O(1)
-- Provides NonEmpty
export
snoc : Bytes -> Word8 -> Bytes
snoc (MkB p0 pos len) x
  = unsafeCreateBytes (1 + len) $ \p => do
      copyBuffer p0 pos len p 0
      setByte p len x

-- Intended complexity: O(1)
export
head : (b : Bytes) -> NonEmpty b => Word8
head (MkB b pos _) = unsafePerformIO (getByte b pos)

export
head' : (b : Bytes) -> Word8
head' (MkB b pos len) = if 0 >= len
  then errorCall moduleName "head'" "buffer empty"
  else unsafePerformIO (getByte b pos)

-- Intended complexity: O(1)
export
tail : (b : Bytes) -> NonEmpty b => Bytes
tail (MkB p pos len) = MkB p (1 + pos) (len - 1)

export
tail' : Bytes -> Bytes
tail' (MkB p pos len) = if len > 0
                          then MkB p (1 + pos) (len - 1)
                          else errorCall moduleName "tail'" "buffer empty"

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
      else errorCall moduleName "uncons''" "buffer empty"

-- Intended complexity: O(1)
export
last : (b : Bytes) -> NonEmpty b => Word8
last (MkB p pos len) = unsafePerformIO (getByte p (len + pos - 1))

export
last' : Bytes -> Word8
last' (MkB p pos len) = if len > 0
                          then unsafePerformIO (getByte p (len + pos - 1))
                          else errorCall moduleName "last'" "buffer empty"

-- Intended complexity: O(1)
export
init : (b: Bytes) -> NonEmpty b => Bytes
init (MkB p pos len) = MkB p pos (len - 1)

export
init' : Bytes -> Bytes
init' (MkB p pos len) = if len > 0
                        then MkB p pos (len - 1)
                        else errorCall moduleName "init'" "buffer empty"

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
                              else errorCall moduleName "unsnoc''" "buffer empty"

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

-- Intended complexity: O(n)
export
map : (Word8 -> Word8) -> Bytes -> Bytes
map f (MkB b0 pos len)
    = unsafeCreateBytes len $ \new => map_ 0 len new
  where
    map_ : (pos1 : Int) -> (pos2 : Int) -> MutBuffer -> IO ()
    map_ p q buf = if p >= q then pure ()
                             else do
                               x <- getByte b0 (pos + p)
                               setByte buf p (f x)
                               map_ (p+1) q buf

-- Intended complexity: O(n)
export
reverse : Bytes -> Bytes
reverse b@(MkB buf pos len) = unsafeCreateBytes len $ \new =>
    rev_ 0 (len - 1) new
  where
    rev_ : Int -> Int -> MutBuffer -> IO ()
    rev_ p q new = if 0 > q
      then pure ()
      else do
        x <- getByte buf (pos + p)
        setByte new q x
        rev_ (p+1) (q-1) new

{-

-- Intended complexity: O(n)  -- ???   O(2n - 1)  ?
intersperse : Word8 -> Bytes -> Bytes
intersperse v (MkB b0 pos len)
    = unsafeCreateBytes ((2 * len) `monus` 1) $ \new => go 0 p new
  where
    s' : Int
    s' = cast s
    go : Int -> AnyPtr -> AnyPtr -> IO ()
    go n p1 p2 = if (n+1) >= s'
      then if n >= s'
        then pure ()
        else do x <- peek (p1 `plusPtr` n)
                poke (p2 `plusPtr` (2*n)) x
      else   do x <- peek (p1 `plusPtr` n)
                poke (p2 `plusPtr` (2*n)) x
                poke (p2 `plusPtr` (2*n+1)) v
                putStrLn $ show (2*n) ++ " " ++ show x ++ " - " ++ show (2*n+1) ++ " " ++ show v
                go (n+1) p1 p2
-- :exec printLn (intercalate (pack [1,2,3]) [pack [5,6], pack [8,9]])
-- can I use foldr/l for this?
-- what do I do here? sweep for the total size and keep a running loc?
intercalate : Bytes -> List Bytes -> Bytes
intercalate bs [] = bs
intercalate bs@(MkB p s l) bss =
    let count = length bss
        size = sum (map length bss) + ((count `monus` 1) * length bs)
        new@(MkB p s l) = unsafeCreateBytes size (\_ => pure ())
        MkB p' s' _ = foldl go new bss
    in MkB p' s' 0 -- reset the location
  where
    go : Bytes -> Bytes -> Bytes
    go (MkB p2 s2 l2) (MkB p1 s1 l1) = unsafePerformIO $ do
      -- copy b1 into b2, progress l
      copyBlock p1 l1 (cast s1) p2 l2
      copyBlock p l (cast s) p2 (l2 + cast s1)
      pure $ MkB p2 s2 (l2 + cast s1 + cast s)


transpose : List Bytes -> List Bytes

-}