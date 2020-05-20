module Data.Bytes

import public Data.Bytes.Internal

import Data.Word.Word8

import Data.List -- List.length

import System -- sleep test

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

This hopefully demonstrates the Idris community spirit: be provably safe when it can be made convenient, but don't force anyone to jump through hoops.
I'm sure there's better wording for that though.

Included are some intended complexity values. They might be a lie since I don't
understand complexity very well. Hopefully they're in the ballpark, which is
all big-o is anyway.

Open Design Questions:

Is there a reason to allow negative position values in a Bytes? If we were
using pointers there might be some cute things we could do with that but
overall it doesn't really seem useful. If we can get Nat or Fin to compile to
Int, instead of the Integer that Nar compiles to, then Nat for both fields of
Bytes makes sense.

https://hackage.haskell.org/package/bytestring-0.10.10.0/docs/src/Data.ByteString.Internal.html#concat
Should helpers take all their arguments explicitly to avoid capturing as much
in closures? Does this happen in idris?


If you feel something is missing from the API it might not be! Bytes.Internal
implements common interfaces, be sure to check there first. e.g. <+>

-}

---------------------------------------------------------------------
-- API
---------------------------------------------------------------------

-- Intended complexity: O(1)
empty : Bytes
empty = neutral

-- Intended complexity: O(1)
-- Provides NonEmpty
singleton : Word8 -> Bytes
singleton w = unsafeCreateBytes 1 $ \p => setByte p 0 w

private
packLenBytes : Nat -> List Word8 -> Bytes
packLenBytes len xs0
    = unsafeCreateBytes len $ \b => go 0 b xs0
  where
    go : Int -> MutBuffer -> List Word8 -> IO ()
    go p buf [] = pure ()
    go p buf (x::xs) = setByte buf p x *> go (p+1) buf xs

-- Intended complexity: O(n)
pack : List Word8 -> Bytes
pack xs = packLenBytes (length xs) xs

-- Intended complexity: O(n)
unpack : Bytes -> List Word8
unpack (MkB fp pos len) =
  if pos < 0 then errorCall moduleName "unpack" "position was negative"
             else map cast . take len . drop (intToNat pos)
                           . unsafePerformIO . bufferData $ fp

total
-- Intended complexity: O(1)
length : Bytes -> Nat
length (MkB _ _ len) = len
 
total
length' : Bytes -> Int
length' (MkB _ _ len) = cast len

total
-- Intended complexity: O(1)
null : Bytes -> Bool
null buf = 0 >= length buf

-- Intended complexity: O(1)
-- Provides NonEmpty
cons : Word8 -> Bytes -> Bytes
cons x (MkB p0 pos len)
    = do unsafeCreateBytes (1 + len) $ \p => do
         setByte p 0 x
         copyBuffer p0 pos (cast len) p 1

-- Intended complexity: O(1)
-- Provides NonEmpty
snoc : Bytes -> Word8 -> Bytes
snoc (MkB p0 pos len) x
  = unsafeCreateBytes (1 + len) $ \p => do
      let s'= cast len
      copyBuffer p0 pos s' p 0
      setByte p s' x

-- Intended complexity: O(1)
head : (b : Bytes) -> NonEmpty b => Word8
head (MkB p _ _) = unsafePerformIO (getByte p 0)

head' : (b : Bytes) -> Word8
head' (MkB p s l) = if 0 >= s
  then errorCall moduleName "head" "buffer empty"
  else unsafePerformIO (getByte p 0)

-- Intended complexity: O(1)
tail : (b : Bytes) -> NonEmpty b => Bytes
tail (MkB p pos (S len)) = MkB p (1 + pos) len

tail' : Bytes -> Bytes
tail' (MkB p _ Z) = errorCall moduleName "tail" "buffer empty"
tail' (MkB p pos (S len)) = MkB p (1 + pos) len

-- Intended complexity: O(1)

uncons : (b : Bytes) -> NonEmpty b => (Word8, Bytes)
uncons bs@(MkB _ _ _) = (head bs, tail bs)

uncons' : Bytes -> Maybe (Word8, Bytes)
uncons'    (MkB _ _ Z) = Nothing
uncons' bs@(MkB _ _ (S _)) = Just $ (head bs, tail bs)

uncons'' : Bytes -> (Word8, Bytes)
uncons''    (MkB _ _ Z) = errorCall moduleName "uncons''" "buffer empty"
uncons'' bs@(MkB _ _ (S _)) = (head bs, tail bs)

-- Intended complexity: O(1)

last : (b : Bytes) -> NonEmpty b => Word8
last (MkB p pos len) = unsafePerformIO (getByte p (cast len + pos - 1))

last' : Bytes -> Word8
last' (MkB _ _ Z) = errorCall moduleName "last" "buffer empty"
last' (MkB p pos len) = unsafePerformIO (getByte p (cast len + pos - 1))

-- Intended complexity: O(1)

init : (b: Bytes) -> NonEmpty b => Bytes
init (MkB p pos (S len)) = MkB p pos len

init' : Bytes -> Bytes
init' (MkB _ _ Z) = errorCall moduleName "init" "buffer empty"
init' (MkB p pos (S len)) = MkB p pos len

-- Intended complexity: O(1)
unsnoc : (b : Bytes) -> NonEmpty b => (Bytes, Word8)
unsnoc bs@(MkB _ _ (S _)) = (init bs, last bs)

unsnoc' : Bytes -> Maybe (Bytes, Word8)
unsnoc' (MkB _ _ Z) = Nothing
unsnoc' bs@(MkB _ _ (S _)) = Just $ (init bs, last bs)

unsnoc'' : Bytes -> (Bytes, Word8)
unsnoc'' (MkB _ _ Z) = errorCall moduleName "last" "buffer empty"
unsnoc'' bs@(MkB _ _ (S _)) = (init bs, last bs)

-- Intended complexity: O(n)
foldl : (a -> Word8 -> a) -> a -> Bytes -> a
foldl f v (MkB b pos len)
    = unsafePerformIO $ go v pos (cast len + pos)
  where
    go : a -> (pos1 : Int) -> (pos2 : Int) -> IO a
    go z p q
      = if p >= q then pure z
                  else go (f z !(getByte b p)) (p+1) q

-- Intended complexity: O(n)
foldr : (Word8 -> a -> a) -> a -> Bytes -> a
foldr f v (MkB bs pos len)
    = unsafePerformIO $ go v (cast len + pos) pos
  where
    go : a -> (pos1 : Int) -> (pos1 : Int) -> IO a
    go z p q
      = if p <= q then pure z
                  else go (f !(getByte bs p) z) (p-1) q

-- Intended complexity: O(n)
map : (Word8 -> Word8) -> Bytes -> Bytes
map f (MkB b0 pos len)
    = unsafeCreateBytes len $ \new => map_ 0 (cast len) new
  where
    map_ : (pos1 : Int) -> (pos2 : Int) -> MutBuffer -> IO ()
    map_ p q buf = if p >= q then pure ()
                             else do
                               x <- getByte b0 (pos + p)
                               setByte buf p (f x)
                               map_ (p+1) q buf

-- Intended complexity: O(n)
reverse : Bytes -> Bytes
reverse b@(MkB buf pos len) = unsafeCreateBytes len $ \new =>
    rev_ 0 (cast len - 1) new
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



trybs1 : Bytes -> IO ()
trybs1 bs = printLn $ unpack bs

trybs2 : Show a => (Bytes -> a) -> Bytes -> IO ()
trybs2 f bs = printLn (f bs)

trybs3 : IO ()
trybs3 = printLn (foldr (+) 0 (pack [1,2,3]))

-}

faf1 : Bytes
faf1 = foldr cons empty (replicate 20000 3)

faf2 : Bytes
faf2 = foldl snoc empty (replicate 20 3)
-- :exec printLn (tail (pack [1,5,6,9] `snoc` 3))


-- 2.1g residency
test : IO ()
test = do
    printLn faf1 <* sleep 1
    printLn faf1 <* sleep 1
    printLn faf1 <* sleep 1
    printLn faf1 <* sleep 1
    printLn faf1 <* sleep 1
    printLn faf1 <* sleep 1
    printLn faf1 <* sleep 1
    printLn faf1 <* sleep 1
    printLn faf1 <* sleep 1
    printLn faf1 <* sleep 1
    printLn faf1 <* sleep 1
    printLn faf1 <* sleep 1
    printLn faf1 <* sleep 1
    printLn faf1 <* sleep 1
    printLn faf1 <* sleep 1
    printLn faf1 <* sleep 1
    printLn faf1 <* sleep 1
    printLn faf1 <* sleep 1
    printLn faf1 <* sleep 1
