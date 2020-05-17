module Data.Bytes

import Data.Bytes.Internal

import Data.List -- List.length

import System

import Debug.Trace

moduleName : String
moduleName = "Data.Bytes"


-- This concept might have a glaring flaw, possibly nothing frees
-- these memory blocks when a Bytes's life has ended!

-- I might need a concept of ForeignPtr where a finalizer is ran if it
-- dies.

---------------------------------------------------------------------
-- API
---------------------------------------------------------------------

empty : Bytes
empty = unsafeCreateBytes 0 (\_ => pure ())

singleton : Word8 -> Bytes
singleton w = unsafeCreateBytes 1 $ \p => setByte p 0 w


-- for some reason when this starts with 0 it fucks up?
unsafePackLenBytes : Nat -> List Word8 -> Bytes
unsafePackLenBytes len xs0
    = unsafeCreateBytes len $ \b => go (cast len) b xs0
  where
    go : Int -> Block -> List Word8 -> IO ()
    go p b [] = pure ()
    go p b (x::xs) = setByte b 0 x *> go (p+1) b xs

pack : List Word8 -> Bytes
pack xs = unsafePackLenBytes (length xs) xs

cons : Word8 -> Bytes -> Bytes
cons x (MkB p0 pos len)
    = do unsafeCreateBytes (1 + len) $ \p => do
         setByte p 0 x
         copyBlock p0 pos (cast len) p 1
         -- ^ loc because the 'offset' is where a buffer starts

{-
unpack : Bytes -> List Word8
unpack (MkB fp s l) = unsafePerformIO $ withForeignPtr $ \p0 => go (p0 `plusPtr` l) (p0 `plusPtr` (l + cast s))
  where
    go : AnyPtr -> AnyPtr -> IO (List Word8)
    go p q = if p `eq` q
      then pure []
      else do
        x <- getByte p 0
        xs <- go (p `plusPtr` 1) q
        pure (x :: xs)



total
length : Bytes -> Nat
length (MkB _ s _) = s

total
length' : Bytes -> Int
length' (MkB _ s _) = cast s

total
null : Bytes -> Bool
null buf = 0 >= length buf





snoc : Bytes -> Word8 -> Bytes
snoc (MkB p0 s l) x
  = unsafeCreateBytes (1 + s) $ \p => do
      let s'= cast s
      copyBlock p0 l s' p 0
      setByte p s' x -- s is the end because indices start at 0
      -- ^ loc because the 'offset' is where a buffer starts


append : Bytes -> Bytes -> Bytes
append (MkB lp ls ll) (MkB rp rs rl)
  = unsafeCreateBytes (ls + rs) $ \p => do
      let ls' = cast ls
          rs' = cast rs
      copyBlock lp ll ls' p 0
      copyBlock rp rl rs' p ls'


head : (b : Bytes) -> NonEmpty b => Word8
head (MkB p _ _) = unsafePerformIO (getByte p 0)

head' : (b : Bytes) -> Word8
head' (MkB p s l) = if 0 >= s
  then errorCall moduleName "head" "buffer empty"
  else unsafePerformIO (getByte p 0)

tail : (b : Bytes) -> NonEmpty b => Bytes
tail (MkB p (S s) loc) = MkB p s loc

tail' : Bytes -> Bytes
tail'    (MkB p Z _) = errorCall moduleName "tail" "buffer empty"
tail' bs@(MkB p (S s) loc) = MkB p s (1 + loc)

uncons : (b : Bytes) -> NonEmpty b => (Word8, Bytes)
uncons bs@(MkB _ _ _) = (head bs, tail bs)

uncons' : Bytes -> Maybe (Word8, Bytes)
uncons'    (MkB _ Z _) = Nothing
uncons' bs@(MkB _ (S _) _) = Just $ (head bs, tail bs)

last : (b : Bytes) -> NonEmpty b => Word8
last (MkB p s l) = unsafePerformIO (getByte p (cast s + l - 1))

last' : Bytes -> Word8
last' (MkB p Z l) = errorCall moduleName "last" "buffer empty"
last' (MkB p s l) = unsafePerformIO (getByte p (cast s + l - 1))

init : (b: Bytes) -> NonEmpty b => Bytes
init (MkB p (S s) l) = MkB p s l

init' : Bytes -> Bytes
init' (MkB p Z l) = errorCall moduleName "init" "buffer empty"
init' (MkB p (S s) l) = MkB p s l

unsnoc : (b : Bytes) -> NonEmpty b => (Bytes, Word8)
unsnoc bs@(MkB _ (S _) _) = (init bs, last bs)

unsnoc' : Bytes -> Maybe (Bytes, Word8)
unsnoc' (MkB _ Z _) = Nothing
unsnoc' bs@(MkB _ (S _) _) = Just $ (init bs, last bs)

foldl : (a -> Word8 -> a) -> a -> Bytes -> a
foldl f v (MkB p s l)
    = unsafePerformIO $ go v (p `plusPtr` l) (p `plusPtr` (l + cast s))
  where
    go : a -> AnyPtr -> AnyPtr -> IO a
    go z p q
      = if p `eq` q then pure z
                    else go (f z !(peek p)) (p `plusPtr` 1) q

foldr : (Word8 -> a -> a) -> a -> Bytes -> a
foldr f v (MkB p s l)
    = unsafePerformIO $ go v (p `plusPtr` (cast s + l - 1)) (p `plusPtr` (l-1))
  where
    go : a -> AnyPtr -> AnyPtr -> IO a
    go z p q
      = if p `eq` q then pure z
                    else go (f !(peek p) z) (p `plusPtr` (-1)) q

-- nooooo baddd
-- map : (Word8 -> Word8) -> Bytes -> Bytes
-- map f bs = foldl (\b,x => f x `cons` b) empty bs
map : (Word8 -> Word8) -> Bytes -> Bytes
map f (MkB p s0 l)
    = unsafeCreateBytes s0 $ \new => map_ 0 (p `plusPtr` l) new
  where
    s : Int
    s = cast s0
    map_ : Word8 -> AnyPtr -> AnyPtr -> IO ()
    map_ n p1 p2 = if n >= s -- could cast here if cast is a no-op
      then pure ()
      else do
        x <- peek (p1 `plusPtr` n)
        poke (p2 `plusPtr` n) (f x)
        map_ (n+1) p1 p2



-- :exec printLn (reverse $ map (+1) (pack [1..10]))
reverse : Bytes -> Bytes
reverse b@(MkB p s l) = unsafeCreateBytes s $ \new =>
    rev_ offset (p`plusPtr`l) new
  where
    offset : Int
    offset = cast s - 1
    rev_ : Int -> AnyPtr -> AnyPtr -> IO ()
    rev_ n p1 p2 = if 0 > n
      then pure ()
      else do
        x <- peek (p1 `plusPtr` n)
        poke (p2 `plusPtr` (offset - n)) x
        rev_ (n-1) p1 p2


-- Should I require NonEmpty for this here?
-- I don't think there's a compelling reason to do so, other than to
-- propogate a NonEmpty
intersperse : Word8 -> Bytes -> Bytes
intersperse v (MkB p s l)
    = unsafeCreateBytes ((2 * s) `monus` 1) $ \new => go 0 p new
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

faf : Bytes
faf = foldr cons empty (replicate 5000 1)


-- 2.1g residency
test : IO ()
test = do
    printLn faf <* sleep 1
    printLn faf <* sleep 1
    printLn faf <* sleep 1
    printLn faf <* sleep 1
    printLn faf <* sleep 1
    printLn faf <* sleep 1
    printLn faf <* sleep 1
    printLn faf <* sleep 1
    printLn faf <* sleep 1
    printLn faf <* sleep 1
    printLn faf <* sleep 1
    printLn faf <* sleep 1
    printLn faf <* sleep 1
    printLn faf <* sleep 1
    printLn faf <* sleep 1
    printLn faf <* sleep 1
    printLn faf <* sleep 1
    printLn faf <* sleep 1
    printLn faf <* sleep 1
