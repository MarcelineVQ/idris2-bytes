module Test.LazyTest

import Data.Bytes.Lazy
import Data.Bytes

import Data.Word.Word8

-- import System.Directory

import System.File

import Data.Bytes.Util

cas : Char -> Word8
cas c = cast (cast {to=Int} c)

-- read some number of file chacters, crash with msg on problem or eof, this is
-- to check if we're reading lazily.
-- If we eof crash then we read the whole file at once.
readN : File -> Int -> IO (List Word8)
readN f n = if 0 >= n then pure []
                      else
                        do Right c <- fGetChar f
                             | Left err => idris_crash "readN Left"
                           False <- fEOF f
                             | True => idris_crash "readN eof"
                             -- If we're at fEOF then c was \NUL !
                           [| pure (cas c) :: readN f (n-1) |]

take : Nat -> LBytes -> LBytes
take Z lb = Empty
take (S k) Empty = Empty
take (S k) (Chunk b bs) = Chunk b (take k bs)

fileTest : IO LBytes
fileTest = do Right f <- openFile "foo.txt" Read
                | Left err => idris_crash (show err)
              r <- make f []
              pure $ buildup f r
  where
    make : File -> List Word8 -> IO (Bytes, List Word8)
    make f rem
      = do rs@(_::_) <- readN f 32
             | [] => pure (empty,[])
           pure (packUpToNBytes 32 (rem ++ rs))
    buildup : File -> (Bytes, List Word8) -> LBytes
    buildup f (b,rem)
      = if null b then Empty
                  else Chunk b (buildup f (unsafePerformIO $ make f rem))
    -- ^ unsafePerformIO is so that we're not in IO, so we can do this creation
    -- lazily, don't do this yourself! Make or use a streaming IO lib. In the
    -- real world we desire predictable errors, thus our use of IO should be
    -- predictable, who knows where in our program our errrors are going to
    -- explode up now that we've lied and said `make` is pure.

printo : LBytes -> IO ()
printo lb = go 0 lb
  where
    go : Int -> LBytes -> IO ()
    go n Empty = putStrLn "End of lazy bytes. If you didn't get an eof error\nthen we succeeded in reading only part of the file."
    go n (Chunk _ lbs) = do
      putStrLn $ "Chunk " ++ show n
      go (n + 1) lbs

export
lazyIOTest : IO ()
lazyIOTest = do f <- fileTest
                putStrLn "Lazy Bytes test:"
                printo (take 2 f)
                putStrLn "Eat to eof test:"
                printo f -- This should eof crash.
           
