module Data.Bytes.Lazy.IO

-- functions for reading/writing Bytes to and from files

-- Data.Bytes.Strict.IO should be safe and stable to depend on, though the
-- module might move over time. Conversely Data.Bytes.Lazy.IO is not a good
-- idea to depend on despite being the more useful API. It is being provided
-- for the purposes of developing this library and for people to prototype with
-- but it is lazy IO, and one of the lessons of Haskell is that while lazyness
-- is good, lazy io isn't. There's just no way to deal with IO errors in pure
-- code, and doing IO lazily will give you exactly that. Say we've read some
-- file into a lazy bytes and pass it along to our pure code. If the file is
-- closed before we start consuming the LBytes, we'll get a file error in a
-- place it can't be addressed/handled. We don't have an exception mechanism in
-- idris2 currently so this is slightly less of a problem than it is in haskell
-- but at the same time it'd be a real hassle to make every LBytes method end
-- with `Either Error a`. The ideal way to go is a streaming IO library that
-- provides our LBytes rather than lazy io because you can structure your
-- program around the streaming lib and have confidence about when issues will
-- crop up and where to handle them.



import Data.Bytes.Lazy
import Data.Bytes
import Data.Bytes.Util

import Data.Word.Word8

import System.File

-------------------------------------------------
-- primmmmmsssss
-------------------------------------------------

-- fGetChar grabs an `c unsigned char` and casts it to idris2 Char
-- Should really just have our own that casts to Word8
-- an fGetByte iow, idris2 just calls fgetc and so can we

support : String -> String
support fn = "C:" ++ fn ++ ", libidris2_support"

%foreign support "idris2_fileError"
prim_error' : FilePtr -> PrimIO Int

%foreign support "idris2_fileErrno"
prim_fileErrno' : PrimIO Int

%foreign support "fgetc"
prim__readChar' : FilePtr -> PrimIO Int

returnError : IO (Either FileError a)
returnError
    = do err <- primIO prim_fileErrno'
         case err of
              0 => pure $ Left FileReadError
              1 => pure $ Left FileWriteError
              2 => pure $ Left FileNotFound
              3 => pure $ Left PermissionDenied
              4 => pure $ Left FileExists
              _ => pure $ Left (GenericFileError (err-5))

fGetByte : (h : File) -> IO (Either FileError Word8)
fGetByte (FHandle h)
    = do c <- primIO (prim__readChar' h)
         ferr <- primIO (prim_error' h)
         if (ferr /= 0)
            then returnError
            else pure (Right (cast c))

-------------------------------------------------

-- read some number of file chacters, crash with msg on problem or eof, this is
-- to check if we're reading lazily.
-- If we eof crash then we read the whole file at once.
partial
readN : File -> Int -> IO (List Word8)
readN f n = if 0 >= n then pure []
                      else
                        do Right c <- fGetByte f
                             | Left err => idris_crash "readN Left"
                           False <- fEOF f
                             | True => pure []
                             -- If we're at fEOF then c was \NUL !
                           [| pure c :: readN f (n-1) |]

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
    go n Empty = putStrLn $ "End of lazy bytes. If you didn't get an eof"
      ++ "error\nthen we succeeded in reading only part of the file."
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
           


-- super basic boys

readFile : (filename : String) -> IO (Either FileError LBytes)

writeFile : File -> (filename : String) -> LBytes -> IO (Maybe FileError)

