module Data.Bytes.Prim

import Data.Bytes.FFI

import Data.Bytes.Util

import Data.IOArray

moduleName : String
moduleName = "Data.Bytes.Prim"

-- Should everything actually be using Ptr Word8?



---------------------------------------------------------------------
-- Ptr operations
--------------------------------------------------------------------

export
peek : AnyPtr -> IO Word8
peek p = primIO $ prim__getByte p 0

export
poke : AnyPtr -> Word8 -> IO ()
poke p v = primIO $ prim__setByte p 0 v

export
ptrToInt : AnyPtr -> Int
ptrToInt = believe_me 

export
intToPtr : Int -> AnyPtr
intToPtr = believe_me 

export
plusPtr : AnyPtr -> Int -> AnyPtr
plusPtr p n = intToPtr . (+ n) . ptrToInt $ p

infixl 4 `eq`  
export
eq : AnyPtr -> AnyPtr -> Bool
p `eq` q = ptrToInt p == ptrToInt q

-- export
-- -- Return non-zero if the pointer is null
-- isNullPtr : AnyPtr -> Bool
-- isNullPtr p = case prim__nullPtr p of
--                 0 => False
--                 _ => True

---------------------------------------------------------------------
-- Operations for blocks of memory
---------------------------------------------------------------------

-- export
-- getBlockSize : AnyPtr -> Int
-- getBlockSize = prim__blockSize

-- export
-- freeBlock : AnyPtr -> IO ()
-- freeBlock = primIO . prim__freeBlock

export
setByte : (p : Block) -> (loc : Int) -> (val : Word8) -> IO ()
setByte p loc val = primIO $ prim__arraySet p loc val

export
getByte : (1 p : Block) -> Int -> IO Word8
getByte p loc = primIO $ prim__arrayGet p loc

-- Our primitives don't provide a copy
export
copyBlock : (src : Block) -> (s_loc : Int) -> (count : Int)
         -> (dest : Block) -> (d_loc : Int) -> IO ()
copyBlock s s_loc count d d_loc = go count
  where
    go : Int -> IO ()
    go c = if c > 0
      then do let c' = c - 1
              x <- getByte s (s_loc + c')
              setByte d (d_loc + c') x
              go c'
      else pure ()


-- idk man, we don't have failure reporting for this primitive.
-- I wish we had some cleanup method for the c stuff.
export
allocateBlock : Int -> IO Block
allocateBlock len
    = do block <- primIO (prim__newArray len 1)
         if len > 0
           then if !(getByte block 0) /= 1
             then errorCall moduleName "allocateBlock" "allocation failed"
             else pure block
           else pure block

-- allocate and then use a function to populate the Block
export
allocateAndFill : Int -> (Block -> IO ()) -> IO Block
allocateAndFill len f = do
  b <- allocateBlock len
  f b
  pure b

