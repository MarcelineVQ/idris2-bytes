module Data.Bytes.Prim

import Data.Bytes.Util

import Data.Buffer -- Why is this visible in Data.Bytes + Data.Bytes.Internal?

import Data.Word.Word8

moduleName : String
moduleName = "Data.Bytes.Prim"

---------------------------------------------------------------------
-- Operations for blocks of memory
---------------------------------------------------------------------

-- This is only useable so long as idris2 prefers scheme as the backend. It'd
-- be better to add this equivalent to Data.Buffer in base so that backend
-- makers are incentivised to support it broadly.
%foreign "scheme:eq?"
prim_exactEqBuff : Buffer -> Buffer -> PrimIO Bool
-- TODO: Check if I can use AnyPtr equality here.


-- Compare if two Buffers are the same object, in the sense of ptr equality.
export
exactEqBuff : Buffer -> Buffer -> IO Bool
exactEqBuff x y = primIO $ prim_exactEqBuff x y


export
getByte : Buffer -> (loc : Int) -> IO Word8
getByte b loc = cast <$> Data.Buffer.getByte b loc

export
data MutBuffer = MkMB Buffer

namespace Mutable
  -- getByte overload, a typeclass for Buffer+MutBuffer would be silly
  export
  getByte : MutBuffer -> (loc : Int) -> IO Word8
  getByte (MkMB mb) loc = cast <$> Data.Buffer.getByte mb loc

---------------------------------------------------------------------
-- Allocation
---------------------------------------------------------------------


-- Bytes are immutable blocks of memory. Here we create enforcement that the
-- only place we're allowed to mutate a buffer is somewhere we're creating a
-- new one.



export
setByte : MutBuffer -> Int -> Word8 -> IO ()
setByte (MkMB mb) pos v = Data.Buffer.setByte mb pos (cast v)

export
copyBuffer : (src : Buffer) -> (start, len : Int) ->
             (dest : MutBuffer) -> (loc : Int) -> IO ()
copyBuffer src start len (MkMB dest) loc
  = Data.Buffer.copyData src start len dest loc



-- Kind of a weird one, Data.Buffer.newBuffer can't actually return a Nothing
-- currently. 18/5/2020
private
allocateBlock : Int -> IO Buffer
allocateBlock len
  = do Just block <- newBuffer len
         | Nothing => errorCall moduleName "allocateBlock" "allocation failed"
       pure block

---------------------------------------------------------------------
-- Buffer Allocation
-- These are here to avoid needing Data.Buffer elsewhere.
---------------------------------------------------------------------

-- NB: The `f`'s below are the only place in Bytes that we can work with a
-- `MutBuffer`. The only ways to set bytes that we provide work on
-- `MutBuffer`s. This means that only here, where a new Buffer is being made,
-- can we mutate and ensures the immutability of Bytes.


-- Allocate and then use a function to populate the block.
export
allocateAndFill : Int -> (MutBuffer -> IO ()) -> IO Buffer
allocateAndFill len f = do
  b <- allocateBlock len
  f (MkMB b)
  pure b

-- Allocate and then use a function to populate the block, return some extra
-- value.
export
allocateAndFill' : Int -> (MutBuffer -> IO a) -> IO (Buffer, a)
allocateAndFill' len f = do
  b <- allocateBlock len
  r <- f (MkMB b)
  pure (b,r)

-- Allocate and then use a function to populate the block, return the length of
-- what was filled.
export
allocateAndFillToN : Int -> (MutBuffer -> IO Int) -> IO (Buffer, Int)
allocateAndFillToN len f = do
  b <- allocateBlock len
  i <- f (MkMB b)
  pure (b,i)

-- Allocate and then use a function to populate the block, return the length of
-- what was filled and some extra value.
export
allocateAndFillToN' : Int -> (MutBuffer -> IO (Int, a)) -> IO (Buffer, Int, a)
allocateAndFillToN' len f = do
  b <- allocateBlock len
  (i,a) <- f (MkMB b)
  pure (b,i,a)

-- Allocate and then use a function to populate the block, trim the resulting Buffer based on the length we actually filled.
export
allocateAndTrim : Int -> (MutBuffer -> IO Int) -> IO (Buffer, Int)
allocateAndTrim len f = do
    (xb,len') <- allocateAndFill' len f
    yb <- allocateAndFill len' $ \zb => copyBuffer xb 0 len' zb 0
    pure (yb,len')

-- Allocate and then use a function to populate the block, trim the resulting Buffer based on the slice we actually filled.
export
allocateAndTrim' : Int -> (MutBuffer -> IO (Int,Int,a)) -> IO (Buffer, Int, a)
allocateAndTrim' len0 f = do
    (xb, (pos, len, a)) <- allocateAndFill' len0 f
    yb <- allocateAndFill len $ \zb => copyBuffer xb pos len zb 0
    pure (yb, len, a)
