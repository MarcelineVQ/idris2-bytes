module Data.Bytes.Prim

import Data.Bytes.Util

import Data.Buffer

import Data.Word.Word8

moduleName : String
moduleName = "Data.Bytes.Prim"

---------------------------------------------------------------------
-- Operations on blocks of memory
---------------------------------------------------------------------

-- Should probably be a newtype
export
Block : Type
Block = Buffer

-- This is only useable so long as idris2 prefers scheme as the backend. It'd
-- be better to add this equivalent to Data.Buffer in base so that backend
-- makers are incentivised to support it broadly.
%foreign "scheme:eq?"
prim_exactEqBuff : Buffer -> Buffer -> PrimIO Bool
-- TODO: Check if I can use AnyPtr equality here.


-- Compare if two Blocks are the same object, in the sense of ptr equality.
export
exactEqBlock : Block -> Block -> IO Bool
exactEqBlock x y = primIO $ prim_exactEqBuff x y

export
getByte : Block -> (loc : Int) -> IO Word8
getByte b loc = cast <$> Data.Buffer.getByte b loc

export
blockData : Block -> IO (List Word8)
blockData b = map cast <$> bufferData b

---------------------------------------------------------------------
-- Mutation
---------------------------------------------------------------------

-- Bytes are immutable `Block`s of memory. Here, combined with the allocation
-- functions in the next section, we enforce that the only place we're allowed
-- to mutate a Block is during its creation.

export
data MutBlock = MkMB Block

export
setByte : MutBlock -> Int -> Word8 -> IO ()
setByte (MkMB mb) pos v = Data.Buffer.setByte mb pos (cast v)

export
copyBlock : (src : Block) -> (start, len : Int) ->
            (dest : MutBlock) -> (loc : Int) -> IO ()
copyBlock src start len (MkMB dest) loc
  = Data.Buffer.copyData src start len dest loc

-- Overloading isn't quite up to snuff for this to share the name of getByte,
-- even with namespaces. Still, we might desire to read from a MutBlock so this
-- is provided.
export
getMByte : MutBlock -> (loc : Int) -> IO Word8
getMByte (MkMB mb) loc = cast <$> Data.Buffer.getByte mb loc

---------------------------------------------------------------------
-- Block Allocation
---------------------------------------------------------------------

-- Kind of a weird one, Data.Buffer.newBuffer can't actually return a Nothing
-- currently. 18/5/2020
private
allocateBlock : Int -> IO Block
allocateBlock len
  = do Just block <- newBuffer len
         | Nothing => lieErrorCall moduleName "allocateBlock" "allocation failed"
       pure block

-- NB: The `f`'s below are the only place in Bytes that we can work with a
-- `MutBlock`. The only ways to set bytes that we provide work on
-- `MutBlock`s. This means that only here, where a new Block is being made,
-- can we mutate and ensures the immutability of Bytes.

-- Allocate and then use a function to populate the block.
export
allocateAndFill : Int -> (MutBlock -> IO ()) -> IO Block
allocateAndFill len f = do
  b <- allocateBlock len
  f (MkMB b)
  pure b

-- Allocate and then use a function to populate the block, return some extra
-- value.
export
allocateAndFill' : Int -> (MutBlock -> IO a) -> IO (Block, a)
allocateAndFill' len f = do
  b <- allocateBlock len
  r <- f (MkMB b)
  pure (b,r)

-- Allocate and then use a function to populate the block, return the length of
-- what was filled.
export
allocateAndFillToN : Int -> (MutBlock -> IO Int) -> IO (Block, Int)
allocateAndFillToN len f = do
  b <- allocateBlock len
  i <- f (MkMB b)
  pure (b,i)

-- Allocate and then use a function to populate the block, return the length of
-- what was filled and some extra value.
export
allocateAndFillToN' : Int -> (MutBlock -> IO (Int, a)) -> IO (Block, Int, a)
allocateAndFillToN' len f = do
  b <- allocateBlock len
  (i,a) <- f (MkMB b)
  pure (b,i,a)

-- Allocate and then use a function to populate the block, trim the resulting
-- Block based on the length we actually filled, and return that length.
export
allocateAndTrim : Int -> (MutBlock -> IO Int) -> IO (Block, Int)
allocateAndTrim len f = do
    (xb,len') <- allocateAndFill' len f
    yb <- allocateAndFill len' $ \zb => copyBlock xb 0 len' zb 0
    pure (yb,len')

-- Allocate and then use a function to populate the block, trim the resulting
-- Block based on the slice we actually filled and return that slice and some
-- extra value.
export
allocateAndTrim' : Int -> (MutBlock -> IO (Int,Int,a)) -> IO (Block, (Int,Int), a)
allocateAndTrim' len0 f = do
    (xb, (pos, len, a)) <- allocateAndFill' len0 f
    yb <- allocateAndFill len $ \zb => copyBlock xb pos len zb 0
    pure (yb, (pos,len), a)
