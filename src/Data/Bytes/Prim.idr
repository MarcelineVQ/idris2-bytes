module Data.Bytes.Prim

import Data.Bytes.Util

import Data.Buffer -- Why is this visible in Data.Bytes + Data.Bytes.Internal?

import Data.Word.Word8

moduleName : String
moduleName = "Data.Bytes.Prim"

---------------------------------------------------------------------
-- Operations for blocks of memory
---------------------------------------------------------------------

-- This is only portable so long as idris2 prefers scheme as the backend. It'd
-- be better to add this equivalent to Data.Buffer in base so that backend
-- makers are incentivised to support it broadly.
%foreign "scheme:eq?"
prim__exactEqBuff : Buffer -> Buffer -> PrimIO Bool
-- TODO: Check if I can use AnyPtr equality here.

-- Compare if two Buffers are the same object, in the sense of ptr equality.
export
exactEqBuff : Buffer -> Buffer -> IO Bool
exactEqBuff x y = primIO $ prim__exactEqBuff x y


-- Bytes are immutable blocks of memory. Here we create enforcement that the
-- only place we're allowed to mutate a buffer is somewhere we're creating a
-- new one.

export
getByte : Buffer -> (loc : Int) -> IO Word8
getByte b loc = cast <$> Data.Buffer.getByte b loc

namespace Mutable
  export
  data MutBuffer = MkMB Buffer

  export
  setByte : MutBuffer -> Int -> Word8 -> IO ()
  setByte (MkMB mb) pos v = Data.Buffer.setByte mb pos (cast v)

  export
  copyBuffer : (src : Buffer) -> (start, len : Int) ->
               (dest : MutBuffer) -> (loc : Int) -> IO ()
  copyBuffer src start len (MkMB dest) loc
    = Data.Buffer.copyData src start len dest loc

  -- getByte overload, a typeclass for Buffer+MutBuffer would be silly
  export
  getByte : MutBuffer -> (loc : Int) -> IO Word8
  getByte (MkMB mb) loc = cast <$> Data.Buffer.getByte mb loc

  -- Kind of a weird one, Data.Buffer.newBuffer can't actually return a Nothing
  -- currently. 18/5/2020
  private
  allocateBlock : Int -> IO Buffer
  allocateBlock len
    = do Just block <- newBuffer len
           | Nothing => errorCall moduleName "allocateBlock" "allocation failed"
         pure block

  -- allocate and then use a function to populate the block
  export
  allocateAndFill : Int -> (MutBuffer -> IO ()) -> IO Buffer
  allocateAndFill len f = do
    b <- allocateBlock len
    f (MkMB b)
    pure b
