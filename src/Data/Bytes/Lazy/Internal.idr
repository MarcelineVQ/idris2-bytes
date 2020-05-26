module Data.Bytes.Lazy.Internal

import Data.Bytes

private
moduleName : String
moduleName = "Data.Bytes.Lazy.Internal"

---------------------------------------------------------------------
-- This module is experimental as idris2 doesn't check productivity/totality
-- very well, if at all
---------------------------------------------------------------------

-- Because we don't have good namespace management yet this is being given a
-- different name for the type. We might want the same name as strict Bytes so
-- people can swap it in types easier as they iterate their software's design.
-- That being said ByteString of Haskell calling lazy and nonlazy ByteString's
-- the same has certainly caused confusion so I'm inclined to keep LBytes.

-- Lazy or Inf for this type?
public export
data LBytes : Type where
  Empty : LBytes
  Chunk : (b : Bytes) -> (bs : Inf LBytes) -> LBytes

public export
data NonEmpty : LBytes -> Type where
  IsNonEmpty : NonEmpty (Chunk _ _)


---------------------------------------------------------------------
-- Sizing
---------------------------------------------------------------------

-- The idea/distant-hope is to fit what we're working on into L1/L2 cache, this
-- means it's very much backend dependent and possibly not even a realistic
-- goal. Do you know the memory layout of a chez scheme bytevector? A gambit
-- bytevector? JS vector?

-- We can micromanage it ourselves via C (given an appropriate GC to clean up
-- our immutable bytes) but that cuts out a swath of possible backends, e.g.
-- javascript.

-- These should be tested with various backends. In fact they should have a
-- mechanism to allow them to depend on the backend. Perhaps backends should
-- provide sizeOf operator for values.
defaultChunkSize : Int
defaultChunkSize = 32*1024

smallChunkSize : Int
smallChunkSize = 4*1024
-- Also the goal size for chunks from things like
-- unpackBytes/unpackAppendBytesLazy

chunkOverhead : Int
chunkOverhead = 3*64 -- Complete guess!
-- Chunk Bytes LBytes
--  Int   Ptr   Ptr

---------------------------------------------------------------------


-- Should we simply require NonEmpty for Chunk in the first place instead of
-- enforcing/checking invariants?

-- Having Lazy in the type is neat and all but do I also need Lazy in functions
-- or is it enough that I'm constructing an LBytes? Without
-- totality/productivity checking I'm not quite sure.


export
chunk : (b : Bytes) -> NonEmpty b => LBytes -> LBytes
chunk b bs = Chunk b bs

export
chunk' : (b : Bytes) -> LBytes -> LBytes
chunk' b bs = if null b then bs else Chunk b bs

export
foldrChunk : (Bytes -> b -> b) -> b -> LBytes -> b
foldrChunk f z Empty = z
foldrChunk f z (Chunk b bs) = f b (foldrChunk f z bs)

export
foldlChunk : (b -> Bytes -> b) -> b -> LBytes -> b
foldlChunk f z Empty = z
foldlChunk f z (Chunk b bs) = foldlChunk f (f z b) bs




