module Data.Bytes.Lazy.API

import Data.Bytes.Lazy.Internal
import Data.Bytes

empty : LBytes
empty = Empty

%inline -- pretty safe inline here
singleton : Word8 -> LBytes
singleton w = Chunk (singleton w) Empty

pack : List Word8 -> Bytes
unpack : Word8 -> List Word8

fromChunks : List Bytes -> LBytes

-- I probably want Lazy Lists here
toChunks : LBytes -> List Bytes

fromStrict : Bytes -> LBytes
fromStrict b = chunk' b Empty

-- expensive, who knows how big this LBytes is!
toStrict : LBytes -> Bytes
toStrict lb = ?saddfs
  where
    goLen0 : LBytes -> Bytes
    goLen1 : LBytes -> Bytes
    goLen  : LBytes -> Bytes
    goCopy : LBytes -> Bytes


