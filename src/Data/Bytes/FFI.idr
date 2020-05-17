module Data.Bytes.FFI

import Data.Bytes.Util

-- import Data.Bytes.Finalizer

-- Unfortunately as far as I can tell this would dramatically
-- increase copying.

-- Implemented externally
export
data ArrayData : Type -> Type where

public export
Block : Type
Block = ArrayData Word8

-- 'unsafe' primitive access, backend dependent
-- get and set assume that the bounds have been checked. Behavious is undefined
-- otherwise.
export
%extern prim__newArray : forall a . Int -> a -> PrimIO (ArrayData a)
export
%extern prim__arrayGet : forall a . (1 p : ArrayData a) -> Int -> PrimIO a
export
%extern prim__arraySet : forall a . (1 p : ArrayData a) -> Int -> a -> PrimIO ()




moduleName : String
moduleName = "Data.Bytes.FFI"

-- Redefinitions from base because Buffer isn't quite what I'd like.

support : String -> String
support fn = "C:" ++ fn ++ ", libidris2_support"

---------------------------------------------------------------------
-- Block management
---------------------------------------------------------------------

-- An opaque block of memory.
-- data Block : Type where

-- We're using scheme since it's the current main target of idris2
-- Will switch to C to increase portability when idris2 supports
-- finalizers.
-- I was unable to find a satisfactory way of using linearity to
-- enforce freeing memory or to write a shcheme wrapper to create
-- finalizing.


-- I can't catch scheme's exception if make-bytevector fails due to no
-- memory so we can't provide a more informative error message in that
-- case.

-- unitialized Block
export
%foreign "scheme:make-bytevector"
prim__newBlock : Int -> PrimIO Block

-- block of some Word8
export
%foreign "scheme:make-bytevector"
prim__newBlockOf : Int -> Word8 -> PrimIO Block


export
%foreign "scheme:bytevector-truncate!"
prim__truncateBlock : AnyPtr -> Int -> PrimIO Block

export
%foreign "scheme:bytevector-copy!"
prim__copyBlock : (1 src : AnyPtr) -> Int -> Int -> (1 dest : AnyPtr) -> Int -> PrimIO ()

export
%foreign "scheme:bytevector-copy"
prim__cloneBlock' : (1 src : AnyPtr) -> Int -> PrimIO AnyPtr

export
%foreign "scheme:bytevector-u8-ref"
prim__getByte : (1 p : AnyPtr) -> Int -> PrimIO Word8

export
%foreign "scheme:bytevector-u8-set!"
prim__setByte : (1 p : AnyPtr) -> Int -> (1 w : Word8) -> PrimIO ()



-- export
-- %foreign support "idris2_getBufferSize"
-- prim__blockSize : AnyPtr -> Int

-- export
-- %foreign "scheme:"
-- prim__nullPtr : AnyPtr -> Int
-- 
-- export
-- %foreign support "idris2_newBuffer"
-- prim__newBlock : Int -> PrimIO AnyPtr
-- 
-- export
-- %foreign support "idris2_freeBuffer"
-- prim__freeBlock : AnyPtr -> PrimIO ()
-- 
-- export
-- freeBlock : AnyPtr -> IO ()
-- freeBlock p = primIO $ prim__freeBlock p
-- 
-- -- guardian : (p : AnyPtr) -> (1 fin : AnyPtr -> IO ()) -> PrimIO Guardian
-- 
-- 
-- 
-- 
-- 
-- ---------------------------------------------------------------------
-- -- get/set + copy
-- ---------------------------------------------------------------------
-- 
-- export
-- %foreign support "idris2_setBufferByte"
-- prim__setByte : (1 p : AnyPtr) -> Int -> Int -> PrimIO ()
-- 
-- export
-- %foreign support "idris2_getBufferByte"
-- prim__getByte : (1 p : AnyPtr) -> Int -> PrimIO Int
-- 
-- export
-- %foreign support "idris2_copyBuffer"
-- prim__copyBlock : (1 src : AnyPtr) -> Int -> Int -> (1 dest : AnyPtr) -> Int -> PrimIO ()
-- 
-- ---------------------------------------------------------------------
-- -- More specific get/set's
-- ---------------------------------------------------------------------
-- 
-- export
-- %foreign support "idris2_setBufferInt"
-- prim__setInt : AnyPtr -> Int -> Int -> PrimIO ()
-- 
-- export
-- %foreign support "idris2_getBufferInt"
-- prim__getInt : AnyPtr -> Int -> PrimIO Int
-- 
-- export
-- %foreign support "idris2_setBufferDouble"
-- prim__setDouble : AnyPtr -> Int -> Double -> PrimIO ()
-- 
-- export
-- %foreign support "idris2_getBufferDouble"
-- prim__getDouble : AnyPtr -> Int -> PrimIO Double
-- 
-- ---------------------------------------------------------------------
-- -- string prims
-- -- TODO: investigate if these are faster
-- ---------------------------------------------------------------------
-- 
-- -- Get the length of a string in bytes, rather than characters
-- 
-- export
-- %foreign support "strlen"
-- stringByteLength : String -> Int
-- 
-- export
-- %foreign support "idris2_setBufferString"
-- prim__setString : AnyPtr -> Int -> String -> PrimIO ()
-- 
-- export
-- %foreign support "idris2_getBufferString"
-- prim__getString : AnyPtr -> Int -> Int -> PrimIO String
-- 
-- ---------------------------------------------------------------------
-- -- File prims
-- ---------------------------------------------------------------------
-- 
-- export
-- %foreign support "idris2_readBufferFromFile"
-- prim__readBufferFromFile : String -> PrimIO AnyPtr
-- 
-- export
-- %foreign support "idris2_writeBufferToFile"
-- prim__writeBuffer : String -> AnyPtr -> Int -> PrimIO Int
