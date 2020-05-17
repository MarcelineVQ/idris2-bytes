module Data.Bytes.Lin

import Data.Bytes.Util

support : String -> String
support fn = "C:" ++ fn ++ ", libidris2_support"

data Block : Type where
  MkB : AnyPtr -> (off : Int) -> (len : Int) -> Block

-- NB: We could use plain-ass C memory operations if this library was
-- fully linear. Since we would be safe to tie freeing to exiting a
-- context. Further we would have reason to add basic mem opts to
-- idris itself so that backends support them for us.

%foreign support "idris2_newBytes"
prim__newBlock : Int -> AnyPtr

%foreign support "idris2_freeBytes"
prim__freeBlock : (1 p : AnyPtr) -> ()

%foreign support "idris2_newBuffer"
prim__newBytes : Int -> PrimIO AnyPtr

%foreign support "idris2_freeBuffer"
prim__freeBytes : (1 p : AnyPtr) -> ()

%foreign support "idris2_getBufferByte"
prim__getByte : (1 p : AnyPtr) -> Int -> Word8

%foreign support "idris2_setBufferByte"
prim__setByte : (p : AnyPtr) -> Int -> Word8 -> ()

%foreign support "idris2_setBufferByte"
prim__setByteL : (1 p : AnyPtr) -> Int -> Word8 -> ()

setByteL : (1 p : AnyPtr) -> Int -> Word8 -> ()
setByteL p n = ?fsfdsfd

%foreign support "idris2_copyBuffer"
prim__copyBuffer : (1 src : AnyPtr) -> (s_off : Int) -> (count : Int) -> (1 dest : AnyPtr) -> (d_off : Int) -> ()

-- troouble, I want to be able to set more than once, how do I return
-- a 'new' AnyPtr. Where must I lie?
setByte : (1 p : AnyPtr) -> Int -> Word8 -> AnyPtr
-- setByte p o v = let () = prim__setByte p o v in ?sdsfd
copyBuffer : (1 src : AnyPtr) -> (s_off : Int) -> (count : Int) -> (1 dest : AnyPtr) -> (d_off : Int) -> AnyPtr

-- What if I could 'open' a Ptr for many writes, close it when done?
-- Would this just be ST? 
-- Can I have an ST expressing hidden many-use in the way that normaly
-- ST expresses hidden state?
-- Would I need linearity polymorphism?

data Use : Type -> Type where
  Once : (1 x : a) -> Use a
  Many : (  x : a) -> Use a
  None : (0 x : a) -> Use a

data ST : Type -> Type -> Type where
  MkST : (Use s -> (Use s, a)) -> ST s a

runST : (forall s. ST s a) -> a
runST st = let MkST r = the (ST () a) st in ?runST_rhs



-- newtype ST s a = ST (STRep s a)
-- type STRep s a = State# s -> (# State# s, a #)


newBlock' : Int -> (1 f : (1 y : AnyPtr) -> b) -> b
newBlock' x f = f (prim__newBlock x)

newBlock'' : Int -> (1 f : (1 y : (forall c. AnyPtr -> c)) -> b) -> b
newBlock'' x f = f ?dff

-- how do I flag setByte as not consuming r? So that I can consume it
-- with free.
cons : Word8 -> Block -> Block
cons v (MkB p off len) = newBlock' (len+1) $ \r =>
    let () = prim__setByteL r 0 v
    in ?sdfsd

