module Data.Bytes.Internal

import Data.Bytes.Util
import Data.Bytes.Prim

import Data.List

import Data.Bytes.Finalizer

moduleName : String
moduleName = "Data.Bytes.Internal"

-- Our Bytes type, a Ptr to a `block` of memory, its size in bytes,
-- and the current 0-based offset into that memory.
-- I might change this later to match ByteString, which uses 's' as
-- the off(s)et and 'l' as the (l)ength.
public export
data Bytes : Type where
     MkB : (1 b : Block) -> (pos : Int) -> (len : Nat) -> Bytes

restore : (0 loc : Int) -> Int
restore _ = 0

-- Performance: If Nat is compiled to Int then this is as performant
-- as if we didn't use Nat but we gain static size computing. However
-- if it's compiled to Integer then I'm not sure we gained anything
-- over simply having two constructors for Bytes, empty and not. The
-- default for Nat to compile to is Integer but realistically Int is
-- safe since you'll run out of memory before you fill up an Int.
-- That's especially the cast once we have Word (uint) types if we
-- compile Nat to those, which is only reasonable since Nat can't be
-- negative anyway.


public export
data NonEmpty : Bytes -> Type where
  IsNonEmpty : NonEmpty (MkB _ _ (S _))

-- ^ how can I provide this proof without making the user need access
-- to MkB? Simply by having my own functions imlpicity prove that when
-- used? Things like cons and singleton and append? And if a
-- ByteString comes from elsewhere we use DecEq?


private
show_foldr : (Word8 -> a -> a) -> a -> Bytes -> a
show_foldr f v (MkB bs pos len)
    = unsafePerformIO $ go v (cast len + pos - 1) (pos - 1)
  where
    go : a -> (offset1 : Int) -> (offset2 : Int) -> IO a
    go z p q
      = if p == q then pure z
                  else go (f !(getByte bs p) z) (p-1) q

-- debug use only, don't ever use this
export
Show Bytes where
  show x@(MkB _ s l) = "MkB " ++ "[" ++ concat (intersperse "," (show_foldr (\b,bs => show b :: bs) [] x)) ++ "] " ++ show s ++ " " ++ show l


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

infixl 4 <$,$>

(<$) : Functor f => a -> f b -> f a
x <$ y = map (const x) y

($>) : Functor f => f a -> b -> f b
($>) = flip (<$)

----------------------------------------------------------------------

{- NB: In the bytestring lib of haskell they use unsafePerformIO
because of how the strictness analzer and sharing works, they note:
The use of unsafePerformIO in allocating functions (unsafeCreate) is critical!

Otherwise:
 singleton 255 `compare` singleton 127
is compiled to:
 case mallocByteString 2 of
     ForeignPtr f internals ->
          case writeWord8OffAddr# f 0 255 of _ ->
          case writeWord8OffAddr# f 0 127 of _ ->
          case eqAddr# f f of
                 False -> case compare (GHC.Prim.plusAddr# f 0)
                                       (GHC.Prim.plusAddr# f 0)
TODO: Find out if this distinction is true in idris.
-}

-- This exists in this form to support NonEmpty
-- This is also why it's public export
public export
unsafeCreateBytes : Nat -> (Block -> IO ()) -> Bytes
unsafeCreateBytes len f = MkB (unsafePerformIO $ allocateAndFill (cast len) f) 0 len


