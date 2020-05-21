module Data.Bytes.Internal

import Data.Bytes.Util
import Data.Bytes.Prim

import Data.Word.Word8

-- Unfortunately neccesary addition atm, no idea why. For some reason things
-- from Data.Buffer are in scope.
-- For instance I shouldn't be able to define Bytes here as it uses Buffer
-- from Data.Bytes which is not re-exported from Data.Bytes.Prim
-- Supposedly this is also occuring in contrib:Data/List/TailRec.idr
%hide Data.Buffer.getByte

private
moduleName : String
moduleName = "Data.Bytes.Internal"

-- Our Bytes type, a Ptr to a `block` of memory, its size in bytes,
-- and the current 0-based offset into that memory.
public export
data Bytes : Type where
     MkB : (1 b : Buffer) -> (pos : Int) -> (len : Nat) -> Bytes

-- Performance: For the purposes of proving NonEmpty, if Nat is compiled to Int
-- then this is as performant as if we didn't use Nat but we gain static size
-- computing. However if it's compiled to Integer then I'm not sure we gained
-- anything over simply having two constructors for Bytes, empty and not. The
-- default for Nat to compile to is Integer but realistically Int is safe since
-- you'll run out of memory before you fill up an Int. That's especially the
-- case once we have Word (uint) types if we compile Nat to those, which is
-- only reasonable since Nat can't be negative anyway.


public export
data NonEmpty : Bytes -> Type where
  IsNonEmpty : NonEmpty (MkB _ _ (S _))

-- ^ how can I provide this proof without making the user need access
-- to MkB? Simply by having my own functions imlpicity prove that when
-- used? Things like cons and singleton and append? And if a
-- ByteString comes from elsewhere we use Dec?

export
nonEmpty : (b : Bytes) -> Dec (NonEmpty b)
nonEmpty (MkB _ _ 0) = No (\case _ impossible)
nonEmpty (MkB _ _ (S _)) = Yes IsNonEmpty

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

-- This exists in this form to support NonEmpty.
-- This is also why it's public export.
public export
unsafeCreateBytes : Nat -> (MutBuffer -> IO ()) -> Bytes
unsafeCreateBytes len f = MkB (unsafePerformIO $ allocateAndFill (cast len) f) 0 len


-- Intended complexity: O(n+m)
-- No need to export, simply use <+>
private
append : Bytes -> Bytes -> Bytes
append (MkB xs xpos xlen) (MkB ys ypos ylen)
  = unsafeCreateBytes (xlen + ylen) $ \p => do
      let xlen' = cast xlen
          ylen' = cast ylen
      copyBuffer xs xpos xlen' p 0
      copyBuffer ys ypos ylen' p xlen'

private
show_buffer : Buffer -> String
show_buffer buf
    = "[" ++ fastAppend (intersperse "," (map show (unsafePerformIO (bufferData buf)))) ++ "]"
         

-- Debug use ONLY, **don't ever use this**
export
Show Bytes where
  show (MkB b pos len) = "MkB " ++ show_buffer b ++ " " ++ show pos ++ " " ++ show len

private
compareBytes : Bytes -> Bytes -> Ordering
compareBytes (MkB _   _    0)    (MkB _   _    0)    = EQ
compareBytes (MkB xb xpos xlen) (MkB yb ypos ylen) =
    unsafePerformIO $ go xlen ylen xb yb
  where
    go : Nat -> Nat -> Buffer -> Buffer -> IO Ordering
    go 0 (S _) _ _ = pure LT
    go (S _) 0 _ _ = pure GT
    go 0 0 xb yb = [| getByte xb 0 `compare` getByte yb 0 |]
    go (S k) (S j) xb yb
      = do EQ <- [| getByte xb (cast k) `compare` getByte yb (cast j) |]
             | res => pure res
           go k j xb yb

-- Offset will tend to be 0 so we first compare length, then buffer object
-- equality, then offset. The buffer should be a simple ptr comparison so it's
-- worth doing before trying for a full compareBytes. This order is partly
-- because it makes sense and partly because ByteString does it this way.
-- However I'm not sure why length is checked before buffer object equality,
-- perhaps in ghc length has a cheaper comparison than than the extra
-- foreignPtr prodding that ptr equality needs.

-- To ask if we are nominally equal.
infixl 9 `basicEq`
private
basicEq : Bytes -> Bytes -> Bool
MkB xb xpos xlen `basicEq` MkB yb ypos ylen
    = xlen == ylen && (unsafePerformIO (exactEqBuff xb yb) && xpos == ypos)

export
implementation
Eq Bytes where
  x == y = x `basicEq` y || compareBytes x y == EQ

-- basicEq is much speedier than comparison, use it when possible!
export
Ord Bytes where
  compare = compareBytes
  x >= y = x `basicEq` y || compareBytes x y == GT
  x <= y = x `basicEq` y || compareBytes x y == LT

export
Semigroup Bytes where
  (<+>) = append

export
Monoid Bytes where
  neutral = unsafeCreateBytes 0 (\_ => pure ())