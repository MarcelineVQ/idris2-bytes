module Data.Bytes.Util

moduleName : String
moduleName = "Data.Bytes.Util"

-- temp until we have a Word8 type
-- public export
-- Word8 : Type
-- Word8 = Int

-- Try not to use this, it'd be quite slow, better to use a casting prim of
-- some kind when one exists. This still has to +1 over and over after Nat is
-- optimized to Integer.
export
intToNat : Int -> Nat
intToNat i = if i >= 0 then go i else Z
  where
    go : Int -> Nat
    go 0 = Z
    go k = 1 + go (k-1)

public export
partial
errorCall : String -> String -> String -> a
errorCall mod fn_name msg = idris_crash $ mod ++ ":" ++ fn_name ++ ": " ++ msg

-- It'd be nice to have have statically enforced size limits but it's a little
-- onerous at this stage and not something we should be forcing on users.
-- Integer would be safer but there's a (likely) size and performance hit that
-- would need investigating, tied with the fact that our Bytes can only be Int
-- large in the first place so that just moves the check not elimates it.
export
checkedAdd : String -> String -> Int -> Int -> Int
checkedAdd mod fun x y
  = let v = x + y
    in if v >= 0 then v else errorCall mod fun $
         "numeric overflow: " ++ show x ++ " + " ++ show y ++ " = " ++ show v

public export
total
monus : Nat -> Nat -> Nat
monus n 0 = n
monus 0 n = 0
monus (S j) (S k) = monus j k

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

infixl 4 <$,$>

export
(<$) : Functor f => a -> f b -> f a
x <$ y = map (const x) y

export
($>) : Functor f => f a -> b -> f b
($>) = flip (<$)

-- For when Lazy is causing type problems
infixr 4 &&|
export
(&&|) : Bool -> Bool -> Bool
(&&|) x y = x && y