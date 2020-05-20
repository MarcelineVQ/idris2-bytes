module Data.Bytes.Util

moduleName : String
moduleName = "Data.Bytes.Util"

-- temp until we have a Word8 type
-- public export
-- Word8 : Type
-- Word8 = Int

-- Will this get optomized if Nat is optomized to Int?
-- I doubt it. We almost need a prim for that.
export
intToNat : Int -> Nat
intToNat i = if i >= 0 then go i else Z
  where
    go : Int -> Nat
    go 0 = Z
    go k = 1 + go (k-1)

public export
total
errorCall : String -> String -> String -> a
errorCall mod fn_name msg = idris_crash $ mod ++ ":" ++ fn_name ++ ":" ++ msg

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