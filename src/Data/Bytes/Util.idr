module Data.Bytes.Util

moduleName : String
moduleName = "Data.Bytes.Util"

export
replicateM : Applicative f => Int -> f a -> f (List a)
replicateM n act = if 0 >= n then pure []
                             else [| act :: replicateM (n-1) act |]

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

-------------------------------------------------
-- Lies
-------------------------------------------------

-- Why lie about totality?

-- One may ask "If a thing can fail is it not partial?" but consider the total
-- expression x :: xs, if we fail to allocate space when applying :: does that
-- mean :: was partial all along? That's simply a failure of the system we're
-- running on not a failure of totality. The single intended use of this lie is
-- during allocation of bytes in memory. This is why we're lying here: There's
-- no reason to infect a whole codebase with partiality when it's outside
-- allocation that is failing.

-- TODO: bring this up with other devs for idris about it's merits and negatives
-- The less backdoors the better imo but this seems like a innocuous thing.
private
total
%foreign "scheme:lambda (x) (blodwen-error-quit x)"
lie_idris_crash : String -> a

export
total
lieErrorCall : String -> String -> String -> a
lieErrorCall mod fn_name msg = lie_idris_crash $ mod ++ ":" ++ fn_name ++ ": " ++ msg

-------------------------------------------------


export
partial
errorCall : String -> String -> String -> a
errorCall mod fn_name msg = idris_crash $ mod ++ ":" ++ fn_name ++ ": " ++ msg

-- It'd be nice to have have statically enforced size limits but it's a little
-- onerous at this stage and not something we should be forcing on users.
-- Integer would be safer but there's a (likely) size and performance hit that
-- would need investigating, tied with the fact that our Bytes can only be Int
-- large in the first place so that just moves the check not elimates it.
export
partial
checkedAdd : String -> String -> Int -> Int -> Int
checkedAdd mod fun x y
  = let v = x + y
    in if v >= 0 then v else errorCall mod fun $
         "numeric overflow: " ++ show x ++ " + " ++ show y ++ " = " ++ show v

public export
total -- minus where negative results are just 0
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