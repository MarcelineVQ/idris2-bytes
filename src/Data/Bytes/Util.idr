module Data.Bytes.Util

moduleName : String
moduleName = "Data.Bytes.Util"

-- temp until we have a Word8 type
public export
Word8 : Type
Word8 = Int

public export
total
errorCall : String -> String -> String -> a
errorCall mod fn_name msg = idris_crash $ mod ++ ":" ++ fn_name ++ ":" ++ msg

total
public export
monus : Nat -> Nat -> Nat
monus n 0 = n
monus 0 n = 0
monus (S j) (S k) = monus j k
