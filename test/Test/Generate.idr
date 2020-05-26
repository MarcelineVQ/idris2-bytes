module Test.Generate

import Data.Word.Word8
import Data.Bytes
-- import Control.Monad.State -- for making a MonadIO

import System.Random
-- TODO: Later on this should be a seedable prng for reproducable testing, and
-- for making pure values without unsafePerformIO


data PropResult = Error String | Sucess

data Tests : Type where
  MkTests : List PropResult -> Tests

prop : String -> Bool -> PropResult
prop name b = if b then Sucess
                   else Error (name ++ " failure")

runProps : List (String, Tests) -> IO ()
runProps ps = do for ps $ (\(name,p) => ?sdfsd)
                 ?sdsfd

replicateA : Applicative m => Nat -> m a -> m (List a)
replicateA 0 _ = pure []
replicateA (S k) act = [| act :: replicateA k act |]

-- remove the range on this later to test Word8 itself
arbitrary : Word8
arbitrary = unsafePerformIO $
  let r : IO Int = randomRIO (0,255) in cast <$> r

-- forAll : (Word8 -> a -> Bool) -> a -> IO Bool
-- forAll wf x = pure (wf !(arbitraryWord8) x)

test1 : a -> (a -> Bool) -> Bool
test1 x f = f x

test2 : a -> b -> (a -> b -> Bool) -> Bool
test2 x y f = f x y

infixr 0 &&
(&&) : a -> (a -> b) -> b

consTests : Tests
consTests = MkTests
  [ prop "cons1" $ arbitrary && \x => cons x empty == singleton x
  ]

appendTests : Tests
appendTests = MkTests
  [ prop "append1" $ cons 1 empty == singleton 1
  , prop "append2" $ cons 3 empty == singleton 3
  , prop "append3" $ cons 3 (singleton 2) == ?fdssd
  ]

test : IO ()
test = runProps $ [("cons"  , consTests  )
                  ,("append", appendTests)
                  ]





-- :exec replicateA 100000 (arbitraryWord8 >>= printLn)