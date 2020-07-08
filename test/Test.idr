module Test

import Test.LazyTest

testStrict  : IO ()
testLazy    : IO ()
testBuilder : IO ()

partial
main : IO ()
main = do
  -- testStrict
  -- testLazy
  -- testBuilder
  printLn "Testing lazy file IO:" *> lazyIOTest
  -- This crashes currently for errors, obvious we'll want a better testing
  -- method as we go.

