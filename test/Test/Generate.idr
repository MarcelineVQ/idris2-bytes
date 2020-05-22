module Test.Generate

import System.Random
import Data.Word.Word8

import Control.Monad.State -- for making a MonadIO

replicateA : Applicative m => Nat -> m a -> m (List a)
replicateA 0 _ = pure []
replicateA (S k) act = [| act :: replicateA k act |]

-- remove the range on this later to test Word8 itself
arbitraryWord8 : IO Word8
arbitraryWord8 = let r : IO Int = randomRIO (0,255) in cast <$> r



-- :exec replicateA 100000 (arbitraryWord8 >>= printLn)