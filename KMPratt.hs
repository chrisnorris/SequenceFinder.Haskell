-- ./KMPratt 400000  "/Users/OdeMoor/Documents/text2.txt" "l"  +RTS -K20M  -N2 -s -RTS
-- ghc -O2 KMPratt.hs -rtsopts -threaded -eventlog

import           Control.Exception
import           System.Environment
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString               as B
import           Data.Word
import           Control.Parallel.Strategies
import           Control.Monad.Par
import           Control.DeepSeq
import           Control.Applicative
import           Data.Int
import           Data.ByteString.Search

deep :: NFData b => b -> b
deep a = deepseq a a
data Rep a = Null | Node a (Rep a) (Rep a)

matches :: [Word8] -> [Word8] -> [Int]
matches ws = map fst . filter (ok . snd) . scanl step (0, root)
 where
  ok (Node vs l r) = null vs
  step (n, t) x = (n + 1, op t x)
  op Null                x = root
  op (Node []       l r) x = op l x
  op (Node (v : vs) l r) x = if v == x then r else op l x
  root = grep Null ws
  grep l []       = Node [] l Null
  grep l (v : vs) = Node (v : vs) l (grep (op l v) vs)

chunk :: Int -> B.ByteString -> [B.ByteString]
chunk chunkSize bstring = if bstring == B.empty
  then []
  else let (a, b) = B.splitAt chunkSize bstring in a : chunk chunkSize b

searchTarget :: [Word8]
searchTarget = [66, 65, 65, 65, 65, 65, 66, 65, 67, 67, 67, 68, 67, 67]

main = do
  (chunkSizeString : sourceFile : flag : _) <- getArgs
  let chunkSize = read chunkSizeString :: Int
  input <- B.readFile sourceFile
  case flag of
    "i" -> do
      b <-
        evaluate
        $   deep
        $   runEval
        $   parMp (matches searchTarget)
        $   B.unpack
        <$> chunk chunkSize input
      mapM_ print $ concat b
    "l" -> do
      b <-
        evaluate
        $ deep
        $ runEval
        $ parMp (indices $ B.pack searchTarget)
        $ chunk chunkSize input
      mapM_ print $ concat b

    "" -> return ()

parMp :: (t -> a) -> [t] -> Eval [a]
parMp f []       = return []
parMp f (a : as) = do
  b  <- rpar (f a)
  bs <- parMp f as
  return (b : bs)
