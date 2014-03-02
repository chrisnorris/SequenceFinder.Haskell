-- ./KMPratt 400000  "/Users/OdeMoor/Documents/text2.txt" "l"  +RTS -K20M  -N2 -s -RTS
-- ghc -O2 KMPratt.hs -rtsopts -threaded -eventlog

import Control.Exception
import System.Environment
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import Data.Word
import Control.Parallel.Strategies
import Control.Monad.Par
import Control.DeepSeq
import Control.Applicative
import Data.Int
import Data.ByteString.Search

main = do
    ( chunkSizeString : sourceFile : flag : _ ) <- getArgs
    let chunkSize = read chunkSizeString :: Int
    let tgt = B.pack [84,84,71,84,65,67,65,67,67,71,84,71,65,84,67,65]
    input <- LB.readFile sourceFile
    b <- evaluate $ deep
                  $ runEval
                  $ parMp (indices tgt)
                  $ (chunk chunkSize input)
    --mapM_ print $ concat b
    print $ scanl (\ (a1,a2) b1 -> (a1+1, fmap (+ (a1 * chunkSize)) b1 ) ) (0, []) b

deep ::  NFData b => b -> b
deep a = deepseq a a
data Rep a = Null | Node a (Rep a) (Rep a)

chunk ::  Int -> LB.ByteString -> [B.ByteString]
chunk chunkSize bstring =
    let sfbit = (fromInteger $ fromIntegral chunkSize) :: Int64 in
    if bstring == LB.empty then [] else
    let (a, b) = LB.splitAt sfbit bstring
    in  (LB.toStrict a) : chunk chunkSize b

--searchTarget :: [Word8]
--searchTarget = [84,84,71,84,65,67,65,67,67,71,84,71,65,84,67,65]

parMp ::  (t -> a) -> [t] -> Eval [a]
parMp f [] = return []
parMp f (a:as) = do
    b  <- rpar(f a)
    bs <- parMp f as
    return (b:bs)
