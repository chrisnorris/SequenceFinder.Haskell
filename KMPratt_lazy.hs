-- ghc -O2 KMPratt.hs -rtsopts -threaded -eventlog
-- ./KMPratt_lazy 5000000  "/Users/RBird/Downloads/LargeDataSet.txt" +RTS -K16M -H128m -N8 -s -RTS

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad.Par           hiding (parMap)
import Control.Parallel.Strategies hiding (parMap)
import Data.ByteString.Search
import Data.Int
import Data.Word
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

main = do
    ( chunkSizeString : sourceFile : _ ) <- getArgs
    let chunkSize = read chunkSizeString :: Int
    let tgt = B.pack [84,84,71,84,65,67,65,67,67,71,84,71,65,84,67,65]
    input <- LB.readFile sourceFile
    result <- evaluate $ deep $ runEval
                       $ parMap (indices tgt) (chunk chunkSize input)
    print "Results as [(chunkNumber, [file_positions])]"
    print $ filter ( \ (_,resultsList) -> not (null resultsList) )
          $ scanl  ( \ (index, _) chunkResult  ->
                       (index+1, fmap (+ (index * chunkSize)) chunkResult) )
                       (0, []) result

deep ::  NFData b => b -> b
deep a = deepseq a a

chunk ::  Int -> LB.ByteString -> [B.ByteString]
chunk chunkSize bstring =
    let sfbit = (fromInteger $ fromIntegral chunkSize) :: Int64 in
    if bstring == LB.empty then [] else
    let (first, rest) = LB.splitAt sfbit bstring
    in  LB.toStrict first : chunk chunkSize rest

parMap ::  (t -> a) -> [t] -> Eval [a]
parMap f [] = return []
parMap f (a:as) = do
    b  <- rpar(f a)
    bs <- parMap f as
    return (b:bs)
