SequenceFinder.Haskell
======================
compile with:

ghc -O2 KMPratt.hs -rtsopts -threaded -eventlog

run with:
./KMPratt [chunksize] [sourcefile] +RTS [stacksize{-K20M}]  -N2 -s -RTS
