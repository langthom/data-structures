#!/bin/bash

# compile it
ghc -threaded --make Main.hs

# this call uses all cores available
./Main +RTS -N -RTS

# clean up afterwards
rm Main *.{hi,o}
