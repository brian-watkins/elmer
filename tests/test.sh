#!/bin/sh

set -ex

rm -f elm.js
rm -Rf elm-stuff
rm -Rf elm_home/0.19.0/package/elm-explorations/elmer

mkdir -p elm_home
mkdir -p elm_home/0.19.0/package
mkdir -p elm_home/0.19.0/package/elm-explorations/elmer/2.0.0

cp -R ../src ./elm_home/0.19.0/package/elm-explorations/elmer/2.0.0/
node ./prepareElmJson.js ./testableModules.json ../elm.json ./elm_home/0.19.0/package/elm-explorations/elmer/2.0.0/elm.json

# Note -- Don't need to do this everytime, only when the dependencies change
# if which runhaskell; then
    # this produces ./versions.dat, but that file
    # is checked in, so it's fine to skip regenerating if
    # haskell is not available (such as on CI)
    # runhaskell MakeTestRegistry.hs
# fi
# cp ./versions.dat elm_home/0.19.0/package/versions.dat

export ELM_HOME="$(pwd)"/elm_home

elm make src/Main.elm --output elm.js

node runTests.js
