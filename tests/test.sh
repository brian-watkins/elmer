#!/bin/sh

set -ex

export ELMER_VERSION="4.0.0"
export ELM_HOME="$(pwd)"/elm_home
export ELM_PACKAGES_HOME="${ELM_HOME}/0.19.0/package"
export ELMER_HOME="${ELM_PACKAGES_HOME}/elm-explorations/elmer/${ELMER_VERSION}"

rm -f elm.js
rm -Rf elm-stuff
rm -Rf ${ELMER_HOME}

mkdir -p ${ELMER_HOME}

cp -R ../src ${ELMER_HOME}/
node ./prepareElmJson.js ./testableModules.json ../elm.json ${ELMER_HOME}/elm.json

# Note -- Don't need to do this everytime, only when the dependencies change
# if which runhaskell; then
    # this produces ./versions.dat, but that file
    # is checked in, so it's fine to skip regenerating if
    # haskell is not available (such as on CI)
    # runhaskell MakeTestRegistry.hs
# fi
# cp ./versions.dat ${ELM_PACKAGES_HOME}/versions.dat

elm make src/Main.elm --output elm.js

node runTests.js
