#!/bin/sh

set -ex

export ELMER_VERSION="5.0.0"
export ELM_HOME="$(pwd)"/elm_home
export ELM_PACKAGES_HOME="${ELM_HOME}/0.19.0/package"
export ELMER_HOME="${ELM_PACKAGES_HOME}/elm-explorations/elmer/${ELMER_VERSION}"

rm -f elm.js
rm -rf elm-stuff
rm -rf ${ELMER_HOME}

mkdir -p ${ELMER_HOME}

cp -R ../src ${ELMER_HOME}/
node ./prepareElmJson.js ./testableModules.json ../elm.json ${ELMER_HOME}/elm.json

node ./packageRegistryWriter.js ${ELM_PACKAGES_HOME}/versions.dat

elm make src/Main.elm --output elm.js

node runTests.js
