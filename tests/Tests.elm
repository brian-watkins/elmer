module Tests exposing (..)

import Test exposing (..)

import Elmer.ElmerTests as ElmerTests
import Elmer.MatcherTests as MatcherTests
import Elmer.EventTests as EventTests
import Elmer.AppTests as AppTests
import Elmer.TestExtraTests as TestExtraTests

all : Test
all =
  describe "Elmer"
    [ ElmerTests.all
    , MatcherTests.all
    , EventTests.all
    , AppTests.all
    , TestExtraTests.all
    ]
