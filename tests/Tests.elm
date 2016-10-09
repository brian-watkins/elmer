module Tests exposing (..)

import Test exposing (..)

import Elmer.ElmerTests as ElmerTests
import Elmer.MatcherTests as MatcherTests
import Elmer.Event.ClickTests as ClickTests
import Elmer.Event.InputTests as InputTests
import Elmer.AppTests as AppTests

all : Test
all =
  describe "Elmer"
    [ ElmerTests.all
    , MatcherTests.all
    , ClickTests.all
    , InputTests.all
    , AppTests.all
    ]
