module Tests exposing (..)

import Test exposing (..)

import Elmer.ElmerTests as ElmerTests
import Elmer.MatcherTests as MatcherTests
import Elmer.EventTests as EventTests
import Elmer.AppTests as AppTests
import Elmer.TestExtraTests as TestExtraTests
import Elmer.NavigationTests as NavigationTests
import Elmer.ComponentTests as ComponentTests
import Elmer.HttpTests as HttpTests
import Elmer.HttpStubTests as HttpStubTests
import Elmer.RuntimeTests as RuntimeTests
import Elmer.NodeTests as NodeTests

all : Test
all =
  describe "Elmer"
    [ ElmerTests.all
    , MatcherTests.all
    , EventTests.all
    , AppTests.all
    , TestExtraTests.all
    , NavigationTests.all
    , ComponentTests.all
    , HttpTests.all
    , HttpStubTests.all
    , RuntimeTests.all
    , NodeTests.all
    ]
