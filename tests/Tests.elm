module Tests exposing (..)

import Test exposing (..)

import Elmer.ElmerTests
import Elmer.HtmlTests
import Elmer.MatcherTests
import Elmer.EventTests
import Elmer.MouseEventTests
import Elmer.DemoAppTests
import Elmer.NavigationTests
import Elmer.ComponentTests
import Elmer.HttpTests
import Elmer.HttpStubTests
import Elmer.HttpMatcherTests
import Elmer.RuntimeTests
import Elmer.ElementTests
import Elmer.CommandTests
import Elmer.PrinterTests
import Elmer.SubscriptionTests

all : Test
all =
  describe "Elmer"
    [ Elmer.ElmerTests.all
    , Elmer.HtmlTests.all
    , Elmer.ElementTests.all
    , Elmer.MatcherTests.all
    , Elmer.EventTests.all
    , Elmer.MouseEventTests.all
    , Elmer.DemoAppTests.all
    , Elmer.NavigationTests.all
    , Elmer.ComponentTests.all
    , Elmer.HttpTests.all
    , Elmer.HttpStubTests.all
    , Elmer.HttpMatcherTests.all
    , Elmer.RuntimeTests.all
    , Elmer.CommandTests.all
    , Elmer.SubscriptionTests.all
    , Elmer.PrinterTests.all
    ]
