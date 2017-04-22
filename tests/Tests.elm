module Tests exposing (..)

import Test exposing (..)

import Elmer.ElmerTests
import Elmer.HtmlTests
import Elmer.MatcherTests
import Elmer.EventTests
import Elmer.MouseEventTests
import Elmer.FocusEventTests
import Elmer.InputEventTests
import Elmer.DemoAppTests
import Elmer.NavigationTests
import Elmer.ComponentTests
import Elmer.HttpTests
import Elmer.HttpStubTests
import Elmer.HttpMatcherTests
import Elmer.HttpResultTests
import Elmer.HttpRequestTests
import Elmer.RuntimeTests
import Elmer.ElementTests
import Elmer.CommandTests
import Elmer.PrinterTests
import Elmer.SubscriptionTests
import Elmer.SpyTests
import Elmer.SpyMatcherTests
import Elmer.PortTests
import Elmer.ComponentStateTests

all : Test
all =
  describe "Elmer"
    [ Elmer.ElmerTests.all
    , Elmer.ComponentStateTests.all
    , Elmer.HtmlTests.all
    , Elmer.ElementTests.all
    , Elmer.MatcherTests.all
    , Elmer.EventTests.all
    , Elmer.MouseEventTests.all
    , Elmer.FocusEventTests.all
    , Elmer.InputEventTests.all
    , Elmer.DemoAppTests.all
    , Elmer.NavigationTests.all
    , Elmer.ComponentTests.all
    , Elmer.HttpTests.all
    , Elmer.HttpStubTests.all
    , Elmer.HttpMatcherTests.all
    , Elmer.HttpResultTests.all
    , Elmer.HttpRequestTests.all
    , Elmer.RuntimeTests.all
    , Elmer.CommandTests.all
    , Elmer.SubscriptionTests.all
    , Elmer.PrinterTests.all
    , Elmer.SpyTests.all
    , Elmer.SpyMatcherTests.all
    , Elmer.PortTests.all
    ]
