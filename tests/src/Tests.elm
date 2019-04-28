module Tests exposing (all)

import Test exposing (..)

import Elmer.HtmlTests as HtmlTests
import Elmer.HtmlQueryTests as HtmlQueryTests
import Elmer.HtmlMatcherTests as HtmlMatcherTests
import Elmer.EventTests as EventTests
import Elmer.InputEventTests as InputEventTests
import Elmer.FocusEventTests as FocusEventTests
import Elmer.MouseEventTests as MouseEventTests
import Elmer.HtmlLazyTests as HtmlLazyTests
import Elmer.HtmlKeyedTests as HtmlKeyedTests
import Elmer.ElementTests as ElementTests
import Elmer.ElmerTests as ElmerTests
import Elmer.SpyTests as SpyTests
import Elmer.SpyMatcherTests as SpyMatcherTests
import Elmer.SpyFakeTests as SpyFakeTests
import Elmer.CommandTests as CommandTests
import Elmer.TaskTests as TaskTests
import Elmer.RuntimeTests as RuntimeTests
import Elmer.HttpTests as HttpTests
import Elmer.HttpMatcherTests as HttpMatcherTests
import Elmer.HttpRouteTests as HttpRouteTests
import Elmer.HttpRequestTests as HttpRequestTests
import Elmer.HttpResultTests as HttpResultTests
import Elmer.HttpTaskTests as HttpTaskTests
import Elmer.GivenCommandTests as GivenCommandTests
import Elmer.PrinterTests as PrinterTests
import Elmer.SubscriptionTests as SubscriptionTests
import Elmer.TestStateTests as TestsStateTests
import Elmer.PortTests as PortTests
import Elmer.RandomTests as RandomTests
import Elmer.WorkerTests as WorkerTests
import Elmer.TripleComponentTests as TripleComponentTests
import Elmer.ApplicationTests as ApplicationTests
import Elmer.DocumentTests as DocumentTests
import Elmer.NavigationTests as NavigationTests
import Elmer.ComponentTests as ComponentTests
import Elmer.DemoAppTests as DemoAppTests
import Elmer.BrowserTests as BrowserTests
import Elmer.HtmlCustomTests as HtmlCustomTests
import Elmer.SpySpanTests as SpySpanTests
import Elmer.ValueTests as ValueTests

all : Test
all =
    Test.concat 
    [ BrowserTests.all
    , DemoAppTests.all
    , ComponentTests.all
    , NavigationTests.all
    , DocumentTests.all
    , ApplicationTests.all
    , TripleComponentTests.all
    , WorkerTests.all
    , RandomTests.all
    , PortTests.all
    , TestsStateTests.all
    , SubscriptionTests.all
    , PrinterTests.all
    , HttpTaskTests.all
    , HttpResultTests.all
    , GivenCommandTests.all
    , HttpRequestTests.all
    , HttpRouteTests.all
    , HttpMatcherTests.all
    , HttpTests.all
    , RuntimeTests.all
    , TaskTests.all
    , CommandTests.all
    , SpyFakeTests.all
    , SpyMatcherTests.all
    , SpyTests.all
    , SpySpanTests.all
    , ElementTests.all
    , ElmerTests.all
    , HtmlKeyedTests.all
    , HtmlLazyTests.all
    , HtmlCustomTests.all
    , HtmlTests.all
    , HtmlQueryTests.all
    , HtmlMatcherTests.all
    , EventTests.all
    , InputEventTests.all
    , FocusEventTests.all
    , MouseEventTests.all
    , ValueTests.all
    ]