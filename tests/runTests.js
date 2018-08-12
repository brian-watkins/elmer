const Elm = require("./elm.js")
const reporter = require("./testReporter")


var app = Elm.Elm.Main.init();

app.ports.sendTestEvent.subscribe((event) => {
    if (event === "DONE") {
        reporter.testSuiteDidFinish()
    }
})

app.ports.sendTestResult.subscribe((testResult) => {
    reporter.testDidFinish(testResult)
    app.ports.runNextTest.send(null)
})

reporter.testSuiteWillStart()
app.ports.runNextTest.send(null)


