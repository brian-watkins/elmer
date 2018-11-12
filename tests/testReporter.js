var reporter = () => {

    var total = 0
    var passed = 0
    var failed = 0

    var failureMessages = []

    var testSuiteWillStart = () => {
        console.log("Running tests ...")
    }

    var testDidFinish = (testResult) => {
        total += 1
        if (testResult.messages.length === 0) {
            passed += 1
        } else {
            failed += 1
            failureMessages.push({
                description: testResult.descriptions.reverse().join(", "),
                message: testResult.messages.join("\n")
            })
        }
    }

    var testSuiteDidFinish = () => {
        console.log("Test suite finished.")
        console.log()
        failureMessages.forEach((failure) => {
            console.log("-----------------------\n")
            console.log("Test failed:", failure.description)
            console.log()
            console.log(failure.message)
            console.log()
        })
        console.log("-----------------------\n")
        console.log("Total tests:", total)
        console.log("Passed:", passed)
        console.log("Failed:", failed)
        console.log()
    }

    return {
        testSuiteWillStart,
        testSuiteDidFinish,
        testDidFinish
    }
}

module.exports = reporter()