const fs = require('fs')

const testableModulesFile = process.argv[2]
const inputFile = process.argv[3]
const outputFile = process.argv[4]

let testableModulesJson = JSON.parse(fs.readFileSync(testableModulesFile))
let elmJson = JSON.parse(fs.readFileSync(inputFile))

elmJson["exposed-modules"] = elmJson["exposed-modules"].concat(testableModulesJson["exposed-modules"])

fs.writeFileSync(outputFile, JSON.stringify(elmJson, null, 2))

