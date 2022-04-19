const fs = require("fs/promises")

async function parseFile(file, fn) {
    let fh;
    try {
        fh = await fs.open(file)

        let data;
        try {
            data = await fh.readFile("UTF-8")
        } catch (e) {
            console.error(`Error reading ${file}. ${e.message}`)
        }
        
        fn(data)

    } catch (e) {
        console.error(`Error opening ${file}. ${e.message}`)
    } finally {
        fh.close()
    }
}

function firstPass(data) {
    const lines = data.split("\n")
    
    for (let line of lines) {
        if (line.startsWith("test")) {
            testsResults.write(TestResult.from(line, 1))
        }
    }
}

function secondPass(data) {
    const lines = data.split("\n")
    
    for (let line of lines) {
        if (line.startsWith("test")) {
            let testResult = TestResult.from(line, 2)
            let compTestResult = testsResults.read(testResult.name)
            if (compTestResult) {
                if (testResult.result !== compTestResult.result) {
                    testResult.difference = {type: TestResult.DIFFERENT_RESULT, diff: `${compTestResult.result} vs ${testResult.result}`}
                    differentResults.write(testResult)
                }
            } else {
                testResult.difference = {type: TestResult.NOT_EXISTS}
                differentResults.write(testResult)
            }
        }
    }
}

class TestResultCollection {
    tests = {}

    write(testResult) {
        const testName = testResult.name.split("/")
        let root = this.tests
        for (let i = 0; i < testName.length - 1; ++i) {
            const segment = testName[i]
            if (!root[segment]) {
                root[segment] = {}
            }
            root = root[segment]
        }
        root[testName[testName.length - 1]] = testResult
    }

    read(testName) {
        testName = testName.split("/")
        let root = this.tests
        for (let i = 0; i < testName.length - 1; ++i) {
            const segment = testName[i]
            if (!root[segment]) {
                root[segment] = {}
            }
            root = root[segment]
        }
        return root[testName[testName.length - 1]]
    }

    flatten() {
        const array = []

        function flattenObject(obj, array) {
            const keys = Object.keys(obj)
            for (let key of keys) {
                let val = obj[key]
                if (val instanceof TestResult) {
                    array.push(val)
                } else {
                    flattenObject(val, array)
                }
            }
        }
        flattenObject(this.tests, array)

        return array
    }
}

class TestResult {
    static NOT_EXISTS = Symbol("not exists")
    static DIFFERENT_RESULT = Symbol("different result")

    constructor(name, result, sourceFile, difference) {
        this.name = name;
        this.result = result;
        this.sourceFile = sourceFile;
        this.difference = difference;
    }

    static from(str, file) {
        const components = str.split(" | ")
        return new TestResult(components[0], components[1].replace(/[_\*]*/g, ""), file, undefined)
    }
}

let file1 = process.argv[2]
let file2 = process.argv[3]

var testsResults = new TestResultCollection()
var differentResults = new TestResultCollection();

(async () => {
    await parseFile(file1, firstPass)
    await parseFile(file2, secondPass)

    for (let testResult of differentResults.flatten()) {
        if (testResult.difference.type === TestResult.DIFFERENT_RESULT) {
            console.warn(testResult.name, testResult.difference.diff)
        }
    }

})()
