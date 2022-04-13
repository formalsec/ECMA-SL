var NotEarlyErrorString = "NotEarlyError";
var EarlyErrorRePat = "^((?!" + NotEarlyErrorString + ").)*$";
var NotEarlyError = new Error(NotEarlyErrorString);

function Test262Error(message) {
    this.message = message || "";
    //console.setInternalClass(this, "Error")
    //console.setInternalName(this, "Test262Error")
}

Test262Error.prototype.toString = function () {
    return "Test262Error: " + this.message;
};

var $ERROR;
$ERROR = function $ERROR(message) {
    throw new Test262Error(message);
};

function testFailed(message) {
    $ERROR(message);
}

/* @id harnessAssert */
function assert(mustBeTrue, message) {
    if (mustBeTrue === true) {
        return;
    }

    if (message === undefined) {
        message = 'Expected true but got ' + String(mustBeTrue);
    }
    $ERROR(message);
}

/* @id harnessIsSameValue */
assert._isSameValue = function (a, b) {
    if (a === b) {
        // Handle +/-0 vs. -/+0
        return a !== 0 || 1 / a === 1 / b;
    }

    // Handle NaN vs. NaN
    return a !== a && b !== b;
};

/* @id harnessSameValue */
assert.sameValue = function (actual, expected, message) {
    if (assert._isSameValue(actual, expected)) {
        return;
    }

    if (message === undefined) {
        message = '';
    } else {
        message += ' ';
    }

    message += 'Expected SameValue(<<' + String(actual) + '>>, <<' + String(expected) + '>>) to be true';

    $ERROR(message);
};

/* @id harnessNotSameValue */
assert.notSameValue = function (actual, unexpected, message) {
    if (!assert._isSameValue(actual, unexpected)) {
        return;
    }

    if (message === undefined) {
        message = '';
    } else {
        message += ' ';
    }

    message += 'Expected SameValue(<<' + String(actual) + '>>, <<' + String(unexpected) + '>>) to be false';

    $ERROR(message);
};

/* @id harnessThrows */
assert.throws = function (expectedErrorConstructor, func, message) {
    if (typeof func !== "function") {
        $ERROR('assert.throws requires two arguments: the error constructor ' +
            'and a function to run');
        return;
    }
    if (message === undefined) {
        message = '';
    } else {
        message += ' ';
    }

    try {
        func();
    } catch (thrown) {
        if (typeof thrown !== 'object' || thrown === null) {
            message += 'Thrown value was not an object!';
            $ERROR(message);
        } else if (thrown.constructor !== expectedErrorConstructor) {
            message += 'Expected a ' + expectedErrorConstructor.name + ' but got a ' + thrown.constructor.name;
            $ERROR(message);
        }
        return;
    }

    message += 'Expected a ' + expectedErrorConstructor.name + ' to be thrown but no exception was thrown at all';
    $ERROR(message);
};

/* @id harnessIsConfigurable */
function isConfigurable(obj, name) {
    try {
        delete obj[name];
    } catch (e) {
        if (!(e instanceof TypeError)) {
            $ERROR("Expected TypeError, got " + e);
        }
    }
    return !obj.hasOwnProperty(name);
}

function isEnumerable(obj, name) {
    return obj.hasOwnProperty(name) &&
    obj.propertyIsEnumerable(name);
}

function isEqualTo(obj, name, expectedValue) {
    var actualValue = obj[name];

    return assert._isSameValue(actualValue, expectedValue);
}

function isWritable(obj, name, verifyProp, value) {
    var newValue = value || "unlikelyValue";
    var hadValue = obj.hasOwnProperty(name);
    var oldValue = obj[name];
    var writeSucceeded;

    try {
        obj[name] = newValue;
    } catch (e) {
        if (!(e instanceof TypeError)) {
            $ERROR("Expected TypeError, got " + e);
        }
    }

    writeSucceeded = isEqualTo(obj, verifyProp || name, newValue);

    // Revert the change only if it was successful (in other cases, reverting
    // is unnecessary and may trigger exceptions for certain property
    // configurations)
    if (writeSucceeded) {
      if (hadValue) {
        obj[name] = oldValue;
      } else {
        delete obj[name];
      }
    }

    return writeSucceeded;
}

function verifyEqualTo(obj, name, value) {
    if (!isEqualTo(obj, name, value)) {
        $ERROR("Expected obj[" + String(name) + "] to equal " + value +
                   ", actually " + obj[name]);
    }
}

function verifyWritable(obj, name, verifyProp, value) {
    if (!verifyProp) {
        assert(Object.getOwnPropertyDescriptor(obj, name).writable,
               "Expected obj[" + String(name) + "] to have writable:true.");
    }
    if (!isWritable(obj, name, verifyProp, value)) {
        $ERROR("Expected obj[" + String(name) + "] to be writable, but was not.");
    }
}

function verifyNotWritable(obj, name, verifyProp, value) {
    if (!verifyProp) {
        assert(!Object.getOwnPropertyDescriptor(obj, name).writable,
               "Expected obj[" + String(name) + "] to have writable:false.");
    }
    if (isWritable(obj, name, verifyProp)) {
        $ERROR("Expected obj[" + String(name) + "] NOT to be writable, but was.");
    }
}

function verifyEnumerable(obj, name) {
    assert(Object.getOwnPropertyDescriptor(obj, name).enumerable,
           "Expected obj[" + String(name) + "] to have enumerable:true.");
    if (!isEnumerable(obj, name)) {
        $ERROR("Expected obj[" + String(name) + "] to be enumerable, but was not.");
    }
}

function verifyNotEnumerable(obj, name) {
    assert(!Object.getOwnPropertyDescriptor(obj, name).enumerable,
           "Expected obj[" + String(name) + "] to have enumerable:false.");
    if (isEnumerable(obj, name)) {
        $ERROR("Expected obj[" + String(name) + "] NOT to be enumerable, but was.");
    }
}

function verifyConfigurable(obj, name) {
    assert(Object.getOwnPropertyDescriptor(obj, name).configurable,
           "Expected obj[" + String(name) + "] to have configurable:true.");
    if (!isConfigurable(obj, name)) {
        $ERROR("Expected obj[" + String(name) + "] to be configurable, but was not.");
    }
}

function verifyNotConfigurable(obj, name) {
    assert(!Object.getOwnPropertyDescriptor(obj, name).configurable,
           "Expected obj[" + String(name) + "] to have configurable:false.");
    if (isConfigurable(obj, name)) {
        $ERROR("Expected obj[" + String(name) + "] NOT to be configurable, but was.");
    }
}

function getPrecision(num) {
	//TODO: Create a table of prec's,
	//      because using Math for testing Math isn't that correct.

	var log2num = Math.log(Math.abs(num)) / Math.LN2;
	var pernum = Math.ceil(log2num);
	return 2 * Math.pow(2, -52 + pernum);
}

var prec;

function isEqual(num1, num2)
{
        if ((num1 === Infinity)&&(num2 === Infinity))
        {
                return(true);
        }
        if ((num1 === -Infinity)&&(num2 === -Infinity))
        {
                return(true);
        }
        prec = getPrecision(Math.min(Math.abs(num1), Math.abs(num2)));
        return(Math.abs(num1 - num2) <= prec);
        //return(num1 === num2);
}

var $MAX_ITERATIONS = 100000;

function arrayContains(arr, expected) {
    var found;
    for (var i = 0; i < expected.length; i++) {
        found = false;
        for (var j = 0; j < arr.length; j++) {
            if (expected[i] === arr[j]) {
                found = true;
                break;
            }
        }
        if (!found) {
            return false;
        }
    }
    return true;
}

function compareArray (a, b) {
    if (b.length !== a.length) {
        return false;
    }

    for (var i = 0; i < a.length; i++) {
        if (b[i] !== a[i]) {
            return false;
        }
    }
    return true;
}

assert.compareArray = compareArray;

function assertRelativeDateMs(date, expectedMs) {
	  var actualMs = date.valueOf();
	  var localOffset = date.getTimezoneOffset() * 60000;

	  if (actualMs - localOffset !== expectedMs) {
	    $ERROR(
	      'Expected ' + date + ' to be ' + expectedMs +
	      ' milliseconds from the Unix epoch, instead of ' + (actualMs - localOffset)
	    );
	  }
	}

var HoursPerDay = 24;
var MinutesPerHour = 60;
var SecondsPerMinute = 60;

var msPerDay = 86400000;
var msPerSecond = 1000;
var msPerMinute = 60000;
var msPerHour = 3600000;

var date_1899_end = -2208988800001;
var date_1900_start = -2208988800000;
var date_1969_end = -1;
var date_1970_start = 0;
var date_1999_end = 946684799999;
var date_2000_start = 946684800000;
var date_2099_end = 4102444799999;

var date_2100_start = 4102444800000;

function verifyProperty(obj, name, desc, options) {
    assert(
        arguments.length > 2,
        'verifyProperty should receive at least 3 arguments: obj, name, and descriptor'
    );

    var originalDesc = Object.getOwnPropertyDescriptor(obj, name);
    var nameStr = String(name);

    // Allows checking for undefined descriptor if it's explicitly given.
    if (desc === undefined) {
        assert.sameValue(
        originalDesc,
        undefined,
        "obj['" + nameStr + "'] descriptor should be undefined"
        );

        // desc and originalDesc are both undefined, problem solved;
        return true;
    }

    assert(
        Object.prototype.hasOwnProperty.call(obj, name),
        "obj should have an own property " + nameStr
    );

    assert.notSameValue(
        desc,
        null,
        "The desc argument should be an object or undefined, null"
    );

    assert.sameValue(
        typeof desc,
        "object",
        "The desc argument should be an object or undefined, " + String(desc)
    );

    var failures = [];

    if (Object.prototype.hasOwnProperty.call(desc, 'value')) {
        /* changed to isSameValue to assert._isSameValue */
        if (!assert._isSameValue(desc.value, originalDesc.value)) {
        failures.push("descriptor value should be " + desc.value);
        }
    }

    if (Object.prototype.hasOwnProperty.call(desc, 'enumerable')) {
        if (desc.enumerable !== originalDesc.enumerable ||
            desc.enumerable !== isEnumerable(obj, name)) {
        failures.push('descriptor should ' + (desc.enumerable ? '' : 'not ') + 'be enumerable');
        }
    }

    if (Object.prototype.hasOwnProperty.call(desc, 'writable')) {
        if (desc.writable !== originalDesc.writable ||
            desc.writable !== isWritable(obj, name)) {
        failures.push('descriptor should ' + (desc.writable ? '' : 'not ') + 'be writable');
        }
    }

    if (Object.prototype.hasOwnProperty.call(desc, 'configurable')) {
        if (desc.configurable !== originalDesc.configurable ||
            desc.configurable !== isConfigurable(obj, name)) {
        failures.push('descriptor should ' + (desc.configurable ? '' : 'not ') + 'be configurable');
        }
    }

    assert(!failures.length, failures.join('; '));

    if (options && options.restore) {
        Object.defineProperty(obj, name, originalDesc);
    }

    return true;
}


function $DETACHBUFFER(buffer) {
    if (!$262 || typeof $262.detachArrayBuffer !== "function") {
        throw new Test262Error("No method available to detach an ArrayBuffer");
    }
    $262.detachArrayBuffer(buffer);
}


// Copyright (C) 2015 André Bargull. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
description: |
    Collection of functions used to assert the correctness of TypedArray objects.
defines:
  - typedArrayConstructors
  - floatArrayConstructors
  - intArrayConstructors
  - TypedArray
  - testWithTypedArrayConstructors
  - testWithAtomicsFriendlyTypedArrayConstructors
  - testWithNonAtomicsFriendlyTypedArrayConstructors
  - testTypedArrayConversions
---*/

/**
 * Array containing every typed array constructor.
 */
var typedArrayConstructors = [
    Float64Array,
    Float32Array,
    Int32Array,
    Int16Array,
    Int8Array,
    Uint32Array,
    Uint16Array,
    Uint8Array,
    Uint8ClampedArray
];
  
var floatArrayConstructors = typedArrayConstructors.slice(0, 2);
var intArrayConstructors = typedArrayConstructors.slice(2, 7);
  
/**
 * The %TypedArray% intrinsic constructor function.
 */
var TypedArray = Object.getPrototypeOf(Int8Array);
  
/**
 * Callback for testing a typed array constructor.
 *
 * @callback typedArrayConstructorCallback
 * @param {Function} Constructor the constructor object to test with.
 */

/**
 * Calls the provided function for every typed array constructor.
 *
 * @param {typedArrayConstructorCallback} f - the function to call for each typed array constructor.
 * @param {Array} selected - An optional Array with filtered typed arrays
 */
function testWithTypedArrayConstructors(f, selected) {
    var constructors = selected || typedArrayConstructors;
    for (var i = 0; i < constructors.length; ++i) {
        var constructor = constructors[i];
        try {
            f(constructor);
        } catch (e) {
            e.message += " (Testing with " + constructor.name + ".)";
            throw e;
        }
    }
}
  
/**
 * Calls the provided function for every non-"Atomics Friendly" typed array constructor.
 *
 * @param {typedArrayConstructorCallback} f - the function to call for each typed array constructor.
 * @param {Array} selected - An optional Array with filtered typed arrays
 */
function testWithNonAtomicsFriendlyTypedArrayConstructors(f) {
    testWithTypedArrayConstructors(f, [
        Float64Array,
        Float32Array,
        Uint8ClampedArray
    ]);
}
  
/**
 * Calls the provided function for every "Atomics Friendly" typed array constructor.
 *
 * @param {typedArrayConstructorCallback} f - the function to call for each typed array constructor.
 * @param {Array} selected - An optional Array with filtered typed arrays
 */
function testWithAtomicsFriendlyTypedArrayConstructors(f) {
    testWithTypedArrayConstructors(f, [
        Int32Array,
        Int16Array,
        Int8Array,
        Uint32Array,
        Uint16Array,
        Uint8Array,
    ]);
}
  
/**
 * Helper for conversion operations on TypedArrays, the expected values
 * properties are indexed in order to match the respective value for each
 * TypedArray constructor
 * @param  {Function} fn - the function to call for each constructor and value.
 *                         will be called with the constructor, value, expected
 *                         value, and a initial value that can be used to avoid
 *                         a false positive with an equivalent expected value.
 */
function testTypedArrayConversions(byteConversionValues, fn, selected) {
    var values = byteConversionValues.values;
    var expected = byteConversionValues.expected;

    testWithTypedArrayConstructors(function(TA) {
        var name = TA.name.slice(0, -5);

        return values.forEach(function(value, index) {
            var exp = expected[name][index];
            var initial = 0;
            if (exp === 0) {
                initial = 1;
            }
            fn(TA, value, exp, initial);
        });
    }, selected);
}

/*---
description: |
    Test if a given function is a constructor function.
defines: [isConstructor]
---*/

function isConstructor(f) {
    try {
        Reflect.construct(function(){}, [], f);
    } catch (e) {
        return false;
    }
    return true;
}

var NaNs = [
    NaN,
    Number.NaN,
    NaN * 0,
    0/0,
    Infinity/Infinity,
    -(0/0),
    Math.pow(-1, 0.5),
    -Math.pow(-1, 0.5),
    Number("Not-a-Number"),
];

var byteConversionValues = {
    values: [
        127,         // 2 ** 7 - 1
        128,         // 2 ** 7
        32767,       // 2 ** 15 - 1
        32768,       // 2 ** 15
        2147483647,  // 2 ** 31 - 1
        2147483648,  // 2 ** 31
        255,         // 2 ** 8 - 1
        256,         // 2 ** 8
        65535,       // 2 ** 16 - 1
        65536,       // 2 ** 16
        4294967295,  // 2 ** 32 - 1
        4294967296,  // 2 ** 32
        9007199254740991, // 2 ** 53 - 1
        9007199254740992, // 2 ** 53
        1.1,
        0.1,
        0.5,
        0.50000001,
        0.6,
        0.7,
        undefined,
        -1,
        -0,
        -0.1,
        -1.1,
        NaN,
        -127,        // - ( 2 ** 7 - 1 )
        -128,        // - ( 2 ** 7 )
        -32767,      // - ( 2 ** 15 - 1 )
        -32768,      // - ( 2 ** 15 )
        -2147483647, // - ( 2 ** 31 - 1 )
        -2147483648, // - ( 2 ** 31 )
        -255,        // - ( 2 ** 8 - 1 )
        -256,        // - ( 2 ** 8 )
        -65535,      // - ( 2 ** 16 - 1 )
        -65536,      // - ( 2 ** 16 )
        -4294967295, // - ( 2 ** 32 - 1 )
        -4294967296, // - ( 2 ** 32 )
        Infinity,
        -Infinity,
        0
    ],

    expected: {
        Int8: [
            127,  // 127
            -128, // 128
            -1,   // 32767
            0,    // 32768
            -1,   // 2147483647
            0,    // 2147483648
            -1,   // 255
            0,    // 256
            -1,   // 65535
            0,    // 65536
            -1,   // 4294967295
            0,    // 4294967296
            -1,   // 9007199254740991
            0,    // 9007199254740992
            1,    // 1.1
            0,    // 0.1
            0,    // 0.5
            0,    // 0.50000001,
            0,    // 0.6
            0,    // 0.7
            0,    // undefined
            -1,   // -1
            0,    // -0
            0,    // -0.1
            -1,   // -1.1
            0,    // NaN
            -127, // -127
            -128, // -128
            1,    // -32767
            0,    // -32768
            1,    // -2147483647
            0,    // -2147483648
            1,    // -255
            0,    // -256
            1,    // -65535
            0,    // -65536
            1,    // -4294967295
            0,    // -4294967296
            0,    // Infinity
            0,    // -Infinity
            0
        ],
        Uint8: [
            127, // 127
            128, // 128
            255, // 32767
            0,   // 32768
            255, // 2147483647
            0,   // 2147483648
            255, // 255
            0,   // 256
            255, // 65535
            0,   // 65536
            255, // 4294967295
            0,   // 4294967296
            255, // 9007199254740991
            0,   // 9007199254740992
            1,   // 1.1
            0,   // 0.1
            0,   // 0.5
            0,   // 0.50000001,
            0,   // 0.6
            0,   // 0.7
            0,   // undefined
            255, // -1
            0,   // -0
            0,   // -0.1
            255, // -1.1
            0,   // NaN
            129, // -127
            128, // -128
            1,   // -32767
            0,   // -32768
            1,   // -2147483647
            0,   // -2147483648
            1,   // -255
            0,   // -256
            1,   // -65535
            0,   // -65536
            1,   // -4294967295
            0,   // -4294967296
            0,   // Infinity
            0,   // -Infinity
            0
        ],
        Uint8Clamped: [
            127, // 127
            128, // 128
            255, // 32767
            255, // 32768
            255, // 2147483647
            255, // 2147483648
            255, // 255
            255, // 256
            255, // 65535
            255, // 65536
            255, // 4294967295
            255, // 4294967296
            255, // 9007199254740991
            255, // 9007199254740992
            1,   // 1.1,
            0,   // 0.1
            0,   // 0.5
            1,   // 0.50000001,
            1,   // 0.6
            1,   // 0.7
            0,   // undefined
            0,   // -1
            0,   // -0
            0,   // -0.1
            0,   // -1.1
            0,   // NaN
            0,   // -127
            0,   // -128
            0,   // -32767
            0,   // -32768
            0,   // -2147483647
            0,   // -2147483648
            0,   // -255
            0,   // -256
            0,   // -65535
            0,   // -65536
            0,   // -4294967295
            0,   // -4294967296
            255, // Infinity
            0,   // -Infinity
            0
        ],
        Int16: [
            127,    // 127
            128,    // 128
            32767,  // 32767
            -32768, // 32768
            -1,     // 2147483647
            0,      // 2147483648
            255,    // 255
            256,    // 256
            -1,     // 65535
            0,      // 65536
            -1,     // 4294967295
            0,      // 4294967296
            -1,     // 9007199254740991
            0,      // 9007199254740992
            1,      // 1.1
            0,      // 0.1
            0,      // 0.5
            0,      // 0.50000001,
            0,      // 0.6
            0,      // 0.7
            0,      // undefined
            -1,     // -1
            0,      // -0
            0,      // -0.1
            -1,     // -1.1
            0,      // NaN
            -127,   // -127
            -128,   // -128
            -32767, // -32767
            -32768, // -32768
            1,      // -2147483647
            0,      // -2147483648
            -255,   // -255
            -256,   // -256
            1,      // -65535
            0,      // -65536
            1,      // -4294967295
            0,      // -4294967296
            0,      // Infinity
            0,      // -Infinity
            0
        ],
        Uint16: [
            127,   // 127
            128,   // 128
            32767, // 32767
            32768, // 32768
            65535, // 2147483647
            0,     // 2147483648
            255,   // 255
            256,   // 256
            65535, // 65535
            0,     // 65536
            65535, // 4294967295
            0,     // 4294967296
            65535, // 9007199254740991
            0,     // 9007199254740992
            1,     // 1.1
            0,     // 0.1
            0,     // 0.5
            0,     // 0.50000001,
            0,     // 0.6
            0,     // 0.7
            0,     // undefined
            65535, // -1
            0,     // -0
            0,     // -0.1
            65535, // -1.1
            0,     // NaN
            65409, // -127
            65408, // -128
            32769, // -32767
            32768, // -32768
            1,     // -2147483647
            0,     // -2147483648
            65281, // -255
            65280, // -256
            1,     // -65535
            0,     // -65536
            1,     // -4294967295
            0,     // -4294967296
            0,     // Infinity
            0,     // -Infinity
            0
        ],
        Int32: [
            127,         // 127
            128,         // 128
            32767,       // 32767
            32768,       // 32768
            2147483647,  // 2147483647
            -2147483648, // 2147483648
            255,         // 255
            256,         // 256
            65535,       // 65535
            65536,       // 65536
            -1,          // 4294967295
            0,           // 4294967296
            -1,          // 9007199254740991
            0,           // 9007199254740992
            1,           // 1.1
            0,           // 0.1
            0,           // 0.5
            0,           // 0.50000001,
            0,           // 0.6
            0,           // 0.7
            0,           // undefined
            -1,          // -1
            0,           // -0
            0,           // -0.1
            -1,          // -1.1
            0,           // NaN
            -127,        // -127
            -128,        // -128
            -32767,      // -32767
            -32768,      // -32768
            -2147483647, // -2147483647
            -2147483648, // -2147483648
            -255,        // -255
            -256,        // -256
            -65535,      // -65535
            -65536,      // -65536
            1,           // -4294967295
            0,           // -4294967296
            0,           // Infinity
            0,           // -Infinity
            0
        ],
        Uint32: [
            127,        // 127
            128,        // 128
            32767,      // 32767
            32768,      // 32768
            2147483647, // 2147483647
            2147483648, // 2147483648
            255,        // 255
            256,        // 256
            65535,      // 65535
            65536,      // 65536
            4294967295, // 4294967295
            0,          // 4294967296
            4294967295, // 9007199254740991
            0,          // 9007199254740992
            1,          // 1.1
            0,          // 0.1
            0,          // 0.5
            0,          // 0.50000001,
            0,          // 0.6
            0,          // 0.7
            0,          // undefined
            4294967295, // -1
            0,          // -0
            0,          // -0.1
            4294967295, // -1.1
            0,          // NaN
            4294967169, // -127
            4294967168, // -128
            4294934529, // -32767
            4294934528, // -32768
            2147483649, // -2147483647
            2147483648, // -2147483648
            4294967041, // -255
            4294967040, // -256
            4294901761, // -65535
            4294901760, // -65536
            1,          // -4294967295
            0,          // -4294967296
            0,          // Infinity
            0,          // -Infinity
            0
        ],
        Float32: [
            127,                  // 127
            128,                  // 128
            32767,                // 32767
            32768,                // 32768
            2147483648,           // 2147483647
            2147483648,           // 2147483648
            255,                  // 255
            256,                  // 256
            65535,                // 65535
            65536,                // 65536
            4294967296,           // 4294967295
            4294967296,           // 4294967296
            9007199254740992,     // 9007199254740991
            9007199254740992,     // 9007199254740992
            1.100000023841858,    // 1.1
            0.10000000149011612,  // 0.1
            0.5,                  // 0.5
            0.5,                  // 0.50000001,
            0.6000000238418579,   // 0.6
            0.699999988079071,    // 0.7
            NaN,                  // undefined
            -1,                   // -1
            -0,                   // -0
            -0.10000000149011612, // -0.1
            -1.100000023841858,   // -1.1
            NaN,                  // NaN
            -127,                 // -127
            -128,                 // -128
            -32767,               // -32767
            -32768,               // -32768
            -2147483648,          // -2147483647
            -2147483648,          // -2147483648
            -255,                 // -255
            -256,                 // -256
            -65535,               // -65535
            -65536,               // -65536
            -4294967296,          // -4294967295
            -4294967296,          // -4294967296
            Infinity,             // Infinity
            -Infinity,            // -Infinity
            0
        ],
        Float64: [
            127,         // 127
            128,         // 128
            32767,       // 32767
            32768,       // 32768
            2147483647,  // 2147483647
            2147483648,  // 2147483648
            255,         // 255
            256,         // 256
            65535,       // 65535
            65536,       // 65536
            4294967295,  // 4294967295
            4294967296,  // 4294967296
            9007199254740991, // 9007199254740991
            9007199254740992, // 9007199254740992
            1.1,         // 1.1
            0.1,         // 0.1
            0.5,         // 0.5
            0.50000001,  // 0.50000001,
            0.6,         // 0.6
            0.7,         // 0.7
            NaN,         // undefined
            -1,          // -1
            -0,          // -0
            -0.1,        // -0.1
            -1.1,        // -1.1
            NaN,         // NaN
            -127,        // -127
            -128,        // -128
            -32767,      // -32767
            -32768,      // -32768
            -2147483647, // -2147483647
            -2147483648, // -2147483648
            -255,        // -255
            -256,        // -256
            -65535,      // -65535
            -65536,      // -65536
            -4294967295, // -4294967295
            -4294967296, // -4294967296
            Infinity,    // Infinity
            -Infinity,   // -Infinity
            0
        ]
    }
};
