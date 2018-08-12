/*
*/

function _Value_global(name) {
  return eval(name)
}

function _Value_cast(value) {
  return value
}

function _Value_nativeType(value) {
  var type = typeof(value)
  if (type === "number") {
    if (Number.isInteger(value)) {
      type = "int"
    } else {
      type = "float"
    }
  }
  return type
}

var _Value_assign = F2(function(name, value) {
  eval(name + " = value")
  return value
})

var _Value_print = F2(function(label, value) {
  console.log(label, value)
  return value
})

function _Value_wrap(value) {
  return { '$': 0, 'a': value }
}

function _Value_unwrap(value) {
  if (value.$ === 0) {
    return value.a
  }
  return value
}