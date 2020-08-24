# Xlang Instructions

## literal

Syntax:
```
literal <type> <value>
```

Produces a literal of the given type. Errors if *literal* is not valid for *type*.

Value types are *i8 (with any pointer attributes, except that it may only be bounded by `#static`)
