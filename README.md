# expr

JSON representation example:
```
{
    "operator": "equals",
    "member": { "name": "abc", "alias": "a", "table": "t" }, // member can be a string, for example: "abc"
    "values": [{ "value": "2019-01-01", "type": "date" }]    // if it is a simple number or string, it can be 1 | "1"
}

// a = DATE('2019-01-01')
// for Oracle example: a = TO_DATE('2019-01-01', 'YYYY-MM-DD')
```

An expression statement represents an example:
```
abc = DATE('2019-01-01')

// a = DATE('2019-01-01')
// for Oracle example: a = TO_DATE('2019-01-01', 'YYYY-MM-DD')
```
