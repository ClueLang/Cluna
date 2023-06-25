local myTable = {
    -- Key-value pairs
    key1 = "value1",
    key2 = 42,
    key3 = true,
    ["key with spaces"] = "value with spaces",
    [10] = "value with numeric key",
    [a] = "value with variable key",
    -- Nested tables
    nestedTable = {
        nestedKey = "nestedValue",
        anotherNestedTable = {
            foo = "bar",
            hello = "world",
        },
    },

    -- Arrays
    array = { 1, 2, 3, 4, 5 },

    -- Mixed value types
    mixedTable = {
        "string value",
        123,
        nested = {
            true,
            false,
        },
    },

    -- Using semicolons as separators
    key4 = "value4",
    key5 = "value5",
}
