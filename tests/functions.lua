function f(a)
    return {
        "1",
        2
    }
end

function g(...)
    return 2
end

local function h(a)
    return function()
        return "h" + a
    end
end
