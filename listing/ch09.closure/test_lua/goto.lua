local f
local first = true

::again::

-- step.1
if f then -- set after goto
    -- step.4
    f('first')
    f('first')
    f('first')
    first = false
end

-- step.2 and step.5
local i = 0 -- this local variable should be closed on goto
f = function (prefix)
    i = i + 1
    print(prefix, i)
end

if first then
    -- step.3
    goto again -- close `local i`
end

-- step.6
-- `f` is a new closure, with a new `i`
-- so the following calls should print: 1,2,3
f('after')
f('after')
f('after')
