local function factory()
    local i = 0
    return function()
        print(i)
	i = i + 1
    end
end

local f1 = factory()
f1()
f1()
local f2 = factory()
f2()
f1()
f2()
f1()
