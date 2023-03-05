local function iter(t, i)
    i = i + 1
    local v = t[i]
    if v then
        return i, v
    end
end

local function my_ipairs(t)
    return iter, t, 0
end

local z = {'hello', 123, 456}
for i,v in my_ipairs(z) do
	print(i, v)
end

for i,v in iter,z,0 do
	print(i, v)
end

for i,v in my_ipairs(z) do
	print(i, v)

	-- Change the control-variable during the loop.
	-- This is disallowed in Lua's manual
	-- (https://www.lua.org/manual/5.4/manual.html#3.3.5).
	-- This behaves diffrent from Lua PUC-Rio.
	i = i+1
end
