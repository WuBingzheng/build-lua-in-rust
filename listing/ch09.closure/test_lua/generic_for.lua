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
	i = i+1 -- update ctrl-var during loop
end
