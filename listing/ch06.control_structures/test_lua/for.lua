-- 1~3
for i = 1, 3, 1 do
    print(i)
end

-- negetive step, 1~-2
for i = 1, -2, -1 do
    print(i)
end

-- float limit, 1~3
for i = 1, 3.2 do
    print(i)
end

-- float i, 1.0~3.0
for i = 1.0, 3 do
    print(i)
end

-- special case, should not run
local max = 9223372036854775807
for i = max, max*10.0, -1 do
    print (i)
end
