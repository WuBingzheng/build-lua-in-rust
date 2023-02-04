local foos = {}
local bars = {}
for i = 1,4 do
    local up = 0
    foos[i] = function()
        up = up + 1
	return up
    end
    do
        bars[i] = function()
            i = i + 1
            return i
        end
    end
end

print(foos[1](), foos[1](), "|", foos[4](), foos[4]())
print(foos[1](), foos[1](), "|", foos[4](), foos[4]())
print()
print(bars[1](), bars[1](), "|", bars[4](), bars[4]())
print(bars[1](), bars[1](), "|", bars[4](), bars[4]())
