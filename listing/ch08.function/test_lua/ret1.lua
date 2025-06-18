g1, g2, g3, g4 = 1,2,3,4

function foo1(...)
	local a
	a = g1 + g2*g3
	local a,b,c,d,e=...
	print(a,b,c,d,e)
	local a,b=...
	print(a,b)
	print(...)
	return ...
end


g1 = foo1(100, 200, 300)
print(":", g1)
g1 = foo1(100)
print(":", g1)
