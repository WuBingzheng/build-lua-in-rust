function f1(a, b)
    return a+b, a-b
end
function f2(a, b)
    return f1(a+b, a-b) -- return MULTRET
end

x,y = f2(f2(3, 10)) -- MULTRET arguments
print(x)
print(y)
