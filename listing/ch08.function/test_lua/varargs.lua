function f(x, ...)
    local a,b,c = ...
    print(x)
    print(a)
    print(b)
    print(c)
end
function f2(x, ...)
    f(x,...)
end
function f3(x, ...)
    f(...,x)
end

f('x', 1,2,3)
f('x', 1,2)
f2('x', 1,2,3,4)
f3('x', 1,2,3,4)
