function f(n)
    if n > 10000 then return n end
    return f(n+1)
end

print(f(0))
