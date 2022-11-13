function f(n)
    if n > 4 then return n end
    print(n+1)
    return f(n+1)
end

print(f(0))
