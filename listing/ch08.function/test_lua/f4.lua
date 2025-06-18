g1, g2, g3 = 100, 200, 300

function foo(a, b, ...)
  return a+b, g1+g2*g3, ...
end

print(foo(1,2))
print(foo(1,2,3,4,5))


