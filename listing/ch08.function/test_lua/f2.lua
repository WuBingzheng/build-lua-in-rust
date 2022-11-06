local function f1()
    local f2 = function() print "internal" end
    print (f2)
end

print (f1)
