g1 = 1
g2 = 2

if g1 or g2 and g3 then
    print "test only once"
end

if g3 or g1 and g2 then
    print "test 3 times"
end

if (g3 or g1) and (g2 or g4) then
    print "test 3 times"
end

if (g3 or g1) and (g2 and g4) then
    print "test 4 times and fail"
end
