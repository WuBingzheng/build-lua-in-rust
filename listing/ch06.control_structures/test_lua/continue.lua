-- validate compatibility
continue = print -- continue as global variable name, and assign it a value
continue(continue) -- call continue as function

-- continue in while loop
local c = true
while c do
    print "hello, while"
    if true then
      c = false
      continue
    end
    print "should not print this!"
end

-- continue in repeat loop
repeat
    print "hello, repeat"
    local ok = true
    if true then
      continue -- continue after local
    end
    print "should not print this!"
until ok

-- continue skip local in repeat loop
-- PANIC!
repeat
    print "hello, repeat again"
    if true then
      continue -- skip `ok`!!! error in parsing
    end
    local ok = true
until ok
