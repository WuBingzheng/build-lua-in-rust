local a = "hello, local!"  -- define a local by string
local b = a  -- define a local by another local
print(b)  -- print local variable
print(print)  -- print global variable
local print = print  -- define a local by global variable with same name
print "I'm local-print!"  -- call local function
