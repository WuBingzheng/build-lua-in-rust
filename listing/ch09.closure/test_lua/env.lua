local function my_print(a)
    print("test _ENV:", a)
end

-- _ENV as local variable
local function test_local_env()
    local _ENV = { print = my_print }
    print "hello, world!" -- this `print` is my_print
end

test_local_env()

-- _ENV as upvalue
local _ENV = { print = my_print }
local function test_upvalue_env()
    print "hello, upvalue!" -- this `print` is my_print
end

test_upvalue_env()
