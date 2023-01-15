local f, g, h
local up1 = 1
do
  local up2 = 2
  do
    local up3 = 3

    -- closure with local variable in block
    f = function()
      up3 = up3 + 1
      print(up3)
    end

    -- closure with local variable out of block
    g = function()
	up2 = up2 + 1
        print(up2)
    end

    -- closure with local variable out of block 2 levels
    h = function()
	up1 = up1 + 1
        print(up1)
    end

    -- call these closures in block
    f()
    g()
    h()
    print(up1, up2, up3)
  end

  -- call these closures out of block
  f()
  g()
  h()
  print(up1, up2)

end

-- call these closures out of block
f()
g()
h()
print(up1)
