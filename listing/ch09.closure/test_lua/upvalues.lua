g1, g2 = 1, 2
local up1, up2, up3, up4 = 11, 12, 13, 14
local print = print
local function foo()
    local l1, l2 = 101, 102
    l1, g1 = g2, l2
    print(l1, g1)

    -- assign to upvalues
    up1, up2, up3 = l1, g1, up4
    print(up1, up2, up3)

    -- assign by upvalues
    l1, g1, up1 = up2, up3, up4
    print(l1, g1, up1)

    local inner = function()
        -- assign to upvalues
        up1, up2, up3 = 101, g2, up4
        print(up1, up2, up3)
    end
    inner()
end

foo()
