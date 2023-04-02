-- $Id: ackermann.lua,v 1.5 2000/12/09 20:07:43 doug Exp $
-- http://www.bagley.org/~doug/shootout/

local function Ack(M, N)
    if (M == 0) then
        return N + 1
    end
    if (N == 0) then
        return Ack(M - 1, 1)
    end
    return Ack(M - 1, Ack(M, (N - 1)))
end

print(Ack(2,10))

-- use `--release` to run this:
--print(Ack(3,10))
