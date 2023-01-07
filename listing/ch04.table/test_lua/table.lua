local key = "kkk"
local t = { 100, 200, 300;  -- list style
        x="hello", y="world";  -- record style
        [key]="vvv";  -- general style
      }
print(t[1])
print(t['x'])
print(t.kkk)
print(t)
