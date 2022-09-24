local a = 123
do
  local a = 789
  local b = 789
end
  print(a) -- 123
  print(b) -- nil
