local z = 1
while z do
  while z do
    print "break inner"
    break
  end

  print "break out"
  break
end
