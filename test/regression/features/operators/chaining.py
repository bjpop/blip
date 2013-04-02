x = 3
y = 2
z = 4
print(x < z > y)   # should be same as (3 < 4 and 4 > 2)
print((x < z) > y) # should be same as (3 < 4 and True > 2)
