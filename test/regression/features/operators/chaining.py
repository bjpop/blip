w = 12
x = 3
y = 2
z = 4
print(y < z) # no chaining, with True answer
print(z < y) # no chaining, with False answer
print(x < z > y)   # should be same as (3 < 4 and 4 > 2), True answer
print(x < z > w)   # should be same as (3 < 4 and 4 > 12), False answer
print((x < z) > y) # should be same as (3 < 4 and True > 2)
print(y < x < z < w) # 5-chain True answer
print(y < z < x < w) # 5-chain False answer
print(y < z < x + 2 < w) # 5-chain True answer, nested expression

print(w & (2 ** 4) != 0)
