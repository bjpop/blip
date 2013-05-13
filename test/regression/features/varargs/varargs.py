def f(*x):
    return(x)

print(f())
print(f(1))
print(f(1,2))
print(f(1,2,3))
print(f((1,2,3)))

def g(x, *y):
    return(x, y)

print(g(1))
print(g(1,2))
print(g(1,2,3))
print(g((1,2,3)))
