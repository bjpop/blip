def g(f):
    return lambda x: f(x) + 1

@g
def f(x):
    return x + 1

print(f(1))
