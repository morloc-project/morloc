def pid(x): 
    return x

def pdiv(x):
    if x == 0:
        raise ValueError("Cannot divide by zero")
    else:
        return 1 / x
