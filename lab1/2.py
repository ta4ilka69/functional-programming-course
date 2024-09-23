sum = 2
fib1 = 1
fib2 = 2
while fib2 < 4000000:
    fib1, fib2 = fib2, fib1 + fib2
    if fib2 % 2 == 0:
        sum += fib2
print(sum)