
t = os.time()
print(t)

tb = os.date('*t', t)
table.foreach(tb, print)

t2 = os.time(tb)

print(t2)
print(t2 == t)
