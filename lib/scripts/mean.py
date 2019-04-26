try:
    fhand = open('dpst1f.dat')
except:
    print('File dpst1f.dat cannot be opened in mean.py')
    exit()

total = 0
count = 0
for inp in fhand:
    try:
        value = float(inp)
        total = total + value
        count = count + 1
    except:
        print('Invalid input in mean.py')
        exit()

fhand.close()
average = 0.0
if count > 0:
    average = total / count

try:
    fhand2 = open('dpst2f.dat','w')
except:
    print('File dpst2f.dat cannot be opened in mean.py')
    exit()

fhand2.write(str(average))
fhand2.close()
