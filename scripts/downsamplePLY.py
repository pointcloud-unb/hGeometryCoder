import math as m
import sys

filename = sys.argv[1]

extension = filename[-4:]

name = filename[:-4]

originalBits = int(sys.argv[2])

goalBits = int(sys.argv[3])

outputname = name + str(originalBits) + "to" + str(goalBits) + extension

file = open(filename, "r")
output = open(outputname, "w")

lines = file.readlines()
total = len(lines)

for i in range(0,7):
    output.write(lines[i])

for i in range(7, total):
    line = lines[i].split(' ')
    for number in line:
        norm = (int(number)) / ((2**originalBits) - 1)
        newValue = norm * ((2**goalBits) - 1)
        if (newValue - int(newValue)) >= 0.5:
            newValue += 1
        output.write(str(m.floor(newValue)))
        output.write(' ')
    output.write('\n')







