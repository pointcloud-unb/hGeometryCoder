import math as m

print("Insira o caminho do arquivo (.ply): ")

filename = input()

extension = filename[-4:]

name = filename[:-4]

print("Bits do arquivo original: ")

originalBits = int(input())

print("Bits do arquivo alvo: ")

goalBits = int(input())

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







