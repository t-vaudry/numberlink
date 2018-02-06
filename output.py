import sys
import ast

elements = sys.argv[1][1:-1].replace(' ','').split(',', 2)

boardSize = int(elements[0])
numbers = int(elements[1])

output = ''
output += "(%s, %s,\n" % (boardSize, numbers)
idx = 1
for path in ast.literal_eval(sys.argv[2]):
    output += ("(%s:" % idx)
    for point in path:
        output += " ("
        for coordinate in point:
            output += ("%s," % (coordinate + 1))
        output = output[:-1]
        output += "),"
    output = output[:-1]
    output += "),\n"
    idx += 1
output = output[:-2]
output += ")"
print output