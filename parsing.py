import sys
import ast
from pyswip import Prolog

elements = sys.argv[1][1:-1].replace(' ','').split(',', 2)

boardSize = int(elements[0])
numbers = int(elements[1])

paths = ast.literal_eval(elements[2].replace(':',','))

board = []
points = []
for i in range(0,boardSize):
    row = []
    for j in range (0,boardSize):
        row.append('_')
    board.append(row)

for path in paths:
    board[int(path[1][1]) - 1][int(path[1][0]) - 1] = path[0]
    board[int(path[2][1]) - 1][int(path[2][0]) - 1] = path[0]
    pair = []
    pair.append((int(path[1][1]) - 1, int(path[1][0]) - 1))
    pair.append((int(path[2][1]) - 1, int(path[2][0]) - 1))
    points.append(pair)

prolog = Prolog()
prolog.consult("numberlink.pl")

query = "numberlink(%s,%s,%s,Paths)" % (boardSize - 1, str(board).replace('\'','').replace(' ',''), str(points).replace(' ',''))

for result in prolog.query(query):
    output = ''
    output += "(%s, %s,\n" % (boardSize, numbers)
    idx = 1
    for path in result["Paths"]:
        output += ("(%s :" % idx)
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