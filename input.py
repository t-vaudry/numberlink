import sys
import ast

elements = sys.argv[1].replace('\n','')[1:-1].replace(' ','').split(',', 2)

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
    board[path[1][1] - 1][path[1][0] - 1] = path[0]
    board[path[2][1] - 1][path[2][0] - 1] = path[0]
    pair = []
    pair.append((path[1][1] - 1, path[1][0] - 1))
    pair.append((path[2][1] - 1, path[2][0] - 1))
    points.append(pair)

print boardSize - 1
print str(board).replace('\'','').replace(' ','')
print str(points).replace(' ','')