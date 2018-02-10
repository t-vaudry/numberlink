import sys
import ast

# Grab the elements from the command line
# Strip the first and last character
# Replace the endlines and spaces with nothing
# Split the elements into three blocks, at the first 2 commas
elements = sys.argv[1].replace('\n','')[1:-1].replace(' ','').split(',', 2)

# Size of the board is the first element
boardSize = int(elements[0])
# Number of Links to create is the second element
numbers = int(elements[1])

# Parse the paths, and replace the colon for a comma
paths = ast.literal_eval(elements[2].replace(':',','))

# Iterate over the boardSize, and create the NxN board
board = []
points = []
for i in range(0,boardSize):
    row = []
    for j in range (0,boardSize):
        row.append('_')
    board.append(row)

# Fill the board with the values from the path elements
# Create the point pairs list for Prolog
for path in paths:
    board[path[1][1] - 1][path[1][0] - 1] = path[0]
    board[path[2][1] - 1][path[2][0] - 1] = path[0]
    pair = []
    pair.append((path[1][1] - 1, path[1][0] - 1))
    pair.append((path[2][1] - 1, path[2][0] - 1))
    points.append(pair)

# Print the board and points list to STDOUT to be read by Prolog
print str(board).replace('\'','').replace(' ','')
print str(points).replace(' ','')