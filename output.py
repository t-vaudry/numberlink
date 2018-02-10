import sys
import ast

# Read the elements from the command line
# Strip the first and last character
# Replace the space with nothing
# Split the elements at the first 2 commas
elements = sys.argv[1][1:-1].replace(' ','').split(',', 2)

# Reading the input again to get the board size for output
boardSize = int(elements[0])
numbers = int(elements[1])

# Parse the output, which is a list of paths, to output the paths
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

# Print the output string to STDOUT for Prolog
print output