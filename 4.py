test_input = """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX""".split('\n')

with open('4.txt','r') as f:
    input = f.readlines()

# input = test_input

def char_at(x,y):
    if y < 0 or x < 0 or y > len(input)-1:
        return "."
    line = input[y]
    if x>len(line)-1:
        return '.'
    return line[x]

def get_3_chars_above(x,y):
    return char_at(x,y-1) + char_at(x,y-2) + char_at(x,y-3)

def get_3_chars_below(x,y):
    return char_at(x,y+1) + char_at(x,y+2) + char_at(x,y+3)

def get_3_chars_to_left(x,y):
    return char_at(x-1,y) + char_at(x-2,y) + char_at(x-3,y)

def get_3_chars_to_right(x,y):
    return char_at(x+1,y) + char_at(x+2,y) + char_at(x+3,y)

def get_3_chars_to_top_right(x,y):
    return char_at(x+1,y-1) + char_at(x+2,y-2) + char_at(x+3,y-3)

def get_3_chars_to_top_left(x,y):
    return char_at(x-1,y-1) + char_at(x-2,y-2) + char_at(x-3,y-3)

def get_3_chars_to_bottom_right(x,y):
    return char_at(x+1,y+1) + char_at(x+2,y+2) + char_at(x+3,y+3)

def get_3_chars_to_bottom_left(x,y):
    return char_at(x-1,y+1) + char_at(x-2,y+2) + char_at(x-3,y+3)

sum=0
for y in range(len(input)):
    line = input[y]
    for x in range(len(line)):
        if char_at(x,y) == 'X':
            if get_3_chars_above(x,y) == 'MAS':
                sum+=1
            if get_3_chars_below(x,y) == 'MAS':
                sum+=1
            if get_3_chars_to_left(x,y) == 'MAS':
                sum+=1
            if get_3_chars_to_right(x,y) == 'MAS':
                sum+=1
            if get_3_chars_to_top_right(x,y) == 'MAS':
                sum+=1
            if get_3_chars_to_top_left(x,y) == 'MAS':
                sum+=1
            if get_3_chars_to_bottom_right(x,y) == 'MAS':
                sum+=1
            if get_3_chars_to_bottom_left(x,y) == 'MAS':
                sum+=1

print(sum)