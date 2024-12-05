test_input = """.M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........""".split('\n')

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

def get_2_chars_to_top_right(x,y):
    return char_at(x+1,y-1) + char_at(x+2,y-2)

def get_2_chars_to_bottom_right(x,y):
    return char_at(x+1,y+1) + char_at(x+2,y+2)

def get_2_chars_to_bottom_left(x,y):
    return char_at(x-1,y+1) + char_at(x-2,y+2)

def get_char_2_above(x,y):
    return char_at(x,y-2)

def get_char_2_below(x,y):
    return char_at(x,y+2)

def get_char_2_to_right(x,y):
    return char_at(x+2,y)

def get_char_2_to_left(x,y):
    return char_at(x-2,y)

sum=0
for y in range(len(input)):
    line = input[y]
    for x in range(len(line)):
        if char_at(x,y) == 'M':
            if get_2_chars_to_top_right(x,y) == 'AS':
                if get_char_2_above(x,y) == 'S' and get_char_2_to_right(x,y) == 'M':
                    sum+=1
            if get_2_chars_to_bottom_right(x,y) == 'AS':
                if get_char_2_below(x,y) == 'M' and get_char_2_to_right(x,y) == 'S':
                    sum+=1
                if get_char_2_below(x,y) == 'S' and get_char_2_to_right(x,y) == 'M':
                    sum+=1
            if get_2_chars_to_bottom_left(x,y) == 'AS':
                if get_char_2_below(x,y) == 'M' and get_char_2_to_left(x,y) == 'S':
                    sum+=1

print(sum)