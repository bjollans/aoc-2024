from dataclasses import dataclass


test_input = """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"""

with open('2.txt','r') as f:
    input = f.readlines()

# input = test_input.splitlines()

@dataclass
class Direction:
    direction: str = None

INCR = "increasing"
DECR = "decreasing"


def set_direction(num1,num2,direction):
    if not num1 or not num2:
        return
    if num2 < num1:
        direction.direction=DECR
    if num2 > num1:
        direction.direction=INCR


def is_num_combi_safe(num1, num2, direction):
    if num1 == None or num2 == None:
        return True
    if num2 == num1 or abs(num2 - num1) > 3:
        return False
    if direction.direction == DECR and num1 < num2:
        return False
    if direction.direction == INCR and num1 > num2:
        return False
    if not direction.direction:
        set_direction(num1,num2, direction)
    return True

def is_report_safe(level_str, is_highest_level = True):
    if not level_str:
        return False
    levels = [int(x) for x in level_str.split()]
    i=1
    direction = Direction()
    while i < len(levels):
        this_level = levels[i]
        prev_level = levels[i-1]
        prev_prev_level = levels[i-2] if i > 1 else None
        next_level = levels[i+1] if i < len(levels)-1 else None
        if not is_num_combi_safe(prev_level, this_level, direction):
            if is_highest_level:
                levels_without_this = " ".join([str(x) for x in levels[:i] + levels[i+1:]])
                levels_without_prev = " ".join([str(x) for x in levels[:i-1] + levels[i:]])
                levels_without_prev_prev = " ".join([str(x) for x in levels[:i-2] + levels[i-1:]]) if prev_prev_level else ""
                levels_without_next = " ".join([str(x) for x in levels[:i] + levels[i+1:]]) if next_level else ""
                is_safe_without_this = is_report_safe(levels_without_this, is_highest_level=False)
                is_safe_without_prev = is_report_safe(levels_without_prev, is_highest_level=False)
                is_safe_without_prev_prev = is_report_safe(levels_without_prev_prev, is_highest_level=False)
                is_safe_without_next = is_report_safe(levels_without_next, is_highest_level=False)
                return is_safe_without_this or is_safe_without_prev or is_safe_without_prev_prev or is_safe_without_next
            return False
        i+=1
    return True
            

assert not is_report_safe("2 6 7 2 9 2 10")
assert not is_report_safe("6 7 0 9 0 10")
assert not is_report_safe("6 7 2 9 2 10")
assert is_report_safe("5 7 6 7 8 9")
assert is_report_safe("7 6 7 8 9")
assert is_report_safe("6 7 8 9 0")
assert is_report_safe("6 7 8 9 0 10")


# print(is_report_safe("5 7 6 7 8 9"))


safe_sum = sum([(1 if is_report_safe(line) else 0) for line in input])

print(safe_sum)