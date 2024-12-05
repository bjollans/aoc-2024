import re

with open('3.txt','r') as f:
    input = "++".join(f.readlines())

# input = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
# input = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

regex = 'mul\([1-9][0-9]*,[1-9][0-9]*\)'

matches_part_1 = re.findall(regex, input)

def do_mult(mult_str):
    subbed_str = mult_str
    subbed_str = re.sub('mul\(','',subbed_str)
    subbed_str = re.sub('\)','',subbed_str)
    nums = [int(x) for x in subbed_str.split(",")]
    return nums[0] * nums[1]

donts = input.split("don't()")[1:]
donts_dos_list = [d.split("do()") for d in donts]
dos = ["++".join(d[1:]) for d in donts_dos_list if len(d) > 1]
dos.append(input.split("don't()")[0])

cleaned_input = "++".join(dos)

matches_part_2 = re.findall(regex, cleaned_input)

print(matches_part_2)

print(sum([do_mult(x) for x in matches_part_2]))
# print(do_mult('mul(4,6)'))