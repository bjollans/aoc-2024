list1=[]
list2=[]
with open('1.txt','r') as f:
    for line in f:
        splits = line.strip().split()
        list1.append(int(splits[0]))
        list2.append(int(splits[1]))

# min_sum=0
# while list1 and list2:
#     min_sum += abs(min(list1) - min(list2))
#     list1.remove(min(list1))
#     list2.remove(min(list2))

# print(list1)
# print(list2)
# print(min_sum)

amount_counter = [0] * (max(max(list1),max(list2)) +1)
for x in list2:
    amount_counter[x] += 1

sim_sum = sum([amount_counter[x] * x for x in list1])

print(sim_sum)