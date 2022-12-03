from priority import priority

def rucksack_reorg ():
    fhand = open ('input.txt', 'r')
    prioritysum = 0
    for line in fhand:
        value = 0
        intlen = int(len(line))
        half_len = int(len(line)/2)
        first_half = line [0:half_len]
        second_half = line [half_len:intlen]
        for character in first_half:
            if character in second_half:
                    value = priority [character]
                    prioritysum = prioritysum + value
                    break

    return f'priority sum part 1: {prioritysum}'


def badge_finder ():
    fhand = open ('input.txt', 'r')
    prioritysum = 0
    counter = 0
    templist = []
    for line in fhand:
        line = line.rstrip('\n')
        
        templist.append(line)
        counter+=1
        
        if counter == 3:
            bag1 = templist [0]
            bag2 = templist [1]
            bag3 = templist [2]
            for char in bag1:
                if char in bag2:
                    if char in bag3:
                        prioritysum += priority[char]
                        break
                    else:
                        pass
                else:
                    pass
            counter = 0
            templist = []

    return f'priority sum part 2: {prioritysum}'



print(rucksack_reorg())
print(badge_finder())
