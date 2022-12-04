def sec_counter ():
    fhand = open ('input.txt','r')
    pairs = 0
    for line in fhand:
        list1, list2 = [], []
        line = line.rstrip('\n')
        components = line.split(',')
        range1, range2 = components[0].split('-'), components[1].split('-')
        for itervar in range(int(range1[0]),int(range1[1])+1):
            list1.append(itervar)
        for itervar in range(int(range2[0]),int(range2[1])+1):
            list2.append(itervar)
        if len(set(list1).intersection(list2)) == len(list1) or len(set(list1).intersection(list2)) == len(list2):
            pairs+=1
    return pairs

def sec_counter2 ():
    fhand = open ('input.txt','r')
    overlaps = 0
    for line in fhand:
        list1, list2 = [], []
        line = line.rstrip('\n')
        components = line.split(',')
        range1, range2 = components[0].split('-'), components[1].split('-')
        for itervar in range(int(range1[0]),int(range1[1])+1):
            list1.append(itervar)
        for itervar in range(int(range2[0]),int(range2[1])+1):
            list2.append(itervar)
        if len(set(list1).intersection(list2)) > 0:
            overlaps += 1
    return overlaps
                  
print(sec_counter())
print(sec_counter2())