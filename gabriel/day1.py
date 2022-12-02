def thiccboi ():

    fhand = open ('input.txt', 'r')
    templist = []
    permlist = []
    final = []

    for line in fhand:
        line.replace (' ','')
        edited = line.rstrip('\n')
        if len(edited) == 0:
            permlist.append (templist)
            templist = []
        else:
            nice = int(edited)
            templist.append (nice)

    for itervar in permlist:
        final.append(sum(itervar))

    final.sort(reverse=True)

    top3_elves = f'3 absolute units: {sum(final [0:3])} kcal'
    print(top3_elves)

    max_kcal = f'thiccest elf: {max(final)} kcal'
    return max_kcal

    
print(thiccboi ())

    
