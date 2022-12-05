def parse_text ():
    templist = []
    permdict = {}
    fhand = open ('input.txt','r')
    for line in fhand:
        line = line.rstrip('\n')
        templist.append (line)
    first_chunk = templist [:8]
    second_chunk = templist [10:]

    for line in first_chunk:
        counter = 1
        lit = []
        line = line.replace ('[',' ')
        line = line.replace (']',' ')
        for character in line:
            lit.append(character)
        del lit[0]
        i = 0
        while i < 33:
            string = lit[i:i+4]
            i += 4
            del string [1:]
            for item in string:
                if item.isalpha():
                    permdict [counter] = permdict.get(counter,'') + item
                else:
                    permdict [counter] = ''
            counter += 1  
        #determine initial starting postiion
    
    permdict1 = permdict

    for lines in second_chunk:
        newlist = lines.split(' ')
        quantity = int(''.join(newlist [1:2]))
        origstack = int(''.join(newlist [3:4]))
        newstack = int(''.join(newlist [5:6]))
        #print (quantity, origstack, newstack)
        for itervar in range(quantity):
            permdict[newstack] = (permdict[origstack])[0] + permdict[newstack]
            permdict[origstack] = permdict[origstack][1:]
            #actual moving of the crates

    for lines in second_chunk:
        newlist = lines.split(' ')
        quantity = int(''.join(newlist [1:2]))
        origstack = int(''.join(newlist [3:4]))
        newstack = int(''.join(newlist [5:6]))
        #print (quantity, origstack, newstack)
        permdict1[newstack] = (permdict1[origstack])[:quantity] + permdict1[newstack]
        permdict1[origstack] = permdict1[origstack][quantity:]
            #actual moving of the crates

    return f'part 1 output: {permdict}\npart 2 output: {permdict1}'


print(parse_text())
