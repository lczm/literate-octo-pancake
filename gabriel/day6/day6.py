def signaller ():
    fhand = open ('input.txt','r')
    for string in fhand:
        for itervar in range(len(string)):
            tempdict = {}
            sample = string[itervar:itervar+4]
    
            if len(sample)!=4:
                print('sample length less than 4')
                break
            #to account for remaining few iterations of sample where there are insufficient characters
        
            for character in sample:
                tempdict [character] = tempdict.get(character,0) + 1

            if len (tempdict) == 4:
                print (f'part 1: first start-of-packet marker position: {itervar+4}')
                break
            #4 added to final result to point to index of final character in each instance of the sample

            else:
                pass

        return string

            
def reservist ():
    string = signaller ()
    for itervar in range(len(string)):
        tempdict = {}
        sample = string[itervar:itervar+14]

        if len(sample)!=14:
            print('sample length less than 14')
            break
        #account for samples nearing end of string with lengths less than 14

        for character in sample:
            tempdict [character] = tempdict.get(character, 0) + 1

        if len (tempdict) == 14:
            print (f'part 2: first start-of-packet marker position: {itervar+14}')
            break
        #14 added to final result to point to index of final character in each instance of the sample
        else:
            pass


reservist()
