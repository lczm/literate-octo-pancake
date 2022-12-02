def score_calculator ():
    total_score = 0
    fhand = open ('input.txt', 'r')
    for line in fhand:
        comp_score = 0
        your_score = 0
        components = line.split(' ')
        opponent = components[0]
        you = components[1].rstrip('\n')
        if you == 'X':
            your_score = 1
            if opponent == 'A':
                comp_score = 3
            elif opponent == 'B':
                comp_score = 0
            elif opponent == 'C':
                comp_score = 6
        if you == 'Y':
            your_score = 2
            if opponent == 'B':
                comp_score = 3
            elif opponent == 'A':
                comp_score = 6
            elif opponent == 'C':
                comp_score = 0
        if you == 'Z':
            your_score = 3
            if opponent == 'C':
                comp_score = 3
            elif opponent == 'A':
                comp_score = 0
            elif opponent == 'B':
                comp_score = 6
        #print (f'opponent: {opponent} you: {you} ')
        temp_score = your_score + comp_score
        total_score = total_score + temp_score
        temp_score = 0
    return f'total score: {total_score}'

def guide_decryptor ():
    #X means lose
    #Y means draw
    #Z means win
    total_score = 0
    fhand = open ('input.txt', 'r')
    for line in fhand:
        comp_score = 0
        your_score = 0
        components = line.split(' ')
        opponent = components[0]
        yourmove = components[1].rstrip('\n')
        if yourmove == 'X':
            comp_score = 0
            if opponent == 'A':
                your_score = 3
            elif opponent == 'B':
                your_score = 1
            elif opponent == 'C':
                your_score = 2

        if yourmove == 'Y':
            comp_score = 3
            if opponent == 'A':
                your_score = 1
            elif opponent == 'B':
                your_score = 2
            elif opponent == 'C':
                your_score = 3         

        if yourmove == 'Z':
            comp_score = 6
            if opponent == 'A':
                your_score = 2
            elif opponent == 'B':
                your_score = 3
            elif opponent == 'C':
                your_score = 1
        
        temp_score = your_score + comp_score
        total_score = total_score + temp_score
        temp_score = 0
    return f'new total score: {total_score}'


print (score_calculator())
print (guide_decryptor())