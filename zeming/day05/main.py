f = open('in', 'r')
lines = f.readlines()
lines = [line.rstrip() for line in lines]

cut = 0
for i, line in enumerate(lines):
    if line == '':
        cut = i

max_width  = int(lines[cut-1][-1])
max_height = max_width ** 2

g = [['' for _ in range(max_width)] for _ in range(max_height)]

for i in range(cut-2, -1, -1):
    l = lines[i]
    v_index = 0
    for j in range(0, len(l), 4):
        c = l[j+1]
        g[cut-2-i][v_index] = c
        v_index += 1

def print_row(g, i):
    t = []
    for j in range(max_width):
        t.append(g[i][j])
    print(' '.join(t))

def print_grid(g):
    for i in reversed(range(max_height)):
        if ''.join(g[i]) == '':
            continue
        print_row(g, i)

def top(g, quantity, col):
    t = []
    for i in reversed(range(max_height)):
        if len(t) == quantity:
            break
        if g[i][col].strip() != '':
            t.append(g[i][col])
            g[i][col] = ' '
    return t

def load(g, movables, col):
    limit = 0
    for i in reversed(range(max_height)):
        if g[i][col].strip() != '':
            limit = i
            break

    limit += 1

    for m in movables:
        g[limit][col] = m
        limit += 1

    return 0

def move(g, quantity, before, after):
    movables = top(g, quantity, before)

    # Part 2, comment out for Part 1
    movables = reversed(movables)
    load(g, movables, after)

print(g[0])

for i in range(cut+1, len(lines)):
    l = lines[i].split(' ')
    quantity, before, after = int(l[1]), int(l[3]), int(l[5])

    before -=1
    after -=1

    move(g, quantity, before, after)

print_grid(g)
