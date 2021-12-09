with open('input.txt') as f:
    lines = f.read().splitlines()

data = [line.split(" ") for line in lines]

def pilot_ship():
    depth = 0
    horizontal = 0
    for i in data:
        iter_num = float(i[1])
        if i[0] == "down":
            depth += iter_num
        elif i[0]=="up":
            depth -= iter_num
        elif i[0]=="forward":
            horizontal += iter_num

    print("Course Completed:")
    print(depth*horizontal)

pilot_ship()




def pilot_aim():
    depth = 0
    horizontal = 0
    aim = 0
    for i in data:
        iter_num = float(i[1])
        if i[0] == "down":
            aim += iter_num
        elif i[0]=="up":
            aim -= iter_num
        elif i[0]=="forward":
            horizontal += iter_num
            depth += iter_num*aim

    print("Course Completed:")
    print(depth*horizontal)


pilot_aim()