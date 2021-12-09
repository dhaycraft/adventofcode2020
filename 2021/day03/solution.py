from itertools import compress

with open('input.txt') as f:
    lines = f.read().splitlines()

def diagnostic():
    chars = len(lines[0])
    diag_cnts = [0]*chars
    for line in lines:
        for i in range(chars):
            diag_cnts[i]+=int(line[i])

    high_freq = [str(int(i/len(lines)>0.5)) for i in diag_cnts]
    low_freq = [str(int(int(i)!= True)) for i in high_freq]
    high_int = int("".join(high_freq),2)
    low_int = int("".join(low_freq),2)
    print(high_int)
    print(low_int)
    print(high_int*low_int)

diagnostic()




def type_generator(type):
    '''
    Input:
        type: Must be either oxygen or CO2
    '''


    iter_lines = lines
    chars = len(lines[0])
    char_mark = 0
    ones = 0

    while len(iter_lines)!= 1:
        print(iter_lines)
        print(len(iter_lines))
        # tabulate values in place
        for line in iter_lines:
            ones += int(line[char_mark])

        # determine 0 or 1
        if type == "oxygen":
            # if there are equal amounts of 0s and 1s choose 1s
            filter_str = str(int(ones/len(iter_lines)>=.5))
        else:
            # if equal number of 0s and 1s choose 0 for CO2
            filter_str = str(int(ones/len(iter_lines)<.5))

        iter_filt = [filter_str == i[char_mark] for i in iter_lines]
        iter_lines = list(compress(iter_lines, iter_filt))
        # increase mark val 
        # modulo to ensure doesn't exceed char len
        char_mark = (char_mark+1) % chars
        ones = 0

    return int(iter_lines[0],2)
print("Get Oxygen")
oxy_val = type_generator(type = "oxygen")
print("Get CO2")
co2_val = type_generator(type = "CO2")

print(oxy_val*co2_val)

