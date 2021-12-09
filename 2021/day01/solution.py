with open('input.txt') as f:
    lines = f.read().splitlines()

lines = [float(line) for line in lines]

def window_inequality(size):
    directions = []
    for i in range(len(lines)-size):
        if size == 1:
            directions.append(lines[i+1]>lines[i])
        else:
            print(sum(lines[i+1:i+1+size]))
            print(sum(lines[i:i+size]))
            directions.append(sum(lines[i+1:i+1+size])>sum(lines[i:i+size]))
    print(sum(directions))

if __name__ == "__main__":
    window_inequality(size=1)
    window_inequality(size=3)