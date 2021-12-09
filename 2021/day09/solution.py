import numpy as np

with open('input.txt') as f:
    lines = f.read().splitlines()

elevation = [[int(char) for char in line] for line in lines]


elevation = np.array(elevation)

arr_shape = elevation.shape

## Part 1
# Find all low points in the basin


def get_point_val(i,j):
    if min([i,j])<0 or i>arr_shape[0]-1 or j>arr_shape[1]-1:
        # setting to 9 so is higher than all other points so will never be lower
        point_val = 9
    else:
        point_val = elevation[i,j]
    return point_val



low_points = []
for i,j in np.ndindex(arr_shape):
    # get points that are next to iter point
    up = get_point_val(i=i+1,j=j)
    down = get_point_val(i=i-1,j=j)
    right = get_point_val(i=i,j=j+1)
    left = get_point_val(i=i, j=j-1)
    iter_point = elevation[i,j]
    
    if all([up>iter_point, down>iter_point, right>iter_point, left>iter_point]):
        low_points.append((i,j))

print(low_points)
risk_vals = []
for (i,j) in low_points:

    risk_vals.append(elevation[i,j]+1)

print(risk_vals)

print(sum(risk_vals))


## Part 2
# Find 3 largest basin

def get_new_points(i,j):
    new_points = [(i+1,j),(i-1,j),(i,j-1),(i,j+1)]
    for (x,y) in new_points:
        if min([x,y])<0 or x>arr_shape[0]-1 or y>arr_shape[1]-1:
            searched_points.append((x,y))
        else:
            queue_points.append((x,y))

# collect all previously used basin_points
basin_prev = []
basin_cnts = []
for (i,j) in low_points:
    print("Searching near:"+str(i)+str(j))
    if (i,j) not in basin_prev:
        basin_points = []
        searched_points = []
        queue_points = []
        get_new_points(i=i, j=j)
        prev_queue = 1
        while prev_queue:
            print((i,j))
            print(get_point_val(i=i, j=j))
            if get_point_val(i=i, j=j)!=9:
                basin_points.append((i,j))
                get_new_points(i=i,j=j)
            searched_points.append((i,j))
            queue_points = list(set(queue_points) - set(searched_points))
            prev_queue = len(queue_points)
            if queue_points:
                (i,j) = queue_points.pop()


    print(basin_points)
    basin_cnts.append(len(basin_points))
    basin_prev.extend(basin_points)


basin_cnts.sort(reverse = True)
print(basin_cnts)
print(np.prod(basin_cnts[0:3]))









