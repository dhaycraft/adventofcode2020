# --- Day 12: Rain Risk ---
#   Your ferry made decent progress toward the island, but the storm came in faster than anyone expected. The ferry needs to take evasive actions!
#   
#   Unfortunately, the ship's navigation computer seems to be malfunctioning; rather than giving a route directly to safety, it produced extremely circuitous instructions. When the captain uses the PA system to ask if anyone can help, you quickly volunteer.
# 
# The navigation instructions (your puzzle input) consists of a sequence of single-character actions paired with integer input values. After staring at them for a few minutes, you work out what they probably mean:
# 
# Action N means to move north by the given value.
# Action S means to move south by the given value.
# Action E means to move east by the given value.
# Action W means to move west by the given value.
# Action L means to turn left the given number of degrees.
# Action R means to turn right the given number of degrees.
# Action F means to move forward by the given value in the direction the ship is currently facing.
# The ship starts by facing east. Only the L and R actions change the direction the ship is facing. (That is, if the ship is facing east and the next instruction is N10, the ship would move north 10 units, but would still move east if the following action were F.)
# 
# For example:
# 
# F10
# N3
# F7
# R90
# F11
# These instructions would be handled as follows:
# 
# F10 would move the ship 10 units east (because the ship starts by facing east) to east 10, north 0.
# N3 would move the ship 3 units north to east 10, north 3.
# F7 would move the ship another 7 units east (because the ship is still facing east) to east 17, north 3.
# R90 would cause the ship to turn right by 90 degrees and face south; it remains at east 17, north 3.
# F11 would move the ship 11 units south to east 17, south 8.
# At the end of these instructions, the ship's Manhattan distance (sum of the absolute values of its east/west position and its north/south position) from its starting position is 17 + 8 = 25.
# 
# Figure out where the navigation instructions lead. What is the Manhattan distance between that location and the ship's starting position?


ship_direction <- read.table("2020/day12/ship_direction.txt", quote="\"", comment.char="")

dist_ns = 0
dist_ew = 0
curr_direction=0 

iter_dist_ns <- c()
iter_dist_ew <- c()

direct_df <- data.frame(dir_value = 0:3, direction = c("E", "S", "W", "N"))


ship_direction <- ship_direction %>% 
  mutate(direction = gsub("[0-9]", "", V1),
         movement = as.numeric(gsub("[A-Z]", "", V1)))

ship_direction <- left_join(ship_direction, direct_df)



for(i in 1:nrow(ship_direction)){
  print(curr_direction)
  iter_dir = ifelse(is.na(ship_direction$dir_value[i]), ship_direction$direction[i], ship_direction$dir_value[i])
  iter_movement = ship_direction$movement[i]
  
  if(iter_dir %in% c("R", "L")){
    dir_diff=iter_movement/90
  if(iter_dir=="R"){
    curr_direction = (curr_direction+dir_diff) %% 4
  } else if(iter_dir=="L"){
    curr_direction= (curr_direction-dir_diff) %% 4
  } 
  } else if(iter_dir %in% c(0:3)){
  if(iter_dir==0){
    dist_ew <- dist_ew + iter_movement
  } else if(iter_dir==2){
    dist_ew <- dist_ew - iter_movement
  } else if(iter_dir==1){
    dist_ns <- dist_ns-iter_movement
  }else if(iter_dir==3){
    dist_ns <- dist_ns+iter_movement
  } 
  } else(
    
    if(curr_direction==0){
      dist_ew <- dist_ew + iter_movement
    } else if(curr_direction==2){
      dist_ew <- dist_ew - iter_movement
    } else if(curr_direction==1){
      dist_ns <- dist_ns-iter_movement
    }else if(curr_direction==3){
      dist_ns <- dist_ns+iter_movement
    }
  )
iter_dist_ns <- append(iter_dist_ns, dist_ns)
iter_dist_ew <- append(iter_dist_ew, dist_ew)
print(paste(iter_dir, curr_direction))
}


(abs(iter_dist_ew)+ abs(iter_dist_ns)) %>% tail(1)



# --- Part Two ---
#   Before you can give the destination to the captain, you realize that the actual action meanings were printed on the back of the instructions the whole time.
# 
# Almost all of the actions indicate how to move a waypoint which is relative to the ship's position:
# 
# Action N means to move the waypoint north by the given value.
# Action S means to move the waypoint south by the given value.
# Action E means to move the waypoint east by the given value.
# Action W means to move the waypoint west by the given value.
# Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
# Action R means to rotate the waypoint around the ship right (clockwise) the given number of degrees.
# Action F means to move forward to the waypoint a number of times equal to the given value.
# The waypoint starts 10 units east and 1 unit north relative to the ship. The waypoint is relative to the ship; that is, if the ship moves, the waypoint moves with it.
# 
# For example, using the same instructions as above:
# 
# F10 moves the ship to the waypoint 10 times (a total of 100 units east and 10 units north), leaving the ship at east 100, north 10. The waypoint stays 10 units east and 1 unit north of the ship.
# N3 moves the waypoint 3 units north to 10 units east and 4 units north of the ship. The ship remains at east 100, north 10.
# F7 moves the ship to the waypoint 7 times (a total of 70 units east and 28 units north), leaving the ship at east 170, north 38. The waypoint stays 10 units east and 4 units north of the ship.
# R90 rotates the waypoint around the ship clockwise 90 degrees, moving it to 4 units east and 10 units south of the ship. The ship remains at east 170, north 38.
# F11 moves the ship to the waypoint 11 times (a total of 44 units east and 110 units south), leaving the ship at east 214, south 72. The waypoint stays 4 units east and 10 units south of the ship.
# After these operations, the ship's Manhattan distance from its starting position is 214 + 72 = 286.
# 
# Figure out where the navigation instructions actually lead. What is the Manhattan distance between that location and the ship's starting position?



iter_waypoint_x <- 10
iter_waypoint_y <- 1


dist_ns = 0
dist_ew = 0

iter_dist_ns <- c()
iter_dist_ew <- c()

for(i in 1:nrow(ship_direction)){
  iter_dir = ship_direction$direction[i]
  iter_movement = ship_direction$movement[i]
  
  if(iter_dir %in% c("R","L")){
    if(iter_movement==180){
      # 180 rotation same for both
      next_waypoint_x = -iter_waypoint_x
      next_waypoint_y = -iter_waypoint_y
    } else if((iter_movement==90 & iter_dir=="L")|(iter_movement==270 & iter_dir=="R")){
      # counter clockwise rotation of 90%
      next_waypoint_x =  -iter_waypoint_y
      next_waypoint_y = iter_waypoint_x
    } else if((iter_movement==90 & iter_dir=="R")|(iter_movement==270 & iter_dir=="L")){
      next_waypoint_x = iter_waypoint_y
      next_waypoint_y = -iter_waypoint_x
    }
    iter_waypoint_x <- next_waypoint_x
    iter_waypoint_y <- next_waypoint_y
  } else if(iter_dir=="N"){
    iter_waypoint_y = iter_waypoint_y+iter_movement
  } else if(iter_dir=="S"){
    iter_waypoint_y = iter_waypoint_y-iter_movement
  } else if(iter_dir=="E"){
    iter_waypoint_x = iter_waypoint_x+iter_movement
  } else if(iter_dir=="W"){
    iter_waypoint_x = iter_waypoint_x-iter_movement
  } else{
    dist_ew = dist_ew + iter_movement* iter_waypoint_x
    dist_ns = dist_ns + iter_movement*iter_waypoint_y
  }
  iter_dist_ns <- append(iter_dist_ns, dist_ns)
  iter_dist_ew <- append(iter_dist_ew, dist_ew)
  print(paste(iter_waypoint_x, iter_waypoint_y))
}

iter_dist_ew
iter_dist_ns

(abs(iter_dist_ew)+ abs(iter_dist_ns)) %>% tail(1)
