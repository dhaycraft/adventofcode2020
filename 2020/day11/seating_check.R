seating <- readLines("2020/day11/seating.txt")
rows_mat <- length(seating)
width_mat <- 90


seating <- unlist(strsplit(seating, split =""))

seat_matrix <- matrix(seating, nrow=rows_mat,byrow = TRUE)


library(tidyverse)
library(tibble)
# make reshape matrix function 

convert_matrix <- function(input_mat){
  as.data.frame.matrix(input_mat) %>% 
  rownames_to_column("y") %>% 
  pivot_longer(cols = -y, names_to= 'x', values_to= "values") %>%
  mutate(y= as.numeric(y),
         x= as.numeric(gsub("V","",x)))
}


curr_values <- convert_matrix(seat_matrix)

# join to values that are nonempty
# find sum filled
# rest are empty

iter_check=FALSE
while(iter_check==FALSE){
  
  curr_ru <- data.frame(x = curr_values$x+1, y= curr_values$y+1) %>% left_join(curr_values)
  curr_r <- data.frame(x = curr_values$x+1, y= curr_values$y) %>% left_join(curr_values)
  curr_rd <- data.frame(x = curr_values$x+1, y= curr_values$y-1) %>% left_join(curr_values)
  curr_d <- data.frame(x = curr_values$x, y = curr_values$y-1) %>% left_join(curr_values)
  curr_ld <- data.frame(x = curr_values$x-1, y= curr_values$y-1) %>% left_join(curr_values)
  curr_l <- data.frame(x = curr_values$x-1, y= curr_values$y) %>% left_join(curr_values)
  curr_lu <- data.frame(x = curr_values$x-1, y= curr_values$y+1) %>% left_join(curr_values)
  curr_u <- data.frame(x = curr_values$x, y= curr_values$y+1) %>% left_join(curr_values)
  
  adjacent_values <- data.frame(curr_ru = curr_ru$values,
                                curr_r = curr_r$values,
                                curr_rd = curr_rd$values,
                                curr_d = curr_d$values,
                                curr_ld = curr_ld$values,
                                curr_l = curr_l$values,
                                curr_lu = curr_lu$values,
                                curr_u = curr_u$values)
 
  curr_values <- curr_values %>% 
                  mutate(occupy_cnt = rowSums(adjacent_values=="#", na.rm=TRUE),
                         next_values = case_when(values=="L" & occupy_cnt==0 ~ "#",
                                                 values=="#" & occupy_cnt >=4 ~ "L",
                                                 TRUE ~ values))
  
  
  iter_check <- identical(curr_values$values, curr_values$next_values)
  
  curr_values <- curr_values %>% 
                    select(y,x,next_values) %>%
                    rename(values = next_values)
}

table(curr_values$values)



s1 <- matrix(curr_values$values,ncol=10, byrow=TRUE)

s2 <- matrix(curr_values$next_values,ncol=10, byrow=TRUE)

#.##.L#.##
#L###LL.L#
L.#.#..#..
#L##.##.L#
#.##.LL.LL
#.###L#.##
..#.#.....
#L######L#
#.LL###L.L
#.#L###.##


while(iter_check==FALSE){
  
  curr_ru <- mapply(function(x,y){
    if(y==1 | x==width_mat){
      "L"
    }else{
      len_y=y-1
      len_x = width_mat-x
      len_seq= min(len_x,len_y)
      diag_df = data.frame(y=seq(from= y-1, by=-1, length.out=len_seq), 
                           x=seq(from= x+1, by=1, length.out=len_seq))
      diag_df <- left_join(diag_df, curr_values)
      if(any(diag_df$values=="#")){
        "#"
      }else{
        "L"
      }
    }}, curr_values$x, curr_values$y, SIMPLIFY=TRUE)
  
  
  curr_rd <- mapply(function(x,y){
    if(y==rows_mat | x==width_mat){
      "L"
    }else{
      len_y=rows_mat-y
      len_x = width_mat-x
      len_seq= min(len_x,len_y)
      diag_df = data.frame(y=seq(y+1 , by=1, length.out=len_seq), 
                           x=seq(from= x+1, by=1, length.out=len_seq))
      diag_df <- left_join(diag_df, curr_values)
      if(any(diag_df$values=="#")){
        "#"
      }else{
        "L"
      }
    }}, curr_values$x, curr_values$y, SIMPLIFY=TRUE)
  
  curr_ld <- mapply(function(x,y){
    if(y==rows_mat | x==1){
      "L"
    }else{
      len_y=rows_mat-y
      len_x = x-1
      len_seq= min(len_x,len_y)
      diag_df = data.frame(y=seq(y+1 , by=1, length.out=len_seq), 
                           x=seq(from= x-1, by=-1, length.out=len_seq))
      diag_df <- left_join(diag_df, curr_values)
      if(any(diag_df$values=="#")){
        "#"
      }else{
        "L"
      }
    }}, curr_values$x, curr_values$y, SIMPLIFY=TRUE)
    

curr_lu <- mapply(function(x,y){
  if(y==rows_mat | x==1){
    "L"
  }else{
    len_y= y-1
    len_x = x-1
    len_seq= min(len_x,len_y)
    diag_df = data.frame(y=seq(y-1 , by=-1, length.out=len_seq), 
                         x=seq(from= x-1, by=-1, length.out=len_seq))
    diag_df <- left_join(diag_df, curr_values)
    if(any(ifelse(is.na(diag_df$values),"L",diag_df$values)=="#")){
      "#"
    }else{
      "L"
    }
  }}, curr_values$x, curr_values$y, SIMPLIFY=TRUE)

curr_l <- mapply(function(x,y){
  if(x==1){
    "L"
  }else{
    len_y= y-1
    len_x = x-1
    len_seq= min(len_x,len_y)
    diag_df = data.frame(y=seq(y-1 , by=-1, length.out=len_seq), 
                         x=seq(from= x-1, by=-1, length.out=len_seq))
    diag_df <- left_join(diag_df, curr_values)
    if(any(ifelse(is.na(diag_df$values),"L",diag_df$values)=="#")){
      "#"
    }else{
      "L"
    }
  }}, curr_values$x, curr_values$y, SIMPLIFY=TRUE)

  curr_r <- data.frame(x = curr_values$x+1, y= curr_values$y) %>% left_join(curr_values)
  curr_rd <- data.frame(x = curr_values$x+1, y= curr_values$y-1) %>% left_join(curr_values)
  curr_d <- data.frame(x = curr_values$x, y = curr_values$y-1) %>% left_join(curr_values)
  curr_ld <- data.frame(x = curr_values$x-1, y= curr_values$y-1) %>% left_join(curr_values)
  curr_l <- data.frame(x = curr_values$x-1, y= curr_values$y) %>% left_join(curr_values)
  curr_lu <- data.frame(x = curr_values$x-1, y= curr_values$y+1) %>% left_join(curr_values)
  curr_u <- data.frame(x = curr_values$x, y= curr_values$y+1) %>% left_join(curr_values)
  
  adjacent_values <- data.frame(curr_ru = curr_ru$values,
                                curr_r = curr_r$values,
                                curr_rd = curr_rd$values,
                                curr_d = curr_d$values,
                                curr_ld = curr_ld$values,
                                curr_l = curr_l$values,
                                curr_lu = curr_lu$values,
                                curr_u = curr_u$values)
  
  curr_values <- curr_values %>% 
    mutate(occupy_cnt = rowSums(adjacent_values=="#", na.rm=TRUE),
           next_values = case_when(values=="L" & occupy_cnt==0 ~ "#",
                                   values=="#" & occupy_cnt >=4 ~ "L",
                                   TRUE ~ values))
  
  
  iter_check <- identical(curr_values$values, curr_values$next_values)
  
  curr_values <- curr_values %>% 
    select(y,x,next_values) %>%
    rename(values = next_values)
}
