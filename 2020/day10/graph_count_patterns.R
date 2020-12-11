adapters <- read.table("C:/Users/haycr/Desktop/Repositories/adventofcode/2020/day10/adapters.txt", quote="\"", comment.char="")

adapters <- adapters %>% arrange(V1)
adapters <- data.frame(adapt = c(0,adapters$V1, (max(adapters$V1)+3)))

  
adapt_df <- adapters %>% 
                  mutate(step_adapt = lead(adapt,1)) %>%
                  filter(!is.na(step_adapt)) %>%
                  mutate(diff_step = step_adapt-adapt)

prod(table(adapt_df$diff_step))

adapters$adapt <- adapters$adapt+1

paths <- rep(0, max(adapters$adapt))
paths[1] <- 1

for(i in 1:max(adapters$adapt)){
  for(j in 1:3){
    iter_index = i-j
    if(iter_index %in% adapters$adapt){
      paths[i] = paths[i]+paths[iter_index]
    }
  }
}

options("scipen" = 10)
print(paths[length(paths)])



