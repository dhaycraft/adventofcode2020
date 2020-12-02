library(stringr)
library(dplyr)
library(tidyr)

passwords <- read.table("C:/Users/haycr/Desktop/Repositories/adventofcode/2020/day2/passwords.txt", quote="\"", comment.char="")

passwords <- passwords %>% mutate(V2= gsub(":", "", V2)) %>%
                           separate(sep="-",
                                    col=V1,
                                    into=c("low",'high')) %>%
                           mutate(low=as.numeric(low),
                                  high=as.numeric(high))

passwords$cnts <- mapply(function(x,y) {str_count(y,x)}, passwords$V2, passwords$V3, USE.NAMES = FALSE)

passwords %>% filter(cnts>=low & cnts<=high) %>% nrow()


 passwords %>% mutate(pos1 = substr(V3, low, low),
                     pos2 = substr(V3, high, high)) %>%
              filter((pos1==V2 & pos2!=V2) | (pos1!=V2 & pos2==V2)) %>%
              nrow()
