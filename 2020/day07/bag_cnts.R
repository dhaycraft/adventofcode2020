# --- Day 7: Handy Haversacks ---
#   You land at the regional airport in time for your next flight. In fact, it looks like you'll even have time to grab some food: all flights are currently delayed due to issues in luggage processing.
# 
# Due to recent aviation regulations, many rules (your puzzle input) are being enforced about bags and their contents; bags must be color-coded and must contain specific quantities of other color-coded bags. Apparently, nobody responsible for these regulations considered how long they would take to enforce!
# 
# For example, consider the following rules:
# 
# light red bags contain 1 bright white bag, 2 muted yellow bags.
# dark orange bags contain 3 bright white bags, 4 muted yellow bags.
# bright white bags contain 1 shiny gold bag.
# muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
# shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
# dark olive bags contain 3 faded blue bags, 4 dotted black bags.
# vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
# faded blue bags contain no other bags.
# dotted black bags contain no other bags.
# These rules specify the required contents for 9 bag types. In this example, every faded blue bag is empty, every vibrant plum bag contains 11 bags (5 faded blue and 6 dotted black), and so on.
# 
# You have a shiny gold bag. If you wanted to carry it in at least one other bag, how many different bag colors would be valid for the outermost bag? (In other words: how many colors can, eventually, contain at least one shiny gold bag?)
# 
# In the above rules, the following options would be available to you:
# 
# A bright white bag, which can hold your shiny gold bag directly.
# A muted yellow bag, which can hold your shiny gold bag directly, plus some other bags.
# A dark orange bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
# A light red bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
# So, in this example, the number of bag colors that can eventually contain at least one shiny gold bag is 4.
# 
# How many bag colors can eventually contain at least one shiny gold bag? (The list of rules is quite long; make sure you get all of it.)


bag_rules <- readLines("2020/day7/bag_rules.txt")


library(stringr)

find_bags <- function(x){
  bags_found <- c()
  bag_new = x
  while(length(bag_new)!=0){
    rules_iter <- bag_rules[grepl(paste("contain.*",bag_new,collapse="|"), bag_rules)]
    bag_new = gsub(" bags contain", "",str_extract(rules_iter,"^.* bags contain"))
    bag_new <- setdiff(bag_new, bags_found)
    bags_found <- append(bags_found, bag_new)
  }
  return(length(bags_found))
}

find_bags("shiny gold")



# --- Part Two ---
#   It's getting pretty expensive to fly these days - not because of ticket prices, but because of the ridiculous number of bags you need to buy!
# 
# Consider again your shiny gold bag and the rules from the above example:
# 
# faded blue bags contain 0 other bags.
# dotted black bags contain 0 other bags.
# vibrant plum bags contain 11 other bags: 5 faded blue bags and 6 dotted black bags.
# dark olive bags contain 7 other bags: 3 faded blue bags and 4 dotted black bags.
# So, a single shiny gold bag must contain 1 dark olive bag (and the 7 bags within it) plus 2 vibrant plum bags (and the 11 bags within each of those): 1 + 1*7 + 2 + 2*11 = 32 bags!
# 
# Of course, the actual rules have a small chance of going several levels deeper than this example; be sure to count all of the bags, even if the nesting becomes topologically impractical!
# 
# Here's another example:
#   
#   shiny gold bags contain 2 dark red bags.
# dark red bags contain 2 dark orange bags.
# dark orange bags contain 2 dark yellow bags.
# dark yellow bags contain 2 dark green bags.
# dark green bags contain 2 dark blue bags.
# dark blue bags contain 2 dark violet bags.
# dark violet bags contain no other bags.
# In this example, a single shiny gold bag must contain 126 other bags.
# 
# How many individual bags are required inside your single shiny gold bag?
                              
                   
next_iter_bags <- function(x){
  bags_next = unlist(str_extract_all(x, "[0-9] [a-z]+ [a-z]+"))
  container_name <- gsub(" bags contain", "",str_extract(x,".* bags contain"))
  bag_names = str_extract(bags_next, "[a-z]+ [a-z]+")
  bag_cnts = as.numeric(str_extract(bags_next, "[0-9]"))
  return(list(data.frame(container=container_name,bag_names=bag_names, bag_cnts=bag_cnts)))
}


bag_cnts <- bind_rows(mapply(function(x) next_iter_bags(x), bag_rules[!grepl("bags contain no other bags.", bag_rules)], SIMPLIFY=FALSE, USE.NAMES = FALSE))

count_bags <- function(x){
  curr_iter <- bag_cnts %>% filter(container %in% x) %>%
    rename(bag_cnts_curr=bag_cnts,
           bag_names_curr=bag_names)
  tot_bags=sum(curr_iter$bag_cnts_curr)
  while(sum_curr_iter!=0){
    next_iter <- bag_cnts %>% filter(container %in% curr_iter$bag_names_curr) %>%
                              rename(bag_names_next=bag_names,
                                     bag_cnts_next=bag_cnts)
    
    both_iter <- inner_join(curr_iter, next_iter, by=c("bag_names_curr"="container"))
    both_iter$bag_cnts_next <- both_iter$bag_cnts_curr* both_iter$bag_cnts_next
    sum_curr_iter <- sum(both_iter$bag_cnts_next, na.rm=TRUE)
    tot_bags = tot_bags+sum_curr_iter
    curr_iter <- both_iter %>% select(bag_names_next, bag_cnts_next) %>%
                      rename(bag_names_curr=bag_names_next,
                             bag_cnts_curr= bag_cnts_next)
  }
  return(tot_bags)
  
}

count_bags("shiny gold")
