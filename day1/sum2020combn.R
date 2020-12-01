# Find the two entries that sum to 2020; what do you get if you multiply them together?

input_numbers=read.table("C:/Users/haycr/Desktop/Repositories/adventofcode2020/day1/input_numbers.txt", quote="\"", comment.char="")

get_n_sum_2020 <- function(input, pairs){
  all_combn <- data.frame(t(combn(input,pairs)))
  combn_sum_2020 <- all_combn[which(rowSums(all_combn)==2020),]
  prod_sum_2020 <- apply(combn_sum_2020, 1, prod)
  return(list(combn_sum_2020, prod_sum_2020))
}

get_n_sum_2020(input_numbers$V1, 2)

# In your expense report, what is the product of the three entries that sum to 2020?


get_n_sum_2020(input_numbers$V1, 3)
