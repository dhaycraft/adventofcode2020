# Find the two entries that sum to 2020; what do you get if you multiply them together?

input_numbers=read.table("day1/input_numbers.txt", quote="\"", comment.char="")

# Function that finds a given number of elements n from a numeric vector that sum to the number 2020.
# then returns the elements as a data frame as well as their product as a list.
get_n_sum_2020 <- function(input, n){
  all_combn <- data.frame(t(combn(input, n)))
  combn_sum_2020 <- all_combn[which(rowSums(all_combn)==2020),]
  prod_sum_2020 <- apply(combn_sum_2020, 1, prod)
  return(list(combn_sum_2020, prod_sum_2020))
}

get_n_sum_2020(input_numbers$V1, 2)

# In your expense report, what is the product of the three entries that sum to 2020?


get_n_sum_2020(input_numbers$V1, 3)
