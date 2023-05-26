# libraries
library(readr)
library(docstring)

# parameters
size <- 18 #size of the matrix grid
gamma <- -0.07 #commuting cost

# commuting distances 
# c <- toeplitz(0:size) #creates a matrix with size+1 rows and columns where first row goes from 0-size - original had size = 9
gravity_commute <- read_delim("Gravity commute.csv", 
                              ";", escape_double = FALSE, col_names = FALSE, 
                              trim_ws = TRUE)
c <- as.matrix(Gravity_commute) 

# shares of housing and employment (first is the excel file and second is a random allocation more flexible to the size of the matrix)
# h_share <- c(0.05,0.075,0.1,0.15,0.15,0.15,0.15,0.1,0.05,0.025)
# e_share <- c(0.6,0.15,0.025,0.025,0.025,0.025,0.025,0.05,0.05,0.025)

h_share <- diff(c(0, sort(runif(size)), 1)) #creates an array of [size] random numbers in a random sequence that all add up to 1
e_share <- diff(c(0, sort(runif(size)), 1)) #creates an array of [size] random numbers in a random sequence that all add up to 1

# initial alphas and betas (first is for the excel file and second is more flexible to the size of the matrix)
# alpha <- numeric(10)
# beta <- numeric(10)

alpha <- numeric(size + 1)
beta <- numeric(size + 1)

get_beta <- function(alpha) {
  #' @title calculates betas
  #' @description This function creates a matrix as a mid calculation to
  #' calculate the betas based on the alphas. 
  #' @param alpha a vector of values used to calculate the betas
  #' @return the updated betas
  a_matrix <- exp(alpha + t(c) * gamma)
  #print(a_matrix)
  beta <- log(e_share) - log(colSums(a_matrix))
  beta_1 <<- log(e_share) - log(colSums(a_matrix))
  beta <<- beta - beta[[1]]
}


get_alpha <- function(beta) {
  #' @title calculates alphas
  #' @description This function creates a matrix as a mid calculation to
  #' calculate the alphas based on the betas 
  #' @param beta a vector of values used to calculate the alphas
  #' @return the updated alphas
  b_matrix <- t(exp(beta + t(c) * gamma))
  alpha <- log(h_share) - log(rowSums(b_matrix))
  alpha <<- alpha - alpha[[1]]
}

# calculates the share of each pair of housing and employment
get_pi_ij <- function(alpha,beta) {
  # creates a matrix where the alphas are repeated along columns
  alphas <-
    matrix(alpha,
           nrow = length(alpha),
           ncol = length(alpha),
           byrow = FALSE)
  # creates a matrix where the betas are repeated along rows
  betas <-
    matrix(beta_1,
           nrow = length(beta_1),
           ncol = length(beta_1),
           byrow = TRUE) 
  # makes this calculation more fluid to have created the matrices:
  pi_ij <<- exp(alphas + betas + gamma * c)
}

# calculates the average commute
get_commutes <- function(pi_ij) {
  commute <- pi_ij * c
  commute <<- pi_ij * c
  avgcommute <<- sum(commute)
}

#iterates through the above functions until the right alphas and betas have been found
iteration <- 0
precision <- 0.0001
repeat { 
  get_beta(alpha)
  get_alpha(beta)
  get_pi_ij(alpha, beta)
  get_commutes(pi_ij)
  print(paste0("average commute: ", avgcommute))
  print(paste0("iterations: ", iteration))
  iteration <- iteration + 1
  if (abs(h_share - rowSums(pi_ij)) < precision &&
      abs(e_share - colSums(pi_ij)) < precision)
    break
} 

# make a function that does the repeat thing of the other functions 
# and each time it returns the average commute from a new gamma that increases
# every time that it runs by 1% 