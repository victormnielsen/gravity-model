


distances <- cbind(c(0,1,2,3,4,5,6,7,8,9),
                   c(1,0,1,2,3,4,5,6,7,8),
                   c(2,1,0,1,2,3,4,5,6,7),
                   c(3,2,1,0,1,2,3,4,5,6),
                   c(4,3,2,1,0,1,2,3,4,5),
                   c(5,4,3,2,1,0,1,2,3,4),
                   c(6,5,4,3,2,1,0,1,2,3),
                   c(7,6,5,4,3,2,1,0,1,2),
                   c(8,7,6,5,4,3,2,1,0,1),
                   c(9,8,7,6,5,4,3,2,1,0))

hshare <- c(0.05,0.075,0.1,0.15,0.15,0.15,0.15,0.1,0.05,0.025)
jshare <- c(0.6,0.15,0.025,0.025,0.025,0.025,0.025,0.05,0.05,0.025)

c <- -0.07

init_a <- c(0,0,0,0,0,0,0,0,0,0)

matrix_a_init <- exp(init_a+t(distances)*c)
matrix_a_init

init_b_ <- log(jshare)-log(colSums(matrix_a_init))
init_b_
init_b <- init_b_-init_b_[[1]]
init_b

matrix_b_init <- t(exp(init_b+t(distances)*c))
matrix_b_init

first_a_ <- log(hshare)-log(rowSums(matrix_b_init))
first_a_
first_a <- first_a_-first_a_[[1]]
first_a

matrix_a_1 <- exp(first_a+t(distances)*c)
matrix_a_1

first_b_ <- log(jshare)-log(colSums(matrix_a_1))
first_b_
first_b <- first_b_-first_b_[[1]]
first_b

matrix_b_1 <- t(exp(first_b+t(distances)*c))
matrix_b_1

second_a_ <- log(hshare)-log(rowSums(matrix_b_1))
second_a_
second_a <- second_a_-second_a_[[1]]
second_a

matrix_a_2 <- exp(second_a+t(distances)*c)
matrix_a_2

second_b_ <- log(jshare)-log(colSums(matrix_a_2))
second_b_
second_b <- second_b_-second_b_[[1]]
second_b

second_a_asmatrix <- matrix(second_a, nrow=length(second_a), ncol=length(second_a), byrow=FALSE)
second_a_asmatrix
second_b_asmatrix <- matrix(second_b_, nrow=length(second_b_), ncol=length(second_b_), byrow=TRUE)
second_b_asmatrix

fractions <- exp(second_a_asmatrix+second_b_asmatrix+c*distances)
fractions

hshare_confirm <- rowSums(fractions)
hshare_confirm
jshare_confirm <- colSums(fractions)
jshare_confirm

commutes <- fractions*distances
commutes

average <- sum(commutes)
average

### I should be able to automate line 25-60