a <- c(3,4,5)

data <- data.frame(a = c(1,2,3), b = c(2,3,4), status = c(T,F,T))

myfun <- function(data, outcome) {


}

myfun(data, survival::Surv(a, status))
