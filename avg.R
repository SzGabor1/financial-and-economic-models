atlag <- function(x)
{
    sum <- 0
    n <- length(x)
    for (i in x) {
        sum <- sum + i
    }
    avg <- sum / n
    print(avg)
}
x = rnorm(100)
atlag(x)