#Library for timing comparison
library(microbenchmark)

x <- 1:100000
microbenchmark(a = sum(x),
               b = {
                 s0 <- 0
                 for (i in seq_along(x)) {
                   s0 <- s0 + x[i]
                 }
               })
