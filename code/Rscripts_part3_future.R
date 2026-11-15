needed.packages <- c("foreach", "doParallel","parallel","tictoc","pracma","microbenchmark","parallely","future","future.apply")
new.packages <- needed.packages[!(needed.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(future)
library(future.apply)

# One computationally expensive task
f <- function(i) {
  x <- rnorm(5e6)
  c(task = i, statistic = mean(exp(x)))
}

n_tasks <- 8
n_workers <- 2

# 1. Sequential
plan(sequential)

set.seed(2026)
t_seq <- system.time({
  res_seq <- future_lapply(
    1:n_tasks, f,
    future.seed = TRUE
  )
})

# 2. Multisession (PSOCK)
plan(multisession, workers = n_workers)

set.seed(2026)
t_session <- system.time({
  res_session <- future_lapply(
    1:n_tasks, f,
    future.seed = TRUE
  )
})

# 3. Multicore (forking: Linux/macOS only)
library(parallelly)

options(parallelly.fork.enable = TRUE)
supportsMulticore()

Sys.info()[["sysname"]]
Sys.getenv("RSTUDIO")
getOption("parallelly.fork.enable")


if (supportsMulticore()) {
  plan(multicore, workers = n_workers)
  
  set.seed(2026)
  t_core <- system.time({
    res_core <- future_lapply(
      1:n_tasks, f,
      future.seed = TRUE
    )
  })
}

# Reset
plan(sequential)

# Compare elapsed times
times <- c(
  sequential   = t_seq["elapsed"],
  multisession = t_session["elapsed"],
  multicore    = if (supportsMulticore())
    t_core["elapsed"] else NA
)

print(times)



