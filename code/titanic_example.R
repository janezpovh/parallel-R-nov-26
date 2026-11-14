# Parallel Cross-Validation Example
needed.packages <- c("foreach", "doParallel","parallel","tictoc","pracma","microbenchmark","parallely","future","future.apply","RhpcBLASctl")
new.packages <- needed.packages[!(needed.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(future)
library(foreach)    # install.packages('foreach')
library(doParallel) # install.packages('doParallel')


# 1. Load and expand the built-in Titanic dataset into 2,201 individual rows
data("Titanic")

# Define the function
evaluate_titanic_glm <- function(train_ratio = 0.8, threshold = 0.5, seed = 42) {
  
  # 1. Load and expand the built-in Titanic dataset into 2,201 rows
  titanic_df <- as.data.frame(Titanic)
  titanic_full <- titanic_df[rep(1:nrow(titanic_df), titanic_df$Freq), -5]
  rownames(titanic_full) <- NULL
  
  # 2. Split data into Training and Testing sets
  set.seed(seed)
  train_index <- sample(1:nrow(titanic_full), size = train_ratio * nrow(titanic_full))
  train_data  <- titanic_full[train_index, ]
  test_data   <- titanic_full[-train_index, ]
  
  # 3. Train the logistic regression model
  glm_model <- glm(Survived ~ Class + Sex + Age, 
                   data = train_data, 
                   family = "binomial")
  
  # 4. Make predictions on the test set
  pred_probs <- predict(glm_model, newdata = test_data, type = "response")
  pred_class <- ifelse(pred_probs > threshold, "Yes", "No")
  
  # 5. Calculate confusion matrix and accuracy
  conf_matrix <- table(Predicted = pred_class, Actual = test_data$Survived)
  accuracy <- mean(pred_class == test_data$Survived)
  
  # 6. Return both results in a named list
  return(list(
    confusion_matrix = conf_matrix,
    accuracy = round(accuracy * 100, 2)
  ))
}


#########################
# 1 core per Blas process
#########################

library(RhpcBLASctl)

# Force 1 thread so BLAS/OpenMP overhead doesn't choke small GLM fits
blas_set_num_threads(1)
omp_set_num_threads(1)

N <- 1000

time_ser <- system.time({
  results <- lapply(1:N, function(ind) {
    evaluate_titanic_glm(seed = ind)
  })
  final_conf_matrices <- lapply(results, function(x) x$confusion_matrix)
  final_accuracies <- sapply(results, function(x) x$accuracy)
})

print(time_ser)

#parallel

#delete old cluster if it exists
if (exists("cl")) {
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  rm(cl)
  showConnections()
}

# create new cluster with 1 core per Blas process

parallel_compute<-function(N=1000L,cores=n_cores){
  cl <- parallel::makePSOCKcluster(cores)
  
  parallel::clusterEvalQ(cl, {
    library(RhpcBLASctl)
    blas_set_num_threads(1)
    omp_set_num_threads(1)
  })
  doParallel::registerDoParallel(cl)
  
  # Clean up even if a later step fails.
  on.exit({
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
  }, add = TRUE)
  
  # Confirm active workers (should print 3)
  #  getDoParWorkers()
  #  getDoParName()
  
  
  results <- foreach(ind = 1:N, .export = c("evaluate_titanic_glm")) %dopar% {
    evaluate_titanic_glm(seed = ind)
  }
  
  final_conf_matrices <- lapply(results, function(x) x$confusion_matrix)
  final_accuracies <- sapply(results, function(x) x$accuracy)
  return(list(final_conf_matrices,final_accuracies))
}

# final parallel experiment
workers <- c(1:30)

benchmark_time=time_ser

benchmark_time <- foreach(w = workers, .combine = rbind) %do% {
  system.time(parallel_compute(N, cores = w))
}







time_par <- sapply(workers, function(w) {
  library(RhpcBLASctl)
  system.time(paralel_compute(N,cores=w))
})

benchmark_time=rbind(time_ser,time_par)


time_par<-system.time({
  cl <- parallel::makePSOCKcluster(n_cores)
  
  parallel::clusterEvalQ(cl, {
    library(RhpcBLASctl)
    blas_set_num_threads(1)
    omp_set_num_threads(1)
  })
  doParallel::registerDoParallel(cl)
  # Confirm active workers (should print 3)
#  getDoParWorkers()
#  getDoParName()
  
  results <- foreach(ind=c(1:N)) %dopar% {
    evaluate_titanic_glm(seed=ind)
    # Get the fold data where 'fold' is nothing more than a list of indexes for test observations in the data
  }
  final_accuracies <- sapply(results, function(x) x$accuracy)
  final_conf_matrices <- lapply(results, function(x) x$confusion_matrix)
  final_accuracies <- sapply(results, function(x) x$accuracy)
})


times_summary<-rbind(time_ser,time_par) 


# improved by copilot
library(doParallel)
library(foreach)

# Prepare the data once, not once per model fit.
titanic_df <- as.data.frame(Titanic)

titanic_full <- titanic_df[
  rep(seq_len(nrow(titanic_df)), titanic_df$Freq),
  c("Class", "Sex", "Age", "Survived")
]

rownames(titanic_full) <- NULL

######################################
# evaluate_titanic_glm
evaluate_titanic_glm <- function(seed,
                                 train_ratio = 0.8,
                                 threshold = 0.5) {
  set.seed(seed)
  
  n <- nrow(titanic_full)
  
  train_index <- sample.int(
    n,
    size = floor(train_ratio * n)
  )
  
  train_data <- titanic_full[train_index, , drop = FALSE]
  test_data  <- titanic_full[-train_index, , drop = FALSE]
  
  model <- glm(
    Survived ~ Class + Sex + Age,
    data = train_data,
    family = binomial()
  )
  
  probabilities <- predict(
    model,
    newdata = test_data,
    type = "response"
  )
  
  predicted <- ifelse(probabilities > threshold, "Yes", "No")
  
  list(
    confusion_matrix = table(
      Predicted = predicted,
      Actual = test_data$Survived
    ),
    accuracy = round(
      100 * mean(predicted == test_data$Survived),
      2
    )
  )
}

#########################################
# evaluate_batch
# Each worker processes a batch locally.
evaluate_batch <- function(seeds) {
  lapply(seeds, evaluate_titanic_glm)
}

benchmark_titanic <- function(N = 1000L, workers = 3L) {
  stopifnot(N >= 1L, workers >= 1L)
  
  seeds <- seq_len(N)
  workers <- min(as.integer(workers), N)
  
  # Serial baseline
  time_ser <- system.time({
    results_ser <- lapply(seeds, evaluate_titanic_glm)
  })
  
  # Measure startup separately.
  time_setup <- system.time({
    cl <- parallel::makePSOCKcluster(3)
    
    parallel::clusterEvalQ(cl, {
      library(RhpcBLASctl)
      blas_set_num_threads(1)
      omp_set_num_threads(1)
    })
    
    doParallel::registerDoParallel(cl)
  })
  
  # Clean up even if a later step fails.
  on.exit({
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
  }, add = TRUE)
  
  doParallel::registerDoParallel(cl)
  
  # Contiguous batches preserve seed order when flattened.
  batches <- split(
    seeds,
    ceiling(seq_along(seeds) / ceiling(N / workers))
  )
  
  time_par <- system.time({
    batch_results <- foreach(
      batch = batches,
      .export = c(
        "titanic_full",
        "evaluate_titanic_glm",
        "evaluate_batch"
      ),
      .options.snow = list(preschedule = TRUE)
    ) %dopar% {
      evaluate_batch(batch)
    }
    
    # Flatten one level: batches -> individual model results.
    results_par <- do.call(c, unname(batch_results))
  })
  
  # Check that both methods produced the same results.
  same_results <- isTRUE(all.equal(results_ser, results_par))
  
  timings <- rbind(
    serial = time_ser,
    parallel = time_par
  )
  
  list(
    workers = workers,
    timings = timings,
    cluster_startup_seconds = unname(time_setup["elapsed"]),
    speedup_excluding_startup = unname(
      time_ser["elapsed"] / time_par["elapsed"]
    ),
    speedup_including_startup = unname(
      time_ser["elapsed"] /
        (time_setup["elapsed"] + time_par["elapsed"])
    ),
    same_results = same_results,
    final_accuracies = vapply(
      results_par,
      function(x) x$accuracy,
      numeric(1)
    ),
    final_conf_matrices = lapply(
      results_par,
      function(x) x$confusion_matrix
    )
  )
}

out <- benchmark_titanic(N = 1000L, workers = 3L)

out$timings
out$speedup_excluding_startup
out$speedup_including_startup
out$same_results


#another trial
worker_tests <- lapply(1:4, function(w) {
  out <- benchmark_titanic(N = 1000L, workers = w)
  
  data.frame(
    workers = w,
    serial_seconds = unname(out$timings["serial", "elapsed"]),
    parallel_seconds = unname(out$timings["parallel", "elapsed"]),
    speedup = out$speedup_excluding_startup,
    same_results = out$same_results
  )
})

do.call(rbind, worker_tests)


#####################
RhpcBLASctl::blas_get_num_procs()
RhpcBLASctl::blas_set_num_threads(1)
RhpcBLASctl::omp_set_num_threads(1)

parallel::clusterEvalQ(cl, {
  RhpcBLASctl::blas_set_num_threads(1L)
  RhpcBLASctl::omp_set_num_threads(1L)
})
