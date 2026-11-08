# Parallel Cross-Validation Example

library(foreach)    # install.packages('foreach')
library(caret)      # install.packages('caret', dependencies = c("Depends", "Suggests"))
library(doParallel) # install.packages('doParallel')

#data <- read.csv(…) # Assuming this is your dataset 

if (Sys.info()["sysname"]=="Windows"){
  setwd("C:/Users/janez/OneDrive - Rudolfovo – znanstveno in tehnološko središče Novo Mesto/Documents/UL FS/Projekti/2022/EuroCC II/Izobrazecanja UL/parallel-R-sep-24/")
} else 
  setwd("/home/rstudio/parallel-R-sep-24")

dir()


data<-read.csv(file= "data/K_data_clean.txt") 
head(data)


N=100;
n=round(nrow(data)*0.3)
# 'dopar' here would run this on multiple threads (change to just 'do' for synchronous runs)
time_ser<-system.time({
  results <- foreach(ind=c(1:N)) %do% {
  
  # Get the fold data where 'fold' is nothing more than a list of indexes for test observations in the data
  fold=sample(1:nrow(data),n,replace =FALSE)
  data.train <- data[-fold,] # Get the opposite of the test observations to train on
  data.test <- data[fold,]
  
  # Fit the model and make predictions - logistic regression
  fit <- glm(class ~ Feature_1+Feature_2+Feature_3+Feature_4+group, data=data.train, family='binomial')
  y.pred <- as.numeric(predict(fit, newdata=data.test, type="response")>0.5)
  y.true <- data.test$class
  
  # Return 2x2 table of predictions vs actual values as well as the fit model (so you could check coefficients)
  table(y.pred, y.true, dnn=c('predicted', 'actual'))
}

final_tab<-apply(simplify2array(results), c(1,2), sum)
final_acc=sum(diag(final_tab))/sum(final_tab)
})

registerDoParallel(makeCluster(10)) # Use 10 cores for parallel CV
#registerDoParallel(2)  # use multicore, set to the number of our cores - needed for foerach dopar
getDoParName()

time_par<-system.time({
  results <- foreach(ind=c(1:N)) %dopar% {
    
    # Get the fold data where 'fold' is nothing more than a list of indexes for test observations in the data
    fold=sample(1:nrow(data),n,replace =FALSE)
    data.train <- data[-fold,] # Get the opposite of the test observations to train on
    data.test <- data[fold,]
    
    # Fit the model and make predictions - logistic regression
    fit <- glm(class ~ Feature_1+Feature_2+Feature_3+Feature_4+group, data=data.train, family='binomial')
    y.pred <- as.numeric(predict(fit, newdata=data.test, type="response")>0.5)
    y.true <- data.test$class
    
    # Return 2x2 table of predictions vs actual values as well as the fit model (so you could check coefficients)
    table(y.pred, y.true, dnn=c('predicted', 'actual'))
  }
  
  final_tab<-apply(simplify2array(results), c(1,2), sum)
  final_acc=sum(diag(final_tab))/sum(final_tab)
})


#stopCluster(clust)

rbind(time_ser,time_par)


