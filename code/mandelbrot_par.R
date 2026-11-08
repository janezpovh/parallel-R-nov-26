# Set arguments ----

resolution <- as.integer(100) # Resolution of output image. This increases the exponentially x^2.
max.iter <- as.integer(1000)
processes <- 4

# Load parallel library ----
library(parallel)

if (Sys.info()["sysname"]=="Linux"){
  cl <- parallel::makeForkCluster(processes)
} else cl <- makeCluster(processes, type="PSOCK")  

# Defining functions ----

mandelbrot <- function(c, max.iter = 100) {
  z <- 0
  for (i in 1:max.iter) {
    z <- z ^ 2 + c
    if (abs(z) > 2) {
      return(i)
    }
  }
  return(-1)
}

# Prepare input pairs ----

x <- seq(-2, 1, length.out = resolution)
y <- seq(-1.5, 1.5, length.out = resolution)
points <-
  outer(x, y, function(x, y)
    complex(real = x, imaginary = y))

print(microbenchmark::microbenchmark(
  par_apply = parApply(cl,
           X = points,
           MARGIN = c(1, 2),
           FUN = mandelbrot)
))

out_parapply <- parApply(cl,
                         X = points,
                         MARGIN = c(1, 2),
                         FUN = mandelbrot)


stopCluster(cl)
# Visualize ----

# Plotly solution
plotly::plot_ly(z = t(out_for),
                type = "heatmap")


Rcpp::cppFunction(
"
int Mandel(double real, double im, int max_iter=100)
{
std::complex<double> c(real, im);
std::complex<double> z=c;
  for (int i=0;i < max_iter;i++){
    z=z*z+c;
    if (std::abs(z) > 2){
      return i;
    }
  }
  return max_iter;
}
"  
  
)
