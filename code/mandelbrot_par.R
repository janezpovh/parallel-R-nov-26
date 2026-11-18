# Set arguments ----

# Same settings as in mandelbrot.R, so the results can be compared
resolution <- as.integer(1000) # Resolution of output image. This increases the exponentially x^2.
max.iter <- as.integer(100)
cmin <- complex(real = -1.5, imaginary = -1)
cmax <- complex(real = 0.5, imaginary = 1)
processes <- 4

# Load parallel library ----
library(parallel)

if (Sys.info()["sysname"]=="Linux"){
  cl <- parallel::makeForkCluster(processes)
} else cl <- makeCluster(processes, type="PSOCK")  

# Defining functions ----

# Escape time: number of iterations n of z = z^2 + c (starting at z = 0)
# until |z| > 2; max.iter if the point does not escape (it is in the set)
mandelbrot <- function(c, max.iter = 100) {
  z <- 0
  for (i in 1:max.iter) {
    z <- z ^ 2 + c
    if (abs(z) > 2) {
      return(i)
    }
  }
  return(max.iter)
}

# Plot helper ----
# - colours change on a log scale: most points escape within a few iterations,
#   with linearly spaced colours they would all look the same
# - points in the set (value max_iter) are left empty and drawn black
# - axes in real coordinates with equal aspect ratio
plot_mandelbrot <- function(out, x, y, max_iter) {
  z <- out
  z[out == max_iter] <- NA
  lo <- min(z, na.rm = TRUE)
  hi <- max(z, na.rm = TRUE)
  cols <- colorRampPalette(c("#000764", "#206BCB", "#EDFFFF", "#FFAA00", "#7A1E00"))(32)
  pos <- (exp(seq(log(lo), log(hi), length.out = length(cols))) - lo) / (hi - lo)
  plotly::plot_ly(x = x, y = y, z = t(z), type = "heatmap",
                  colorscale = data.frame(pos, cols),
                  colorbar = list(title = "Iterations"),
                  hovertemplate = "c = %{x:.3f} %{y:+.3f}i<br>iterations: %{z}<extra></extra>") |>
    plotly::layout(plot_bgcolor = "black",
                   xaxis = list(title = "Re(c)", showgrid = FALSE, zeroline = FALSE,
                                constrain = "domain"),
                   yaxis = list(title = "Im(c)", showgrid = FALSE, zeroline = FALSE,
                                scaleanchor = "x", constrain = "domain"))
}

# Prepare input pairs ----

# Same grid as in mandelbrot.R
dc <- cmax - cmin
x <- Re(cmin) + ((1:resolution - 1) / resolution * Re(dc))
y <- Im(cmin) + ((1:resolution - 1) / resolution * Im(dc))
points <-
  outer(x, y, function(x, y)
    complex(real = x, imaginary = y))

out_parapply <- parApply(cl,
                         X = points,
                         MARGIN = c(1, 2),
                         FUN = mandelbrot,
                         max.iter = max.iter)


# Visualize ----

# Plotly solution
plot_mandelbrot(out_parapply, x, y, max.iter)


# Parallel solution in C++ with RcppParallel ----
# RcppParallel wraps Intel TBB (bundled with the package, nothing else to
# install). You write a 'Worker' whose operator()(begin, end) processes a
# range of rows; parallelFor() splits the rows into chunks and schedules them
# on a thread pool with work stealing (automatic load balancing).
# Important: no R API calls inside the worker - the R interpreter is
# single-threaded. RMatrix/RVector are thread-safe accessors to R memory
# allocated beforehand on the main thread.

Rcpp::cppFunction(
  depends = "RcppParallel",
  includes = "#include <RcppParallel.h>\n#include <complex>",
  code = "
IntegerMatrix MandelbrotRcppPar(NumericVector x, NumericVector y,
                                int max_iter = 100, int grain_size = 1)
{
  struct MandelWorker : public RcppParallel::Worker {
    const RcppParallel::RVector<double> xs, ys;   // inputs
    RcppParallel::RMatrix<int> out;               // output
    const int max_iter;

    MandelWorker(const NumericVector x, const NumericVector y,
                 IntegerMatrix out, int max_iter)
      : xs(x), ys(y), out(out), max_iter(max_iter) {}

    // Compute rows [begin, end)
    void operator()(std::size_t begin, std::size_t end) {
      for (std::size_t i = begin; i < end; i++) {
        for (std::size_t j = 0; j < ys.length(); j++) {
          std::complex<double> c(xs[i], ys[j]);
          std::complex<double> z(0.0, 0.0);
          int res = max_iter;   // value for points in the set
          for (int k = 1; k <= max_iter; k++) {
            z = z * z + c;
            if (std::abs(z) > 2.0) { res = k; break; }
          }
          out(i, j) = res;
        }
      }
    }
  };

  IntegerMatrix out(x.size(), y.size());   // allocated on the main thread
  MandelWorker worker(x, y, out, max_iter);
  RcppParallel::parallelFor(0, x.size(), worker, grain_size);
  return out;
}
")

# Number of threads is set from R (default: all cores)
RcppParallel::setThreadOptions(numThreads = processes)

out_rcpppar <- MandelbrotRcppPar(x, y, max_iter = max.iter)

# Check results correctness
all.equal(out_parapply, out_rcpppar)

# Compare 1 thread with 'processes' threads
print(microbenchmark::microbenchmark(
  par_apply = parApply(cl,
           X = points,
           MARGIN = c(1, 2),
           FUN = mandelbrot,
           max.iter = max.iter),
  rcpp_parallel_1 = {
    RcppParallel::setThreadOptions(numThreads = 1)
    MandelbrotRcppPar(x, y, max.iter)
  },
  rcpp_parallel = {
    RcppParallel::setThreadOptions(numThreads = processes)
    MandelbrotRcppPar(x, y, max.iter)
  },
  times = 5
))

stopCluster(cl)