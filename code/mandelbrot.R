# Set arguments ----

resolution <-
  as.integer(1000) # Resolution of output image. This increases the exponentially x^2.
max_iter <- as.integer(100)
cmin <- complex(real = -1.5, imaginary = -1)
cmax <- complex(real = 0.5, imaginary = 1)


# Defining functions ----

mandelbrot <- function(c, max_iter = 100) {
  z <- c
  for (i in 1:max_iter-1) {
    z <- z ^ 2 + c
    if (abs(z) > 2) {
      return(i)
    }
  }
  return(max_iter)
}

# For loop function definition

for_loop <- function(resolution,
                     max_iter = 100,
                     cmin = complex(real = -1.5, imaginary = -1),
                     cmax = complex(real = 0.5, imaginary = 1)) {
  # Compute initial points at each pixel
  dc <- cmax - cmin
  x <- y <- 1:resolution-1
  x <- Re(cmin) + (x / resolution * Re(dc))
  y <- Im(cmin) + (y / resolution * Im(dc))
  points <-
    outer(x, y, function(x, y)
      complex(real = x, imaginary = y))
  # Pre-allocate matrix
  result_for <- matrix(NA,
                       nrow = dim(points)[1],
                       ncol = dim(points)[2])
  # Compute divergence everything in for loop
  for (x in 1:dim(points)[1]) {
    for (y in 1:dim(points)[2]) {
      result_for[x, y] <- mandelbrot(points[x, y],
                                     max_iter)
    }
  }
  return(result_for)
}

# Compute results ----
out_for <- for_loop(resolution = resolution,
                    max_iter = max_iter,
                    cmin = cmin,
                    cmax = cmax)

# Visualise ----
plotly::plot_ly(z = t(out_for),
                type = "heatmap")

# Inner-most for loop optimization in C ----

Rcpp::cppFunction(
"
 int Mandel(double real, double im,
           int max_iter = 100)
{
std::complex<double> c(real, im);
std::complex<double> z = c;
  for (int i=0; i< max_iter; i++){
    z = z * z + c;
    if (std::abs(z) > 2) {
      return i;
    }
  }
 return max_iter;
}
"
)

for_loop_c <- function(resolution,
                       max_iter = 100,
                       cmin = complex(real = -1.5, imaginary = -1),
                       cmax = complex(real = 0.5, imaginary = 1)) {
  # Compute initial points at each pixel
  dc <- cmax - cmin
  x <- y <- 1:resolution-1
  x <- Re(cmin) + (x / resolution * Re(dc))
  y <- Im(cmin) + (y / resolution * Im(dc))
  points <-
    outer(x, y, function(x, y)
      complex(real = x, imaginary = y))
  # Pre-allocate matrix
  result_for <- matrix(NA,
                       nrow = dim(points)[1],
                       ncol = dim(points)[2])
  # Compute divergence everything in for loop
  for (x in 1:dim(points)[1]) {
    for (y in 1:dim(points)[2]) {
      c <- points[x, y]
      result_for[x, y] <- Mandel(real = Re(c),
                                 im = Im(c),
                                 max_iter)
    }
  }
  return(result_for)
}

# Compute results
out_for_c <- for_loop_c(resolution = resolution,
                        max_iter = max_iter,
                        cmin = cmin,
                        cmax = cmax)

# Visualise
 plotly::plot_ly(z = t(out_for_c),
                 type = "heatmap")

# Check results correctness
all.equal(out_for,
          out_for_c)

# Complete C solution ----

Rcpp::cppFunction(
"
IntegerMatrix Mandelbrot(
  int resolution,
  int max_iter,
  std::complex<double> cmin,
  std::complex<double> cmax )
{
  std::complex<double> dc = cmax - cmin;
  IntegerMatrix out( resolution );
  for (int i=0; i < resolution; i++){
    for(int j=0; j < resolution; j++){
      double helper = static_cast<double>(i);
      double helper2 = static_cast<double>(j);
      double fx = helper / resolution * real(dc);
      double fy = helper2 / resolution * imag(dc);
      std::complex<double> c(real(cmin) + fx, imag(cmin) + fy);
      std::complex<double> z = c;
      int iteration = -1;
      while(abs(z) < 2 && iteration < max_iter)
      {
        z = z*z + c;
        iteration++;
      }
      out(i, j) = iteration;
    }
  }
  return out;
}
"
)

# Compute results
out_c <- Mandelbrot(resolution = 1000,
                    max_iter = 100,
                    cmin = cmin,
                    cmax = cmax)

# Visualise
 plotly::plot_ly(z = t(out_c),
                 type = "heatmap")

# Check results correctness
all.equal(out_for,
          out_c)

# Benchmark ----

microbenchmark::microbenchmark(
  for_loop = for_loop(resolution = resolution,
                      max_iter = max_iter,
                      cmin = cmin,
                      cmax = cmax),
  for_c = for_loop_c(resolution = resolution,
                      max_iter = max_iter,
                      cmin = cmin,
                      cmax = cmax),
  `c` =  Mandelbrot(resolution = resolution,
                    max_iter = max_iter,
                    cmin = cmin,
                    cmax = cmax),
  times = 2
)
