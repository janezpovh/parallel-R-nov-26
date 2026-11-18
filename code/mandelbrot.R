# Set arguments ----

resolution <-
  as.integer(1000) # Resolution of output image. This increases the exponentially x^2.
max_iter <- as.integer(100)
cmin <- complex(real = -1.5, imaginary = -1)
cmax <- complex(real = 0.5, imaginary = 1)

# Pixel coordinates (real and imaginary part of c), used for the plot axes.
# Same formula as inside the functions below.
dc <- cmax - cmin
x <- Re(cmin) + ((1:resolution - 1) / resolution * Re(dc))
y <- Im(cmin) + ((1:resolution - 1) / resolution * Im(dc))


# Defining functions ----

# Escape time: number of iterations n of z = z^2 + c (starting at z = 0)
# until |z| > 2; max_iter if the point does not escape (it is in the set)
mandelbrot <- function(c, max_iter = 100) {
  z <- 0
  for (i in 1:max_iter) {
    z <- z ^ 2 + c
    if (abs(z) > 2) {
      return(i)
    }
  }
  return(max_iter)
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
plot_mandelbrot(out_for, x, y, max_iter)

# Inner-most for loop optimization in C ----

Rcpp::cppFunction(
"
 int Mandel(double real, double im,
           int max_iter = 100)
{
std::complex<double> c(real, im);
std::complex<double> z = 0;
  for (int i=1; i <= max_iter; i++){
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
 plot_mandelbrot(out_for_c, x, y, max_iter)

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
      double fx = static_cast<double>(i) / resolution * real(dc);
      double fy = static_cast<double>(j) / resolution * imag(dc);
      std::complex<double> c(real(cmin) + fx, imag(cmin) + fy);
      std::complex<double> z = 0;
      int iteration = max_iter;   // value for points in the set
      for (int k = 1; k <= max_iter; k++)
      {
        z = z*z + c;
        if (std::abs(z) > 2) {
          iteration = k;
          break;
        }
      }
      out(i, j) = iteration;
    }
  }
  return out;
}
"
)

# Compute results
out_c <- Mandelbrot(resolution = resolution,
                    max_iter = max_iter,
                    cmin = cmin,
                    cmax = cmax)

# Visualise
 plot_mandelbrot(out_c, x, y, max_iter)

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
