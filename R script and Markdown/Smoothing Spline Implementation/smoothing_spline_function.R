my.smooth.spline <- function(x, y, lambda) {
  library(ggplot2)
  x.original <- x
  y.original <- y
  
  # sort points by x values
  o <- order(x)
  x <- x[o]
  y <- y[o]
  
  # remove duplicate x-values by averaging their respective y-values
  n <- length(x)
  c <- 0
  x.unique <- rep(NA, n)
  y.unique <- rep(NA, n)
  prev <- x[1]
  prev.count <- 1
  prev.sum <- y[1]
  for (i in 2:length(x)) {
    if (prev == x[i]) {
      prev.count <- prev.count + 1
      prev.sum <- prev.sum + y[i]
    } else {
      c <- c + 1
      x.unique[c] <- prev
      y.unique[c] <- prev.sum / prev.count
      prev <- x[i]
      prev.count <- 1
      prev.sum <- y[i]
    }
  }
  x <- x.unique[1:c]
  y <- y.unique[1:c]
  n <- length(x)
  
  # calculations for smoothing splines
  h = diff(x)
  
  delta = matrix(0, n - 2, n)
  for (i in 1:(n - 2)) {
    delta[i, i] <- 1 / h[i]
    delta[i, i + 1] <- 0 - (1 / h[i]) - (1 / h[i + 1])
    delta[i, i + 2] <- 1 / h[i + 1]
  }
  
  W = matrix(0, n - 2, n - 2)
  for (i in 1:(n - 2)) {
    W[i, i] = (h[i] + h[i + 1]) / 3
    W[i - 1, i] = h[i] / 6
    W[i, i - 1] = h[i] / 6
  }
  
  K = t(delta) %*% solve(W) %*% delta
  mu = solve(diag(n) + lambda * K) %*% y
  
  # plot results
  original.df <- data.frame(x.original, y.original)
  ss.df <- data.frame(x, mu)
  plt <-  ggplot(data=original.df, aes(x=x.original, y=y.original)) + geom_point() +
    geom_line(data=ss.df, aes(x=x, y=mu), color="red", linewidth = 1.5) + labs(x="", y="")
  return (list("SS_plot" = plt))
}

# example usage on mcycle dataset
library(MASS)
data("mcycle")

players <- read.csv("Data/NBA_players.csv")

#plot(players$MP_PER_G, players$PF_PER_G) #somewhat trend (check)

x = players$MP_PER_G
y = players$PF_PER_G
lambda = 0
result <- my.smooth.spline(x, y, lambda)
p.1 <- result$SS_plot
p.1
loess_fit <- myloess(x, y)
p.2 <- loess_fit$loessplot 
library(gridExtra)
grid.arrange(loess_fit$loessplot, result$SS_plot)

