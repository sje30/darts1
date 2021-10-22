throw_n_darts <- function(n) {
  ## Throw N darts onto the board.  Return as Nx2 matrix with x, y location.
  x <- runif(n, -1, +1)
  y <- runif(n, -1, +1)
  cbind(x, y)
}

inside <- function(darts) {
  ## return true if the darts hit inside the circle of the dartboard.
  r <- sqrt( darts[,1]^2 + darts[,2]^2)
  r < 1
}


plot_dartboard <- function(darts, inside) {
  plot(NA, xlim=c(-1, 1), ylim=c(-1, 1), asp=1)
  points(darts[,1], darts[,2], pch=19,  col=ifelse(inside, 'green', 'orangered'))
  symbols(x=0, y=0, circles=1, inches=FALSE, add=TRUE)
}
