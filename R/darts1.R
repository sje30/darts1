
##' Simulate throwing N darts at a dartboard
##'
##' The x and y locations are drawn independently from a uniform 
##' distribution -1 to +1.
##' @title Simulate throwing darts at a board.
##' @param n The number of darts to thrown
##' @return A nx2 matrix with each row containing (x,y) location of dart.
##' @author Stephen Eglen
##' @importFrom graphics points symbols
##' @importFrom stats runif
##' @export
throw_n_darts <- function(n) {
  ## Throw N darts onto the board.  Return as Nx2 matrix with x, y location.
  x <- runif(n, -1, +1)
  y <- runif(n, -1, +1)
  cbind(x, y)
}

##' Did the darts land within the dartboard?
##'
##' Return TRUE/FALSE for each element of the darts
##' @title Test whether each dart landed within the dartboard
##' @param darts nx2 matrix containing dart locations
##' @return Boolean vector of length n stating whether each dart landed in dartboard.
##' @author Stephen Eglen
##' @export
inside <- function(darts) {
  ## return true if the darts hit inside the circle of the dartboard.
  r <- sqrt( darts[,1]^2 + darts[,2]^2)
  r < 1
}

##' Plot the dartboard with the darts showing which are inside/outside.
##'
##' 
##' @title Plot the dartboard and darts
##' @param darts nx2 matrix containing dart locations
##' @param inside Vector of length n stating whether each dart landed in dartboard.
##' @return Nothing
##' @author Stephen Eglen
##' @export
plot_dartboard <- function(darts, inside) {
  plot(NA, xlim=c(-1, 1), ylim=c(-1, 1), asp=1)
  points(darts[,1], darts[,2], pch=19,  col=ifelse(inside, 'green', 'orangered'))
  symbols(x=0, y=0, circles=1, inches=FALSE, add=TRUE)
}
