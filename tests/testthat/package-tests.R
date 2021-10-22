context("darts")

expect_equal( dim(throw_n_darts(100)), c(100, 2))

context("inside")

expect_equal( mode( inside(throw_n_darts(100))), "logical")

