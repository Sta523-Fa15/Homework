context("Test is_undirected")

test_that("Check arg lists", {
  expect_equal(names(formals(is_undirected)), c("g"))
})


test_that("Bad graphs", {
  bad_g1 = list(A = list())
  bad_g2 = list(A = list(edges = 1L))
  bad_g3 = list(A = list(weights = 1))
  bad_g4 = list(list(weights = 1, edges = 1L))

  expect_error(is_undirected(bad_g1))
  expect_error(is_undirected(bad_g2))
  expect_error(is_undirected(bad_g3))
  expect_error(is_undirected(bad_g4))
})


test_that("Undirected",{
  g0 = list(A = list(edges   = integer(),
                     weights = numeric()))

  g1 = list(A = list(edges   = c(1L),
                     weights = c(1)))

  g2 = list(A = list(edges   = integer(),
                     weights = numeric()),
            B = list(edges   = integer(),
                     weights = numeric()))

  g3 = list(A = list(edges   = c(2L),
                     weights = c(1)),
            B = list(edges   = c(1L),
                     weights = c(1)))

  g4 = list(A = list(edges   = c(1L),
                     weights = c(1)),
            B = list(edges   = c(2L),
                     weights = c(1)))

  g5 = list(A = list(edges   = c(1L,2L),
                     weights = c(1,1)),
            B = list(edges   = c(1L),
                     weights = c(1)))

  g6 = list(A = list(edges   = c(1L,2L),
                     weights = c(1,1)),
            B = list(edges   = c(1L,2L),
                     weights = c(1,1)))

  g7 = list(A = list(edges   = c(2L,4L),
                     weights = c(1,1)),
            B = list(edges   = c(1L),
                     weights = c(1)),
            C = list(edges   = c(4L),
                     weights = c(1)),
            D = list(edges   = c(1L,3L),
                     weights = c(1,1)))

  expect_true(is_undirected(g0))
  expect_true(is_undirected(g1))
  expect_true(is_undirected(g2))
  expect_true(is_undirected(g3))
  expect_true(is_undirected(g4))
  expect_true(is_undirected(g5))
  expect_true(is_undirected(g6))
  expect_true(is_undirected(g7))
})

test_that("Directed - Edges",{
  g1 = list(A = list(edges   = c(2L),
                     weights = c(1)),
            B = list(edges   = integer(),
                     weights = numeric()))

  g2 = list(A = list(edges   = c(1L,2L),
                     weights = c(1,1)),
            B = list(edges   = c(2L),
                     weights = c(1)))

  g3 = list(A = list(edges   = integer(),
                     weights = numeric()),
            B = list(edges   = 1L,
                     weights = 1))

  expect_false(is_undirected(g1))
  expect_false(is_undirected(g2))
  expect_false(is_undirected(g3))
})

test_that("Directed - Weights",{
  g1 = list(A = list(edges   = c(2L),
                     weights = c(1)),
            B = list(edges   = c(1L),
                     weights = c(2)))

  g2 = list(A = list(edges   = c(1L,2L),
                     weights = c(1,1)),
            B = list(edges   = c(1L,2L),
                     weights = c(2,1)))

  expect_false(is_undirected(g1))
  expect_false(is_undirected(g2))
})
