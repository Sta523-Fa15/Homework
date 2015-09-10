context("Test is_undirected")

test_that("Bad graphs", {
  g1 = list(A = list(edges   = c(1L),
                     weights = c(1)))

  bad_g1 = list(list())
  bad_g2 = list(list(edges = 1L))
  bad_g3 = list(list(weights = 1))

  expect_error(is_undirected(g1,bad_g1))
  expect_error(is_undirected(g1,bad_g2))
  expect_error(is_undirected(g1,bad_g3))

  expect_error(is_undirected(bad_g1,g1))
  expect_error(is_undirected(bad_g2,g1))
  expect_error(is_undirected(bad_g3,g1))

  expect_error(is_undirected(bad_g1,bad_g1))
  expect_error(is_undirected(bad_g2,bad_g2))
  expect_error(is_undirected(bad_g3,bad_g3))
})


test_that("Undirected",{
  g0 = list(list(edges   = integer(),
                 weights = numeric()))

  g1 = list(list(edges   = c(1L),
                 weights = c(1)))

  g2 = list(list(edges   = integer(),
                 weights = numeric()),
            list(edges   = integer(),
                 weights = numeric()))

  g3 = list(list(edges   = c(2L),
                 weights = c(1)),
            list(edges   = c(1L),
                 weights = c(1)))
  
  g4 = list(list(edges   = c(1L),
                 weights = c(1)),
            list(edges   = c(2L),
                 weights = c(1)))

  g5 = list(list(edges   = c(1L,2L),
                 weights = c(1,1)),
            list(edges   = c(1L),
                 weights = c(1)))

  g6 = list(list(edges   = c(1L,2L),
                 weights = c(1,1)),
            list(edges   = c(1L,2L),
                 weights = c(1,1)))

  g7 = list(list(edges   = c(2L,4L),
                 weights = c(1,1)),
            list(edges   = c(1L),
                 weights = c(1)),
            list(edges   = c(4L),
                 weights = c(1)),
            list(edges   = c(1L,3L),
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
  g1 = list(list(edges   = c(2L),
                 weights = c(1)),
            list(edges   = integer(),
                 weights = numeric()))

  g2 = list(list(edges   = c(1L,2L),
                 weights = c(1,1)),
            list(edges   = c(2L),
                 weights = c(1)))

  expect_false(is_undirected(g1))
  expect_false(is_undirected(g2))
})

test_that("Directed - Weights",{
  g1 = list(list(edges   = c(2L),
                 weights = c(1)),
            list(edges   = c(1L),
                 weights = c(2)))

  g2 = list(list(edges   = c(1L,2L),
                 weights = c(1,1)),
            list(edges   = c(1L,2L),
                 weights = c(2,1)))

  expect_false(is_undirected(g1))
  expect_false(is_undirected(g2))   
})