context("Test is_isomorphic")


test_that("Check arg lists", {
  expect_equal(names(formals(is_isomorphic)), c("g1","g2"))
})


test_that("Invalid graphs", {
  g = list(A = list(edges   = 1L,
                    weights = 1 ))

  #bad_g1 = list(A = list())
  #bad_g2 = list(A = list(edges = 1L))
  bad_g3 = list(A = list(weights = 1))
  bad_g4 = list(list(weights = 1, edges = 1L))

  #expect_error(is_isomorphic(g,bad_g1))
  #expect_error(is_isomorphic(g,bad_g2))
  expect_error(is_isomorphic(g,bad_g3))
  expect_error(is_isomorphic(g,bad_g4))

  #expect_error(is_isomorphic(bad_g1,g))
  #expect_error(is_isomorphic(bad_g2,g))
  expect_error(is_isomorphic(bad_g3,g))
  expect_error(is_isomorphic(bad_g4,g))


  #expect_error(is_isomorphic(bad_g1,bad_g1))
  #expect_error(is_isomorphic(bad_g2,bad_g2))
  expect_error(is_isomorphic(bad_g3,bad_g3))
  expect_error(is_isomorphic(bad_g4,bad_g4))
})

test_that("Valid graphs", {
  g1 = list(A = list(edges   = integer(),
                     weights = numeric()))

  g2 = list(A = list(edges   = 1L,
                     weights = 1 ))

  g3 = list(A = list(edges   = 2L,
                     weights = 1 ),
            B = list(edges   = 1L,
                     weights = 1 ))

  g4 = list(B = list(edges   = 2L,
                     weights = 1 ),
            A = list(edges   = 1L,
                     weights = 1 ))

  g5 = list(A = list(edges   = c(1L,2L),
                     weights = c(1 ,1 )),
            B = list(edges   = 1L,
                     weights = 1 ))

  g6 = list(B = list(edges   = 2L,
                     weights = 1 ),
            A = list(edges   = c(1L,2L),
                     weights = c(1, 1 )))

  expect_true(is_isomorphic(g1,g1))
  expect_true(is_isomorphic(g2,g2))
  expect_true(is_isomorphic(g3,g3))
  expect_true(is_isomorphic(g4,g4))
  expect_true(is_isomorphic(g5,g5))
  expect_true(is_isomorphic(g6,g6))

  expect_true(is_isomorphic(g3,g4))
  expect_true(is_isomorphic(g4,g3))
  expect_true(is_isomorphic(g5,g6))
  expect_true(is_isomorphic(g6,g5))

  expect_false(is_isomorphic(g1,g2))
  expect_false(is_isomorphic(g3,g5))
  expect_false(is_isomorphic(g3,g6))
  expect_false(is_isomorphic(g4,g5))
  expect_false(is_isomorphic(g4,g6))

  g3[[1]]$weights = 2
  expect_true( is_isomorphic(g3,g3))
  expect_false(is_isomorphic(g3,g4))
  expect_false(is_isomorphic(g4,g3))
})

test_that("Edge ordering", {

  g1 = list(A = list(edges   = c(1L,2L),
                     weights = c(1, 2 )),
            B = list(edges   = 1L,
                     weights = 1 ))

  g2 = list(A = list(edges   = c(2L,1L),
                     weights = c(2, 1 )),
            B = list(edges   = 1L,
                     weights = 1 ))

  g3 = list(B = list(edges   = 2L,
                     weights = 1 ),
            A = list(edges   = c(1L,2L),
                     weights = c(2, 1 )))

  g4 = list(B = list(edges   = 2L,
                     weights = 1 ),
            A = list(edges   = c(2L,1L),
                     weights = c(1, 2 )))

  bad_g1 = list(A = list(edges   = c(1L,2L),
                         weights = c(2, 1 )),
                B = list(edges   = 1L,
                         weights = 1 ))

  bad_g2 = list(B = list(edges   = 2L,
                         weights = 1 ),
                A = list(edges   = c(1L,2L),
                         weights = c(1, 2 )))


  expect_true(is_isomorphic(g1,g2))
  expect_true(is_isomorphic(g1,g3))
  expect_true(is_isomorphic(g1,g4))
  expect_true(is_isomorphic(g2,g3))
  expect_true(is_isomorphic(g2,g4))
  expect_true(is_isomorphic(g3,g4))

  expect_false(is_isomorphic(g1,bad_g1))
  expect_false(is_isomorphic(g2,bad_g1))
  expect_false(is_isomorphic(g3,bad_g1))
  expect_false(is_isomorphic(g4,bad_g1))

  expect_false(is_isomorphic(g1,bad_g2))
  expect_false(is_isomorphic(g2,bad_g2))
  expect_false(is_isomorphic(g3,bad_g2))
  expect_false(is_isomorphic(g4,bad_g2))

  expect_true(is_isomorphic(bad_g1,bad_g2))
})

test_that("Subsets", {
  g1 = list(A = list(edges   = integer(),
                     weights = numeric()  ))

  g2 = list(A = list(edges   = integer(),
                     weights = numeric()  ),
            B = list(edges   = 1L,
                     weights = 1  ))

  expect_false(is_isomorphic(g1,g2))
  expect_false(is_isomorphic(g2,g1))
})
