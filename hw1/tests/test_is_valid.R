context("Test is_valid")


test_that("Check arg lists", {
  expect_equal(names(formals(is_valid)), c("g"))
})


test_that("valid graphs", {

  g1 = list(A = list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
            B = list(edges   = c(1L, 2L),
                     weights = c(1, 1)))

  g2 = list(A = list(edges   = c(2L),
                     weights = c(1)),
            B = list(edges   = c(1L),
                     weights = c(1)))

  g3 = list(A = list(edges   = integer(),
                     weights = numeric()))

  g4 = list(A = list(edges   = c(1L),
                     weights = c(1)))

  g5 = list(B = list(weights = c(1),
                     edges   = c(1L)))

  g6 = list(A = list(edges   = c(2L),
                     weights = c(1)),
            B = list(edges   = integer(),
                     weights = numeric()))

  g7 = list(A = list(edges   = c(1L,2L),
                     weights = c(1,1)),
            B = list(edges   = c(2L),
                     weights = c(1)))

  expect_true(is_valid(g1))
  expect_true(is_valid(g2))
  expect_true(is_valid(g3))
  expect_true(is_valid(g4))
  expect_true(is_valid(g5))
  expect_true(is_valid(g6))
  expect_true(is_valid(g7))
})

test_that("Missing labels", {
  bad_g1 = list(list(edges   = c(1L, 2L),
                     weights = c(1, 1)))
  bad_g2 = list(list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
                list(edges   = c(1L, 2L),
                     weights = c(1, 1)))

  expect_false(is_valid(bad_g1))
  expect_false(is_valid(bad_g2))
})

test_that("Bad structure", {
  bad_g1 = list(list())
  bad_g2 = list(list(edges = 1L))
  bad_g3 = list(list(weights = 1))


  expect_false(is_valid(bad_g1))
  expect_false(is_valid(bad_g2))
  expect_false(is_valid(bad_g3))
})


test_that("Invalid vertex reference", {
  bad_g1 = list(A = list(edges   = c(1L, 2L),
                         weights = c(1, 1)))
  bad_g2 = list(A = list(edges   = c(1L, 2L),
                         weights = c(1, 1)),
                B = list(edges   = c(1L, 3L),
                         weights = c(1, 1)))

  expect_false(is_valid(bad_g1))
  expect_false(is_valid(bad_g2))
})


test_that("Duplicate vertex labels", {
  bad_g = list(A = list(edges   = c(1L, 2L),
                        weights = c(1, 1)),
               A = list(edges   = c(1L, 2L),
                        weights = c(1, 1)))

  expect_false(is_valid(bad_g))
})


test_that("Edge type", {
  bad_g1 = list(A = list(edges   = c(1),
                         weights = c(1)))
  bad_g2 = list(A = list(edges   = c("A"),
                         weights = c(1)))
  bad_g3 = list(A = list(edges   = c(NA+1L),
                         weights = c(1)))

  expect_false(is_valid(bad_g1))
  expect_false(is_valid(bad_g2))
  expect_false(is_valid(bad_g3))
})


test_that("Weight type and value", {
  bad_g1 = list(A = list(edges   = c(1L),
                         weights = c(-1)))
  bad_g2 = list(A = list(edges   = c(1L),
                         weights = c(0)))
  bad_g3 = list(A = list(edges   = c(1L),
                         weights = c(NA+1)))

  expect_false(is_valid(bad_g1))
  expect_false(is_valid(bad_g2))
  expect_false(is_valid(bad_g3))
})


test_that("Duplicated edges", {
  bad_g = list(A = list(edges   = c(1L, 1L),
                        weights = c(1, 1)))

  expect_false(is_valid(bad_g))
})


test_that("Edge and weight length mismatch", {
  bad_g1 = list(A = list(edges   = c(1L),
                         weights = c(1, 1)))
  bad_g2 = list(A = list(edges   = c(1L, 2L),
                         weights = c(1)))
  bad_g3 = list(A = list(edges   = c(1L, 2L),
                         weights = c(1, 1)),
                B = list(edges   = c(1L, 2L),
                         weights = c(1, 1, 1)))
  bad_g4 = list(A = list(edges   = c(1L, 2L),
                         weights = c(1, 1)),
                B = list(edges   = c(1L, 2L, 3L),
                         weights = c(1, 1)))

  expect_false(is_valid(bad_g1))
  expect_false(is_valid(bad_g2))
  expect_false(is_valid(bad_g3))
  expect_false(is_valid(bad_g4))
})
