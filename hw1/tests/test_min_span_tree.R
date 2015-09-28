if (file.exists("extra_credit.R"))
  source("extra_credit.R")

context("Test min_span_tree")


test_that("Check arg lists", {
  if (!exists("min_span_tree"))
    skip("Not implemented")

  expect_equal(names(formals(min_span_tree)), c("g"))
})


test_that("Small valid graphs", {
  if (!exists("min_span_tree"))
    skip("Not implemented")

  g1 = list(A = list(edges   = integer(),
                     weights = numeric()))

  g2 = list(A = list(edges   = c(1L),
                     weights = c(1 )))

  g3 = list(A = list(edges   = c(2L),
                     weights = c(1 )),
            B = list(edges   = c(1L),
                     weights = c(1 )))

  g4 = list(A = list(edges   = c(1L,2L),
                     weights = c(1 ,1 )),
            B = list(edges   = c(1L,2L),
                     weights = c(1 ,1 )))

  mst_graph_len = function(g)
  {
    sum(unlist(lapply(g, function(x) x$weights)))/2
  }

  expect_equal(mst_graph_len(min_span_tree(g1)), 0)
  expect_equal(mst_graph_len(min_span_tree(g2)), 0)
  expect_equal(mst_graph_len(min_span_tree(g3)), 1)
  expect_equal(mst_graph_len(min_span_tree(g4)), 1)
})

test_that("Med valid graph", {
  if (!exists("min_span_tree"))
    skip("Not implemented")

  # See http://en.wikipedia.org/wiki/Minimum_spanning_tree#mediaviewer/File:Multiple_minimum_spanning_trees.svg
  g = list(A = list(edges   = c(2L,4L,5L),
                    weights = c(1 ,4 ,3 )),
           B = list(edges   = c(1L,4L,5L),
                    weights = c(1 ,4 ,2 )),
           C = list(edges   = c(5L,6L),
                    weights = c(4 ,5 )),
           D = list(edges   = c(1L,2L,5L),
                    weights = c(4 ,4 ,4 )),
           E = list(edges   = c(1L,2L,3L,4L,6L),
                    weights = c(3 ,2 ,4 ,4 ,7 )),
           F = list(edges   = c(3L,5L),
                    weights = c(5 ,7 )))

  # Adj matrix
  #
  # 0 1 0 4 3 0
  # 1 0 0 4 2 0
  # 0 0 0 0 4 5
  # 4 4 0 0 4 0
  # 3 2 4 4 0 7
  # 0 0 5 0 7 0


  r = min_span_tree(g)
  len = sum(unlist(lapply(r, function(x) x$weights)))/2

  expect_true(is_undirected(r))
  expect_equal(len, 16)
})

test_that("Invalid graph", {
  if (!exists("min_span_tree"))
    skip("Not implemented")

  bad_g1 = list(A = list())
  bad_g2 = list(A = list(edges = 1L))
  bad_g3 = list(A = list(weights = 1))
  bad_g4 = list(list(edges = 1L,
                     weights = 1))

  expect_error(min_span_tree(bad_g1))
  expect_error(min_span_tree(bad_g2))
  expect_error(min_span_tree(bad_g3))
  expect_error(min_span_tree(bad_g4))
})
