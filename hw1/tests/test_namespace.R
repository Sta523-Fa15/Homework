context("Test namespace")


test_that("Check namespace", {

  funcs = c("is_connected", "is_isomorphic", "is_undirected", "is_valid", 
            "min_span_tree", "read_graph", "shortest_path", "write_graph")  

  # namespace should be clean
  expect_true(all(funcs %in% ls(envir=globalenv())))
  expect_true(all(ls(envir=globalenv()) %in% funcs))
})


test_that("Check arg lists", {
  # check arg length
  expect_equal(length(formals(is_connected)) , 3)
  expect_equal(length(formals(is_isomorphic)), 2)
  expect_equal(length(formals(is_undirected)), 1)
  expect_equal(length(formals(is_valid))     , 1)
  expect_equal(length(formals(min_span_tree)), 1)
  expect_equal(length(formals(read_graph))   , 1)
  expect_equal(length(formals(shortest_path)), 3)
  expect_equal(length(formals(write_graph))  , 2)
})

