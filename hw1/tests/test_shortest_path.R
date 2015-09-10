context("Test shortest_path")

test_that("Bad graphs", {
  bad_g1 = list(list())
  bad_g2 = list(list(edges = 1L))
  bad_g3 = list(list(weights = 1))

  expect_error(shortest_path(bad_g1,1,1))
  expect_error(shortest_path(bad_g2,1,1))
  expect_error(shortest_path(bad_g3,1,1))

  expect_error(shortest_path(1,1,1))
  expect_error(shortest_path(1L,1,1))
  expect_error(shortest_path("1",1,1))
  expect_error(shortest_path(TRUE,1,1))
})

test_that("Unconnected graphs", {
  g1 = list(A = list(edges   = integer(),
                     weights = numeric()))

  g2 = list(A = list(edges   = c(1L),
                     weights = c(1)),
            B = list(edges   = c(2L),
                     weights = c(1)))

  g3 = list(A = list(edges   = c(1L),
                     weights = c(1)),
            B = list(edges   = c(1L, 3L),
                     weights = c(1, 1)),
            C = list(edges   = c(3L),
                     weights = c(1)))

  is_empty_atomic = function(x) is.atomic(x) & length(x) == 0

  expect_identical(shortest_path(g2,1,1), c("A","A"))
  expect_identical(shortest_path(g2,2,2), c("B","B"))
  
  expect_identical(shortest_path(g3,1,1), c("A","A"))
  expect_identical(shortest_path(g3,2,1), c("B","A"))
  expect_identical(shortest_path(g3,2,3), c("B","C"))
  expect_identical(shortest_path(g3,3,3), c("C","C"))

  expect_true(is_empty_atomic(shortest_path(g1,1,1)))

  expect_true(is_empty_atomic(shortest_path(g2,1,2)))
  expect_true(is_empty_atomic(shortest_path(g2,2,1)))

  expect_true(is_empty_atomic(shortest_path(g3,1,2)))
  expect_true(is_empty_atomic(shortest_path(g3,2,2)))
  expect_true(is_empty_atomic(shortest_path(g3,3,2)))
})



test_that("Vertex labels", {
  g1 = list(A = list(edges   = c(1L),
                     weights = c(1)))

  g2 = list(A = list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
            B = list(edges   = c(1L, 2L),
                     weights = c(1, 1)))

  # Good labels
  expect_identical(shortest_path(g1,1,1),     c("A","A"))
  expect_identical(shortest_path(g1,"A","A"), c("A","A"))
  expect_identical(shortest_path(g1,"A",1),   c("A","A"))
  expect_identical(shortest_path(g1,1,"A"),   c("A","A"))

  expect_identical(shortest_path(g2,1,2),     c("A","B"))
  expect_identical(shortest_path(g2,"A","B"), c("A","B"))
  expect_identical(shortest_path(g2,"A",2),   c("A","B"))
  expect_identical(shortest_path(g2,1,"B"),   c("A","B"))

  expect_identical(shortest_path(g1,1,1L),  c("A","A"))
  expect_identical(shortest_path(g1,1L,1),  c("A","A"))
  expect_identical(shortest_path(g1,1L,1L), c("A","A"))

  # Bad labels

  expect_error(shortest_path(g1,1,3))
  expect_error(shortest_path(g1,"A",3))
  expect_error(shortest_path(g1,1,"C"))
  expect_error(shortest_path(g1,"A","C"))

  expect_error(shortest_path(g1,1,TRUE))
  expect_error(shortest_path(g1,"A",TRUE))

  expect_error(shortest_path(g1,1,NaN))
  expect_error(shortest_path(g1,"A",NaN))

  expect_error(shortest_path(g1,1,NA_real_))
  expect_error(shortest_path(g1,"A",NA_real_))
  expect_error(shortest_path(g1,1,NA_integer_))
  expect_error(shortest_path(g1,"A",NA_integer_))
  expect_error(shortest_path(g1,1,NA_character_))
  expect_error(shortest_path(g1,"A",NA_character_))
  expect_error(shortest_path(g1,1,NA_complex_))
  expect_error(shortest_path(g1,"A",NA_complex_))
})


test_that("Connected graphs", {
  g1 = list(A = list(edges   = c(1L),
                     weights = c(1)))

  g2 = list(A = list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
            B = list(edges   = c(1L, 2L),
                     weights = c(1, 1)))

  g3 = list(A = list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
            B = list(edges   = c(1L, 3L),
                     weights = c(1, 1)),
            C = list(edges   = c(2L, 3L),
                     weights = c(1, 1)))
  
  g4 = list(A = list(edges   = c(2L),
                     weights = c(1)),
            B = list(edges   = c(3L),
                     weights = c(1)),
            C = list(edges   = c(4L),
                     weights = c(1)),
            D = list(edges   = c(5L),
                     weights = c(1)),
            E = list(edges   = c(6L),
                     weights = c(1)),
            F = list(edges   = integer(),
                     weights = numeric()))


  expect_identical(shortest_path(g1,1,1), c("A","A"))

  expect_identical(shortest_path(g2,1,1), c("A","A"))
  expect_identical(shortest_path(g2,1,2), c("A","B"))
  expect_identical(shortest_path(g2,2,1), c("B","A"))
  expect_identical(shortest_path(g2,2,2), c("B","B"))

  expect_identical(shortest_path(g3,1,1), c("A","A"))
  expect_identical(shortest_path(g3,1,2), c("A","B"))
  expect_identical(shortest_path(g3,2,1), c("B","A"))
  expect_identical(shortest_path(g3,2,3), c("B","C"))
  expect_identical(shortest_path(g3,3,2), c("C","B"))
  expect_identical(shortest_path(g3,3,3), c("C","C"))

  expect_identical(shortest_path(g4,1,2), c("A","B"))
  expect_identical(shortest_path(g4,1,3), c("A","B","C"))
  expect_identical(shortest_path(g4,1,4), c("A","B","C","D"))
  expect_identical(shortest_path(g4,1,5), c("A","B","C","D","E"))
  expect_identical(shortest_path(g4,1,6), c("A","B","C","D","E","F"))

  is_empty_atomic = function(x) is.atomic(x) & length(x) == 0

  expect_true(is_empty_atomic(shortest_path(g4,6,1)))
  expect_true(is_empty_atomic(shortest_path(g4,6,2)))
  expect_true(is_empty_atomic(shortest_path(g4,6,3)))
  expect_true(is_empty_atomic(shortest_path(g4,6,4)))
  expect_true(is_empty_atomic(shortest_path(g4,6,5)))
})




test_that("Med valid graph", {
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

  expect_identical(shortest_path(g,"B","F"),  c("B","E","F"))
  expect_identical(shortest_path(g,"D","F"),  c("D","E","F"))
  expect_identical(shortest_path(g,"C","D"),  c("C","E","D"))
})

