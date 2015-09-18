context("Test shortest_path")

test_that("Check arg lists", {
  expect_equal(names(formals(shortest_path)), c("g","v1","v2"))
})


test_that("Bad graphs", {
  bad_g1 = list(A = list())
  bad_g2 = list(A = list(edges = 1L))
  bad_g3 = list(A = list(weights = 1))

  expect_error(shortest_path(bad_g1,"A","A"))
  expect_error(shortest_path(bad_g2,"A","A"))
  expect_error(shortest_path(bad_g3,"A","A"))

  expect_error(shortest_path(1,"A","A"))
  expect_error(shortest_path(1L,"A","A"))
  expect_error(shortest_path("1","A","A"))
  expect_error(shortest_path(TRUE,"A","A"))
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


  expect_identical(shortest_path(g2,"A","A"), c("A","A"))
  expect_identical(shortest_path(g2,"B","B"), c("B","B"))
  
  expect_identical(shortest_path(g3,"A","A"), c("A","A"))
  expect_identical(shortest_path(g3,"B","A"), c("B","A"))
  expect_identical(shortest_path(g3,"B","C"), c("B","C"))
  expect_identical(shortest_path(g3,"C","C"), c("C","C"))


  is_empty_atomic = function(x) is.atomic(x) & length(x) == 0

  expect_true(is_empty_atomic(shortest_path(g1,"A","A")))
  expect_true(is_empty_atomic(shortest_path(g2,"A","B")))
  expect_true(is_empty_atomic(shortest_path(g2,"B","A")))
  expect_true(is_empty_atomic(shortest_path(g3,"A","B")))
  expect_true(is_empty_atomic(shortest_path(g3,"B","B")))
  expect_true(is_empty_atomic(shortest_path(g3,"C","B")))
})



test_that("Vertex labels", {
  g1 = list(A = list(edges   = c(1L),
                     weights = c(1)))

  g2 = list(A = list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
            B = list(edges   = c(1L, 2L),
                     weights = c(1, 1)))

  # Good labels
  expect_identical(shortest_path(g1,"A","A"), c("A","A"))
  expect_identical(shortest_path(g2,"A","B"), c("A","B"))

  # Bad labels

  expect_error(shortest_path(g1,1,1))
  expect_error(shortest_path(g1,1,1L))
  expect_error(shortest_path(g1,1L,1))
  expect_error(shortest_path(g1,1L,1L))
  expect_error(shortest_path(g1,"A",1))
  expect_error(shortest_path(g1,1,"A"))
  expect_error(shortest_path(g1,"A","C"))
  expect_error(shortest_path(g1,1,TRUE))
  expect_error(shortest_path(g1,"A",TRUE))

  expect_error(shortest_path(g2,1,2))
  expect_error(shortest_path(g2,"A",2))
  expect_error(shortest_path(g2,1,"B"))

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


  expect_identical(shortest_path(g1,"A","A"), c("A","A"))

  expect_identical(shortest_path(g2,"A","A"), c("A","A"))
  expect_identical(shortest_path(g2,"A","B"), c("A","B"))
  expect_identical(shortest_path(g2,"B","A"), c("B","A"))
  expect_identical(shortest_path(g2,"B","B"), c("B","B"))

  expect_identical(shortest_path(g3,"A","A"), c("A","A"))
  expect_identical(shortest_path(g3,"A","B"), c("A","B"))
  expect_identical(shortest_path(g3,"B","A"), c("B","A"))
  expect_identical(shortest_path(g3,"B","C"), c("B","C"))
  expect_identical(shortest_path(g3,"C","B"), c("C","B"))
  expect_identical(shortest_path(g3,"C","C"), c("C","C"))

  expect_identical(shortest_path(g4,"A","B"), c("A","B"))
  expect_identical(shortest_path(g4,"A","C"), c("A","B","C"))
  expect_identical(shortest_path(g4,"A","D"), c("A","B","C","D"))
  expect_identical(shortest_path(g4,"A","E"), c("A","B","C","D","E"))
  expect_identical(shortest_path(g4,"A","F"), c("A","B","C","D","E","F"))

  is_empty_atomic = function(x) is.atomic(x) & length(x) == 0

  expect_true(is_empty_atomic(shortest_path(g4,"F","A")))
  expect_true(is_empty_atomic(shortest_path(g4,"F","B")))
  expect_true(is_empty_atomic(shortest_path(g4,"F","C")))
  expect_true(is_empty_atomic(shortest_path(g4,"F","D")))
  expect_true(is_empty_atomic(shortest_path(g4,"F","E")))
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

