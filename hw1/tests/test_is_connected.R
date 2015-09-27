context("Test is_connected")


test_that("Check arg lists", {
  expect_equal(names(formals(is_connected)), c("g","v1","v2"))
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


  expect_true(is_connected(g1,"A","A"))

  expect_true(is_connected(g2,"A","A"))
  expect_true(is_connected(g2,"A","B"))
  expect_true(is_connected(g2,"B","A"))
  expect_true(is_connected(g2,"B","B"))

  expect_true(is_connected(g3,"A","A"))
  expect_true(is_connected(g3,"A","B"))
  expect_true(is_connected(g3,"B","A"))
  expect_true(is_connected(g3,"B","C"))
  expect_true(is_connected(g3,"C","B"))
  expect_true(is_connected(g3,"C","C"))


  expect_true(is_connected(g4,"A","B"))
  expect_true(is_connected(g4,"A","C"))
  expect_true(is_connected(g4,"A","D"))
  expect_true(is_connected(g4,"A","E"))
  expect_true(is_connected(g4,"A","F"))

  expect_false(is_connected(g4,"F","A"))
  expect_false(is_connected(g4,"F","B"))
  expect_false(is_connected(g4,"F","C"))
  expect_false(is_connected(g4,"F","D"))
  expect_false(is_connected(g4,"F","E"))
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


  expect_false(is_connected(g1,"A","A"))

  expect_true( is_connected(g2,"A","A"))
  expect_true( is_connected(g2,"B","B"))
  expect_false(is_connected(g2,"A","B"))
  expect_false(is_connected(g2,"B","A"))

  expect_true( is_connected(g3,"A","A"))
  expect_false(is_connected(g3,"A","B"))
  expect_true( is_connected(g3,"B","A"))
  expect_false(is_connected(g3,"B","B"))
  expect_true( is_connected(g3,"B","C"))
  expect_false(is_connected(g3,"C","B"))
  expect_true( is_connected(g3,"C","C"))
})

test_that("Vertex labels", {
  g1 = list(A = list(edges   = c(1L),
                     weights = c(1)))

  g2 = list(A = list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
            B = list(edges   = c(1L, 2L),
                     weights = c(1, 1)))

  expect_true(is_connected(g1,"A","A"))
  expect_error(is_connected(g1,1,1))
  expect_error(is_connected(g1,"A",1))
  expect_error(is_connected(g1,1,"A"))

  expect_true(is_connected(g2,"A","B"))
  expect_error(is_connected(g2,1,2))
  expect_error(is_connected(g2,"A",2))
  expect_error(is_connected(g2,1,"B"))

  expect_error(is_connected(g1,1,1L))
  expect_error(is_connected(g1,1L,1))
  expect_error(is_connected(g1,1L,1L))

  expect_error(is_connected(g1,1,3))
  expect_error(is_connected(g1,"A",3))
  expect_error(is_connected(g1,1,"C"))
  expect_error(is_connected(g1,"A","C"))
  
  #expect_error(is_connected(g1,c("A","A"),"C"))
  #expect_error(is_connected(g1,"A",c("C","C"))))
  #expect_error(is_connected(g1,c("A","A"),c("C","C")))

  expect_error(is_connected(g1,1,TRUE))
  expect_error(is_connected(g1,"A",TRUE))

  expect_error(is_connected(g1,1,NaN))
  expect_error(is_connected(g1,"A",NaN))

  expect_error(is_connected(g1,1,NA_real_))
  expect_error(is_connected(g1,"A",NA_real_))
  expect_error(is_connected(g1,1,NA_integer_))
  expect_error(is_connected(g1,"A",NA_integer_))
  expect_error(is_connected(g1,1,NA_character_))
  expect_error(is_connected(g1,"A",NA_character_))
  expect_error(is_connected(g1,1,NA_complex_))
  expect_error(is_connected(g1,"A",NA_complex_))
})


test_that("Bad graphs", {
  bad_g1 = list(A = list())
  bad_g2 = list(A = list(edges = 1L))
  bad_g3 = list(A = list(weights = 1))
  bad_g4 = list(list(weights = 1, edges = 1L))

  expect_error(is_connected(bad_g1,"A","A"))
  expect_error(is_connected(bad_g2,"A","A"))
  expect_error(is_connected(bad_g3,"A","A"))
  expect_error(is_connected(bad_g4,"A","A"))

  expect_error(is_connected(1,     "A","A"))
  expect_error(is_connected(1L,    "A","A"))
  expect_error(is_connected("1",   "A","A"))
  expect_error(is_connected(TRUE,  "A","A"))
  expect_error(is_connected(c(A=1),"A","A"))
})

