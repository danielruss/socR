test_that("can crosswalk", {
  test_xw <- xwalk(tibble::tibble( oldcodes=c("x1","x1","x2"),old_title=c("old code 1","old code 1","old code 2"),
                                   newcodes=c("n1","n2","n3"),new_title=c("new code 1","new code 2","new code 3")
                                   ))
  expect_equal(nrow(test_xw$data),3)
  expect_equal( crosswalk(c("x1","x2"),test_xw),list(x1=c("n1","n2"),x2="n3"))
  expect_equal( crosswalk(c("n1","n2","n3"),invert=TRUE,test_xw),list(n1="x1",n2="x1",n3="x2"))
})
