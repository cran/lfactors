mon <- lfactor(1:12,
               levels=1:12,
               labels=c("Jan", "Feb", "Mar", "Apr", "May","Jun",
                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

context("equal")
test_that("equal", {
  expect_equal(mon == "Feb", c(FALSE,TRUE,rep(FALSE,10)))
  expect_equal(mon == "Feb", mon==2)
  expect_equal(mon[3] == c("Jan", "Feb", "Mar"), mon[3] == 1:3)
  expect_equal(mon[1:2] == c("Feb", "Tuesday"), mon[1:2] == c(2,-4) )
})

context("not equal")
test_that("not equal", {
  expect_equal(mon != "Feb", mon != 2)						
  expect_equal(mon[3] == c("Jan", "Feb", "Mar"), mon[3] == 1:3)
})

context("in")
test_that("in", {
  skip_on_cran()
  expect_equal(mon %in% c(2, 3), mon %in% c("Feb", "Mar"))	
  expect_equal(c(-4, 14,3,10) %in% mon, c("not a month", "Third December","Mar","Oct") %in% mon)
})

context("GT and GTE")
test_that("GT and GTE", {
  expect_equal(mon >  3, c(rep(FALSE,3), rep(TRUE,9)))
  expect_equal(mon >= 3, c(rep(FALSE,2), rep(TRUE,10)))
  expect_warning(mon >= "Jan")
})

context("droplevels")
test_that("droplevels", {
  dl <- droplevels(x <- lfactor(c(1,3),levels=1:3, labels=LETTERS[1:3]))
  expect_is(dl, "lfactor") 
  expect_equal(attributes(dl)$levels, LETTERS[c(1,3)])
  expect_equal(attributes(dl)$llevels, c(1,3))
  dlprime <- lfactor(c(1,3),levels=c(1,3), labels=LETTERS[c(1,3)])
  expect_equal(dlprime, dl)
})

context("relevel")
test_that("relevel", {
  monp <- relevel(mon, "Jun")
  expect_equal(mon=="Jun", monp=="Jun")
  expect_equal(mon=="Jan", monp=="Jan")
  expect_equal(mon=="Feb", monp=="Feb")
  expect_equal(mon==6, monp==6)
  expect_equal(mon==1, monp==1)
  expect_equal(mon==2, monp==2)
})

context("set text")
test_that("set text", {
  monp <- mon
  monp[4] <- "Jun"
  expect_equal(monp=="Jun", monp==6)
  expect_equal(monp=="Jun", c("Jan", "Feb", "Mar", "Jun", "May","Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")=="Jun")
})

context("set num")
test_that("set num", {
  monp <- mon
  monp[4] <- 6
  expect_equal(monp=="Jun", monp==6)
  expect_equal(monp=="Jun", c("Jan", "Feb", "Mar", "Jun", "May","Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")=="Jun")
  monp <- mon
  monp[4:5] <- c(6,"Feb")
  expect_equal(monp=="Jun", monp==6)
  expect_equal(monp=="Feb", monp==2)
  expect_equal(monp=="Jun", c("Jan", "Feb", "Mar", "Jun", "Feb","Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")=="Jun")
  expect_equal(monp=="Feb", c("Jan", "Feb", "Mar", "Jun", "Feb","Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")=="Feb")

})

context("subset drop argument")
test_that("subset drop argument", {
  expect_equal(levels(mon[1:4,drop=TRUE]), c("Jan", "Feb", "Mar", "Apr"))
  expect_equal(levels(mon[1:4,drop=FALSE]), levels(mon))
})

context("subset with no i works")
test_that("subset with no i works", {
  monb <- mon[1:11]
  expect_equal(mon[1:11,drop=TRUE], monb[,drop=TRUE])
})
