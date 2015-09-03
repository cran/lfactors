mon <- lfactor(1:12,
               levels=1:12,
               labels=c("Jan", "Feb", "Mar", "Apr", "May","Jun",
                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

test_that("equal", {
  expect_equal(mon == "Feb", c(FALSE,TRUE,rep(FALSE,10)))
  expect_equal(mon == "Feb", mon==2)
  expect_equal(mon[3] == c("Jan", "Feb", "Mar"), mon[3] == 1:3)
  expect_equal(mon[1:2] == c("Feb", "Tuesday"), mon[1:2] == c(2,-4) )
})

test_that("not equal", {
  expect_equal(mon != "Feb", mon != 2)						
  expect_equal(mon[3] == c("Jan", "Feb", "Mar"), mon[3] == 1:3)
})

test_that("in", {
  skip_on_cran()
  expect_equal(mon %in% c(2, 3), mon %in% c("Feb", "Mar"))	
  expect_equal(c(-4, 14,3,10) %in% mon, c("not a month", "Third December","Mar","Oct") %in% mon)
})

test_that("GTE", {
  expect_warning(mon >= "Jan")
})

test_that("droplevels", {
  dl <- droplevels(x <- lfactor(1:2,levels=1:3, labels=LETTERS[1:3]))
  expect_is(dl, "lfactor") 
  expect_equal(attributes(dl)$levels, LETTERS[1:2])
  expect_equal(attributes(dl)$llevels, 1:2)
})

test_that("relevel", {
  monp <- relevel(mon, "Jun")
  expect_equal(mon=="Jun", monp=="Jun")
  expect_equal(mon=="Jan", monp=="Jan")
  expect_equal(mon=="Feb", monp=="Feb")
  expect_equal(mon==6, monp==6)
  expect_equal(mon==1, monp==1)
  expect_equal(mon==2, monp==2)
})

test_that("set text", {
  monp <- mon
  monp[4] <- "Jun"
  expect_equal(monp=="Jun", monp==6)
  expect_equal(monp=="Jun", c("Jan", "Feb", "Mar", "Jun", "May","Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")=="Jun")
})

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

test_that("subset drop argument", {
  expect_equal(levels(mon[1:4,drop=TRUE]), c("Jan", "Feb", "Mar", "Apr"))
  expect_equal(levels(mon[1:4,drop=FALSE]), levels(mon))
})

test_that("subset with no i works", {
  monb <- mon[1:11]
  expect_equal(mon[1:11,drop=TRUE], monb[,drop=TRUE])
})
