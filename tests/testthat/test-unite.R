test_that("unite pastes columns together & removes old col", {
  df <- tibble(x = "a", y = "b")
  out <- unite(df, z, x:y)
  expect_equal(names(out), "z")
  expect_equal(out$z, "a_b")
})

test_that("unite does not remove new col in case of name clash", {
  df <- tibble(x = "a", y = "b")
  out <- unite(df, x, x:y)
  expect_equal(names(out), "x")
  expect_equal(out$x, "a_b")
})

test_that("unite preserves grouping", {
  df <- tibble(g = 1, x = "a") %>% dplyr::group_by(g)
  rs <- df %>% unite(x, x)
  expect_equal(df, rs)
  expect_equal(class(df), class(rs))
  expect_equal(dplyr::group_vars(df), dplyr::group_vars(rs))
})

test_that("drops grouping when needed", {
  df <- tibble(g = 1, x = "a") %>% dplyr::group_by(g)
  rs <- df %>% unite(gx, g, x)
  expect_equal(rs$gx, "1_a")
  expect_equal(dplyr::group_vars(rs), character())
})

test_that("preserves row names of data.frames (#1454)", {
  df <- data.frame(x = c("1", "2"), y = c("3", "4"), row.names = c("a", "b"))
  expect_identical(row.names(unite(df, "xy", x, y)), c("a", "b"))
})

test_that("empty var spec uses all vars", {
  df <- tibble(x = "a", y = "b")
  expect_equal(unite(df, "z"), tibble(z = "a_b"))
})

test_that("can remove missing vars on request", {
  df <- expand_grid(x = c("a", NA), y = c("b", NA))
  out <- unite(df, "z", x:y, na.rm = TRUE)

  expect_equal(out$z, c("a_b", "a", "b", ""))
})

test_that("regardless of the type of the NA", {
  vec_unite <- function(df, vars) {
    unite(df, "out", any_of(vars), na.rm = TRUE)$out
  }

  df <- tibble(
    x = c("x", "y", "z"),
    lgl = NA,
    dbl = NA_real_,
    chr = NA_character_
  )

  expect_equal(vec_unite(df, c("x", "lgl")), c("x", "y", "z"))
  expect_equal(vec_unite(df, c("x", "dbl")), c("x", "y", "z"))
  expect_equal(vec_unite(df, c("x", "chr")), c("x", "y", "z"))
})

test_that("validates its inputs", {
  df <- tibble(x = "a", y = "b")

  expect_snapshot(error = TRUE, {
    unite(df)
    unite(df, "z", x:y, sep = 1)
    unite(df, "z", x:y, remove = 1)
    unite(df, "z", x:y, na.rm = 1)
  })
})

test_that("returns an empty string column for empty selections (#1548)", {
  # i.e. it returns the initial value that would be used in a reduction algorithm

  x <- tibble(
    x = c("x", "y", "z"),
    y = c(1, 2, 3)
  )

  out <- unite(x, "new", all_of(c()))

  expect_identical(names(out), c("x", "y", "new"))
  expect_identical(out$new, c("", "", ""))
})

test_that("works with 0 column data frames and empty selections (#1570)", {
  x <- tibble(.rows = 2L)

  # No `...` implies "unite all the columns"
  out <- unite(x, "new")
  expect_identical(names(out), "new")
  expect_identical(out$new, c("", ""))

  # Empty selection
  out <- unite(x, "new", all_of(names(x)))
  expect_identical(names(out), "new")
  expect_identical(out$new, c("", ""))
})
