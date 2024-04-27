
# Timers ------------------------------------------------------------------

test_that("get_values_tic_msg:correct output for all units", {
  sec_sleep <- 0.5
  tictoc::tic()
  tictoc::tic()
  tictoc::tic()

  Sys.sleep(sec_sleep)

  testthat::expect_true(
    abs(get_values_tic_msg(unit = 'sec') - sec_sleep) < 0.01,
    info = 'Seconds failed.')
  testthat::expect_true(
    abs(get_values_tic_msg(unit = 'min') - sec_sleep / 60) < 1e-2,
    info = 'Minutes failed.')
  testthat::expect_true(
    abs(get_values_tic_msg(unit = 'hour') - sec_sleep / 3600) < 1e-4,
    info = 'Hours failed.')
})

# Dates -------------------------------------------------------------------

test_that("stata_month2ym: correct output.", {
  # Test single Stata month value
  testthat::expect_equal(stata_month2ymd(0),
               lubridate::ymd("1960-01-01"))

  # Test multiple Stata month values
  testthat::expect_equal(stata_month2ymd(c(-1, 0, 12)),
                         lubridate::ymd(c("1959-12-01", "1960-01-01", "1961-01-01")))
})

# Dataframes --------------------------------------------------------------

test_that("crosstab: correct output.", {
  df <- data.frame(
    gear = c(3, 3, 4, 4, 4),
    carb = c(5, 5, 2, 2, 3)
  )
  result <- crosstab(df, "gear", "carb")

  expected_output <- dplyr::tibble(
    carb = c(2, 3, 5),
    `3` = c(NA, NA, 2),
    `4` = c(2, 1, NA)
  )

  expect_equal(result, expected_output)
})

# Atomic vectors ----------------------------------------------------------

test_that('shared_elements: correct output', {
  expect_false(shared_elements(1:10, 0))
  expect_true(shared_elements(1:3, rep(1:3, 2)))
  expect_true(shared_elements(rep(1:3, 2), 1:3))
  expect_true(shared_elements(0, rep(0, 3)))
})

# Numbers -----------------------------------------------------------------


test_that('n_digits_natural: correct output.', {
  expect_equal(n_digits_natural(123), 3L)
  expect_equal(n_digits_natural(1:9), rep(1L, 9L))
  expect_equal(n_digits_natural(c(10, 99)), c(2L, 2L))
  expect_equal(n_digits_natural(1e10), 11)
})

test_that('n_digits_int: correct output.', {
  expect_equal(n_digits_int(123), 3L)
  expect_equal(n_digits_int(1:9), rep(1L, 9L))
  expect_equal(n_digits_int(c(10, 99)), c(2L, 2L))
  expect_equal(n_digits_int(1e10), 11)
  expect_equal(n_digits_int(-123), 3L)
  expect_equal(n_digits_int(-c(10, 99)), c(2L, 2L))
  expect_equal(n_digits_int(-1e10), 11)
})


# Strings -----------------------------------------------------------------

test_that('is_subset_bidirectional: correct output.', {
  # 1v1
  expect_false(is_subset_bidirectional("hello", "hi"), 3L)
  # 2v2
  expect_equal(
    is_subset_bidirectional(c("hello", "world"),
                            c("hello world", "world hello")), c(T, T))
  # 1v2
  expect_equal(is_subset_bidirectional("hello", c("hello", "world")), c(T, F))
  # 2v1
  expect_equal(is_subset_bidirectional(c("hello", "world"), "hello world"),
               c(T, T))
  # Support invalid regex.
  expect_equal(is_subset_bidirectional(c("(hello", "world"), "(hello) world)"),
               c(T, T))
})

test_that('get_mode', {
  # Integers.
  expect_equal(get_mode(c(1, 2, 2, 3, 3, 3)), 3)
  # Strings.
  expect_equal(get_mode(c("apple", "banana", "banana", "orange")), 'banana')
  # Logical: Permutation of ties.
  expect_equal(get_mode(c(TRUE, FALSE)), c(TRUE, FALSE))
  expect_equal(get_mode(c(FALSE, TRUE)), c(FALSE, TRUE))
  # Logical: ignore NA.
  expect_equal(get_mode(c(TRUE, FALSE, NA, NA)), c(TRUE, FALSE))
  # Logical: include NA.
  expect_equal(get_mode(c(TRUE, FALSE, NA, NA), na.rm = FALSE), NA)
})

test_that('get_next_consecutives', {
  # Supoirt double integers.
  expect_equal(get_next_consecutives(c(1.0, 2.0), 2L), 2L)
  expect_error(get_next_consecutives(c('1.0', 2.0), 2L))
  # 1v1.
  expect_equal(get_next_consecutives(1, 2), 2L)
  expect_equal(get_next_consecutives(1, 3), numeric(0))
  expect_equal(get_next_consecutives(Inf, Inf + 1), numeric(0))

  # 1v2.
  expect_equal(get_next_consecutives(1, c(1, 2)), 2)
  expect_equal(get_next_consecutives(1, c(4, 3)), numeric(0))
  expect_equal(get_next_consecutives(1, c(4, 3)), numeric(0))
  expect_equal(get_next_consecutives(1, c(2, 2)), c(2, 2))

  # 2v1.
  expect_equal(get_next_consecutives(c(1, 3), 2), 2L)
  expect_equal(get_next_consecutives(c(-1, 3), 2), numeric(0))
  expect_equal(get_next_consecutives(c(-1, 2), 0), 0)

  # 2v2.
  expect_equal(get_next_consecutives(c(1, 3), c(2, 4)), c(2, 4))
  expect_equal(get_next_consecutives(c(1, 3), c(4, 2)), c(4, 2))
  expect_equal(get_next_consecutives(c(1, 3), c(1, 4, 2, 3)), c(4, 2))
  expect_equal(get_next_consecutives(c(1, 3, 9, 20), c(1, 4, 2, 3)), c(4, 2))
  expect_equal(get_next_consecutives(c(1, 3, 5), c(2, 4, 6, 7)), c(2, 4, 6))
})

test_that('is_substring', {
  # Is not commutative for strict substrings.
  expect_true(is_substring('hol', 'hola'))
  expect_false(is_substring('hola', 'hol'))
  # Is commutative for identical strings.
  expect_true(is_substring('hola', 'hola'))
  # Long phrases with accents.
  expect_false(is_substring(
    'El dia de ayer fui a mercar pero no encontré papayas',
    'no encontré'))
  expect_true(is_substring(
    'no encontré',
    'El dia de ayer fui a mercar pero no encontré papayas'))
  # Invalid regex.
  expect_true(is_substring('(ho', '(hola'))
  # Empty psub
  expect_true(is_substring('', 'hola'))
  expect_true(is_substring('', ''))
})

test_that('is_subset_bidirectional', {
  expect_false(is_subset_bidirectional('hola', 'holi'))
  # Is commutative.
  expect_true(is_subset_bidirectional('hol', 'hola'))
  expect_true(is_subset_bidirectional('hola', 'hol'))
  # Error handling.
  expect_error(is_subset_bidirectional('hola', 1))
  expect_error(is_subset_bidirectional(2, 'hola'))
  expect_error(is_subset_bidirectional(2, 2))
  expect_error(is_subset_bidirectional(rep('hola', 2), rep('hola', 3)))
  expect_error(is_subset_bidirectional(rep('hola', 3), rep('hola', 2)))
  # Long phrases with accents.
  expect_true(is_subset_bidirectional(
    'El dia de ayer fui a mercar pero no encontré papayas',
    'no encontré'))
  # Invalid regex.
  expect_true(is_subset_bidirectional('(hola', '(ho'))
  # Empty strings.
  expect_true(is_subset_bidirectional('', 'hola'))
  expect_true(is_subset_bidirectional('hola', ''))
  expect_true(is_subset_bidirectional('', ''))
  expect_true(is_subset_bidirectional('', c('hola', 'chao')) %>% all)
  # 1Vn
  expect_equal(is_subset_bidirectional('ho', rep('hola', 10L)) %>% length, 10L)
  expect_equal(is_subset_bidirectional('ho', c('hola', 'chao')), c(TRUE, FALSE))
  expect_equal(is_subset_bidirectional(c('hola', 'chao'), 'ho'),
               is_subset_bidirectional('ho', c('hola', 'chao')))
  # nVn
  expect_equal(
    is_subset_bidirectional(rep('chao', 10L), rep('hola', 10L)) %>% length, 10L)
  expect_true(
    is_subset_bidirectional(rep('chao', 10L), rep('ha', 10L)) %>% all)
})
