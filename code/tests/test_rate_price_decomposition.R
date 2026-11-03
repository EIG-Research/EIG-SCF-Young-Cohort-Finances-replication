# test_rate_price_decomposition -- hand-checked shift_share_decompose()
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U3 of records/plans/2026-10-12_rate-vs-price-test.md. Run from the
# project root as
#   Rscript code/tests/test_rate_price_decomposition.R
# It stops at the first failed check, naming it. Every expected value is
# worked out by hand in the comment beside it. 15 runs this file first and
# stops if it fails.

################################################################################
###                       1. Load packages and the function                  ###
################################################################################

# 1) Stop early, naming any package that is not installed
if (!requireNamespace("here", quietly = TRUE)) {
  stop("Required package not installed: here", call. = FALSE)
}

# 2) Stop unless this file sits at code/tests/ in the project, then source
#    the shared functions (functions only)
here::i_am("code/tests/test_rate_price_decomposition.R")
source(here::here("code", "00_functions.R"))

# 3) Compare a result with its hand-worked value and stop on a mismatch;
#    the number of checks passed is kept in an environment
check_counter <- new.env()
check_counter$n <- 0L
expect_equal_num <- function(actual, expected, label) {
  if (length(actual) != length(expected) ||
        any(abs(actual - expected) > 1e-9)) {
    stop("FAILED: ", label, " (got ", paste(actual, collapse = ", "),
         "; expected ", paste(expected, collapse = ", "), ")", call. = FALSE)
  }
  check_counter$n <- check_counter$n + 1L
}

################################################################################
###                                 2. Tests                                 ###
################################################################################

# 4) Two classes. N 1 -> 2; shares (0.5, 0.5) -> (1, 0); values (10, 20) ->
#    (30, 40). M1 = 0.5*10 + 0.5*20 = 15, M2 = 30; T1 = 15, T2 = 60,
#    change = 45. Count part = (2-1)*(15+30)/2 = 22.5. Mean N = 1.5. Mix part
#    = 1.5 * [(1-0.5)*(10+30)/2 + (0-0.5)*(20+40)/2] = 1.5 * (10 - 15) = -7.5.
#    Value part = 1.5 * [(30-10)*(0.5+1)/2 + (40-20)*(0.5+0)/2]
#    = 1.5 * (15 + 5) = 30. Parts: 22.5 - 7.5 + 30 = 45
parts <- shift_share_decompose(1, 2, c(0.5, 0.5), c(1, 0), c(10, 20),
                               c(30, 40))
expect_equal_num(parts[["total_first"]], 15, "two classes: total, wave 1")
expect_equal_num(parts[["total_second"]], 60, "two classes: total, wave 2")
expect_equal_num(parts[["change"]], 45, "two classes: change")
expect_equal_num(parts[["count_part"]], 22.5, "two classes: count part")
expect_equal_num(parts[["mix_part"]], -7.5, "two classes: mix part")
expect_equal_num(parts[["value_part"]], 30, "two classes: value part")

# 5) No change at all: every part is 0 and the totals equal 2*(0.25*4 +
#    0.75*8) = 2*7 = 14
parts <- shift_share_decompose(2, 2, c(0.25, 0.75), c(0.25, 0.75), c(4, 8),
                               c(4, 8))
expect_equal_num(parts[c("count_part", "mix_part", "value_part")],
                 c(0, 0, 0), "no change: parts")
expect_equal_num(parts[["total_first"]], 14, "no change: total")

# 6) One class (holding x value per holder): share holding 0.4 -> 0.3,
#    value per holder 100 -> 120. T1 = 40, T2 = 36, change = -4. Holding
#    part = (0.3-0.4)*(100+120)/2 = -11; value part = (0.4+0.3)/2*(120-100)
#    = 7; mix part = 0. Parts: -11 + 0 + 7 = -4
parts <- shift_share_decompose(0.4, 0.3, 1, 1, 100, 120)
expect_equal_num(parts[["change"]], -4, "one class: change")
expect_equal_num(parts[["count_part"]], -11, "one class: holding part")
expect_equal_num(parts[["mix_part"]], 0, "one class: mix part")
expect_equal_num(parts[["value_part"]], 7, "one class: value part")

# 7) Only prices move: N 3 -> 3, shares (0.5, 0.5) fixed, values (10, 30)
#    -> (8, 24). Value part = 3 * (0.5*(-2) + 0.5*(-6)) = -12 = the change
#    (T1 = 3*20 = 60, T2 = 3*16 = 48)
parts <- shift_share_decompose(3, 3, c(0.5, 0.5), c(0.5, 0.5), c(10, 30),
                               c(8, 24))
expect_equal_num(parts[c("change", "count_part", "mix_part", "value_part")],
                 c(-12, 0, 0, -12), "prices only: all in the value part")

# 8) Bad input stops: shares that do not sum to 1, or lengths that differ
bad_calls <- list(
  function() {
    shift_share_decompose(1, 1, c(0.5, 0.4), c(0.5, 0.5), c(1, 1), c(1, 1))
  },
  function() {
    shift_share_decompose(1, 1, c(0.5, 0.5), 1, c(1, 1), c(1, 1))
  }
)
for (bad_index in seq_along(bad_calls)) {
  stopped_flag <- tryCatch({
    bad_calls[[bad_index]]()
    FALSE
  }, error = function(e) TRUE)
  if (!stopped_flag) {
    stop("FAILED: bad input ", bad_index, " did not stop", call. = FALSE)
  }
  check_counter$n <- check_counter$n + 1L
}

message("Shift-share decomposition tests passed: ", check_counter$n,
        " checks")
