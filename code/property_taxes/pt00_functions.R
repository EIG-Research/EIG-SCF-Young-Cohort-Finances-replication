# pt00_functions -- ratios of weighted totals and Suits indexes for the module
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

# Plan records/plans/2026-10-19_property-tax-descriptives.md (U3). Functions
# only: sourcing this file reads no data and writes nothing.
#
# Exceptions to the EIG R style guide, recorded here:
# - No `rm(list = ls())`, `options()`, `set.seed()`, or `here::i_am()`
#   opening block: this file is sourced by pt03, as 03 is by 04-20.
# - data.table, not the tidyverse, to match 03.
#
# Method (the same as code/03_scf_estimators.R, applied to a ratio):
# - A ratio of weighted totals, sum(w * num) / sum(w * den), is pooled over
#   the five implicates with WGT for the point estimate.
# - Imputation variance B: the variance of the five implicate ratios.
# - Sampling variance: the ratio re-estimated on implicate 1 with each of the
#   999 replicate weights, variance around their mean (divisor 998).
# - se = sqrt(sampling variance + (6/5) * B).
# A share of a total (the share of all property tax paid by one group) is
# this ratio with num = tax x 1[group] and den = tax over all households.

################################################################################
###                       1. Ratio of weighted totals                        ###
################################################################################

# 1) The ratio sum(w * num) / sum(w * den) for each group, with its standard
#    error; `num` and `den` name columns of `panel`, `by` the group columns
pt_ratio_estimate <- function(panel, repwgts, num, den, by = NULL,
                              subset = NULL) {
  # data.table symbols used unquoted below
  implicate <- wgt <- num_value <- den_value <- ratio_imp <- se <-
    se_sampling <- se_imputation <- group_key <- `:=` <- NULL

  if (!data.table::is.data.table(panel)) {
    panel <- data.table::as.data.table(panel)
  }
  group_cols <- unique(c("year", by))
  keep_mask <- if (is.null(subset)) {
    rep(TRUE, nrow(panel))
  } else {
    mask <- panel[, eval(parse(text = subset))]
    !is.na(mask) & mask
  }
  keep_mask <- keep_mask & !is.na(panel[[num]]) & !is.na(panel[[den]])
  work <- panel[keep_mask, c(group_cols, "y1", "implicate", "wgt", num, den),
                with = FALSE]
  data.table::setnames(work, c(num, den), c("num_value", "den_value"))

  # a) Pooled point estimate and unweighted count
  pooled <- work[, list(
    estimate = sum(wgt * num_value) / sum(wgt * den_value),
    n_unweighted = sum(implicate == 1L),
    n_weighted = sum(wgt)
  ), by = group_cols]

  # b) Imputation variance across the five implicate ratios
  by_implicate <- work[, list(
    ratio_imp = sum(wgt * num_value) / sum(wgt * den_value)
  ), by = c(group_cols, "implicate")]
  imputation <- by_implicate[, list(
    se_imputation = stats::sd(ratio_imp)
  ), by = group_cols]

  # c) Sampling variance from the replicate weights on implicate 1
  rw_cols <- grep("^rw_[0-9]+$", names(repwgts), value = TRUE)
  first_imp <- work[implicate == 1L]
  match_index <- repwgts[first_imp, on = c("year", "y1"), which = TRUE,
                         mult = "first"]
  if (anyNA(match_index)) {
    stop("pt_ratio_estimate(): implicate-1 cases without replicate weights",
         call. = FALSE)
  }
  weight_matrix <- as.matrix(repwgts[match_index, rw_cols, with = FALSE])
  weight_matrix[is.na(weight_matrix) | weight_matrix < 0] <- 0
  group_key <- do.call(paste, c(first_imp[, group_cols, with = FALSE],
                                sep = "\r"))
  sampling <- lapply(split(seq_len(nrow(first_imp)), group_key),
                     function(rows) {
                       numer <- drop(crossprod(first_imp$num_value[rows],
                                               weight_matrix[rows, ,
                                                             drop = FALSE]))
                       denom <- drop(crossprod(first_imp$den_value[rows],
                                               weight_matrix[rows, ,
                                                             drop = FALSE]))
                       ratio <- numer / denom
                       ratio <- ratio[is.finite(ratio)]
                       data.table::data.table(
                         se_sampling = if (length(ratio) > 1L) {
                           stats::sd(ratio)
                         } else {
                           NA_real_
                         }
                       )
                     })
  sampling_table <- data.table::rbindlist(sampling, idcol = "group_key")
  key_table <- unique(data.table::data.table(
    group_key = group_key, first_imp[, group_cols, with = FALSE]
  ))
  sampling_table <- merge(key_table, sampling_table, by = "group_key")
  sampling_table[, group_key := NULL]

  # d) Combine
  result <- merge(pooled, imputation, by = group_cols)
  result <- merge(result, sampling_table, by = group_cols, all.x = TRUE)
  result[, se := sqrt(se_sampling^2 + (6 / 5) * se_imputation^2)]
  data.table::setcolorder(result, c(group_cols, "estimate", "se",
                                    "se_sampling", "se_imputation",
                                    "n_unweighted", "n_weighted"))
  result[]
}

################################################################################
###                              2. Suits index                              ###
################################################################################

# 2) The Suits (1977) index of one tax: households ranked by income, the area
#    under the curve of the cumulative tax share against the cumulative
#    income share, as K = 1 - L / 0.5 (trapezoid rule). K > 0 is
#    progressive, K < 0 regressive, 0 proportional. Weighted by `weight`;
#    households with income <= 0 are dropped, as is usual
pt_suits_index <- function(income, tax, weight) {
  keep <- !is.na(income) & !is.na(tax) & income > 0
  income <- income[keep]
  tax <- tax[keep]
  weight <- weight[keep]
  order_index <- order(income)
  income_total <- cumsum(weight[order_index] * income[order_index])
  tax_total <- cumsum(weight[order_index] * tax[order_index])
  income_share <- c(0, income_total / income_total[length(income_total)])
  tax_share <- c(0, tax_total / tax_total[length(tax_total)])
  area <- sum(diff(income_share) *
                (tax_share[-1L] + tax_share[-length(tax_share)]) / 2)
  1 - area / 0.5
}
