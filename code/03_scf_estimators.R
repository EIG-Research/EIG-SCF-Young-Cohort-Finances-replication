# 03_scf_estimators -- SCF means, medians, and shares with their standard errors
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

# Unit U5 of records/plans/2026-10-11_stage1-scf-pipeline.md, which approves
# this file as the project's estimator library. It only defines functions:
# sourcing it reads no data, writes nothing, and changes no option.
#
# Exceptions to the EIG R style guide, recorded here:
# - No `rm(list = ls())`, `options()`, or `set.seed()` opening block, and no
#   `here::i_am()`: this file is sourced by other scripts (04, 05), and that
#   block would wipe or change the caller's session. It is not a runner stage.
# - data.table, not the tidyverse: the plan's brief asks for data.table so
#   that 999 replicate columns are handled as one matrix per group.
# - Named functions (section 6): the estimators encode weights and the
#   variance method, which the approved plan (U5) fixes; they are defined once
#   here so 04 and 05 apply one method.
#
# Method, from the Federal Reserve's own documentation:
# - Weight: WGT (= X42001 / 5) for point estimates on the five pooled
#   implicates (registry entry `scf`, weights; bulletin.macro.txt line 321).
# - Point estimate (default `pooling = "pooled"`): the WGT-weighted statistic
#   over all five implicates stacked. This reproduces the Fed's public
#   historical tables exactly (for example, the 2025 under-35 median net worth
#   is 32.82 thousand in scf2025_tables_public_real_historical.xlsx, Table 4;
#   pooling gives $32,820, while the mean of the five implicate medians gives
#   $32,630). `pooling = "implicate_average"` gives the average of the five
#   implicate statistics (the MEANIT macro's printed IMEAN / IMEDIAN).
# - Weighted median: the first value, in ascending order, at which the
#   cumulative weight share reaches 0.5 (codebk2025.txt line 2190,
#   `ID[1,I]=IMP[MIN(LOC(IMP[,2]>=.5)),1]`, and line 2253 for replicates; the
#   same `CUMSUM/TOTPOP >= p` rule in bulletin.macro.txt lines 3961-4009). No
#   averaging of two values when the share hits exactly 0.5.
# - Imputation variance B: the variance of the five implicate-specific
#   estimates, divisor 4 (codebk2025.txt lines 2262-2266).
# - Sampling variance: re-estimate on implicate 1 only with each of the 999
#   replicate weights (WT1B x MM, built in 02), then the variance of the 999
#   estimates around their own mean, divisor 998 (codebk2025.txt lines
#   2268-2272; Standard_Error_Documentation.pdf, page 1, item 1e: "We estimate
#   sampling variability only on the first implicate").
# - Total: se = sqrt(sampling variance + (6/5) * B) (codebk2025.txt line 2085
#   and line 2275; Standard_Error_Documentation.pdf, page 2, item 2a.iii.3,
#   "corrected by (1+[1/5])"). `se_imputation` is sqrt(B), as MEANIT prints
#   it, so se^2 = se_sampling^2 + (6/5) * se_imputation^2.
# - A replicate in which a group has no weight gets no estimate; MEANIT stops
#   in that case (codebk2025.txt lines 2128-2132). Here the variance uses the
#   remaining replicates (divisor: their count minus 1) and a warning names
#   the group. The column `n_replicates_empty` counts those replicates for
#   each group (0 when none; NA when no replicate weights are given or the
#   group has no implicate-1 case), so the affected cells are visible in the
#   output; it changes no estimate or SE.

################################################################################
###                         1. Row selection helper                          ###
################################################################################

# 1) Turn a subset or condition (NULL, logical vector, or a character
#    expression such as "age < 35") into a logical row mask with no NA
scf_row_mask <- function(panel, rows, label) {
  if (is.null(rows)) {
    return(rep(TRUE, nrow(panel)))
  }
  mask <- if (is.character(rows)) {
    panel[, eval(parse(text = rows))]
  } else {
    rows
  }
  if (!is.logical(mask) || length(mask) != nrow(panel)) {
    stop(
      "scf_estimate(): `", label, "` must give one TRUE/FALSE per panel row",
      call. = FALSE
    )
  }
  !is.na(mask) & mask
}

################################################################################
###                    2. Weighted median on sorted values                   ###
################################################################################

# 2) Weighted median of values already sorted ascending: the first value whose
#    cumulative weight share is at least 0.5 (codebook MEANIT convention)
scf_sorted_median <- function(value_sorted, weight_sorted) {
  cum_weight <- cumsum(weight_sorted)
  value_sorted[which(cum_weight >= 0.5 * cum_weight[length(cum_weight)])[1L]]
}

# 3) The same median for every replicate column at once: values sorted
#    ascending, one row per case, one weight column per replicate
scf_replicate_medians <- function(value_sorted, weight_matrix) {
  cum_weight <- apply(weight_matrix, 2L, cumsum)
  if (!is.matrix(cum_weight)) {
    cum_weight <- matrix(cum_weight, nrow = 1L)
  }
  half_total <- 0.5 * cum_weight[nrow(cum_weight), ]
  below_half_count <- colSums(
    cum_weight < rep(half_total, each = nrow(cum_weight))
  )
  estimate <- value_sorted[pmin(below_half_count + 1L, length(value_sorted))]
  estimate[half_total <= 0] <- NA_real_
  estimate
}

################################################################################
###                           3. The estimator                               ###
################################################################################

# 4) Estimate a mean, median, or share from the stacked SCF panel, with the
#    imputation and replicate-weight variance described in the file header
scf_estimate <- function(panel, repwgts, var,
                         stat = c("mean", "median", "share"),
                         by = NULL, subset = NULL, cond = NULL,
                         pooling = c("pooled", "implicate_average")) {
  # data.table column names and symbols used unquoted below, declared so that
  # static checks (codetools, lintr) do not read them as undefined globals
  value_num <- wgt <- implicate <- estimate_imp <- estimate <- pooled <-
    implicate_mean <- imputation_var <- sampling_var <- se <- se_sampling <-
    se_imputation <- n_replicates_empty <- `:=` <- NULL

  stat <- match.arg(stat)
  pooling <- match.arg(pooling)
  if (!data.table::is.data.table(panel)) {
    panel <- data.table::as.data.table(panel)
  }

  # a) Check the inputs the method relies on
  required_cols <- unique(c("year", "implicate", "wgt", var, by))
  missing_cols <- setdiff(required_cols, names(panel))
  if (length(missing_cols) > 0L) {
    stop(
      "scf_estimate(): panel lacks column(s) ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  group_cols <- unique(c("year", by))

  # b) Keep the selected rows with a non-missing value (MEANIT drops missing
  #    values, codebk2025.txt lines 2121-2127)
  keep_mask <- scf_row_mask(panel, subset, "subset") &
    scf_row_mask(panel, cond, "cond") &
    !is.na(panel[[var]])
  key_cols <- intersect(c("y1", "yy1"), names(panel))
  work <- panel[keep_mask, c(group_cols, key_cols, "implicate", "wgt", var),
                with = FALSE]
  data.table::setnames(work, var, "value_num")
  if (stat == "share") {
    if (!is.logical(work$value_num) &&
          !all(work$value_num %in% c(0, 1))) {
      stop(
        "scf_estimate(): a share needs a logical or 0/1 variable; `", var,
        "` is neither",
        call. = FALSE
      )
    }
  }
  work[, value_num := as.numeric(value_num)]
  if (nrow(work) == 0L) {
    stop("scf_estimate(): no rows left after subset and cond", call. = FALSE)
  }
  statistic_fun <- if (stat == "median") {
    scf_sorted_median
  } else {
    function(value_sorted, weight_sorted) {
      sum(value_sorted * weight_sorted) / sum(weight_sorted)
    }
  }

  # c) Sort once by group and value; every median below reads sorted rows
  data.table::setorderv(work, c(group_cols, "value_num"))

  # d) Estimate within each implicate (for B, and for the implicate average)
  implicate_est <- work[
    ,
    list(estimate_imp = statistic_fun(value_num, wgt)),
    by = c(group_cols, "implicate")
  ]
  imputation <- implicate_est[
    ,
    list(
      implicate_mean = mean(estimate_imp),
      imputation_var = if (length(estimate_imp) > 1L) {
        stats::var(estimate_imp)
      } else {
        NA_real_
      },
      n_implicates = length(estimate_imp)
    ),
    by = group_cols
  ]
  if (any(imputation$n_implicates != 5L)) {
    warning(
      "scf_estimate(): some groups of `", var, "` lack one or more of the ",
      "five implicates; their imputation variance uses those present",
      call. = FALSE
    )
  }

  # e) Pooled estimate over all implicates, with cell counts
  pooled_est <- work[
    ,
    list(
      pooled = statistic_fun(value_num, wgt),
      n_unweighted = sum(implicate == 1L),
      n_weighted = sum(wgt)
    ),
    by = group_cols
  ]
  result <- merge(pooled_est, imputation, by = group_cols, sort = TRUE)
  result[, estimate := if (pooling == "pooled") pooled else implicate_mean]

  # f) Sampling variance from the replicate weights on implicate 1
  if (is.null(repwgts)) {
    result[, sampling_var := NA_real_]
    result[, n_replicates_empty := NA_integer_]
  } else {
    sampling <- scf_sampling_variance(work, repwgts, group_cols, stat)
    result <- merge(result, sampling, by = group_cols, all.x = TRUE,
                    sort = TRUE)
  }

  # g) Combine: total variance = sampling + (6/5) * imputation
  result[, se_sampling := sqrt(sampling_var)]
  result[, se_imputation := sqrt(imputation_var)]
  result[, se := sqrt(sampling_var + (6 / 5) * imputation_var)]
  result[, c(group_cols, "estimate", "se", "se_sampling", "se_imputation",
             "n_unweighted", "n_weighted", "n_replicates_empty"),
         with = FALSE]
}

################################################################################
###                    4. Sampling variance from replicates                  ###
################################################################################

# 5) For each group, re-estimate the statistic on implicate 1 with each of
#    the 999 replicate weights and return the variance of those estimates
#    and the number of replicates in which the group has no weight
scf_sampling_variance <- function(work, repwgts, group_cols, stat) {
  # data.table column names and symbols used unquoted below (see above)
  implicate <- row_order <- group_id <- sampling_var <- n_replicates_empty <-
    `:=` <- NULL

  if (!data.table::is.data.table(repwgts)) {
    repwgts <- data.table::as.data.table(repwgts)
  }
  rw_cols <- grep("^rw_[0-9]+$", names(repwgts), value = TRUE)
  rw_cols <- rw_cols[order(as.integer(sub("^rw_", "", rw_cols)))]
  join_key <- intersect(c("y1", "yy1"), names(repwgts))[1L]
  if (length(rw_cols) == 0L || is.na(join_key) ||
        !join_key %in% names(work)) {
    stop(
      "scf_estimate(): repwgts needs year, y1 or yy1 (also in the panel), ",
      "and rw_1...rw_999",
      call. = FALSE
    )
  }

  # a) Implicate-1 rows, still sorted by group and value, matched to their
  #    replicate weights; an unmatched case is an error, not a dropped row
  first_imp <- work[implicate == 1L]
  first_imp[, row_order := seq_len(nrow(first_imp))]
  match_index <- repwgts[first_imp, on = c("year", join_key), which = TRUE,
                         mult = "first"]
  if (anyNA(match_index)) {
    stop(
      "scf_estimate(): ", sum(is.na(match_index)), " implicate-1 case(s) ",
      "have no replicate weights (join on year and ", join_key, ")",
      call. = FALSE
    )
  }
  weight_matrix <- as.matrix(repwgts[match_index, rw_cols, with = FALSE])

  # b) A case not drawn into a replicate has a missing or zero weight; the
  #    codebook takes MAX(0, MM) * MAX(0, WT1B) (codebk2025.txt line 2305)
  weight_matrix[is.na(weight_matrix) | weight_matrix < 0] <- 0

  # c) Loop over groups (few); within a group, all replicates at once
  # rows are sorted by group, so a run id over the group columns numbers them
  first_imp[, group_id := data.table::rleidv(first_imp, group_cols)]
  group_keys <- unique(first_imp[, c("group_id", group_cols), with = FALSE])
  sampling_var <- rep(NA_real_, nrow(group_keys))
  empty_replicate_count <- rep(NA_integer_, nrow(group_keys))
  for (group_index in seq_len(nrow(group_keys))) {
    rows <- first_imp[group_id == group_index, row_order]
    value_sorted <- first_imp$value_num[rows]
    group_weights <- weight_matrix[rows, , drop = FALSE]
    empty_replicate_count[group_index] <- sum(colSums(group_weights) <= 0)
    replicate_est <- if (stat == "median") {
      scf_replicate_medians(value_sorted, group_weights)
    } else {
      weight_total <- colSums(group_weights)
      estimate <- drop(crossprod(value_sorted, group_weights)) / weight_total
      estimate[weight_total <= 0] <- NA_real_
      estimate
    }
    valid_count <- sum(!is.na(replicate_est))
    if (valid_count < length(replicate_est)) {
      warning(
        "scf_estimate(): group ", group_index, " (",
        paste(unlist(group_keys[group_index, -1L]), collapse = ", "),
        ") is empty in ", length(replicate_est) - valid_count,
        " replicate(s); its sampling variance uses the other ", valid_count,
        call. = FALSE
      )
    }
    if (valid_count > 1L) {
      sampling_var[group_index] <- stats::var(replicate_est, na.rm = TRUE)
    }
  }
  group_keys[, sampling_var := sampling_var]
  group_keys[, n_replicates_empty := empty_replicate_count]
  group_keys[, group_id := NULL]
  group_keys[]
}
