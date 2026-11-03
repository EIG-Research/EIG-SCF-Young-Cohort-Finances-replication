# 00_functions -- shared project functions: Lee trimming, shift-share, breaks
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

# Sourced by code/05_young_cohort_descriptives.R, 09_headship_by_cut.R,
# 11_selection_sensitivity.R, 12_u35_balance_sheet.R, 15_rate_price_test.R,
# and the tests in code/tests/; it only defines functions, reads no data,
# writes nothing, and changes no option, so it has no opening block (as
# for 03). Not a runner stage.
#
# scf_definition_breaks() (step 3) returns the one table of documented SCF
# cross-wave definition breaks, so 05 (outcome and cut rows), 09 (race
# rows), and 12 (balance-sheet components) flag the same breaks.
#
# lee_trim_stat() is the Lee (2009) trimming step behind the H2 bounds of
# records/plans/2026-10-11_mr4-headship-selection.md (U5), approved by the
# owner as O7 (2026-10-11). It encodes estimation, so it is defined once
# here and tested in code/tests/test_selection_bounds.R. lee_trim_estimate()
# applies it with the SE method of 03 (pooled estimate; five implicate
# estimates for the imputation variance; 999 replicates on implicate 1);
# with no trimming it reproduces scf_estimate(), which 11 checks.

# 1) A weighted mean or median after trimming a share of the total weight
#    from the top ("drop_top") or the bottom ("drop_bottom") of a sorted
#    distribution. value_sorted is ascending; weight_matrix has one row per
#    value and one column per weight vector (the pooled weight, or one
#    column per replicate), so every column is trimmed by the same share of
#    its own total. The case on the trimming boundary keeps the fraction of
#    its weight that is needed. The median follows the SCF convention of 03:
#    the first value, in ascending order, at which the cumulative kept
#    weight reaches half the kept total. A column with no weight gives NA.
lee_trim_stat <- function(value_sorted, weight_matrix, trim_share,
                          side = c("drop_top", "drop_bottom"),
                          stat = c("mean", "median")) {
  side <- match.arg(side)
  stat <- match.arg(stat)
  weight_matrix <- as.matrix(weight_matrix)
  if (length(value_sorted) != nrow(weight_matrix) ||
        is.unsorted(value_sorted) || trim_share < 0 || trim_share >= 1) {
    stop("lee_trim_stat(): values must be sorted, one per weight row, and ",
         "trim_share in [0, 1)", call. = FALSE)
  }
  row_count <- nrow(weight_matrix)
  # Walk from the end that is kept first: the bottom when dropping the top
  walk_order <- if (side == "drop_top") {
    seq_len(row_count)
  } else {
    rev(seq_len(row_count))
  }
  walk_weights <- weight_matrix[walk_order, , drop = FALSE]
  cum_weight <- apply(walk_weights, 2L, cumsum)
  cum_weight <- matrix(cum_weight, nrow = row_count)
  keep_total <- (1 - trim_share) * cum_weight[row_count, ]
  weight_before <- rbind(
    rep(0, ncol(walk_weights)), cum_weight[-row_count, , drop = FALSE]
  )
  room_left <- matrix(
    rep(keep_total, each = row_count), nrow = row_count
  ) - weight_before
  kept_walk <- pmin(walk_weights, pmax(room_left, 0))
  kept <- kept_walk[order(walk_order), , drop = FALSE]
  kept_total <- colSums(kept)
  estimate <- if (stat == "mean") {
    colSums(kept * value_sorted) / kept_total
  } else {
    kept_cum <- matrix(apply(kept, 2L, cumsum), nrow = row_count)
    below_half_count <- colSums(
      kept_cum < matrix(rep(0.5 * kept_total, each = row_count),
                        nrow = row_count)
    )
    value_sorted[pmin(below_half_count + 1L, row_count)]
  }
  estimate[kept_total <= 0] <- NA_real_
  estimate
}

# 2) A trimmed statistic and its SE for one wave and group of the stacked
#    SCF panel, by the method of 03: the pooled statistic over the five
#    implicates with WGT; the imputation variance from the five implicate
#    statistics (divisor 4) times 6/5; and the sampling variance of the
#    statistic over the replicate weights on implicate 1 (divisor 998),
#    with missing or negative replicate weights set to 0, as 03 does.
#    replicate_lookup is the replicate-weight table keyed on year and y1.
lee_trim_estimate <- function(wave_rows, replicate_lookup, replicate_columns,
                              var, stat, trim_share, side) {
  # data.table column names used unquoted below (see 03)
  year <- y1 <- NULL
  wave_rows <- wave_rows[!is.na(wave_rows[[var]])]
  data.table::setorderv(wave_rows, var)
  pooled <- lee_trim_stat(wave_rows[[var]], wave_rows$wgt, trim_share,
                          side, stat)
  implicate_values <- vapply(1:5, function(implicate_int) {
    implicate_rows <- wave_rows[wave_rows$implicate == implicate_int]
    lee_trim_stat(implicate_rows[[var]], implicate_rows$wgt, trim_share,
                  side, stat)
  }, numeric(1L))
  first_rows <- wave_rows[wave_rows$implicate == 1L]
  first_weights <- as.matrix(replicate_lookup[
    first_rows[, list(year, y1)], replicate_columns,
    with = FALSE
  ])
  first_weights[is.na(first_weights) | first_weights < 0] <- 0
  replicate_values <- lee_trim_stat(first_rows[[var]], first_weights,
                                    trim_share, side, stat)
  c(
    estimate = pooled,
    se = sqrt(stats::var(replicate_values, na.rm = TRUE) +
                (6 / 5) * stats::var(implicate_values))
  )
}

# shift_share_decompose() is the shift-share step of U3 of
# records/plans/2026-10-12_rate-vs-price-test.md, sourced by
# code/15_rate_price_test.R and tested in
# code/tests/test_rate_price_decomposition.R. It is arithmetic on estimates,
# not an estimator: it takes point estimates and returns parts that add up.

# 2) Split the change in a per-household total T = N x sum(s x v) between two
#    waves into a count part (N: items per household), a mix part (s: the
#    items' shares by class, summing to 1), and a value part (v: the mean
#    value per item in each class). Midpoint weights make the three parts
#    add up to the change exactly: N2 M2 - N1 M1 = dN x mean(M) + mean(N) x dM,
#    and dM = sum(ds x mean(v)) + sum(dv x mean(s)), with M = sum(s x v).
#    With one class the mix part is 0, which splits T = share x value per
#    holder into a holding part and a value part.
shift_share_decompose <- function(n_first, n_second, share_first,
                                  share_second, value_first, value_second) {
  class_count <- length(share_first)
  if (length(share_second) != class_count ||
        length(value_first) != class_count ||
        length(value_second) != class_count ||
        abs(sum(share_first) - 1) > 1e-9 ||
        abs(sum(share_second) - 1) > 1e-9) {
    stop("shift_share_decompose(): shares and values need one entry per ",
         "class, and each wave's shares must sum to 1", call. = FALSE)
  }
  mean_first <- sum(share_first * value_first)
  mean_second <- sum(share_second * value_second)
  n_mid <- (n_first + n_second) / 2
  c(
    total_first = n_first * mean_first,
    total_second = n_second * mean_second,
    change = n_second * mean_second - n_first * mean_first,
    count_part = (n_second - n_first) * (mean_first + mean_second) / 2,
    mix_part = n_mid * sum((share_second - share_first) *
                             (value_first + value_second) / 2),
    value_part = n_mid * sum((value_second - value_first) *
                               (share_first + share_second) / 2)
  )
}

# 3) The documented SCF cross-wave definition breaks, one row per target and
#    year (MR1, MR2, and the education finding of the 2026-10-11
#    methodology review; moved here unchanged from 05 step 12 by U1 of
#    records/plans/2026-10-12_u35-definition-breaks.md, with the three rows
#    of decision D1 of records/plans/2026-10-13_young-bottom-earners-
#    closeout.md). Only changes documented in
#    data/docs/scf/scf_cross_wave_changes_1989_2025.md ("doc" below) are
#    listed, and each note cites its section. `target` is a 05 outcome or
#    cut name (homeowner is both); `year` is the first wave measured under
#    the new definition. Sources, by row:
#    - edn_inst 1992 and 1995: doc section 5 (bulletin.macro.txt lines
#      2683-2709): the education-loan grids enter in 1992 and the
#      remaining-loans total X7179 in 1995
#    - ccbal 1992, 2010, 2016: doc section 5 (macro lines 2640-2663): store
#      revolving balance X7575 added; X424 and then X430 no longer used
#    - veh_inst 1995: doc section 5 (macro lines 2683-2709): X7169 added
#    - homeowner 1995: doc section 4 (macro lines 2364-2373): HOUSECL adds
#      X701 code 8 and X7133
#    - fin 2016: doc section 4 (macro lines 1395-1402): prepaid cards
#      (PREPAID) enter LIQ, which is part of FIN
#    - race4 1998, 2004, 2025: doc section 1 (2025 bulletin: the race
#      questions "were also changed in 1998, 2004, and 2025")
#    - educ4 2016: doc section 2 (bulletin Appendix A): the education
#      question changed to highest level or degree
#    D1 (open since records/session_logs/2026-10-11_stage1-scf-pipeline.md
#    line 83; methodology finding MR-SURVEY LOW of 2026-10-12):
#    - vehic 1995: doc section 4 (macro lines 2253-2262): a fourth owned
#      vehicle, X8188, enters VEHIC
#    - retqliq 2001: doc section 4 (macro lines 1777-1820): CURRPEN, account
#      plans paying current benefits, enters RETQLIQ (0 before 2001); the
#      2026-10-12 review measured it at under 0.4 percent of mean RETQLIQ
#      for under-35 households
#    - bus 2010: doc section 4 (macro lines 2458-2496): BUS counts three
#      actively managed businesses through 2007 and two from 2010
#    Added 2026-10-13 (decision D1 extended, and the code review of W1/W2,
#    review-reports/code-w1w2.findings.json). Targets liq, obnd, nmmf,
#    othma, oresre, nnresre, resdbt, odebt, and install are SCF variables
#    that no 05 outcome uses; 12 maps its components to them. fin rows
#    restate, for 05's fin outcome, the changes inside FIN (FIN = LIQ +
#    ... + NMMF + ... + OBND inside BOND + RETQLIQ + ... + OTHMA + ...,
#    macro line 2246):
#    - fin 1992 (obnd), 1998 (othma), 2001 (CURRPEN in retqliq), 2004
#      (liq, nmmf, othma, retqliq), 2010 (retqliq): as the rows below
#    - liq 2004: doc section 4 (macro lines 1404-1483): SAVING, MMDA, and
#      MMMF come from one new savings and money market grid (X3730-X3765)
#      from 2004, replacing the savings grid X3804-X3818 and the money
#      market grid X3706-X3718
#    - obnd 1992: doc section 4 (macro lines 1559-1565): corporate and
#      foreign bonds from X7634 + X7633, replacing X3912
#    - nmmf 2004: doc section 4 (macro lines 1522-1535): other mutual funds
#      (OMUTF, X7787) enter NMMF
#    - othma 1998 and 2004: doc section 4 (macro lines 1903-1919): one item
#      X3942 through 1995; annuities and trusts asked separately from 1998
#      (X6820, X6835); new items from 2004 (X6577, X6587)
#    - retqliq 2004: doc section 4 (macro lines 1572-1706, 1782-1789): IRAKH
#      from per-person IRA items, THRIFT from the new pension grid
#      (X11000 ...), and CURRPEN adds X6957
#    - retqliq 2010: doc section 4 (macro lines 1707-1775, 1791-1797):
#      THRIFT, FUTPEN, and CURRPEN go from six pension grids to four
#    - oresre 2010 and 2013: doc section 4 (macro lines 2379-2406): three
#      property grids through 2007, two from 2010, new item numbers from
#      2013
#    - nnresre 2010: doc section 4 (macro lines 2412-2437): three property
#      grids through 2007, two from 2010
#    - install 1995: doc section 5 (macro lines 2683-2691, 3306-3312): the
#      other-installment mopup X7183 enters INSTALL, but not VEH_INST or
#      EDN_INST, so it is in OTH_INST
#    - install 2022: doc section 5 (macro lines 2666-2691; bulletin Appendix
#      A): buy-now-pay-later balances (BNPL, 0 before 2022) are counted in
#      INSTALL, and so in OTH_INST
#    - resdbt 2010 and 2013: doc section 5 (macro lines 2601-2617): the
#      third property grid's debts dropped in 2010; new item numbers from
#      2013
#    - odebt 1995 and 1998: doc section 5 (macro lines 2733-2741): in 1995
#      alone, margin loans count only if not reported earlier (X7194 = 5),
#      so 1995 changes the series and 1998 changes it back
#    - odebt 2004: doc section 5 (macro lines 2744-2770): pension loans come
#      from the new pension grid (X11027 ...)
#    - odebt 2010: doc section 5 (macro lines 2744-2781): six pension-loan
#      grids through 2007, four from 2010
#    - edn_inst 2016 and 2025: doc section 5 (2016_scf_changes.txt line 649;
#      2025_scf_changes.pdf): the education-loan section was rewritten in
#      2016 and gained consolidation and origination questions in 2025.
#      The macro builds EDN_INST from the same items from 1995 on, so these
#      flag questionnaire changes that can move reported balances
#    - income 2004: doc section 6 (macro lines 847-890): IRA and pension-
#      account withdrawals (PENACCTWD) are added to INCOME. The macro says
#      they were "already included in earlier years", so the change is in
#      how the withdrawals are collected, not in what INCOME is meant to
#      cover; it is flagged as a measurement change
#    - income 2010: doc section 6 (macro lines 866-876): PENACCTWD drops the
#      5th and 6th current and future pension grids
#    Not flagged:
#    - networth, asset, debt: one formula in every wave; the changes sit
#      inside the components (doc section 8). INCOME is not one of these.
#    - nnresre 2013: the doc's row names 2013 for ORESRE and NNRESRE, but
#      the macro has no 2013 branch for NNRESRE
#    - INSTALL 1992: the education-loan grids it adds are also in EDN_INST,
#      so OTH_INST does not change (flagged as edn_inst 1992)
#    - HLIQ 2004 and the SAVING / MMDA / MMMF split: no 05 or 12 series uses
#      them except through LIQ (flagged as liq 2004)
#    - income_group (INCCAT, a cut of 05): the percentile of INCOME, so its
#      groups follow the income 2004 and 2010 changes; the cut's own
#      definition is unchanged (owner question)
#    - year branches that build no part of INCOME, FIN, NFIN, or DEBT
#      (attitudes, institutions, NORMINC, EQUITY, WILSH, pension-plan
#      characteristics): no 05 or 12 series uses them
#    Scope: the changes listed in doc sections 1, 2, and 4-6, and the year
#    branches of macro lines 840-2790 (income, assets, and debts) reviewed
#    on 2026-10-13. A questionnaire change that the macro does not show and
#    the doc does not list may still be missing.
scf_definition_breaks <- function() {
  documented_breaks <- data.table::data.table(
    target = c(
      "edn_inst", "edn_inst", "ccbal", "ccbal", "ccbal", "veh_inst",
      "homeowner", "fin", "race4", "race4", "race4", "educ4",
      "vehic", "retqliq", "bus",
      "fin", "oresre", "oresre", "nnresre", "resdbt", "resdbt", "odebt",
      "install", "income"
    ),
    year = c(
      1992L, 1995L, 1992L, 2010L, 2016L, 1995L,
      1995L, 2016L, 1998L, 2004L, 2025L, 2016L,
      1995L, 2001L, 2010L,
      2001L, 2010L, 2013L, 2010L, 2010L, 2013L, 2010L,
      2022L, 2004L
    ),
    note = c(
      "edn_inst 1992: education-loan grids added (cross-wave doc s5)",
      "edn_inst 1995: remaining-loans total X7179 added (cross-wave doc s5)",
      "ccbal 1992: store revolving balance X7575 added (cross-wave doc s5)",
      "ccbal 2010: balance item X424 no longer used (cross-wave doc s5)",
      "ccbal 2016: balance item X430 no longer used (cross-wave doc s5)",
      "veh_inst 1995: X7169 added (cross-wave doc s5)",
      "homeowner 1995: HOUSECL adds X701 code 8 and X7133 (cross-wave doc s4)",
      "fin 2016: prepaid cards enter LIQ, part of FIN (cross-wave doc s4)",
      "race4 1998: multiple race responses allowed (cross-wave doc s1)",
      "race4 2004: Hispanic-origin question asked first (cross-wave doc s1)",
      "race4 2025: MENA code added, Hispanic question removed (doc s1)",
      "educ4 2016: question changed to highest degree (cross-wave doc s2)",
      "vehic 1995: fourth owned vehicle X8188 added (cross-wave doc s4)",
      "retqliq 2001: plans paying benefits (CURRPEN) added (doc s4)",
      "bus 2010: three active businesses reduced to two (cross-wave doc s4)",
      "fin 2001: CURRPEN enters RETQLIQ, part of FIN (cross-wave doc s4)",
      "oresre 2010: three property grids reduced to two (cross-wave doc s4)",
      "oresre 2013: property grids use new item numbers (cross-wave doc s4)",
      "nnresre 2010: three property grids reduced to two (doc s4)",
      "resdbt 2010: third property grid's debts dropped (cross-wave doc s5)",
      "resdbt 2013: property-grid debts use new item numbers (doc s5)",
      "odebt 2010: six pension-loan grids reduced to four (doc s5)",
      "install 2022: buy-now-pay-later balances added (cross-wave doc s5)",
      "income 2004: pension withdrawals asked separately, added (doc s6)"
    )
  )
  # Rows added on 2026-10-13 from the macro sweep (sources above)
  swept_breaks <- data.table::data.table(
    target = c(
      "fin", "fin", "fin", "fin",
      "liq", "obnd", "nmmf", "othma", "othma", "retqliq", "retqliq",
      "install", "odebt", "odebt", "odebt", "edn_inst", "edn_inst",
      "income"
    ),
    year = c(
      1992L, 1998L, 2004L, 2010L,
      2004L, 1992L, 2004L, 1998L, 2004L, 2004L, 2010L,
      1995L, 1995L, 1998L, 2004L, 2016L, 2025L,
      2010L
    ),
    note = c(
      "fin 1992: corporate and foreign bond items change (doc s4)",
      "fin 1998: other managed assets asked as annuities, trusts (doc s4)",
      "fin 2004: liquid, fund, managed, IRA, pension items change (doc s4)",
      "fin 2010: retirement pension grids reduced from six to four (doc s4)",
      "liq 2004: savings and money market accounts in one new grid (doc s4)",
      "obnd 1992: corporate and foreign bonds from X7634, X7633 (doc s4)",
      "nmmf 2004: other mutual funds (OMUTF) added (cross-wave doc s4)",
      "othma 1998: annuities and trusts asked separately (doc s4)",
      "othma 2004: annuity and trust items renumbered (cross-wave doc s4)",
      "retqliq 2004: IRA and pension-grid items change, X6957 added (doc s4)",
      "retqliq 2010: pension grids reduced from six to four (doc s4)",
      "install 1995: other-installment mopup X7183 added (doc s5)",
      "odebt 1995: margin loans counted only if not reported earlier (s5)",
      "odebt 1998: the 1995 margin-loan rule dropped again (doc s5)",
      "odebt 2004: pension loans from the new pension grid (doc s5)",
      "edn_inst 2016: education-loan section rewritten (doc s5)",
      "edn_inst 2025: consolidation, origination questions added (doc s5)",
      "income 2010: 5th and 6th pension grids dropped from PENACCTWD (s6)"
    )
  )
  definition_breaks <- rbind(documented_breaks, swept_breaks)
  definition_break_key <- paste(definition_breaks$target,
                                definition_breaks$year)
  if (anyDuplicated(definition_break_key) > 0L) {
    stop("definition_breaks: a target and year appear twice", call. = FALSE)
  }
  definition_breaks
}
