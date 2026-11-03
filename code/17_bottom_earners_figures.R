# 17_bottom_earners_figures -- change in median net worth and income by income
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Workstream W3 of records/plans/2026-10-13_young-bottom-earners-closeout.md.
# A diagnostic figure, not a draft. It reads
#   output/tables/bottom_earners_changes.csv  (written by 16)
# and writes, to output/figures/:
#   bottom_earners_change_2022_2025.png   percent change from 2022 to 2025
#                                         in median net worth and median
#                                         income by the Fed's income
#                                         percentile group, with 95% CIs
# Run from the project root as
#   Rscript code/17_bottom_earners_figures.R
#
# Every dollar value is on the project basis `pce25` (headline PCE chain-type
# price index, 2025 dollars; income, which refers to the prior calendar
# year, deflated with the prior year's index). Income groups are INCCAT,
# the percentile of income over all families in each wave. The styling
# follows Figure C of 13 (networth_change_by_age_2022_2025.png): the same
# theme, tokens, fonts, bar and error-bar geometry, and interval text. The
# intervals are 16's log-ratio intervals (pct_ci_low, pct_ci_high), with
# households re-ranked into income groups in every replicate and implicate.

################################################################################
###                       1. Load packages and paths                         ###
################################################################################

here::i_am("code/17_bottom_earners_figures.R")

library(ggplot2)

source(here::here("code", "figure_theme.R"))

path_input <- here::here("output", "tables", "bottom_earners_changes.csv")
path_figures <- here::here("output", "figures")
path_figure <- file.path(path_figures, "bottom_earners_change_2022_2025.png")

# 1) Stop early when the figure input is missing
if (!file.exists(path_input)) {
  stop(
    "Figure input not found: ", path_input,
    ". Run code/16_bottom_earners.R first."
  )
}
dir.create(path_figures, showWarnings = FALSE, recursive = TRUE)

# 2) Load the EIG tokens and check the fonts
eig_tokens <- eig_load_tokens()
eig_assert_fonts(eig_tokens, allow_fallback = TRUE)
body_family <- eig_resolve_family("body", tokens = eig_tokens)

# Emphasis (the interval header and panel titles) uses the body family's
# semibold weight, as 13 does; without a semibold alias, face = "bold"
strong_family <- eig_semibold_family(body_family)
strong_face <- "plain"
if (is.null(strong_family)) {
  strong_family <- body_family
  strong_face <- "bold"
}

# 3) Colors from the tokens only: the two bottom income groups (the plan's
#    "bottom earners", decision D4) in the first series color, the other
#    groups in the neutral axis gray, as 13 marks the under-35 group
color_bottom <- eig_tokens$EIG_COLORS[["eig_teal_900"]]
color_other <- eig_tokens$EIG_NEUTRALS[["eig_neutral_axis"]]
color_black <- eig_tokens$EIG_COLORS[["eig_black"]]

# 4) Text sizes in points, from the type roles; geom_text takes mm
label_pt <- eig_role_size("axis_legend_table_body", eig_tokens)
header_pt <- eig_role_size("subtitle", eig_tokens)
text_floor_pt <- eig_tokens$EIG_SIZING$text_floor_pt$print
if (min(label_pt, header_pt) < text_floor_pt) {
  stop("A label size is below the ", text_floor_pt, "pt print floor.")
}
label_size_mm <- label_pt / ggplot2::.pt

# 5) The groups, in the Fed's table order, and the two bottom groups
income_levels <- c("Less than 20", "20-39.9", "40-59.9", "60-79.9",
                   "80-89.9", "90-100")
bottom_levels <- c("Less than 20", "20-39.9")
measure_levels <- c("networth", "income")
measure_labels <- c("Median net worth", "Median income")

################################################################################
###                            2. Read the input                             ###
################################################################################

# 6) Read the change table (written by 16) with explicit types and keep the
#    2022-2025 percent change of median net worth and median income by
#    income group
changes <- readr::read_csv(
  path_input,
  col_types = readr::cols(
    period = readr::col_character(),
    grouping = readr::col_character(),
    group = readr::col_character(),
    outcome = readr::col_character(),
    statistic = readr::col_character(),
    basis = readr::col_character(),
    significant_flag = readr::col_logical(),
    small_cell = readr::col_logical(),
    tiny_cell = readr::col_logical(),
    definition_break = readr::col_character(),
    measurement_note = readr::col_character(),
    pct_significant_flag = readr::col_logical(),
    pct_suppressed_reason = readr::col_character(),
    .default = readr::col_double()
  )
)
figure_data <- changes |>
  dplyr::filter(
    period == "2022-2025", grouping == "income_group",
    statistic == "median", outcome %in% measure_levels
  )
if (nrow(figure_data) != length(income_levels) * length(measure_levels) ||
      !setequal(figure_data$group, income_levels) ||
      anyNA(figure_data$pct_change) || anyNA(figure_data$pct_ci_low)) {
  stop("The figure needs a 2022-2025 percent change and interval for each ",
       "income group and both measures")
}
if (any(figure_data$small_cell) ||
      any(!is.na(figure_data$definition_break)) ||
      any(!is.na(figure_data$pct_suppressed_reason))) {
  stop("A plotted cell is a small cell, crosses a definition break, or has ",
       "a suppressed percent change; the figure note would need to say so")
}
message("Read ", nrow(figure_data), " cells from ", path_input)

################################################################################
###                         3. Build the drawing data                        ###
################################################################################

# 7) Groups ordered as in 13 (first level at the bottom), the bar color, and
#    the text column: the change and its 95% interval
# (whole percents, as 13; a value under 1 in absolute size gets one
# decimal, so an interval end such as -0.5 does not print as "0%")
format_pct <- function(x) {
  digits_shown <- dplyr::if_else(abs(x) < 1, 1L, 0L)
  rounded <- round(x, digits_shown)
  sign_text <- dplyr::case_when(rounded < 0 ~ "−", rounded > 0 ~ "+",
                                TRUE ~ "")
  paste0(sign_text, mapply(formatC, abs(rounded), format = "f",
                           digits = digits_shown), "%")
}
figure_data <- figure_data |>
  dplyr::mutate(
    group_label = factor(group, levels = income_levels),
    measure_label = factor(measure_labels[match(outcome, measure_levels)],
                           levels = measure_labels),
    bar_fill = dplyr::if_else(group %in% bottom_levels, color_bottom,
                              color_other),
    interval_text = paste0(
      format_pct(pct_change), "  (", format_pct(pct_ci_low), " to ",
      format_pct(pct_ci_high), ")"
    )
  )

# 8) One x scale for both panels, so bar lengths compare across panels; the
#    interval text sits to the right of the widest interval
x_text <- max(figure_data$pct_ci_high) + 8
x_limits <- c(min(figure_data$pct_ci_low) - 4, x_text + 62)
header_data <- data.frame(
  measure_label = factor(measure_labels, levels = measure_labels),
  x = x_text, y = length(income_levels) + 0.75,
  label = "Change (95% interval)"
)

figure_note <- paste0(
  "Note: Bars are the percent change from 2022 to 2025 in the median, ",
  "in 2025 dollars deflated with the PCE price index (income for the ",
  "prior calendar year, deflated with that year's index). Groups are ",
  "percentiles of income over all families in each survey year. Error ",
  "bars and the intervals in parentheses are 95% confidence intervals ",
  "for the ratio of two independent medians, computed on the log scale, ",
  "with standard errors from the SCF's replicate weights and five ",
  "implicates; households are re-ranked into income groups in every ",
  "replicate and implicate. An interval that crosses zero is not ",
  "distinguishable from no change. ",
  "Teal bars are the two lowest income groups."
)
# the theme does not wrap captions, so break the note into lines here
figure_note <- paste0(
  paste(strwrap(figure_note, width = 175L), collapse = "\n"), "\n",
  "Source: Federal Reserve Board, Survey of Consumer Finances, 2022 and ",
  "2025; EIG calculations."
)

################################################################################
###                                4. Draw                                   ###
################################################################################

# 9) Two panels side by side, bars and 95% error bars as in 13's Figure C
figure_income <- ggplot(figure_data, aes(y = group_label)) +
  geom_col(aes(x = pct_change, fill = bar_fill), width = 0.62) +
  geom_vline(xintercept = 0, color = color_black, linewidth = 0.4) +
  geom_errorbar(
    aes(xmin = pct_ci_low, xmax = pct_ci_high),
    width = 0.22, color = color_black, linewidth = 0.4,
    orientation = "y"
  ) +
  geom_text(
    aes(x = x_text, label = interval_text),
    hjust = 0, size = label_size_mm, color = color_black, family = body_family
  ) +
  geom_text(
    data = header_data, aes(x = x, y = y, label = label),
    hjust = 0, vjust = 0, size = label_size_mm, color = color_black,
    family = strong_family, fontface = strong_face, inherit.aes = FALSE
  ) +
  facet_wrap(~measure_label, nrow = 1L) +
  scale_fill_identity() +
  scale_x_continuous(
    breaks = seq(-50, 50, by = 25),
    labels = function(x) {
      dplyr::if_else(x == 0, "0", paste0(dplyr::if_else(x < 0, "−",
                                                        "+"), abs(x), "%"))
    }
  ) +
  coord_cartesian(xlim = x_limits, ylim = c(0.5, length(income_levels) + 1),
                  expand = FALSE, clip = "off") +
  labs(
    title = paste0(
      "Change from 2022 to 2025 in median net worth and median income, ",
      "adjusted for inflation, by income percentile"
    ),
    subtitle = "Percent change, with 95% confidence intervals",
    x = NULL, y = "Percentile of income",
    caption = figure_note
  ) +
  eig_theme_ggplot(eig_tokens) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing.x = grid::unit(1.2, "lines"),
    strip.text = element_text(family = strong_family, face = strong_face,
                              size = header_pt, hjust = 0),
    legend.position = "none"
  )

ggsave(
  path_figure, figure_income,
  width = 11, height = 4.9, units = "in", dpi = 300,
  device = ragg::agg_png
)
message("Wrote ", path_figure)
