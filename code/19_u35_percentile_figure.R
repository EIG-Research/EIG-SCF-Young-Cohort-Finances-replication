# 19_u35_percentile_figure -- change in under-35 net worth by percentile
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U3 of records/plans/2026-10-16_draft-v2-replies.md. It reads
#   output/tables/u35_networth_percentiles.csv   (written by 18)
# and writes, to output/figures/:
#   u35_networth_percentile_change_2022_2025.png  percent change in under-35
#                                                 net worth at the 40th-90th
#                                                 percentiles, 2022-2025,
#                                                 with 95% error bars; the
#                                                 10th-30th as levels
# Run from the project root as
#   Rscript code/19_u35_percentile_figure.R
#
# Styled as Figure C of 13 (change in median net worth by age). Below the
# 40th percentile, under-35 net worth is near or below zero, so 18
# suppresses the percent change there (16's rule); those rows show the two
# levels in the text column instead of a bar. The 50th percentile (the
# median the draft discusses) is in the series color and the others in
# gray. Dollars are PCE 2025 dollars; young households exclude young adults
# living in another household's primary economic unit.

################################################################################
###                       1. Load packages and paths                         ###
################################################################################

# 1) Stop early, naming any package that is not installed
required_packages <- c("here", "ggplot2", "dplyr", "readr", "ragg")
installed_flag <- vapply(
  required_packages, requireNamespace, logical(1L),
  quietly = TRUE
)
if (!all(installed_flag)) {
  stop(
    "Required package(s) not installed: ",
    paste(required_packages[!installed_flag], collapse = ", "),
    call. = FALSE
  )
}

here::i_am("code/19_u35_percentile_figure.R")
library(ggplot2)
source(here::here("code", "figure_theme.R"))

path_input <- here::here("output", "tables", "u35_networth_percentiles.csv")
path_figures <- here::here("output", "figures")
path_figure <- file.path(path_figures,
                         "u35_networth_percentile_change_2022_2025.png")
if (!file.exists(path_input)) {
  stop("Figure input not found: ", path_input,
       ". Run code/18_u35_networth_percentiles.R first.", call. = FALSE)
}
dir.create(path_figures, showWarnings = FALSE, recursive = TRUE)

# 2) EIG tokens, fonts, colors, and type sizes, as in 13
eig_tokens <- eig_load_tokens()
eig_assert_fonts(eig_tokens, allow_fallback = TRUE)
body_family <- eig_resolve_family("body", tokens = eig_tokens)
strong_family <- eig_semibold_family(body_family)
strong_face <- "plain"
if (is.null(strong_family)) {
  strong_family <- body_family
  strong_face <- "bold"
}
color_highlight <- eig_tokens$EIG_COLORS[["eig_teal_900"]]
color_black <- eig_tokens$EIG_COLORS[["eig_black"]]
color_axis <- eig_tokens$EIG_NEUTRALS[["eig_neutral_axis"]]
label_pt <- eig_role_size("axis_legend_table_body", eig_tokens)
text_floor_pt <- eig_tokens$EIG_SIZING$text_floor_pt$print
if (label_pt < text_floor_pt) {
  stop("The label size is below the ", text_floor_pt, "pt print floor.",
       call. = FALSE)
}
label_size_mm <- label_pt / ggplot2::.pt

################################################################################
###                           2. Read and shape                              ###
################################################################################

# 3) Read the percentile table (18) with explicit types
percentiles <- readr::read_csv(
  path_input,
  col_types = readr::cols(
    percentile = readr::col_integer(),
    pct_suppressed_reason = readr::col_character(),
    significant_flag = readr::col_logical(),
    .default = readr::col_double()
  ),
  na = c("", "NA")
)
if (!identical(sort(percentiles$percentile), seq(10L, 90L, by = 10L))) {
  stop("The figure needs the 10th-90th percentiles in steps of 10 (18)",
       call. = FALSE)
}
percentiles$pct_suppressed_reason[is.na(percentiles$pct_suppressed_reason)] <-
  ""
shown_flag <- percentiles$pct_suppressed_reason == ""
if (anyNA(percentiles[shown_flag, c("pct_change", "ci_low", "ci_high")])) {
  stop("A plotted percentile lacks its log-ratio interval (18)",
       call. = FALSE)
}

# 4) Labels: the percentile, and the text column, which gives the change and
#    its 95% interval, or the two levels where no percent change is shown
format_pct <- function(x) {
  paste0(dplyr::if_else(x < 0, "−", "+"),
         formatC(abs(x), format = "f", digits = 0L), "%")
}
format_k <- function(x) {
  paste0(dplyr::if_else(x < 0, "−", ""), "$",
         formatC(abs(x) / 1000, format = "f", digits = 1L), "k")
}
ordinal_label <- function(x) paste0(x, "th")
percentiles <- percentiles |>
  dplyr::mutate(
    shown_flag = pct_suppressed_reason == "",
    percentile_label = factor(ordinal_label(percentile),
                              levels = ordinal_label(seq(10L, 90L, 10L))),
    bar_fill = dplyr::if_else(percentile == 50L, color_highlight, color_axis),
    interval_text = dplyr::if_else(
      shown_flag,
      paste0(format_pct(pct_change), "  (", format_pct(ci_low), " to ",
             format_pct(ci_high), ")"),
      paste0(format_k(estimate_2022), " to ", format_k(estimate_2025),
             " (levels)")
    )
  )
bars <- dplyr::filter(percentiles, shown_flag)
x_text <- max(bars$ci_high) + 12
x_limits <- c(min(bars$ci_low) - 4, x_text + 42)

lowest_shown <- min(bars$percentile)
figure_note <- paste0(
  "Note: Bars are the percent change from 2022 to 2025 in the net worth of ",
  "under-35 households at each percentile, in 2025 dollars deflated with\n",
  "the PCE price index. Error bars and the intervals in parentheses are 95% ",
  "confidence intervals from the SCF's replicate weights and five\n",
  "implicates, on the log of the ratio of the two percentiles. Below the ",
  ordinal_label(lowest_shown), " percentile, net worth is near or below ",
  "zero, so the two levels are given instead.\n",
  "The two years are independent samples. Age is the reference person's. ",
  "\"Young households\" exclude young adults living in another household's\n",
  "primary economic unit.\n",
  "Source: Federal Reserve Board, Survey of Consumer Finances, 2022 and ",
  "2025; EIG calculations."
)

################################################################################
###                                3. Draw                                   ###
################################################################################

# 5) Horizontal bars, 10th percentile at the bottom, as Figure C of 13
figure_percentiles <- ggplot(percentiles, aes(y = percentile_label)) +
  geom_col(data = bars, aes(x = pct_change, fill = bar_fill), width = 0.62) +
  geom_vline(xintercept = 0, color = color_black, linewidth = 0.4) +
  geom_errorbar(
    data = bars, aes(xmin = ci_low, xmax = ci_high),
    width = 0.22, color = color_black, linewidth = 0.4, orientation = "y"
  ) +
  geom_text(
    aes(x = x_text, label = interval_text),
    hjust = 0, size = label_size_mm, color = color_black, family = body_family
  ) +
  annotate(
    "text", x = x_text, y = nrow(percentiles) + 0.75,
    label = "Change (95% interval)", hjust = 0, vjust = 0,
    size = label_size_mm, color = color_black, family = strong_family,
    fontface = strong_face
  ) +
  scale_fill_identity() +
  scale_y_discrete(limits = levels(percentiles$percentile_label)) +
  scale_x_continuous(
    breaks = seq(-40, 20, by = 20),
    labels = function(x) {
      dplyr::if_else(x == 0, "0", paste0(dplyr::if_else(x < 0, "−", "+"),
                                         abs(x), "%"))
    }
  ) +
  coord_cartesian(xlim = x_limits, ylim = c(0.5, nrow(percentiles) + 1),
                  expand = FALSE, clip = "off") +
  labs(
    title = paste0("Change from 2022 to 2025 in the net worth of young ",
                   "households, by percentile"),
    subtitle = paste0("Under-35 households, percent change adjusted for ",
                      "inflation, with 95% confidence intervals"),
    x = NULL, y = "Percentile of net worth",
    caption = figure_note
  ) +
  eig_theme_ggplot(eig_tokens) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

ggsave(
  path_figure, figure_percentiles,
  width = 8, height = 5.2, units = "in", dpi = 300,
  device = ragg::agg_png
)
message("Wrote ", path_figure)
