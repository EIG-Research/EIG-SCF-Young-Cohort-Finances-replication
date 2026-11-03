# 06_young_cohort_figures -- diagnostic figures for young households
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U6 (figures) of records/plans/2026-10-11_stage1-scf-pipeline.md.
# These are diagnostic figures, not a draft. It reads
#   output/tables/fig_inputs_young.csv  (written by 05)
# and writes, to output/figures/:
#   young_median_networth_by_year.png  median net worth, under 35 and 35-44
#   cohort_median_networth_by_age.png  median net worth by mean age, one line
#                                      per five-year birth band, from mean
#                                      age 25, without cells of 10 or fewer
#                                      cases
# Run from the project root as
#   Rscript code/06_young_cohort_figures.R
#
# Every dollar value is on the project basis `pce25` (headline PCE chain-type
# price index, 2025 dollars). Young households exclude young adults living in
# another household's primary economic unit (PROJECT.md, Additional Context).

################################################################################
###                       1. Load packages and paths                         ###
################################################################################

here::i_am("code/06_young_cohort_figures.R")

library(ggplot2)

source(here::here("code", "figure_theme.R"))

path_input <- here::here("output", "tables", "fig_inputs_young.csv")
path_figures <- here::here("output", "figures")

# 1) Stop early when the figure input is missing
if (!file.exists(path_input)) {
  stop(
    "Figure input not found: ", path_input,
    ". Run code/05_young_cohort_descriptives.R first."
  )
}
dir.create(path_figures, showWarnings = FALSE, recursive = TRUE)

# 2) Load the EIG tokens and check the fonts
eig_tokens <- eig_load_tokens()
eig_assert_fonts(eig_tokens, allow_fallback = TRUE)

color_series_1 <- eig_tokens$EIG_COLORS[["eig_teal_900"]]
color_series_2 <- eig_tokens$EIG_COLORS[["eig_blue_800"]]
color_gray <- eig_tokens$EIG_NEUTRALS[["eig_neutral_axis"]]
color_caption <- eig_tokens$EIG_NEUTRALS[["eig_neutral_caption"]]
color_white <- eig_tokens$EIG_COLORS[["eig_white"]]

# Text size for direct labels: the axis and legend role, in mm for geom_text
label_size_mm <-
  eig_role_size("axis_legend_table_body", eig_tokens) / ggplot2::.pt

source_line <- paste0(
  "Source: Federal Reserve Board, Survey of Consumer Finances, ",
  "1989-2025; EIG calculations."
)
note_dollars <- paste0(
  "Note: Dollars are 2025 dollars deflated with the PCE price index. ",
  "\"Young households\" exclude young adults\nliving in another ",
  "household's primary economic unit."
)

################################################################################
###                            2. Read the input                             ###
################################################################################

fig_inputs <- readr::read_csv(
  path_input,
  col_types = readr::cols(
    panel = readr::col_character(),
    year = readr::col_integer(),
    group = readr::col_character(),
    outcome = readr::col_character(),
    statistic = readr::col_character(),
    estimate = readr::col_double(),
    se = readr::col_double(),
    n_unweighted = readr::col_integer(),
    small_cell = readr::col_logical(),
    mean_age = readr::col_double(),
    basis = readr::col_character(),
    tiny_cell = readr::col_logical()
  )
)
message("Read ", nrow(fig_inputs), " rows from ", path_input)

################################################################################
###                 3. Figure 1: median net worth by year                    ###
################################################################################

# 1) Keep the median net worth of the two age groups, in 2025 PCE dollars
age_group_mask <-
  fig_inputs$panel == "a_age_group" &
  fig_inputs$outcome == "networth" &
  fig_inputs$statistic == "median" &
  fig_inputs$basis == "pce25"
age_group_data <- dplyr::filter(fig_inputs, age_group_mask)
if (nrow(age_group_data) == 0L) {
  stop("No a_age_group median net worth rows in ", path_input)
}

# 2) 95% confidence band: estimate plus or minus 1.96 standard errors
age_group_data <- age_group_data |>
  dplyr::mutate(
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se,
    group_label = dplyr::if_else(group == "under 35", "Under 35", "35-44"),
    group_label = factor(group_label, levels = c("Under 35", "35-44"))
  )

# 3) Direct labels at the last survey year
age_group_end_labels <- age_group_data |>
  dplyr::filter(year == max(year))

year_breaks <- sort(unique(age_group_data$year))

figure_by_year <- ggplot(
  age_group_data,
  aes(x = year, y = estimate, color = group_label, fill = group_label)
) +
  geom_ribbon(
    aes(ymin = ci_low, ymax = ci_high),
    alpha = 0.15, color = NA
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  geom_text(
    data = age_group_end_labels,
    aes(label = group_label),
    hjust = 0, nudge_x = 0.6, size = label_size_mm,
    family = eig_resolve_family("body", tokens = eig_tokens)
  ) +
  scale_color_manual(values = c(color_series_1, color_series_2)) +
  scale_fill_manual(values = c(color_series_1, color_series_2)) +
  scale_x_continuous(
    breaks = year_breaks,
    expand = expansion(mult = c(0.02, 0.13))
  ) +
  scale_y_continuous(
    labels = scales::label_dollar(),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Median net worth by age of the reference person, 1989-2025",
    x = NULL,
    y = "Median net worth (2025 dollars)",
    caption = paste0(
      note_dollars,
      " Shaded bands are 95% confidence intervals.\n",
      source_line
    )
  ) +
  eig_theme_ggplot(eig_tokens) +
  theme(legend.position = "none")

path_figure_1 <- file.path(path_figures, "young_median_networth_by_year.png")
ggsave(
  path_figure_1, figure_by_year,
  width = 6.5, height = 3.5, units = "in", dpi = 300,
  device = ragg::agg_png
)
message("Wrote ", path_figure_1)

################################################################################
###              4. Figure 2: median net worth by birth cohort               ###
################################################################################

# 1) Keep the birth-band median net worth rows with a mean age, and leave
#    out two kinds of cell (decision D2 of
#    records/plans/2026-10-13_young-bottom-earners-closeout.md, for the
#    methodology finding MR-SAMPLE MEDIUM of 2026-10-12):
#    - cells at a mean age under 25, the SCF's under-25 age band, where only
#      18-22 percent of CPS adults aged 18-24 would be the SCF reference
#      person of their unit in the 13 SCF waves (scf_rule_rp_share in
#      headship_context.csv, against 40-45 percent at 25-29), so a cohort's
#      first points follow a small, selected part of the birth band;
#    - tiny cells, n_unweighted <= 10 (05), whose medians have no usable
#      sampling distribution.
#    The rows stay in fig_inputs_young.csv and cohort_by_age.csv.
cohort_min_mean_age <- 25
cohort_rows_mask <-
  fig_inputs$panel == "b_birth_band" &
  fig_inputs$outcome == "networth" &
  fig_inputs$statistic == "median" &
  fig_inputs$basis == "pce25" &
  !is.na(fig_inputs$mean_age)
cohort_plotted_mask <-
  cohort_rows_mask &
  fig_inputs$mean_age >= cohort_min_mean_age &
  !is.na(fig_inputs$tiny_cell) &
  !fig_inputs$tiny_cell
cohort_data <- dplyr::filter(fig_inputs, cohort_plotted_mask)
if (nrow(cohort_data) == 0L) {
  stop("No b_birth_band median net worth rows to plot in ", path_input)
}
if (any(cohort_data$estimate <= 0)) {
  stop("Non-positive birth-band median; the log scale cannot show it.")
}
message(
  "Cohort figure: ", nrow(cohort_data), " of ", sum(cohort_rows_mask),
  " birth-band cells plotted; left out ",
  sum(cohort_rows_mask & fig_inputs$mean_age < cohort_min_mean_age),
  " at a mean age under ", cohort_min_mean_age, " and ",
  sum(cohort_rows_mask & fig_inputs$mean_age >= cohort_min_mean_age &
        fig_inputs$tiny_cell),
  " other tiny cell(s)"
)

# 2) Highlight two recent birth bands, a decade apart; gray the rest. The
#    2000-04 band, highlighted before D2, has no cell at a mean age of 25 or
#    more through 2025
highlight_bands <- c("1980-84", "1990-94")
highlight_colors <- c(color_series_1, color_series_2)
names(highlight_colors) <- highlight_bands
if (!all(highlight_bands %in% cohort_data$group)) {
  stop("A highlighted birth band has no plotted cell: ",
       paste(setdiff(highlight_bands, cohort_data$group), collapse = ", "))
}

cohort_data <- cohort_data |>
  dplyr::mutate(
    highlight_flag = group %in% highlight_bands,
    cell_label = dplyr::if_else(
      small_cell, "Small cell (flagged)", "Cell size adequate"
    ),
    cell_label = factor(
      cell_label, levels = c("Cell size adequate", "Small cell (flagged)")
    )
  )
cohort_gray_data <- dplyr::filter(cohort_data, !highlight_flag)
cohort_highlight_data <- dplyr::filter(cohort_data, highlight_flag)

# The cell-size legend is drawn only when a plotted cell is small (fewer
# than 100 cases, 05); otherwise it would have a single entry
cohort_legend_position <- if (any(cohort_data$small_cell)) "top" else "none"

# 3) Direct labels near the oldest observed age of each highlighted band,
#    placed above-left in open space so they do not collide with the gray
#    lines
cohort_end_labels <- cohort_highlight_data |>
  dplyr::group_by(group) |>
  dplyr::filter(mean_age == max(mean_age)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    label_text = paste("Born", group),
    label_x = mean_age - 0.5,
    label_y = estimate * 1.7,
    label_hjust = 1
  )

figure_by_cohort <- ggplot(
  mapping = aes(x = mean_age, y = estimate, group = group)
) +
  geom_line(data = cohort_gray_data, color = color_gray, linewidth = 0.5) +
  geom_point(
    data = cohort_gray_data,
    aes(shape = cell_label),
    color = color_gray, fill = color_gray, size = 1.2, stroke = 0.5
  ) +
  geom_line(
    data = cohort_highlight_data,
    aes(color = group),
    linewidth = 0.9
  ) +
  geom_point(
    data = cohort_highlight_data,
    aes(color = group, shape = cell_label, fill = group),
    size = 1.8, stroke = 0.7
  ) +
  geom_text(
    data = cohort_end_labels,
    aes(
      x = label_x, y = label_y, label = label_text, color = group,
      hjust = label_hjust
    ),
    size = label_size_mm,
    family = eig_resolve_family("body", tokens = eig_tokens)
  ) +
  scale_color_manual(values = highlight_colors, guide = "none") +
  scale_fill_manual(values = highlight_colors, guide = "none") +
  scale_shape_manual(
    values = c("Cell size adequate" = 16L, "Small cell (flagged)" = 1L),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = seq(20L, 80L, by = 10L),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_y_log10(
    labels = scales::label_dollar(),
    breaks = c(1000, 10000, 100000, 1000000)
  ) +
  labs(
    title = "Median net worth by age and birth cohort, 1989-2025",
    x = "Mean age of the reference person in the survey year",
    y = "Median net worth\n(2025 dollars, log scale)",
    caption = paste0(
      note_dollars,
      " Gray lines are the other five-year birth bands, 1940-44\n",
      "through 1995-99. Cells at a mean age under 25 and cells with 10 or ",
      "fewer sample households are not shown.\n",
      source_line
    )
  ) +
  eig_theme_ggplot(eig_tokens) +
  theme(
    legend.position = cohort_legend_position,
    legend.justification = "left",
    legend.location = "plot",
    legend.margin = margin(0, 0, 0, 0)
  )

path_figure_2 <- file.path(path_figures, "cohort_median_networth_by_age.png")
ggsave(
  path_figure_2, figure_by_cohort,
  width = 6.5, height = 4.2, units = "in", dpi = 300,
  device = ragg::agg_png
)
message("Wrote ", path_figure_2)
