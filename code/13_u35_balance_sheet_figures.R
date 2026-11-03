# 13_u35_balance_sheet_figures -- balance sheet figures, under-35 median band
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U2 (figures) of records/plans/2026-10-11_u35-networth-sankey.md,
# with the design settled in that plan's Amendments section. These are
# diagnostic figures, not a draft. It reads
#   output/tables/u35_balance_sheet.csv  (written by 12)
# and writes, to output/figures/:
#   u35_balance_sheet_bars_2022_2025.png    stacked bars of every item,
#                                           2022 and 2025, assets above
#                                           zero and debts below
#   u35_balance_sheet_trend.png             small multiples, 1989-2025, one
#                                           panel per balance-sheet item;
#                                           open circles mark 12's
#                                           definition_break years
#   networth_change_by_age_2022_2025.png    percent change in median net
#                                           worth by age, with 95% CIs
# Run from the project root as
#   Rscript code/13_u35_balance_sheet_figures.R
#
# Every dollar value is on the project basis `pce25` (headline PCE chain-type
# price index, 2025 dollars). Young households exclude young adults living in
# another household's primary economic unit (PROJECT.md, Additional Context).
# Each item is the MEAN among under-35 households whose net worth lies in a
# percentile band around the under-35 median (see 12 for why), so the items
# add up to the band's mean net worth.

################################################################################
###                       1. Load packages and paths                         ###
################################################################################

here::i_am("code/13_u35_balance_sheet_figures.R")

library(ggplot2)

source(here::here("code", "figure_theme.R"))

# 1) The band shown in both figures; the sensitivity band is the thin gray
#    line in Figure B. Change the band here, in one line.
main_band <- "p40_p60"
sensitivity_band <- "p45_p55"
sankey_years <- c(2022L, 2025L)

path_input <- here::here("output", "tables", "u35_balance_sheet.csv")
path_figures <- here::here("output", "figures")

# 2) Stop early when the figure input is missing
if (!file.exists(path_input)) {
  stop(
    "Figure input not found: ", path_input,
    ". Run code/12_u35_balance_sheet.R first."
  )
}
dir.create(path_figures, showWarnings = FALSE, recursive = TRUE)

# 3) Load the EIG tokens and check the fonts
eig_tokens <- eig_load_tokens()
eig_assert_fonts(eig_tokens, allow_fallback = TRUE)
body_family <- eig_resolve_family("body", tokens = eig_tokens)

# Emphasis (panel headers, strip titles, significant changes) uses the body
# family's semibold weight, as the subtitle role does; Open Sans Bold is
# often not installed, and face = "bold" would then fall back to another
# family. Without a semibold alias, use face = "bold".
strong_family <- eig_semibold_family(body_family)
strong_face <- "plain"
if (is.null(strong_family)) {
  strong_family <- body_family
  strong_face <- "bold"
}

# 4) Colors encode the side of the balance sheet only (plan Amendments).
#    eig_tan_500 (about 2.5:1 on white) is kept for the debt fills of
#    Figure A, which are large marks; the thin debt lines and points of
#    Figure B use eig_blue_800 (about 8.3:1), above the 3:1 floor for thin
#    marks (eig-figure-style.md section 1; style item S1)
color_asset_ribbon <- eig_tokens$EIG_COLORS[["eig_green_500"]]
color_asset_node <- eig_tokens$EIG_COLORS[["eig_green_700"]]
color_debt <- eig_tokens$EIG_COLORS[["eig_tan_500"]]
color_debt_line <- eig_tokens$EIG_COLORS[["eig_blue_800"]]
color_white <- eig_tokens$EIG_COLORS[["eig_white"]]
color_networth <- eig_tokens$EIG_COLORS[["eig_teal_900"]]
color_black <- eig_tokens$EIG_COLORS[["eig_black"]]
color_caption <- eig_tokens$EIG_NEUTRALS[["eig_neutral_caption"]]
color_grid <- eig_tokens$EIG_NEUTRALS[["eig_neutral_grid"]]
color_axis <- eig_tokens$EIG_NEUTRALS[["eig_neutral_axis"]]
color_fill_band <- eig_tokens$EIG_NEUTRALS[["eig_neutral_fill_band"]]
ribbon_alpha <- 0.45

# 5) Text sizes in points, from the type roles; geom_text takes mm
label_pt <- eig_role_size("axis_legend_table_body", eig_tokens)
small_pt <- eig_role_size("notes_caption", eig_tokens)
header_pt <- eig_role_size("subtitle", eig_tokens)
text_floor_pt <- eig_tokens$EIG_SIZING$text_floor_pt$print
if (min(label_pt, small_pt, header_pt) < text_floor_pt) {
  stop("A label size is below the ", text_floor_pt, "pt print floor.")
}
label_size_mm <- label_pt / ggplot2::.pt
small_size_mm <- small_pt / ggplot2::.pt
header_size_mm <- header_pt / ggplot2::.pt

# 6) Percentile text for the subtitle and notes, built from the band constant
band_bounds <- as.integer(
  regmatches(main_band, gregexpr("[0-9]+", main_band))[[1]]
)
sensitivity_bounds <- as.integer(
  regmatches(sensitivity_band, gregexpr("[0-9]+", sensitivity_band))[[1]]
)
if (length(band_bounds) != 2L || length(sensitivity_bounds) != 2L) {
  stop("Band names must look like p45_p55: ", main_band, ", ", sensitivity_band)
}

# Ordinal suffix for a percentile (45 -> "45th", 51 -> "51st")
ordinal <- function(x) {
  suffix <- dplyr::case_when(
    x %% 100L %in% 11:13 ~ "th",
    x %% 10L == 1L ~ "st",
    x %% 10L == 2L ~ "nd",
    x %% 10L == 3L ~ "rd",
    TRUE ~ "th"
  )
  paste0(x, suffix)
}
band_text <- paste0(
  "the ", ordinal(band_bounds[1L]), " and ", ordinal(band_bounds[2L]),
  " percentiles"
)
sensitivity_text <- paste0(
  ordinal(sensitivity_bounds[1L]), "-", ordinal(sensitivity_bounds[2L])
)

# Dollars as "$XX.Xk"; changes as "+$X.Xk" or "-$X.Xk" (true minus sign)
format_k <- function(x) {
  paste0("$", formatC(x / 1000, format = "f", digits = 1L), "k")
}
format_change_k <- function(x) {
  sign_text <- dplyr::if_else(x < 0, "−", "+")
  paste0(sign_text, format_k(abs(x)))
}
# Dollars rounded as format_k() prints them, so a change taken between two
# rounded amounts equals the difference a reader computes from the labels
round_shown <- function(x) {
  as.numeric(formatC(x / 1000, format = "f", digits = 1L)) * 1000
}

source_years <- function(years) {
  paste0(
    "Source: Federal Reserve Board, Survey of Consumer Finances, ", years,
    "; EIG calculations."
  )
}
note_dollars_one_line <- paste0(
  "Dollars are 2025 dollars deflated with the PCE price index. ",
  "\"Young households\" exclude young adults living in another ",
  "household's primary economic unit."
)

################################################################################
###                            2. Read the input                             ###
################################################################################

balance_sheet <- readr::read_csv(
  path_input,
  col_types = readr::cols(
    year = readr::col_integer(),
    band = readr::col_character(),
    cutoff_lo = readr::col_double(),
    cutoff_hi = readr::col_double(),
    side = readr::col_character(),
    component = readr::col_character(),
    label = readr::col_character(),
    definition = readr::col_character(),
    statistic = readr::col_character(),
    basis = readr::col_character(),
    estimate = readr::col_double(),
    se = readr::col_double(),
    se_sampling = readr::col_double(),
    se_imputation = readr::col_double(),
    n_unweighted = readr::col_integer(),
    n_weighted = readr::col_double(),
    n_replicates_empty = readr::col_integer(),
    small_cell = readr::col_logical(),
    definition_break = readr::col_character()
  )
)
message("Read ", nrow(balance_sheet), " rows from ", path_input)

for (band_name in c(main_band, sensitivity_band)) {
  if (!band_name %in% balance_sheet$band) {
    stop("Band ", band_name, " not found in ", path_input)
  }
}

################################################################################
###                 3. Display nodes shared by both figures                  ###
################################################################################

# 1) Display nodes, in drawing order. Business equity and other nonfinancial
#    assets merge into one node (plan Amendments: business equity is under
#    $1.2k in every year).
#    `label_wrapped` breaks long names so labels fit beside the nodes.
display_nodes <- tibble::tribble(
  ~node, ~column, ~side, ~display_label, ~label_wrapped,
  "home", "asset", "asset", "Primary residence", "Primary\nresidence",
  "vehicles", "asset", "asset", "Vehicles", "Vehicles",
  "liquid", "asset", "asset", "Liquid accounts", "Liquid\naccounts",
  "retirement", "asset", "asset", "Retirement accounts",
  "Retirement\naccounts",
  "other_financial", "asset", "asset", "Other financial assets",
  "Other financial\nassets",
  "business_other", "asset", "asset", "Business and other nonfinancial",
  "Business and other\nnonfinancial",
  "total_assets", "total", "asset", "Total assets", "Total assets",
  "networth", "right", "networth", "Net worth", "Net worth",
  "mortgage", "right", "debt", "Home mortgage and HELOC",
  "Home mortgage\nand HELOC",
  "education", "right", "debt", "Education loans", "Education\nloans",
  "vehicle_loans", "right", "debt", "Vehicle loans", "Vehicle loans",
  "credit_card", "right", "debt", "Credit card balances",
  "Credit card\nbalances",
  "other_debt", "right", "debt", "Other debt", "Other debt"
)

# 2) Band means of every table component, both bands, all years
band_means <- balance_sheet |>
  dplyr::filter(
    band %in% c(main_band, sensitivity_band),
    statistic == "mean",
    basis == "pce25"
  ) |>
  dplyr::select(year, band, component, estimate, se, n_unweighted,
                definition_break)

# 3) Business and other nonfinancial is one node; 12 estimates it directly
#    (component `business_other`), so its SE includes the covariance of
#    the two parts
node_values <- band_means |>
  dplyr::filter(component %in% c(display_nodes$node, "total_debt")) |>
  dplyr::rename(node = component)

# 4) Every display value must be non-negative for the node heights
if (any(node_values$estimate < 0)) {
  negative_rows <- dplyr::filter(node_values, estimate < 0)
  stop(
    "Negative display value(s): ",
    paste(negative_rows$year, negative_rows$band, negative_rows$node,
          collapse = "; ")
  )
}

# 5) Identities, each within $1: the asset nodes sum to total assets, and
#    the right column (net worth plus debts) sums to total assets
identity_check <- node_values |>
  dplyr::left_join(
    dplyr::select(display_nodes, node, column),
    by = "node", relationship = "many-to-one"
  ) |>
  dplyr::group_by(year, band) |>
  dplyr::summarise(
    asset_sum = sum(estimate[column %in% "asset"]),
    right_sum = sum(estimate[column %in% "right"]),
    total_assets = estimate[node == "total_assets"],
    .groups = "drop"
  )
identity_fail <- dplyr::filter(
  identity_check,
  abs(asset_sum - total_assets) > 1 | abs(right_sum - total_assets) > 1
)
if (nrow(identity_fail) > 0L) {
  stop(
    "Balance-sheet identity fails (|gap| > $1) for: ",
    paste(identity_fail$year, identity_fail$band, collapse = "; ")
  )
}
message(
  "Identities hold within $1 for ", nrow(identity_check),
  " year-band cells"
)

# 6) Under-35 median net worth and band median income
u35_median <- balance_sheet |>
  dplyr::filter(
    band == "all_under35", component == "networth",
    statistic == "median", basis == "pce25"
  ) |>
  dplyr::select(year, median_networth = estimate)
band_income <- balance_sheet |>
  dplyr::filter(
    band == main_band, component == "income", statistic == "median",
    basis == "pce25"
  ) |>
  dplyr::select(year, median_income = estimate)

################################################################################
###         4. Figure A: balance-sheet bars, 2022 and 2025, side by side     ###
################################################################################

# Design (owner direction of 2026-10-12): one stacked bar per year on a
# common dollar scale. Assets stack above zero (green) and debts below zero
# (tan; EIG's diverging pair, since red with green is excluded), in the same
# order in both years, so each item occupies the same slot. Light connector
# bands join each item's segment across the two years, so the eye can see
# which items grew and which shrank. Net worth (assets minus debts) is
# given above each bar. Layout units: x in inches across the plot panel;
# y in thousands of 2025 dollars.
bar_width_in <- 0.85
x_bar_first <- 3.05
x_bar_second <- 5.75
label_pad_in <- 0.12
leader_pad_in <- 0.02
connector_alpha <- 0.30
plot_width_in <- 10
plot_height_in <- 7
line_budget <- 1.25

# Space above the bars for the year headers, in points
top_space_pt <- 3.2 * header_pt

# Greedy de-overlap: label centers ordered top to bottom, block heights,
# and bounds; returns centers with no overlap, as close to the input as
# the bounds allow
spread_labels <- function(center, height, lower, upper) {
  n_labels <- length(center)
  y <- center
  y[1L] <- min(y[1L], upper - height[1L] / 2)
  for (i in seq_len(n_labels)[-1L]) {
    max_center <- y[i - 1L] - height[i - 1L] / 2 - height[i] / 2
    y[i] <- min(y[i], max_center)
  }
  y[n_labels] <- max(y[n_labels], lower + height[n_labels] / 2)
  for (i in rev(seq_len(n_labels - 1L))) {
    min_center <- y[i + 1L] + height[i + 1L] / 2 + height[i] / 2
    y[i] <- max(y[i], min_center)
  }
  y
}

# 1) Main-band values of the display items and net worth, both years
first_year <- sankey_years[1L]
second_year <- sankey_years[2L]
bar_items <- display_nodes |>
  dplyr::filter(column %in% c("asset", "right"))
bar_values <- node_values |>
  dplyr::filter(band == main_band, year %in% sankey_years) |>
  dplyr::inner_join(bar_items, by = "node", relationship = "many-to-one")
if (nrow(bar_values) != nrow(bar_items) * length(sankey_years)) {
  stop("Missing bar values for ", main_band, " in ",
       paste(sankey_years, collapse = ", "))
}

# 2) Change from the first to the second year and its 95% test for
#    independent cross-sections, |change| > 1.96 * sqrt(se_1^2 + se_2^2);
#    the SEs re-estimate the band cutoffs (12). The labels show
#    change_shown, the difference of the two rounded amounts they print;
#    the test uses the unrounded change
item_changes <- bar_values |>
  dplyr::select(node, year, estimate, se) |>
  tidyr::pivot_wider(
    names_from = year, values_from = c(estimate, se), names_sep = "_"
  ) |>
  dplyr::rename(
    level_first = !!paste0("estimate_", first_year),
    level_second = !!paste0("estimate_", second_year),
    se_first = !!paste0("se_", first_year),
    se_second = !!paste0("se_", second_year)
  ) |>
  dplyr::mutate(
    change = level_second - level_first,
    significant_flag = abs(change) > 1.96 * sqrt(se_first^2 + se_second^2),
    change_shown = round(round_shown(level_second) - round_shown(level_first))
  ) |>
  dplyr::select(node, change, change_shown, significant_flag)
message(
  "Changes ", first_year, "-", second_year, " beyond 95% error: ",
  sum(item_changes$significant_flag), " of ", nrow(item_changes), " items"
)

# 3) Segments: assets stack up from 0 and debts down from 0, in display
#    order (the first item sits next to zero), in thousands
bar_segments <- bar_values |>
  dplyr::filter(node != "networth") |>
  dplyr::left_join(item_changes, by = "node", relationship = "many-to-one") |>
  dplyr::mutate(
    node_order = match(node, display_nodes$node),
    value_k = estimate / 1000,
    sign = dplyr::if_else(side == "asset", 1, -1)
  ) |>
  dplyr::arrange(year, side, node_order) |>
  dplyr::group_by(year, side) |>
  dplyr::mutate(
    outer = sign * cumsum(value_k),
    inner = outer - sign * value_k
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    y_top = pmax(inner, outer),
    y_bot = pmin(inner, outer),
    x_min = dplyr::if_else(year == first_year, x_bar_first, x_bar_second),
    x_max = x_min + bar_width_in,
    segment_fill = dplyr::if_else(side == "asset", color_asset_node,
                                  color_debt),
    connector_fill = dplyr::if_else(side == "asset", color_asset_ribbon,
                                    color_debt)
  )

# 4) Identities, within $1: the stacks equal total assets and total debt,
#    and assets minus debts equal net worth
totals <- node_values |>
  dplyr::filter(band == main_band, year %in% sankey_years,
                node %in% c("total_assets", "total_debt", "networth")) |>
  dplyr::select(year, node, estimate) |>
  tidyr::pivot_wider(names_from = node, values_from = estimate)
stack_check <- bar_segments |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    asset_stack = 1000 * max(y_top), debt_stack = -1000 * min(y_bot),
    .groups = "drop"
  ) |>
  dplyr::left_join(totals, by = "year", relationship = "one-to-one")
if (any(abs(stack_check$asset_stack - stack_check$total_assets) > 1) ||
      any(abs(stack_check$debt_stack - stack_check$total_debt) > 1) ||
      any(abs(stack_check$total_assets - stack_check$total_debt -
                stack_check$networth) > 1)) {
  stop("Bar stacks do not match total assets, total debt, and net worth")
}

# 5) Net worth by year, for the year headers
networth_bars <- dplyr::select(stack_check, year, networth)

# 6) Connector bands: each item's segment in the first year joined to the
#    same item's segment in the second year
connector_list <- list()
for (item_node in unique(bar_segments$node)) {
  first_segment <- dplyr::filter(bar_segments, node == item_node,
                                 year == first_year)
  second_segment <- dplyr::filter(bar_segments, node == item_node,
                                  year == second_year)
  connector_list[[item_node]] <- data.frame(
    node = item_node,
    x = c(first_segment$x_max, second_segment$x_min,
          second_segment$x_min, first_segment$x_max),
    y = c(first_segment$y_top, second_segment$y_top,
          second_segment$y_bot, first_segment$y_bot),
    connector_fill = first_segment$connector_fill
  )
}
connectors <- dplyr::bind_rows(connector_list)

# 7) Title, subtitle, note, and source lines
band_sizes <- node_values |>
  dplyr::filter(band == main_band, year %in% sankey_years,
                node == "networth") |>
  dplyr::arrange(year)
band_income_bars <- band_income |>
  dplyr::filter(year %in% sankey_years) |>
  dplyr::arrange(year)
u35_median_bars <- u35_median |>
  dplyr::filter(year %in% sankey_years) |>
  dplyr::arrange(year)
if (nrow(band_sizes) != 2L || nrow(band_income_bars) != 2L ||
      nrow(u35_median_bars) != 2L) {
  stop("Missing band size, band income, or median for a figure year.")
}
nl_note <- "\n"
bars_title <- paste0(
  "Balance sheet of under-35 households near the median net worth, ",
  first_year, " and ", second_year
)
bars_subtitle <- paste0(
  "Mean of each item among households between ", band_text,
  " of net worth; thousands of 2025 dollars"
)
bars_note <- paste0(
  "Note: Each segment is the mean of one item among under-35 households ",
  "between ", band_text, " of net worth. Assets (green) stack above",
  nl_note,
  "zero and debts (tan) below it; net worth (assets minus debts) is given ",
  "above each bar. Bands join each item across the two years. Changes in bold",
  nl_note,
  "exceed 1.96 combined standard errors (sampling and imputation, with the ",
  "band cutoffs re-estimated); the others are within that 95% error.",
  nl_note,
  "Each change gives the item's own change and its effect on net worth: ",
  "an asset that rose or a debt that fell raises net worth.", nl_note,
  "The band holds ", band_sizes$n_unweighted[1L], " (", first_year,
  ") and ", band_sizes$n_unweighted[2L], " (", second_year, ") sample ",
  "households; the SCF is a repeated cross-section, so the two bands hold ",
  "different households.", nl_note,
  "Business equity, other real estate, and other nonfinancial assets are ",
  "combined; few households in the band hold them. Income is the band's ",
  "median prior-year income.", nl_note,
  "Dollars are 2025 dollars deflated with the PCE price index. ",
  "\"Young households\" exclude young adults living in another household's ",
  "primary economic unit.", nl_note,
  source_years(paste(sankey_years, collapse = " and "))
)

# 8) Measure the plot panel height on the output device, from a skeleton
#    with the same title, subtitle, note, and theme, so label heights in
#    points convert into thousands of dollars
bars_theme <- eig_theme_ggplot(eig_tokens) +
  theme(
    axis.text.x = element_blank(),
    legend.position = "none"
  )
bars_skeleton <- ggplot() +
  geom_blank(aes(x = c(0, 1), y = c(0, 1))) +
  labs(
    title = bars_title, subtitle = bars_subtitle,
    x = NULL, y = NULL, caption = bars_note
  ) +
  bars_theme
path_measure <- tempfile(fileext = ".png")
ragg::agg_png(
  path_measure, width = plot_width_in, height = plot_height_in,
  units = "in", res = 72
)
skeleton_grob <- ggplotGrob(bars_skeleton)
null_height_flag <- grid::unitType(skeleton_grob$heights) == "null"
fixed_height_in <- sum(grid::convertHeight(
  skeleton_grob$heights[!null_height_flag], "in", valueOnly = TRUE
))
invisible(grDevices::dev.off())
unlink(path_measure)
panel_height_pt <- (plot_height_in - fixed_height_in) * 72
data_top_k <- max(bar_segments$y_top)
data_bottom_k <- min(bar_segments$y_bot)
bottom_space_k <- 0.04 * (data_top_k - data_bottom_k)
k_per_pt <- (data_top_k - data_bottom_k + bottom_space_k) /
  (panel_height_pt - top_space_pt)
if (k_per_pt <= 0) {
  stop("Figure A panel height too small")
}
y_limits_k <- c(data_bottom_k - bottom_space_k,
                data_top_k + top_space_pt * k_per_pt)
message("Figure A panel height: ", round(panel_height_pt / 72, 2), " in")

# 9) Labels: first year to the left of its bar ("Name  $XX.Xk"); second
#    year to the right ("Name  $XX.Xk" and, below, the change in words
#    with its signed effect on net worth, so a debt that fell reads as
#    raising net worth; semibold black when beyond 95% error, regular gray
#    otherwise). Labels are spread apart within each side of zero, with
#    leader lines when moved
label_gap_k <- 0.40 * label_pt * k_per_pt
bar_labels <- bar_segments |>
  dplyr::mutate(
    main_text = paste0(display_label, "  ", format_k(estimate)),
    networth_effect = dplyr::if_else(
      side == "debt", -change_shown, change_shown
    ),
    change_text = dplyr::case_when(
      year != second_year ~ NA_character_,
      change_shown == 0 ~ paste0("unchanged since ", first_year),
      TRUE ~ paste0(
        dplyr::if_else(change_shown < 0, "fell ", "rose "),
        format_k(abs(change_shown)), " since ", first_year, " (net worth ",
        format_change_k(networth_effect), ")"
      )
    ),
    main_height = label_pt * line_budget * k_per_pt,
    change_height = dplyr::if_else(
      is.na(change_text), 0, small_pt * line_budget * k_per_pt
    ),
    block_height = main_height + change_height + label_gap_k,
    segment_center = (y_top + y_bot) / 2
  )
label_list <- list()
for (label_year in sankey_years) {
  for (label_side in c("asset", "debt")) {
    side_rows <- bar_labels |>
      dplyr::filter(year == label_year, side == label_side) |>
      dplyr::arrange(dplyr::desc(segment_center))
    side_rows$block_center <- spread_labels(
      side_rows$segment_center, side_rows$block_height,
      lower = if (label_side == "asset") 0 else y_limits_k[1L],
      upper = if (label_side == "asset") data_top_k else 0
    )
    label_list[[length(label_list) + 1L]] <- side_rows
  }
}
bar_labels <- dplyr::bind_rows(label_list) |>
  dplyr::mutate(
    left_flag = year == first_year,
    block_top = block_center + (block_height - label_gap_k) / 2,
    main_y = block_top - main_height / 2,
    change_y = block_top - main_height - change_height / 2,
    label_x = dplyr::if_else(
      left_flag, x_bar_first - label_pad_in,
      x_bar_second + bar_width_in + label_pad_in
    ),
    label_hjust = dplyr::if_else(left_flag, 1, 0),
    leader_x0 = dplyr::if_else(left_flag, x_bar_first - leader_pad_in,
                               x_bar_second + bar_width_in + leader_pad_in),
    leader_x1 = dplyr::if_else(left_flag, label_x + leader_pad_in,
                               label_x - leader_pad_in),
    moved_flag = abs(block_center - segment_center) >
      0.3 * label_pt * k_per_pt,
    change_emphasis_flag = !is.na(change_text) & significant_flag
  )
leader_lines <- dplyr::filter(bar_labels, moved_flag)

# 10) Year headers with net worth and the band's median income, above each
#     bar
networth_change <- dplyr::filter(item_changes, node == "networth")
year_headers <- networth_bars |>
  dplyr::left_join(band_income_bars, by = "year",
                   relationship = "one-to-one") |>
  dplyr::mutate(
    header_x = dplyr::if_else(year == first_year, x_bar_first,
                              x_bar_second) + bar_width_in / 2,
    year_y = y_limits_k[2L],
    detail_y = year_y - header_pt * 1.45 * k_per_pt,
    detail_text = paste0(
      "Net worth ", format_k(networth),
      dplyr::if_else(
        year == second_year,
        paste0(" (", format_change_k(networth_change$change_shown), ")"), ""
      ),
      "\nMedian income ", format_k(median_income)
    )
  )

# 11) Draw
figure_bars <- ggplot() +
  geom_hline(yintercept = 0, color = color_black, linewidth = 0.4) +
  geom_polygon(
    data = connectors,
    aes(x = x, y = y, group = node, fill = connector_fill),
    alpha = connector_alpha,
    color = eig_tokens$EIG_COLORS[["eig_white"]], linewidth = 0.6
  ) +
  geom_rect(
    data = bar_segments,
    aes(xmin = x_min, xmax = x_max, ymin = y_bot, ymax = y_top,
        fill = segment_fill),
    color = eig_tokens$EIG_COLORS[["eig_white"]], linewidth = 0.4
  ) +
  geom_segment(
    data = leader_lines,
    aes(x = leader_x0, xend = leader_x1, y = segment_center, yend = main_y),
    color = color_caption, linewidth = 0.25
  ) +
  geom_text(
    data = bar_labels,
    aes(x = label_x, y = main_y, label = main_text, hjust = label_hjust),
    size = label_size_mm, color = color_black, family = body_family
  ) +
  geom_text(
    data = dplyr::filter(bar_labels, !is.na(change_text),
                         !change_emphasis_flag),
    aes(x = label_x, y = change_y, label = change_text, hjust = label_hjust),
    size = small_size_mm, color = color_caption, family = body_family
  ) +
  geom_text(
    data = dplyr::filter(bar_labels, change_emphasis_flag),
    aes(x = label_x, y = change_y, label = change_text, hjust = label_hjust),
    size = small_size_mm, color = color_black, family = strong_family,
    fontface = strong_face
  ) +
  geom_text(
    data = year_headers,
    aes(x = header_x, y = year_y, label = year),
    hjust = 0.5, vjust = 1, size = header_size_mm, color = color_black,
    family = strong_family, fontface = strong_face
  ) +
  geom_text(
    data = year_headers,
    aes(x = header_x, y = detail_y, label = detail_text),
    hjust = 0.5, vjust = 1, size = small_size_mm, lineheight = 1,
    color = color_networth, family = body_family
  ) +
  scale_fill_identity() +
  scale_y_continuous(
    breaks = seq(-100, 150, by = 25),
    labels = function(x) {
      dplyr::if_else(x == 0, "$0",
                     paste0(dplyr::if_else(x < 0, "−$", "$"), abs(x), "k"))
    }
  ) +
  coord_cartesian(
    xlim = c(0, plot_width_in - 0.4), ylim = y_limits_k,
    expand = FALSE, clip = "off"
  ) +
  labs(
    title = bars_title,
    subtitle = bars_subtitle,
    x = NULL, y = NULL,
    caption = bars_note
  ) +
  bars_theme

path_figure_a <- file.path(
  path_figures,
  paste0("u35_balance_sheet_bars_", first_year, "_", second_year, ".png")
)
ggsave(
  path_figure_a, figure_bars,
  width = plot_width_in, height = plot_height_in, units = "in", dpi = 300,
  device = ragg::agg_png
)
message("Wrote ", path_figure_a)

################################################################################
###               5. Figure B: small multiples, 1989-2025                    ###
################################################################################

# 1) Panels: net worth, the two totals, then the asset and debt nodes
trend_panels <- tibble::tibble(
  node = c("networth", "total_assets", "total_debt",
           display_nodes$node[display_nodes$column != "total" &
                                display_nodes$node != "networth"]),
  panel_side = c("networth", "asset", "debt", rep("asset", 6L),
                 rep("debt", 5L))
) |>
  dplyr::left_join(
    dplyr::select(display_nodes, node, display_label),
    by = "node", relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    display_label = dplyr::if_else(
      node == "total_debt", "Total debt", display_label
    ),
    line_color = dplyr::case_when(
      panel_side == "asset" ~ color_asset_node,
      panel_side == "debt" ~ color_debt_line,
      TRUE ~ color_networth
    )
  )
if (nrow(trend_panels) != 14L || anyNA(trend_panels$display_label)) {
  stop("Figure B panel list is not the expected 14 panels.")
}

# 2) Main band with its 95% interval (truncated at zero, since every item
#    is non-negative), and the sensitivity band
trend_data <- node_values |>
  dplyr::inner_join(trend_panels, by = "node", relationship = "many-to-one") |>
  dplyr::mutate(
    panel = factor(display_label, levels = trend_panels$display_label),
    ci_low = pmax(estimate - 1.96 * se, 0),
    ci_high = estimate + 1.96 * se
  )
trend_main <- dplyr::filter(trend_data, band == main_band)
trend_sensitivity <- dplyr::filter(trend_data, band == sensitivity_band)

# 2a) Main-band points at a definition break (12's `definition_break`: the
#     first survey measured under a changed SCF definition of the item),
#     drawn as open circles over the line
trend_breaks <- dplyr::filter(trend_main, !is.na(definition_break))
message(
  "Figure B: open circles at ", nrow(trend_breaks), " panel-years: ",
  paste(trend_breaks$display_label, trend_breaks$year, collapse = "; ")
)
trend_median <- u35_median |>
  dplyr::mutate(
    panel = factor("Net worth", levels = trend_panels$display_label)
  )
message(
  "Figure B: ", nrow(trend_main), " main-band and ",
  nrow(trend_sensitivity), " sensitivity points"
)

# 3) Direct labels in the net worth panel, where the lines nearly coincide
networth_main <- dplyr::filter(trend_main, node == "networth")
networth_labels <- tibble::tibble(
  panel = factor("Net worth", levels = trend_panels$display_label),
  year = c(2004L, 2013L),
  label_text = c("band mean (solid)", "under-35 median (dashed)"),
  label_color = c(color_networth, color_networth)
)
networth_labels$y <- c(
  networth_main$ci_high[networth_main$year == 2004L] + 4000,
  networth_main$ci_low[networth_main$year == 2013L] - 4000
)
networth_labels$vjust <- c(0, 1)

survey_years <- sort(unique(trend_main$year))
band_n_range <- range(
  trend_main$n_unweighted[trend_main$node == "networth"]
)

trend_note <- paste0(
  "Note: Lines are the mean of each item among under-35 households between ",
  band_text, " of net worth. Y scales\n",
  "differ by panel and start at zero. Shaded bands are 95% confidence ",
  "intervals (truncated at zero). The thin gray line is the same\n",
  "mean for the ", sensitivity_text, " percentile band. ",
  "The main band holds ", band_n_range[1L], " to ", band_n_range[2L],
  " sample households per survey. Open circles mark surveys\n",
  "in which the SCF changed how that item is measured. Business equity, ",
  "other real estate, and other nonfinancial assets are combined.\n",
  "Dollars are 2025 dollars deflated with the PCE price index. ",
  "\"Young households\" exclude young adults living in another household's\n",
  "primary economic unit. The SCF is a repeated cross-section.\n",
  source_years(paste0(min(survey_years), "-", max(survey_years)))
)

figure_trend <- ggplot(trend_main, aes(x = year, y = estimate)) +
  geom_ribbon(
    aes(ymin = ci_low, ymax = ci_high),
    fill = color_fill_band, color = NA
  ) +
  geom_line(
    data = trend_sensitivity,
    color = color_axis, linewidth = 0.4
  ) +
  geom_line(
    data = trend_median,
    aes(y = median_networth),
    color = color_networth, linewidth = 0.5, linetype = "dashed"
  ) +
  geom_line(aes(color = line_color), linewidth = 0.7) +
  geom_point(aes(color = line_color), size = 1) +
  geom_point(
    data = trend_breaks,
    aes(color = line_color),
    shape = 21L, fill = color_white, size = 2, stroke = 0.7
  ) +
  geom_text(
    data = networth_labels,
    aes(y = y, label = label_text, color = label_color, vjust = vjust),
    size = small_size_mm, family = body_family
  ) +
  scale_color_identity() +
  scale_x_continuous(
    breaks = seq(min(survey_years), max(survey_years), by = 12L),
    expand = expansion(mult = 0.04)
  ) +
  scale_y_continuous(
    labels = function(x) {
      dplyr::if_else(
        x == 0, "$0",
        paste0("$", trimws(formatC(x / 1000, format = "g", digits = 3L)), "k")
      )
    },
    expand = expansion(mult = c(0, 0.06))
  ) +
  expand_limits(y = 0) +
  facet_wrap(~panel, ncol = 3L, scales = "free_y") +
  labs(
    title = paste0(
      "Balance sheet of under-35 households near the median net worth, ",
      min(survey_years), "-", max(survey_years)
    ),
    subtitle = paste0(
      "Mean of each item, households between ", band_text,
      " of net worth; 2025 dollars"
    ),
    x = NULL, y = NULL,
    caption = trend_note
  ) +
  eig_theme_ggplot(eig_tokens) +
  theme(
    strip.text = element_text(
      size = label_pt, hjust = 0, family = strong_family,
      face = strong_face, color = color_black
    ),
    panel.spacing.x = unit(0.25, "in"),
    panel.spacing.y = unit(0.12, "in"),
    legend.position = "none"
  )

path_figure_b <- file.path(path_figures, "u35_balance_sheet_trend.png")
ggsave(
  path_figure_b, figure_trend,
  width = 8, height = 10, units = "in", dpi = 300,
  device = ragg::agg_png
)
message("Wrote ", path_figure_b)

################################################################################
###        6. Figure C: change in median net worth by age, with 95% CI       ###
################################################################################

# A version of the chart in the WSJ article on the 2025 SCF (owner request
# of 2026-10-12): the percent change in real median net worth from 2022 to
# 2025 by the Fed's age groups, here with 95% error bars. It uses the
# project basis (pce25); the Fed's CPI-U-RS basis, which the WSJ chart
# uses, differs by at most 0.2 percentage points (table column `basis`).
# The under-35 bar is in the first series color and the others in gray.
path_age_input <- here::here("output", "tables", "networth_change_by_age.csv")
if (!file.exists(path_age_input)) {
  stop("Figure input not found: ", path_age_input,
       ". Run code/12_u35_balance_sheet.R first.")
}

# 1) Read the change table (written by 12) with explicit types
age_change <- readr::read_csv(
  path_age_input,
  col_types = readr::cols(
    basis = readr::col_character(),
    age_group = readr::col_character(),
    pct_change = readr::col_double(),
    ci_low = readr::col_double(),
    ci_high = readr::col_double(),
    significant_flag = readr::col_logical(),
    ci_method = readr::col_character(),
    .default = readr::col_double()
  )
) |>
  dplyr::filter(basis == "pce25")
age_levels <- c("less than 35", "35-44", "45-54", "55-64", "65-74",
                "75 and over")
age_labels <- c("Less than 35", "35-44", "45-54", "55-64", "65-74",
                "75 or more")
if (nrow(age_change) != length(age_levels) ||
      !setequal(age_change$age_group, age_levels)) {
  stop("Figure C needs one pce25 row for each of the six age groups")
}
# The intervals are on the log-ratio scale (12, step 27); the note says so
if (any(age_change$ci_method != "log ratio") ||
      anyNA(age_change[, c("pct_change", "ci_low", "ci_high")])) {
  stop("Figure C needs log-ratio intervals for every age group (12)")
}

# 2) Order the groups as the WSJ chart does (youngest at the bottom, as
#    the first factor level), and build the text column: the change and
#    its 95% interval
format_pct <- function(x) {
  paste0(dplyr::if_else(x < 0, "−", "+"), formatC(abs(x), format = "f",
                                                  digits = 0L), "%")
}
age_change <- age_change |>
  dplyr::mutate(
    age_label = factor(age_labels[match(age_group, age_levels)],
                       levels = age_labels),
    bar_fill = dplyr::if_else(age_group == "less than 35", color_networth,
                              color_axis),
    interval_text = paste0(
      format_pct(pct_change), "  (", format_pct(ci_low), " to ",
      format_pct(ci_high), ")"
    )
  )
x_text <- max(age_change$ci_high) + 12
x_limits <- c(min(age_change$ci_low) - 4, x_text + 40)

age_note <- paste0(
  "Note: Bars are the percent change in median net worth from 2022 to ",
  "2025, in 2025 dollars deflated with the PCE price index. Error bars and\n",
  "the intervals in parentheses are 95% confidence intervals from the SCF's ",
  "replicate weights and five implicates, computed on the log of the ratio\n",
  "of the two independent medians. An interval that crosses zero ",
  "is not distinguishable from no change. Age is the reference person's.\n",
  "\"Young households\" exclude young adults living in another household's ",
  "primary economic unit.\n",
  source_years("2022 and 2025")
)

# 3) Draw
figure_age <- ggplot(age_change, aes(y = age_label)) +
  geom_col(aes(x = pct_change, fill = bar_fill), width = 0.62) +
  geom_vline(xintercept = 0, color = color_black, linewidth = 0.4) +
  geom_errorbar(
    aes(xmin = ci_low, xmax = ci_high),
    width = 0.22, color = color_black, linewidth = 0.4,
    orientation = "y"
  ) +
  geom_text(
    aes(x = x_text, label = interval_text),
    hjust = 0, size = label_size_mm, color = color_black, family = body_family
  ) +
  annotate(
    "text", x = x_text, y = length(age_levels) + 0.75,
    label = "Change (95% interval)", hjust = 0, vjust = 0,
    size = label_size_mm, color = color_black, family = strong_family,
    fontface = strong_face
  ) +
  scale_fill_identity() +
  scale_x_continuous(
    breaks = seq(-50, 75, by = 25),
    labels = function(x) {
      dplyr::if_else(x == 0, "0", paste0(dplyr::if_else(x < 0, "−", "+"),
                                         abs(x), "%"))
    }
  ) +
  coord_cartesian(xlim = x_limits, ylim = c(0.5, length(age_levels) + 1),
                  expand = FALSE, clip = "off") +
  labs(
    title = paste0(
      "Change from 2022 to 2025 in median net worth, adjusted for ",
      "inflation, by age"
    ),
    subtitle = "Percent change, with 95% confidence intervals",
    x = NULL, y = NULL,
    caption = age_note
  ) +
  eig_theme_ggplot(eig_tokens) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

path_figure_c <- file.path(path_figures, "networth_change_by_age_2022_2025.png")
ggsave(
  path_figure_c, figure_age,
  width = 8, height = 4.6, units = "in", dpi = 300,
  device = ragg::agg_png
)
message("Wrote ", path_figure_c)
