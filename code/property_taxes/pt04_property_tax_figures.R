# pt04_property_tax_figures -- two diagnostic figures from pt03's tables
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U4 of records/plans/2026-10-19_property-tax-descriptives.md. Reads
# output/property_taxes/tables/pt_progressivity.csv and pt_owns_outright.csv
# and writes two EIG-styled diagnostic figures to
# output/property_taxes/figures/:
#   pt_tax_rates_by_income_2025.png      property tax and federal income tax
#                                        as a share of income, by income
#                                        percentile, 2025, with 95% intervals
#   pt_exemption_by_networth_2025.png    each net worth group's share of the
#                                        property tax that owners with no
#                                        mortgage pay, beside its share of
#                                        those owners, 2025
# Run from the project root as
#   Rscript code/property_taxes/pt04_property_tax_figures.R

################################################################################
###                        1. Load packages and style                        ###
################################################################################

# 1) Stop unless this script sits at its path under the project root
here::i_am("code/property_taxes/pt04_property_tax_figures.R")
library(ggplot2)
source(here::here("code", "figure_theme.R"))

# 2) Paths
path_tables <- here::here("output", "property_taxes", "tables")
path_figures <- here::here("output", "property_taxes", "figures")
dir.create(path_figures, showWarnings = FALSE, recursive = TRUE)

# 3) EIG tokens, fonts, and colors
eig_tokens <- eig_load_tokens()
eig_assert_fonts(eig_tokens, allow_fallback = TRUE)
body_family <- eig_resolve_family("body", tokens = eig_tokens)
color_property <- eig_tokens$EIG_COLORS[["eig_teal_900"]]
color_income <- eig_tokens$EIG_NEUTRALS[["eig_neutral_axis"]]
color_black <- eig_tokens$EIG_COLORS[["eig_black"]]
label_size_mm <- eig_role_size("axis_legend_table_body", eig_tokens) /
  ggplot2::.pt

################################################################################
###                    2. Figure A: tax rates by income group                ###
################################################################################

# 4) The 2025 effective rates (ratios of totals) for the two taxes
income_levels <- c("Less than 20", "20-39.9", "40-59.9", "60-79.9",
                   "80-89.9", "90-100")
rate_measures <- c(
  property_tax_rate_all = "Property tax on own home",
  federal_income_tax_rate = "Federal income tax (simulated)"
)
progressivity <- data.table::fread(file.path(path_tables,
                                             "pt_progressivity.csv"))
rates <- progressivity[year == 2025L & measure %in% names(rate_measures) &
                         group %in% income_levels]
if (nrow(rates) != length(income_levels) * length(rate_measures)) {
  stop("pt_progressivity.csv lacks a 2025 rate for every income group")
}
rates[, group_label := factor(group, levels = income_levels)]
rates[, tax_label := factor(rate_measures[measure], levels = rate_measures)]
rates[, low := 100 * (estimate - 1.96 * se)]
rates[, high := 100 * (estimate + 1.96 * se)]
rates[, pct := 100 * estimate]

figure_a_note <- paste0(
  "Note: Each tax as a share of income, as the ratio of the group's total ",
  "tax to its total income, among households with positive income; ",
  "renters count as paying no property tax. The property tax is what ",
  "owners report paying on their own home. The federal income tax is ",
  "simulated with NBER TAXSIM (2023 law, indexed to 2024), after ",
  "refundable credits, so it is negative where credits exceed tax. ",
  "Groups are percentiles of income over all households. Error bars are ",
  "95% confidence intervals from the SCF's replicate weights and five ",
  "implicates."
)
figure_a_note <- paste0(
  paste(strwrap(figure_a_note, width = 150L), collapse = "\n"), "\n",
  "Source: Federal Reserve Board, Survey of Consumer Finances, 2025; ",
  "EIG calculations."
)

figure_a <- ggplot(rates, aes(x = group_label, y = pct, fill = tax_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_errorbar(aes(ymin = low, ymax = high),
                position = position_dodge(width = 0.75), width = 0.2,
                color = color_black, linewidth = 0.4) +
  geom_hline(yintercept = 0, color = color_black, linewidth = 0.4) +
  scale_fill_manual(values = c(color_property, color_income), name = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = paste0("Property tax takes a larger share of income lower ",
                   "down; the income tax, higher up"),
    subtitle = "Tax as a share of income, by income percentile, 2025",
    x = "Percentile of income", y = NULL, caption = figure_a_note
  ) +
  eig_theme_ggplot(eig_tokens) +
  theme(panel.grid.major.x = element_blank(), legend.position = "top")

ggsave(file.path(path_figures, "pt_tax_rates_by_income_2025.png"), figure_a,
       width = 10, height = 6, units = "in", dpi = 300,
       device = ragg::agg_png)
message("Wrote pt_tax_rates_by_income_2025.png")

################################################################################
###            3. Figure B: who gains from exempting owners outright         ###
################################################################################

# 5) Shares of the tax owners with no mortgage pay, and of those owners
networth_levels <- c("Less than 25", "25-49.9", "50-74.9", "75-89.9",
                     "90-100")
share_measures <- c(
  share_of_owners_no_mortgage = "Share of owners with no mortgage",
  share_of_exemption_dollars = "Share of the property tax they pay"
)
owns_outright <- data.table::fread(file.path(path_tables,
                                             "pt_owns_outright.csv"))
shares <- owns_outright[measure %in% names(share_measures) &
                          cut == "Net worth percentile"]
if (nrow(shares) != length(networth_levels) * length(share_measures)) {
  stop("pt_owns_outright.csv lacks a net worth share for every group")
}
shares[, group_label := factor(group, levels = networth_levels)]
shares[, share_label := factor(share_measures[measure],
                               levels = share_measures)]
shares[, pct := 100 * estimate]
shares[, low := 100 * (estimate - 1.96 * se)]
shares[, high := 100 * (estimate + 1.96 * se)]

figure_b_note <- paste0(
  "Note: Owners with no mortgage have no mortgage, land contract, or home ",
  "equity line on their home. The dark bars are each net worth group's ",
  "share of the property tax those owners pay on their homes, which is ",
  "where an exemption for them would send its dollars. Groups are ",
  "percentiles of net worth over all households. Error bars are 95% ",
  "confidence intervals from the SCF's replicate weights and five ",
  "implicates."
)
figure_b_note <- paste0(
  paste(strwrap(figure_b_note, width = 150L), collapse = "\n"), "\n",
  "Source: Federal Reserve Board, Survey of Consumer Finances, 2025; ",
  "EIG calculations."
)

figure_b <- ggplot(shares, aes(x = group_label, y = pct,
                               fill = share_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_errorbar(aes(ymin = low, ymax = high),
                position = position_dodge(width = 0.75), width = 0.2,
                color = color_black, linewidth = 0.4) +
  geom_text(aes(y = high + 1.5, label = paste0(round(pct), "%")),
            position = position_dodge(width = 0.75), vjust = 0,
            size = label_size_mm, family = body_family) +
  scale_fill_manual(values = c(color_income, color_property), name = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = paste0("Half the property tax paid by owners with no mortgage ",
                   "comes from the wealthiest tenth of households"),
    subtitle = "By percentile of net worth, 2025",
    x = "Percentile of net worth", y = NULL, caption = figure_b_note
  ) +
  eig_theme_ggplot(eig_tokens) +
  theme(panel.grid.major.x = element_blank(), legend.position = "top")

ggsave(file.path(path_figures, "pt_exemption_by_networth_2025.png"),
       figure_b, width = 10, height = 6, units = "in", dpi = 300,
       device = ragg::agg_png)
message("Wrote pt_exemption_by_networth_2025.png")
