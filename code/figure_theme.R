# figure_theme -- load the EIG figure theme, or a public fallback
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

# Sourced by code/06, 13, 17, 19, and code/property_taxes/pt04; it only
# defines functions and reads no data, so it has no opening block (as for
# 00). Plan: records/plans/2026-11-03_replication-package-cleanup.md (U4).
#
# Inside the EIG project repository the project .Rprofile sets the option
# eig.theme_path to the internal EIG theme; this file sources it, so the
# figures are drawn exactly as before. In the public replication
# package that theme (and its licensed fonts) is not shipped, so this file
# defines public stand-ins for the six theme functions the figure scripts
# call: the same brand colors and type sizes, a plain theme_minimal()
# base, and the system sans font. The numbers in every figure are the
# same either way; only the typeface and some styling differ.

# 1) The internal theme, when the option names a file that exists; an
#    option set to a missing file warns and falls back
path_eig_theme <- getOption("eig.theme_path", "")

if (nzchar(path_eig_theme) && !file.exists(path_eig_theme)) {
  warning(
    "figure_theme.R: option eig.theme_path names a missing file (",
    path_eig_theme, "); using the public fallback theme.",
    call. = FALSE
  )
}

if (nzchar(path_eig_theme) && file.exists(path_eig_theme)) {
  source(path_eig_theme)
} else {
  message(
    "figure_theme.R: the internal EIG theme is not set in this checkout; ",
    "using the public fallback theme (system sans font)."
  )

  # 2) Brand colors and type sizes the figure scripts read, copied from the
  #    EIG style tokens (version 2.0.0, generated 2026-10-05)
  eig_load_tokens <- function(path = NULL) {
    list(
      EIG_COLORS = c(
        "eig_teal_900" = "#024140",
        "eig_green_700" = "#19644D",
        "eig_green_500" = "#5E9C86",
        "eig_blue_800" = "#194F8B",
        "eig_tan_500" = "#D6936F",
        "eig_black" = "#000000",
        "eig_white" = "#FFFFFF"
      ),
      EIG_NEUTRALS = c(
        "eig_neutral_grid" = "#D9D9D9",
        "eig_neutral_axis" = "#CFCFCF",
        "eig_neutral_caption" = "#444444",
        "eig_neutral_fill_band" = "#ECECEC"
      ),
      EIG_TYPOGRAPHY = list(
        roles = list(
          chart_title = list(size_pt = 15),
          subtitle = list(size_pt = 11),
          axis_legend_table_body = list(size_pt = 9),
          notes_caption = list(size_pt = 8)
        )
      ),
      EIG_SIZING = list(text_floor_pt = list(print = 7))
    )
  }

  # 3) Fonts: the system sans family, with no semibold variant
  eig_assert_fonts <- function(tokens = NULL, allow_fallback = FALSE) {
    invisible(TRUE)
  }

  eig_resolve_family <- function(role = c("headline", "body"),
                                 tokens = NULL, ...) {
    "sans"
  }

  eig_semibold_family <- function(family) {
    NULL
  }

  eig_role_size <- function(role_name, tokens) {
    tokens$EIG_TYPOGRAPHY$roles[[role_name]]$size_pt
  }

  # 4) A plain ggplot2 theme with the same sizes, colors, and gridlines as
  #    the internal theme
  eig_theme_ggplot <- function(tokens = NULL, base_size = NULL) {
    if (is.null(tokens)) tokens <- eig_load_tokens()
    text_pt <- eig_role_size("axis_legend_table_body", tokens)
    black <- tokens$EIG_COLORS[["eig_black"]]
    white <- tokens$EIG_COLORS[["eig_white"]]

    ggplot2::theme_minimal(base_size = text_pt, base_family = "sans") +
      ggplot2::theme(
        plot.title.position = "plot",
        plot.caption.position = "plot",
        text = ggplot2::element_text(color = black),
        plot.title = ggplot2::element_text(
          face = "bold",
          size = eig_role_size("chart_title", tokens),
          color = black,
          hjust = 0
        ),
        plot.subtitle = ggplot2::element_text(
          face = "bold",
          size = eig_role_size("subtitle", tokens),
          color = tokens$EIG_COLORS[["eig_teal_900"]],
          hjust = 0
        ),
        axis.title = ggplot2::element_text(size = text_pt, color = black),
        axis.text = ggplot2::element_text(size = text_pt, color = black),
        legend.title = ggplot2::element_text(size = text_pt, color = black),
        legend.text = ggplot2::element_text(size = text_pt, color = black),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(
          color = tokens$EIG_NEUTRALS[["eig_neutral_grid"]],
          linewidth = 0.3
        ),
        plot.background = ggplot2::element_rect(fill = white, color = NA),
        panel.background = ggplot2::element_rect(fill = white, color = NA),
        plot.caption = ggplot2::element_text(
          size = eig_role_size("notes_caption", tokens),
          color = tokens$EIG_NEUTRALS[["eig_neutral_caption"]],
          hjust = 0
        )
      )
  }
}
