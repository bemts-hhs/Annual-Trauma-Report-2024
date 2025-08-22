###_____________________________________________________________________________
# Deaths ----
###_____________________________________________________________________________

####
# You must first run data_load.R and setup.R before running this script
####

###_____________________________________________________________________________
# Plots for the death data ----
###_____________________________________________________________________________

# top 10 causes of death in the US
top_10_causes_us_plot <- death_cdc_wonder_nation_all_2019_2023_aggregate |>
  dplyr::mutate(
    y_label_push = dplyr::if_else(
      Deaths >= 3100000,
      185000,
      dplyr::if_else(
        Deaths >= 1000000,
        115000,
        dplyr::if_else(
          Deaths >= 500000,
          250000,
          dplyr::if_else(Deaths >= 100000, Deaths + 250000, 100000)
        )
      )
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = reorder(
      stringr::str_wrap(`UCD - 15 Leading Causes of Death`, width = 20),
      Deaths
    ),
    y = Deaths,
    fill = Deaths,
    label = traumar::pretty_number(Deaths, n_decimal = 1)
  )) +
  ggplot2::geom_col(position = ggplot2::position_dodge2(width = 0.5)) +
  ggplot2::geom_text(
    ggplot2::aes(
      y = y_label_push
    ),
    color = dplyr::if_else(
      death_cdc_wonder_nation_all_2019_2023_aggregate$Deaths < 500000,
      "gray",
      "white"
    ),
    family = "Work Sans",
    fontface = "bold",
    size = 8
  ) +
  ggplot2::annotate(
    geom = "segment",
    x = "Accidents",
    xend = "Accidents",
    y = 1600000,
    yend = 1300000,
    arrow = ggplot2::arrow(type = "closed")
  ) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    x = "",
    y = "",
    title = "Top 10 Causes of Death Among All Age Groups in the U.S.",
    subtitle = "Source: CDC WONDER | 2019-2023",
    caption = "Note: 2023 data used in this report via CDC WONDER are complete.",
    fill = "# Deaths"
  ) +
  ggplot2::guides(color = "none") +
  ggplot2::scale_y_continuous(
    labels = function(x) traumar::pretty_number(x, n_decimal = 2)
  ) +
  paletteer::scale_fill_paletteer_c(
    palette = "ggthemes::Orange-Gold",
    direction = 1,
    labels = function(x) traumar::pretty_number(x, n_decimal = 1)
  ) +
  traumar::theme_cleaner(
    base_size = 15,
    title_text_size = 20,
    subtitle_text_size = 18,
    vjust_title = 1.75,
    vjust_subtitle = 1,
    legend_position = "inside",
    legend.position.inside = c(0.75, 0.25)
  )

# save the top causes of death in the US plot

ggplot2::ggsave(
  filename = "top_10_causes_us_plot.png",
  plot = top_10_causes_us_plot,
  path = plot_folder,
  height = 9,
  width = 9 * (16 / 9)
)

# gt() tbl of deaths among 1-44 age population in the U.S.
death_cdc_wisqars_all_1_44_tbl <- death_cdc_wonder_nation_1_44_2019_2023_aggregate |>
  dplyr::select(
    -matches("crude|population|code"),
    `UCD - 15 Leading Causes of Death`,
    Deaths,
    `Age Adjusted Rate`,
    `Age Adjusted Rate Lower 95% Confidence Interval`,
    `Age Adjusted Rate Upper 95% Confidence Interval`
  ) |>
  gt::gt() |>
  gt::tab_header(
    title = "Top 10 Causes of Death Among Persons Ages 1-44 in the U.S.",
    subtitle = "Source: CDC WONDER | 2019-2023"
  ) |>
  gt::tab_source_note(
    source_note = gt::md(paste0(
      fontawesome::fa("magnifying-glass"),
      " 2024 Data included here are provisional."
    ))
  ) |>
  gt::tab_source_note(
    source_note = gt::md(paste0(
      fontawesome::fa("sticky-note"),
      " Injuries remain in the leading causes of death for the years 2019-2023 in the U.S."
    ))
  ) |>
  gt::tab_footnote(
    footnote = "Rate per 100,000 population.",
    locations = gt::cells_column_labels(columns = `Age Adjusted Rate`)
  ) |>
  gt::opt_footnote_marks(marks = "standard") |>
  gt::fmt_number(columns = Deaths, drop_trailing_zeros = TRUE) |>
  gtExtras::gt_duplicate_column(
    column = `Age Adjusted Rate`,
    after = `Age Adjusted Rate`,
    dupe_name = "Rate_Bar"
  ) |>
  gtExtras::gt_plt_bar(column = Rate_Bar, color = "coral") |>
  gt::cols_label(
    `Age Adjusted Rate` = "Age Adjusted Rate (95% CI)",
    Rate_Bar = "",
    `UCD - 15 Leading Causes of Death` = "Cause of Death"
  ) |>
  gt::cols_merge(
    columns = c(
      `Age Adjusted Rate`,
      `Age Adjusted Rate Lower 95% Confidence Interval`,
      `Age Adjusted Rate Upper 95% Confidence Interval`
    ),
    pattern = "{1} ({2}&mdash;{3})"
  ) |>
  tab_style_hhs(border_cols = c(Deaths, `Age Adjusted Rate`))

# top 5 causes of death in Iowa
top_5_causes_iowa_plot <- death_cdc_wonder_iowa_all_2023 |>
  dplyr::slice_head(n = 5) |>
  ggplot2::ggplot(ggplot2::aes(
    x = reorder(
      stringr::str_wrap(`UCD - 15 Leading Causes of Death`, width = 10),
      -Deaths
    ),
    y = Deaths,
    fill = Deaths,
    label = traumar::pretty_number(Deaths)
  )) +
  ggplot2::geom_col(
    width = 0.75,
    position = ggplot2::position_dodge(width = 1)
  ) +
  ggplot2::geom_text(
    family = "Work Sans",
    size = 8,
    fontface = "bold",
    nudge_y = 200
  ) +
  ggplot2::annotate(
    geom = "segment",
    x = stringr::str_wrap("Accidents", width = 10),
    xend = stringr::str_wrap("Accidents", width = 10),
    y = 3500,
    yend = 2300,
    arrow = ggplot2::arrow(type = "closed")
  ) +
  ggplot2::labs(
    x = "",
    y = "",
    title = "Top 5 Causes of Death in Iowa Among All Age Groups",
    subtitle = "Source: Iowa Death Certificate Data | 2023"
  ) +
  paletteer::scale_fill_paletteer_c(
    palette = "ggthemes::Orange-Gold",
    direction = 1,
    labels = function(x) traumar::pretty_number(x)
  ) +
  traumar::theme_cleaner(
    base_size = 15,
    title_text_size = 20,
    subtitle_text_size = 18,
    vjust_title = 1.75,
    vjust_subtitle = 1,
    axis.text.y = ggplot2::element_blank(),
    legend_position = "inside",
    legend.position.inside = c(0.75, 0.75),
    facets = TRUE
  )

# save the top 10 causes of death in Iowa plot

ggplot2::ggsave(
  filename = "top_5_causes_iowa_plot.png",
  plot = top_5_causes_iowa_plot,
  path = plot_folder,
  width = 8
)

# Iowa trauma deaths by intentionality
iowa_deaths_intentionality_plot <- iowa_deaths_intentionality |>
  dplyr::mutate(
    Intentionality = stringr::str_remove_all(
      Intentionality,
      pattern = "^\\d{2}-"
    ),
    labels = dplyr::if_else(
      Year %in% c(2020, 2024) & Deaths >= 6,
      traumar::pretty_number(Deaths),
      dplyr::if_else(
        Year %in% c(2020, 2024) & Deaths < 6,
        traumar::small_count_label(var = Deaths, cutoff = 6, replacement = "*"),
        NA_character_
      )
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = factor(Year),
    y = Deaths,
    color = reorder(Intentionality, -Deaths),
    label = labels,
    group = Intentionality
  )) +
  ggplot2::geom_line(
    alpha = 0.5,
    linewidth = 3,
    lineend = "round",
    linejoin = "round"
  ) +
  ggrepel::geom_text_repel(
    segment.color = NA,
    family = "Work Sans",
    size = 8,
    color = "black",
    direction = "both"
  ) +
  ggplot2::labs(
    x = "",
    y = "",
    title = "Iowa Trauma Deaths by Intentionality",
    subtitle = "Source: Iowa Death Certificate Data | 2019-2023",
    color = "Intentionality",
    caption = "Note: Order of color legend follows descending order of lines.\n'*' indicates a masked value < 6 to protect confidentiality."
  ) +
  ggplot2::scale_y_continuous(
    labels = function(x) traumar::pretty_number(x)
  ) +
  paletteer::scale_color_paletteer_d(
    palette = "colorblindr::OkabeIto",
    direction = 1
  ) +
  traumar::theme_cleaner(
    base_size = 15,
    title_text_size = 20,
    subtitle_text_size = 18,
    vjust_title = 1.75,
    vjust_subtitle = 1,
    legend_position = "right"
  )

# save the Iowa trauma deaths by intentionality plot
ggplot2::ggsave(
  filename = "iowa_deaths_intentionality_plot.png",
  plot = iowa_deaths_intentionality_plot,
  path = plot_folder,
  height = 9,
  width = 9 * (16 / 9)
)

# unintentional trauma deaths by cause in Iowa plot
iowa_deaths_cause_plot <- iowa_deaths_cause |>
  dplyr::mutate(
    Cause = stringr::str_remove_all(Cause, pattern = "^\\d{2}-"),
    labels = dplyr::if_else(
      Year %in% c(2020, 2024) & Deaths >= 6,
      traumar::pretty_number(Deaths),
      dplyr::if_else(
        Year %in% c(2020, 2024) & Deaths < 6,
        traumar::small_count_label(var = Deaths, cutoff = 6, replacement = "*"),
        NA_character_
      )
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = factor(Year),
    y = Deaths,
    color = reorder(Cause, -Deaths),
    label = labels,
    group = Cause
  )) +
  ggplot2::geom_line(
    alpha = 0.5,
    linewidth = 3,
    lineend = "round",
    linejoin = "round"
  ) +
  ggrepel::geom_text_repel(
    segment.color = NA,
    family = "Work Sans",
    size = 8,
    color = "black",
    direction = "both"
  ) +
  ggplot2::labs(
    x = "",
    y = "",
    title = "Iowa Unintentional Trauma Deaths by Cause",
    subtitle = "Source: Iowa Death Certificate Data | 2019-2023",
    color = "Cause of Death",
    caption = "Note: Order of color legend follows descending order of lines."
  ) +
  ggplot2::scale_y_continuous(
    labels = function(x) traumar::pretty_number(x)
  ) +
  ggplot2::scale_color_paletteer_d(
    palette = "colorblindr::OkabeIto",
    direction = 1
  ) +
  traumar::theme_cleaner(
    base_size = 15,
    title_text_size = 20,
    subtitle_text_size = 18,
    vjust_title = 1.75,
    vjust_subtitle = 1,
    legend_position = "right"
  )

# save the iowa_deaths_cause_plot
ggplot2::ggsave(
  filename = "iowa_deaths_cause_plot.png",
  plot = iowa_deaths_cause_plot,
  path = plot_folder,
  height = 9,
  width = 9 * (16 / 9)
)

# trauma suicides by cause plot
iowa_suicides_cause_plot <- iowa_suicides_cause |>
  dplyr::mutate(
    Cause = str_remove_all(Cause, pattern = "^\\d{2}-"),
    labels = dplyr::if_else(
      Year %in% c(2020, 2024) & Deaths >= 6,
      traumar::pretty_number(Deaths),
      dplyr::if_else(
        Year %in% c(2020, 2024) & Deaths < 6,
        small_count_label(var = Deaths, cutoff = 6, replacement = "*"),
        NA_character_
      )
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = factor(Year),
    y = Deaths,
    color = reorder(Cause, -Deaths),
    label = labels,
    group = Cause
  )) +
  ggplot2::geom_line(
    alpha = 0.5,
    linewidth = 3,
    lineend = "round",
    linejoin = "round"
  ) +
  ggrepel::geom_text_repel(
    segment.color = NA,
    family = "Work Sans",
    size = 8,
    color = "black",
    direction = "x"
  ) +
  ggplot2::labs(
    x = "",
    y = "",
    title = "Iowa Trauma Suicide Deaths by Cause",
    subtitle = "Source: Iowa Death Certificate Data | 2019-2023",
    color = "Cause",
    caption = "Note: Order of color legend follows descending order of lines."
  ) +
  ggplot2::scale_y_continuous(
    labels = function(x) traumar::pretty_number(x)
  ) +
  ggplot2::scale_color_paletteer_d(
    palette = "colorblindr::OkabeIto",
    direction = 1
  ) +
  traumar::theme_cleaner(
    base_size = 15,
    title_text_size = 20,
    subtitle_text_size = 18,
    vjust_title = 1.75,
    vjust_subtitle = 1,
    legend_position = "right"
  )

# save trauma suicides by cause plot

ggplot2::ggsave(
  filename = "iowa_suicides_cause_plot.png",
  plot = iowa_suicides_cause_plot,
  path = plot_folder,
  height = 9,
  width = 9 * (16 / 9)
)

# trends in causes of death with 5-year avg
iowa_death_trends_cause_tbl <- iowa_death_trends_cause |>
  dplyr::arrange(Year, desc(Deaths)) |>
  dplyr::mutate(Cause = str_remove_all(Cause, pattern = "^\\d{2}-")) |>
  tidyr::pivot_wider(
    id_cols = Cause,
    names_from = Year,
    values_from = Deaths,
    values_fill = NA_integer_
  ) |>
  dplyr::mutate(
    `Five Year Avg.` = mean(`2020`:`2024`, na.rm = TRUE),
    `% Diff. from Five Year Avg.` = (`2024` - `Five Year Avg.`) /
      `Five Year Avg.`,
    `2019-2023 Trend` = list(c(
      `2020`,
      `2019`,
      `2020`,
      `2021`,
      `2023`,
      `2024`
    )),
    .by = Cause
  ) |>
  dplyr::arrange(desc(`Five Year Avg.`)) |>
  dplyr::select(Cause, `2024`:last_col()) |>
  gt::gt() |>
  gt::tab_header(
    title = "Iowa Trends in Causes of Traumatic Death",
    subtitle = "Source: Iowa Death Certificate Data | 2019-2023"
  ) |>
  gt::cols_label(`2024` = "# Deaths 2024") |>
  gt::fmt_number(
    columns = c(`2024`, `Five Year Avg.`),
    drop_trailing_zeros = TRUE
  ) |>
  gt::fmt_percent(
    columns = `% Diff. from Five Year Avg.`,
    drop_trailing_zeros = TRUE
  ) |>
  gt::tab_footnote(
    footnote = "Bars under cause of death track with count of deaths in 2024.",
    locations = cells_column_labels(columns = Cause)
  ) |>
  gt::tab_footnote(
    footnote = "Five year avg. calculated from counts of each cause of death from 2020 through 2024.",
    locations = cells_column_labels(columns = `Five Year Avg.`)
  ) |>
  gt::opt_footnote_marks(marks = "standard") |>
  gtExtras::gt_plt_dot(
    column = `2024`,
    category_column = Cause,
    palette = "colorblindr::OkabeIto"
  ) |>
  gtExtras::gt_plt_sparkline(
    column = `2019-2023 Trend`,
    type = "shaded",
    same_limit = FALSE,
    label = TRUE,
    fig_dim = c(8, 30)
  ) |>
  tab_style_hhs(border_cols = `2024`:`2019-2023 Trend`)

# trends in unintentional and suicide poisonings
iowa_death_poisoning_plot <- iowa_death_poisoning |>
  dplyr::arrange(Cause, Year) |>
  dplyr::mutate(
    labels = dplyr::if_else(
      Year %in% c(2020, 2024),
      traumar::pretty_number(x = Deaths),
      NA_character_
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = Year,
    y = Deaths,
    color = Cause,
    label = labels,
    group = Cause
  )) +
  ggplot2::annotate(
    geom = "segment",
    x = 2020,
    xend = 2024,
    y = mean(iowa_death_poisoning$Deaths),
    yend = mean(iowa_death_poisoning$Deaths),
    linetype = "dashed",
    color = hhs_palette_1$primary_1,
    alpha = 0.75
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 2019,
    xend = 2019,
    y = mean(iowa_death_poisoning$Deaths) + 15,
    yend = mean(iowa_death_poisoning$Deaths) + 15,
    color = hhs_palette_1$primary_1,
    alpha = 0.75,
    fontface = "bold",
    family = "Work Sans",
    size = 8,
    label = paste0("Avg.: ", round(mean(iowa_death_poisoning$Deaths)))
  ) +
  ggplot2::geom_line(
    linewidth = 2,
    lineend = "round",
    linejoin = "round",
    alpha = 0.9
  ) +
  ggrepel::geom_text_repel(
    direction = "y",
    segment.color = NA,
    color = "black",
    family = "Work Sans",
    size = 8,
    fontface = "bold"
  ) +
  ggplot2::labs(
    title = "Trends in Iowa Poisoning Deaths",
    subtitle = "Source: Iowa Death Certificate Data | 2019-2023",
    x = "",
    y = "# Deaths\n"
  ) +
  ggplot2::scale_color_paletteer_d(palette = "colorblindr::OkabeIto") +
  traumar::theme_cleaner(
    base_size = 15,
    title_text_size = 20,
    subtitle_text_size = 18,
    vjust_title = 1.75,
    vjust_subtitle = 1
  )

# save the poisoning trends plot

ggplot2::ggsave(
  filename = "iowa_death_poisoning_plot.png",
  plot = iowa_death_poisoning_plot,
  path = plot_folder,
  height = 9,
  width = 9 * (16 / 9)
)
