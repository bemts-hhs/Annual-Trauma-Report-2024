###_____________________________________________________________________________
# Deaths ----
###_____________________________________________________________________________

####
# You must first run data_load.R and setup.R before running this script
####

###_____________________________________________________________________________
# Plots for the death data ----
###_____________________________________________________________________________

# top 10 causes of death in the US ----
top_10_causes_us_plot <- death_cdc_wonder_nation_all_2019_2023_aggregate |>
  dplyr::mutate(
    name = gsub(
      x = `UCD - 15 Leading Causes of Death`,
      pattern = "(nt|se|sm)s\\b",
      replacement = "\\1",
      perl = TRUE,
      ignore.case = TRUE
    ),
    name = gsub(
      pattern = "(chron)ic",
      x = name,
      ignore.case = TRUE,
      perl = TRUE,
      replacement = "\\1\\."
    ),
    name = gsub(
      pattern = "(resp)iratory",
      x = name,
      replacement = "\\1\\.",
      ignore.case = TRUE,
      perl = TRUE
    ),
    name = ifelse(
      grepl(
        pattern = "nephritis",
        x = name,
        ignore.case = TRUE
      ),
      "Nephritis, nephrosis, etc.",
      ifelse(
        grepl(
          pattern = "liver\\sdisease",
          x = name,
          ignore.case = TRUE
        ),
        "Chron. liver disease/cirrhosis",
        name
      )
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = reorder(
      name,
      Deaths
    ),
    y = Deaths,
    fill = Deaths,
    label = traumar::pretty_number(Deaths, n_decimal = 2)
  )) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge2(width = 0.5),
    width = 0.75
  ) +
  ggplot2::annotate(
    geom = "segment",
    x = "Accident",
    xend = "Accident",
    y = 1500000,
    yend = 1100000,
    arrow = ggplot2::arrow(type = "closed")
  ) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    x = "",
    y = "",
  ) +
  ggplot2::guides(color = "none") +
  ggplot2::scale_y_continuous(labels = function(x) traumar::pretty_number(x)) +
  paletteer::scale_fill_paletteer_c(
    palette = "ggthemes::Orange-Gold",
    direction = 1,
    labels = function(x) traumar::pretty_number(x)
  ) +
  traumar::theme_cleaner(
    base_size = 30,
    legend_position = "inside",
    legend.position.inside = c(0.75, 0.25)
  )

# save the top causes of death in the US plot ----
ggplot2::ggsave(
  filename = "top_10_causes_us_plot.png",
  plot = top_10_causes_us_plot,
  path = plot_folder,
  height = 7,
  width = 7 * 1.78
)

# gt() tbl of deaths among 1-44 age population in the U.S. ----
death_cdc_wisqars_all_1_44_tbl <- death_cdc_wonder_nation_1_44_2019_2023_aggregate |>
  dplyr::slice_max(Deaths, n = 5) |>
  dplyr::mutate(
    name = gsub(
      x = `UCD - 15 Leading Causes of Death`,
      pattern = "(nt|se|sm)s\\b",
      replacement = "\\1",
      perl = TRUE,
      ignore.case = TRUE
    ),
    name = gsub(
      pattern = "(chron)ic",
      x = name,
      ignore.case = TRUE,
      perl = TRUE,
      replacement = "\\1\\."
    ),
    name = gsub(
      pattern = "(resp)iratory",
      x = name,
      replacement = "\\1\\.",
      ignore.case = TRUE,
      perl = TRUE
    ),
    name = ifelse(
      grepl(
        pattern = "nephritis",
        x = name,
        ignore.case = TRUE
      ),
      "Nephritis, nephrosis, etc.",
      ifelse(
        grepl(
          pattern = "liver\\sdisease",
          x = name,
          ignore.case = TRUE
        ),
        "Chron. liver disease/cirrhosis",
        name
      )
    ),
    .before = 1
  ) |>
  dplyr::select(
    -matches("ucd|crude|population|code"),
    name,
    Deaths,
    `Age Adjusted Rate`,
    `Age Adjusted Rate Lower 95% Confidence Interval`,
    `Age Adjusted Rate Upper 95% Confidence Interval`
  ) |>
  gt::gt() |>
  gt::fmt_number(columns = Deaths, drop_trailing_zeros = TRUE) |>
  gtExtras::gt_duplicate_column(
    column = `Age Adjusted Rate`,
    after = `Age Adjusted Rate`,
    dupe_name = "Rate_Bar"
  ) |>
  gtExtras::gt_plt_bar(column = Rate_Bar, color = "coral") |>
  gt::cols_label(
    `Age Adjusted Rate` ~ "Age Adjusted Rate (95% CI)",
    Rate_Bar ~ "",
    name ~ "Cause of Death"
  ) |>
  gt::cols_merge(
    columns = c(
      `Age Adjusted Rate`,
      `Age Adjusted Rate Lower 95% Confidence Interval`,
      `Age Adjusted Rate Upper 95% Confidence Interval`
    ),
    pattern = "{1} ({2}&mdash;{3})"
  ) |>
  gt::cols_hide(columns = `Age Adjusted Rate Standard Error`) |>
  tab_style_hhs(
    border_cols = c(Deaths, `Age Adjusted Rate`),
    column_labels = 18,
    body = 16
  )

# save the gt table for deaths ages 1-44 US ----
gt::gtsave(
  data = death_cdc_wisqars_all_1_44_tbl,
  filename = "death_cdc_wisqars_all_1_44_tbl.png",
  path = plot_folder
)

# top 5 causes of death in Iowa
top_5_causes_iowa_plot <- death_cdc_wonder_iowa_all_2023 |>
  dplyr::slice_max(Deaths, n = 5) |>
  dplyr::mutate(
    name = gsub(
      x = `UCD - 15 Leading Causes of Death`,
      pattern = "(nt|se|sm)s\\b",
      replacement = "\\1",
      perl = TRUE,
      ignore.case = TRUE
    ),
    name = gsub(
      pattern = "(chron)ic",
      x = name,
      ignore.case = TRUE,
      perl = TRUE,
      replacement = "\\1\\."
    ),
    name = gsub(
      pattern = "(resp)iratory",
      x = name,
      replacement = "\\1\\.",
      ignore.case = TRUE,
      perl = TRUE
    ),
    name = ifelse(
      grepl(
        pattern = "nephritis",
        x = name,
        ignore.case = TRUE
      ),
      "Nephritis, nephrosis, etc.",
      ifelse(
        grepl(
          pattern = "liver\\sdisease",
          x = name,
          ignore.case = TRUE
        ),
        "Chron. liver disease/cirrhosis",
        name
      )
    ),
    .before = 1
  ) |>
  dplyr::slice_head(n = 5) |>
  ggplot2::ggplot(ggplot2::aes(
    x = reorder(
      stringr::str_wrap(name, width = 10),
      -Deaths
    ),
    y = Deaths,
    fill = Deaths,
    label = traumar::pretty_number(x = Deaths, n_decimal = 2)
  )) +
  ggplot2::geom_col(
    width = 0.75,
    position = ggplot2::position_dodge(width = 1)
  ) +
  ggrepel::geom_text_repel(
    direction = "y",
    nudge_y = ifelse(
      grepl(
        pattern = "heart",
        x = death_cdc_wonder_iowa_all_2023$`UCD - 15 Leading Causes of Death`,
        ignore.case = TRUE
      ),
      300,
      10
    ),
    family = "Work Sans",
    size = 15,
    fontface = "bold",
    seed = 10232015,
    segment.color = "transparent"
  ) +
  ggplot2::annotate(
    geom = "segment",
    x = stringr::str_wrap("Accident", width = 10),
    xend = stringr::str_wrap("Accident", width = 10),
    y = 3500,
    yend = 2700,
    arrow = ggplot2::arrow(type = "closed")
  ) +
  paletteer::scale_fill_paletteer_c(
    palette = "ggthemes::Orange-Gold",
    direction = 1,
    labels = function(x) traumar::pretty_number(x)
  ) +
  ggplot2::labs(x = "", y = "") +
  traumar::theme_cleaner(
    base_size = 30,
    axis.text.y = ggplot2::element_blank(),
    legend_position = "inside",
    legend.position.inside = c(0.75, 0.75)
  )

# save the top 10 causes of death in Iowa plot ----
ggplot2::ggsave(
  filename = "top_5_causes_iowa_plot.png",
  plot = top_5_causes_iowa_plot,
  path = plot_folder,
  height = 8.1,
  width = 8.1 * 1.78
)

# gt() tbl of deaths among 1-44 age population in the U.S. ----
death_cdc_wisqars_ia_1_44_tbl <- death_cdc_wonder_iowa_1_44_2019_2023_aggregate |>
  dplyr::slice_max(Deaths, n = 5) |>
  dplyr::mutate(
    name = gsub(
      x = `UCD - 15 Leading Causes of Death`,
      pattern = "(nt|se|sm)s\\b",
      replacement = "\\1",
      perl = TRUE,
      ignore.case = TRUE
    ),
    name = gsub(
      pattern = "(chron)ic",
      x = name,
      ignore.case = TRUE,
      perl = TRUE,
      replacement = "\\1\\."
    ),
    name = gsub(
      pattern = "(resp)iratory",
      x = name,
      replacement = "\\1\\.",
      ignore.case = TRUE,
      perl = TRUE
    ),
    name = ifelse(
      grepl(
        pattern = "nephritis",
        x = name,
        ignore.case = TRUE
      ),
      "Nephritis, nephrosis, etc.",
      ifelse(
        grepl(
          pattern = "liver\\sdisease",
          x = name,
          ignore.case = TRUE
        ),
        "Chron. liver disease/cirrhosis",
        name
      )
    ),
    .before = 1
  ) |>
  dplyr::select(
    -matches("ucd|crude|population|code"),
    name,
    Deaths,
    `Age Adjusted Rate`,
    `Age Adjusted Rate Lower 95% Confidence Interval`,
    `Age Adjusted Rate Upper 95% Confidence Interval`
  ) |>
  gt::gt() |>
  gt::fmt_number(columns = Deaths, drop_trailing_zeros = TRUE) |>
  gtExtras::gt_duplicate_column(
    column = `Age Adjusted Rate`,
    after = `Age Adjusted Rate`,
    dupe_name = "Rate_Bar"
  ) |>
  gtExtras::gt_plt_bar(column = Rate_Bar, color = "coral") |>
  gt::cols_label(
    `Age Adjusted Rate` ~ "Age Adjusted Rate (95% CI)",
    Rate_Bar ~ "",
    name ~ "Cause of Death"
  ) |>
  gt::cols_merge(
    columns = c(
      `Age Adjusted Rate`,
      `Age Adjusted Rate Lower 95% Confidence Interval`,
      `Age Adjusted Rate Upper 95% Confidence Interval`
    ),
    pattern = "{1} ({2}&mdash;{3})"
  ) |>
  gt::cols_hide(columns = `Age Adjusted Rate Standard Error`) |>
  tab_style_hhs(
    border_cols = c(Deaths, `Age Adjusted Rate`),
    column_labels = 18,
    body = 16
  )

# save the gt table for deaths ages 1-44 Iowa ----
gt::gtsave(
  data = death_cdc_wisqars_ia_1_44_tbl,
  filename = "death_cdc_wisqars_ia_1_44_tbl.png",
  path = plot_folder
)

# Iowa trauma deaths by intentionality ----
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
    subtitle = "Source: Iowa Death Certificate Data | 2020-2024",
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
  height = 6,
  width = 8
)

# get fire/burn and drowning counts for caption in the following plot ----
fire_burn_drown <- iowa_deaths_cause |>
  dplyr::filter(
    Year == 2024,
    grepl(pattern = "fire|drown", x = Cause, ignore.case = TRUE)
  ) |>
  dplyr::select(-Year)

# unintentional trauma deaths by cause in Iowa plot
iowa_deaths_cause_plot <- iowa_deaths_cause |>
  dplyr::mutate(
    Cause = stringr::str_remove_all(Cause, pattern = "^\\d{2}-"),
    labels = dplyr::if_else(Year %in% c(2020, 2024), Deaths, NA),
    labels = dplyr::if_else(
      Year == 2024 &
        grepl(pattern = "fire|drown", x = Cause, ignore.case = TRUE),
      NA,
      labels
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
    direction = "both",
    seed = 10232015
  ) +
  ggplot2::labs(
    x = "",
    y = "",
    title = "Iowa Unintentional Trauma Deaths by Cause",
    subtitle = "Source: Iowa Death Certificate Data | 2020-2024",
    color = "Cause of Death",
    caption = glue::glue(
      "Note: Order of color legend follows descending order of lines. || 2024 Fire/burn = {fire_burn_drown[1,2]}, 2024 Drowning = {fire_burn_drown[2,2]}"
    )
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
    legend_position = "right",
    axis.text.y = ggplot2::element_blank()
  )

# save the iowa_deaths_cause_plot
ggplot2::ggsave(
  filename = "iowa_deaths_cause_plot.png",
  plot = iowa_deaths_cause_plot,
  path = plot_folder,
  height = 6,
  width = 8
)

# trauma suicides by cause plot
iowa_suicides_cause_plot <- iowa_suicides_cause |>
  dplyr::mutate(
    Cause = stringr::str_remove_all(Cause, pattern = "^\\d{2}-")
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = factor(Year),
    y = Deaths,
    color = reorder(Cause, -Deaths),
    label = Deaths,
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
    subtitle = "Source: Iowa Death Certificate Data | 2020-2024",
    color = "Cause",
    caption = "Note: Order of color legend follows descending order of lines."
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

# save trauma suicides by cause plot

ggplot2::ggsave(
  filename = "iowa_suicides_cause_plot.png",
  plot = iowa_suicides_cause_plot,
  path = plot_folder,
  height = 6,
  width = 8
)

# trends in causes of death with 5-year avg
iowa_death_trends_cause_tbl <- iowa_death_trends_cause |>
  dplyr::arrange(Year, desc(Deaths)) |>
  dplyr::mutate(Cause = stringr::str_remove_all(Cause, pattern = "^\\d{2}-")) |>
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
    `2020-2024 Trend` = list(c(
      `2020`,
      `2021`,
      `2022`,
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
    subtitle = "Source: Iowa Death Certificate Data | 2020-2024"
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
    locations = gt::cells_column_labels(columns = Cause)
  ) |>
  gt::tab_footnote(
    footnote = "Five year avg. calculated from counts of each cause of death from 2020 through 2024.",
    locations = gt::cells_column_labels(columns = `Five Year Avg.`)
  ) |>
  gt::opt_footnote_marks(marks = "standard") |>
  gtExtras::gt_plt_dot(
    column = `2024`,
    category_column = Cause,
    palette = "colorblindr::OkabeIto"
  ) |>
  gtExtras::gt_plt_sparkline(
    column = `2020-2024 Trend`,
    type = "shaded",
    same_limit = FALSE,
    label = TRUE,
    fig_dim = c(8, 30)
  ) |>
  tab_style_hhs(border_cols = `2024`:`2020-2024 Trend`)

# save the table
gt::gtsave(
  data = iowa_death_trends_cause_tbl,
  filename = "iowa_death_trends_cause_tbl.png",
  path = plot_folder
)

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
    color = "#03617A",
    alpha = 0.75
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 2022,
    xend = 2022,
    y = mean(iowa_death_poisoning$Deaths) + 15,
    yend = mean(iowa_death_poisoning$Deaths) + 15,
    color = "#03617A",
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
    subtitle = "Source: Iowa Death Certificate Data | 2020-2024",
    x = "",
    y = "# Deaths\n"
  ) +
  paletteer::scale_color_paletteer_d(palette = "colorblindr::OkabeIto") +
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
  height = 6,
  width = 8
)
