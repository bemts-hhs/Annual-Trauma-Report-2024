###_____________________________________________________________________________
# Deaths ----
###_____________________________________________________________________________

####
# You must first run data_load.R and setup.R before running this script
####

###_____________________________________________________________________________
# State-wide deaths and CDC WONDER National Center for Health Statistics
# This needs to be updated each year to add on new years that are finalized
# Go to Current Final Multiple Cause of Death Data and make data request
# Use the query to group by UCD 15 leading causes of death
# Include crude rates and age adjusted rates and download all confidence interaval
# and standard error estimations
# You have to download each file year after year. Previous data are finalized
# so no need to download those again unless there are documented updates from CDC
# Select UCD - ICD-10 113 Cause List and MCD - ICD-10 113 Cause List options
# URL below:
# https://wonder.cdc.gov/mcd.html
# Use the same process above to get ages 1-44 data for the entire US as well as
# the all ages US data
# For the Iowa specific data, use the same process to get all ages and ages 1-44
# datasets
###_____________________________________________________________________________
# For Josh Jungling's breakdown, go here and choose the most current year's report
# https://hhs.iowa.gov/public-health/health-statistics
# Go to the most recent year's Vital Statistics of Iowa Annual Report
# Use the chart LEADING CAUSES OF DEATH BY NUMBER AND PERCENT OF TOTAL DEATHS, BY GENDER
# Copy / paste the text data from the last 5 years into ChatGPT and ask it to
# put the data in data.frame format and it will.
# for other estimates, utilize the Tableau workbook at
# https://data.idph.state.ia.us/#/site/IDPH-Data/views/TraumaDeaths/TraumaRequest
###_____________________________________________________________________________

###
# Iowa-specific deaths ----
###

### TODO: #1 Add code to ingest Iowa-specific CDC data

# Iowa all ages ----

# Iowa ages 1-44 ----

###
# CDC WONDER Data ----
# all age groups and deaths
###

# CDC WONDER ALL UNITED STATES all ages ----

# All US and all ages 2019 ----
death_cdc_wonder_nation_all_2020 <- readr::read_csv(
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States All Ages 2019.csv",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2019) |>
  dplyr::mutate(
    `UCD - 15 Leading Causes of Death` = stringr::str_replace_all(
      `UCD - 15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# All US and all ages 2020 ----
death_cdc_wonder_nation_all_2019 <- readr::read_csv(
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States All Ages 2020.csv",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2020) |>
  dplyr::mutate(
    `UCD - 15 Leading Causes of Death` = stringr::str_replace_all(
      `UCD - 15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# All US and all ages 2021 ----
death_cdc_wonder_nation_all_2020 <- readr::read_csv(
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States All Ages 2021.csv",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2021) |>
  dplyr::mutate(
    `UCD - 15 Leading Causes of Death` = stringr::str_replace_all(
      `UCD - 15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# All US and all ages 2022 ----
death_cdc_wonder_nation_all_2021 <- readr::read_csv(
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States All Ages 2022.csv",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2022) |>
  dplyr::mutate(
    `UCD - 15 Leading Causes of Death` = stringr::str_replace_all(
      `UCD - 15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# All US and all ages 2023 ----
death_cdc_wonder_nation_all_2023 <- readr::read_csv(
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States All Ages 2023.csv",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2023) |>
  dplyr::mutate(
    `UCD - 15 Leading Causes of Death` = stringr::str_replace_all(
      `UCD - 15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# All US and all ages 2019-2023 ----
death_cdc_wonder_nation_all_2019_2023_aggregate <- readr::read_csv(
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States All Ages 2019-2023.csv",
  n_max = 10
) |>
  dplyr::select(-Notes) |>
  dplyr::mutate(
    `UCD - 15 Leading Causes of Death` = stringr::str_replace_all(
      `UCD - 15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# All US deaths 2019 - 2023 ----
death_cdc_wonder_nation_all_2019_2023_detail <- dplyr::bind_rows(
  death_cdc_wonder_nation_all_2019,
  death_cdc_wonder_nation_all_2020,
  death_cdc_wonder_nation_all_2021,
  death_cdc_wonder_nation_all_2022,
  death_cdc_wonder_nation_all_2023
) |>
  dplyr::mutate(
    `UCD - 15 Leading Causes of Death` = stringr::str_replace_all(
      `UCD - 15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# Download top 10 deaths yearly detail file to .csv for future reference ----
readr::write_csv(
  death_cdc_wonder_nation_all_2019_2023_detail,
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/death_cdc_wonder_nation_all_2019_2023_detail.csv"
)

# Download top 10 deaths aggregate file to .csv for future reference ----
readr::write_csv(
  death_cdc_wonder_nation_all_2019_2023_aggregate,
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/death_cdc_wonder_nation_all_2019_2023_aggregate.csv"
)

###
# CDC WONDER ages 1-44 ----
###

# CDC ages 1-44 2019 ----
death_cdc_wonder_nation_1_44_2019 <- readr::read_csv(
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States Ages 1-44 2019.csv",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2019) |>
  dplyr::mutate(
    `UCD - 15 Leading Causes of Death` = stringr::str_replace_all(
      `UCD - 15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# CDC ages 1-44 2020 ----
death_cdc_wonder_nation_1_44_2020 <- readr::read_csv(
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States Ages 1-44 2020.csv",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2020) |>
  dplyr::mutate(
    `UCD - 15 Leading Causes of Death` = stringr::str_replace_all(
      `UCD - 15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# CDC ages 1-44 2021 ----
death_cdc_wonder_nation_1_44_2021 <- readr::read_csv(
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States Ages 1-44 2021.csv",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2021) |>
  dplyr::mutate(
    `UCD - 15 Leading Causes of Death` = stringr::str_replace_all(
      `UCD - 15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# CDC ages 1-44 2022 ----
death_cdc_wonder_nation_1_44_2022 <- readr::read_csv(
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States Ages 1-44 2022.csv",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2022) |>
  dplyr::mutate(
    `UCD - 15 Leading Causes of Death` = stringr::str_replace_all(
      `UCD - 15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# CDC ages 1-44 2023 ----
death_cdc_wonder_nation_1_44_2023 <- readr::read_csv(
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States Ages 1-44 2023.csv",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2023) |>
  dplyr::mutate(
    `UCD - 15 Leading Causes of Death` = stringr::str_replace_all(
      `UCD - 15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# CDC ages 1-44 2019-2023 ----
death_cdc_wonder_nation_1_44_2019_2023_aggregate <- readr::read_csv(
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States Ages 1-44 2019-2023.csv",
  n_max = 10
) |>
  dplyr::select(-Notes) |>
  dplyr::mutate(
    `UCD - 15 Leading Causes of Death` = stringr::str_replace_all(
      `UCD - 15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# All US ages 1-44 deaths 2019 - 2023
death_cdc_wonder_nation_1_44_2019_2023_detail <- dplyr::bind_rows(
  death_cdc_wonder_nation_1_44_2019,
  death_cdc_wonder_nation_1_44_2020,
  death_cdc_wonder_nation_1_44_2021,
  death_cdc_wonder_nation_1_44_2022,
  death_cdc_wonder_nation_1_44_2023
) |>
  dplyr::mutate(
    `UCD - 15 Leading Causes of Death` = stringr::str_replace_all(
      `UCD - 15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# Download top 10 deaths ages 1-44 yearly detail file to .csv for future
# reference ----
readr::write_csv(
  death_cdc_wonder_nation_1_44_2019_2023_detail,
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/death_cdc_wonder_nation_1_44_2019_2023_detail.csv"
)

# Download top 10 deaths ages 1-44 aggregate file to .csv for future reference ----
readr::write_csv(
  death_cdc_wonder_nation_1_44_2019_2023_aggregate,
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/death_cdc_wonder_nation_1_44_2019_2023_aggregate.csv"
)

###
# Data for section Trends in Causes of Death ----
###

  # for the Iowa trauma deaths by intentionality plot ----
  iowa_deaths_intentionality <- readr::read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/P15_data.csv"
  ) |>
    dplyr::rename(
      Intentionality = `_TR P15`,
      Year = `The Year`,
      Deaths = `# of Deaths`
    )

  # for the Iowa unintentional trauma deaths by cause plot ----
  iowa_deaths_cause <- readr::read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/P16-1_data.csv"
  ) |>
    dplyr::rename(
      Cause = `_TR P16-1`,
      Year = `The Year`,
      Deaths = `# of Deaths`
    )

  # for the Iowa trauma suicides by cause plot ----
  iowa_suicides_cause <- readr::read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/P16-2_data.csv"
  ) |>
    dplyr::rename(
      Cause = `_TR P16-2`,
      Year = `The Year`,
      Deaths = `# of Deaths`
    )

  # for the trends in causes of death table ----
  iowa_death_trends_cause <- readr::read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/P17_data.csv"
  ) |>
    dplyr::rename(Cause = `_TR P17`, Year = `The Year`, Deaths = `# of Deaths`)

  # for Iowa unintentional falls trends plot ----
  iowa_death_unintentional_falls <- readr::read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/P30_data.csv"
  ) |>
    dplyr::rename(Cause = `_TR P30`, Year = `The Year`, Deaths = `# of Deaths`)

  # for Iowa poisoning death trends plot ----
  # unintentional poisoning
  iowa_death_unintentional_poisoning <- readr::read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/P33-1_data.csv"
  ) |>
    dplyr::rename(
      Cause = `_TR P33-1`,
      Year = `The Year`,
      Deaths = `# of Deaths`
    )

  # suicide poisoning ----
  iowa_death_suicide_poisoning <- readr::read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/P33-2_data.csv"
  ) |>
    dplyr::rename(
      Cause = `_TR P33-2`,
      Year = `The Year`,
      Deaths = `# of Deaths`
    )

  #union the poisoning tables ----
  iowa_death_poisoning <- dplyr::bind_rows(
    iowa_death_unintentional_poisoning,
    iowa_death_suicide_poisoning
  )

###_____________________________________________________________________________
# Plots for the death data ----
###_____________________________________________________________________________

# top 10 causes of death in the US
  top_10_causes_us_plot <- death_cdc_wonder_nation_all_2019_2023_aggregate |>
    ggplot2::ggplot(ggplot2::aes(
      x = reorder(
        stringr::str_wrap(`UCD - 15 Leading Causes of Death`, width = 20),
        Deaths
      ),
      y = Deaths,
      fill = Deaths,
      label = traumar::pretty_number(Deaths, n_decimal = 1)
    )) +
    ggplot2::geom_col(position = position_dodge2(width = 0.5)) +
    ggplot2::geom_text(
      ggplot2::aes(
        y = dplyr::if_else(Deaths < 500000, Deaths + 200000, 250000)
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
    ggplot2::scale_fill_paletteer_c(
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

  plot_save_params(
    filename = "top_10_causes_us_plot.png",
    plot = top_10_causes_us_plot,
    path = plot_path,
    height = 9
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
      source_note = md(paste0(
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
    gt::gt_duplicate_column(
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
    gt::tab_style_hhs(border_cols = c(Deaths, `Age Adjusted Rate`))
}

# top 10 causes of death in Iowa
  top_10_causes_iowa_plot <- death_iowa_state |>
    dplyr::filter(Year == 2024) |>
    ggplot2::ggplot(ggplot2::aes(
      x = reorder(stringr::str_wrap(Cause_of_Death, width = 10), -Deaths),
      y = Deaths,
      fill = Deaths,
      label = traumar::pretty_number(Deaths)
    )) +
    ggplot2::geom_col(width = 0.75, position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_text(
      family = "Work Sans",
      size = 8,
      fontface = "bold",
      nudge_y = 200
    ) +
    ggplot2::annotate(
      geom = "segment",
      x = stringr::str_wrap("Unintentional Injuries", width = 10),
      xend = stringr::str_wrap("Unintentional Injuries", width = 10),
      y = 3500,
      yend = 2300,
      arrow = ggplot2::arrow(type = "closed")
    ) +
    ggplot2::labs(
      x = "",
      y = "",
      title = "Top 10 Causes of Death in Iowa Among All Age Groups",
      subtitle = "Source: Iowa Death Certificate Data | 2024"
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
      axis.text.y = element_blank(),
      legend_position = "inside",
      legend.position.inside = c(0.75, 0.75),
      facets = TRUE
    )

  # save the top 10 causes of death in Iowa plot

  plot_save_params(
    filename = "top_10_causes_iowa_plot.png",
    plot = top_10_causes_iowa_plot,
    path = plot_path,
    width_ratio = 2
  )

# Iowa trauma deaths by intentionality
  iowa_deaths_intentionality_plot <- iowa_deaths_intentionality |>
    dplyr::mutate(
      Intentionality = stringr::str_remove_all(Intentionality, pattern = "^\\d{2}-"),
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
  plot_save_params(
    filename = "iowa_deaths_intentionality_plot.png",
    plot = iowa_deaths_intentionality_plot,
    path = plot_path
  )
}

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
  plot_save_params(
    filename = "iowa_deaths_cause_plot.png",
    plot = iowa_deaths_cause_plot,
    path = plot_path
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

  plot_save_params(
    filename = "iowa_suicides_cause_plot.png",
    plot = iowa_suicides_cause_plot,
    path = plot_path
  )

# trends in causes of death with 5-year avg
  iowa_death_trends_cause_tbl <- iowa_death_trends_cause |>
    dplyr::arrange(Year, desc(Deaths)) |>
    dplyr::mutate(Cause = str_remove_all(Cause, pattern = "^\\d{2}-")) |>
    pivot_wider(
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
    gt() |>
    tab_header(
      title = "Iowa Trends in Causes of Traumatic Death",
      subtitle = "Source: Iowa Death Certificate Data | 2019-2023"
    ) |>
    cols_label(`2024` = "# Deaths 2024") |>
    fmt_number(
      columns = c(`2024`, `Five Year Avg.`),
      drop_trailing_zeros = TRUE
    ) |>
    fmt_percent(
      columns = `% Diff. from Five Year Avg.`,
      drop_trailing_zeros = TRUE
    ) |>
    tab_footnote(
      footnote = "Bars under cause of death track with count of deaths in 2024.",
      locations = cells_column_labels(columns = Cause)
    ) |>
    tab_footnote(
      footnote = "Five year avg. calculated from counts of each cause of death from 2020 through 2024.",
      locations = cells_column_labels(columns = `Five Year Avg.`)
    ) |>
    opt_footnote_marks(marks = "standard") |>
    gt_plt_dot(
      column = `2024`,
      category_column = Cause,
      palette = "colorblindr::OkabeIto"
    ) |>
    gt_plt_sparkline(
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

  plot_save_params(
    filename = "iowa_death_poisoning_plot.png",
    plot = iowa_death_poisoning_plot,
    path = plot_path
  )
