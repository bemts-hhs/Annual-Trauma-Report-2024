####
# Main report section ----
####

###
# This section cannot be run without first running data_load.R and setup.R.
###

# Trauma facility count by trauma level df

trauma_facility_count_by_level <- trauma_2024 |>
  dplyr::distinct(Level, Facility_Name) |>
  dplyr::count(Level, name = "Count")

# Trauma facility count by trauma level plot

{
  trauma_facility_count_by_level_plot <- trauma_facility_count_by_level |>
    ggplot2::ggplot(ggplot2::aes(Level, Count, fill = Level, label = Count)) +
    ggplot2::geom_col(color = "black") +
    ggrepel::geom_text_repel(
      direction = "y",
      color = "black",
      nudge_y = dplyr::if_else(trauma_facility_count_by_level$Count < 10, 1, 0),
      size = 8,
      family = "Work Sans",
      segment.color = NA,
      max.iter = 30000
    ) +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(
      title = "Trauma Facility Count by Trauma Facility Verification Level",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024",
      x = "",
      y = "Count of Facilities"
    ) +
    traumar::theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 2,
      vjust_subtitle = 1.25
    ) +
    ggthemes::scale_fill_colorblind()

  # save the plot

  plot_save_params(
    filename = "trauma_facility_count_by_level_plot.png",
    plot = trauma_facility_count_by_level_plot,
    path = plot_path
  )
}

# Count of cases by trauma facility level

{
  trauma_cases_by_facility_level <- trauma_2024 |>
    dplyr::filter(Level %in% c("I", "II", "III", "IV")) |>
    injury_case_count(Level) |>
    dplyr::mutate(
      percent = n / sum(n),
      percent_label = traumar::pretty_percent(percent, n_decimal = 1)
    )

  # plot Count of cases by trauma facility level

  trauma_cases_by_facility_level_plot <- trauma_cases_by_facility_level |>
    ggplot2::ggplot(ggplot2::aes(
      x = Level,
      y = n,
      fill = Level,
      label = prettyNum(n, big.mark = ",")
    )) +
    ggplot2::geom_col(color = "black") +
    ggrepel::geom_text_repel(
      direction = "y",
      nudge_y = 1,
      color = "black",
      size = 8,
      family = "Work Sans",
      segment.color = NA,
      max.iter = 30000
    ) +
    ggplot2::geom_text(
      ggplot2::aes(y = 300, label = percent_label),
      color = "white",
      family = "Work Sans",
      size = 8
    ) +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(
      title = "Case Count by Trauma Facility Verification Level",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024",
      caption = " These data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a patient enters\n an ED or hospital for treatment of an injury.",
      x = "",
      y = "Count of Cases"
    ) +
    traumar::theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 2,
      vjust_subtitle = 1.25
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) traumar::pretty_number(x, n_decimal = 1)
    ) +
    ggthemes::scale_fill_colorblind()

  # save the plot

  plot_save_params(
    filename = "trauma_cases_by_facility_level_plot.png",
    plot = trauma_cases_by_facility_level_plot,
    path = plot_path
  )

  # document the % increase in case volume by level

  trauma_case_level_increase <- trauma_data_clean |>
    dplyr::filter(Level %in% c("I", "II", "III", "IV"), Year < 2024) |>
    injury_case_count(Year, Level) |>
    dplyr::arrange(Level) |>
    dplyr::mutate(change = (n - dplyr::lag(n)) / dplyr::lag(n), .by = Level)
}

# count of incidents by definitive care facility level

trauma_cases_by_def_care_level <- trauma_2024 |>
  dplyr::filter(Level %in% c("I", "II", "III", "IV"), Receiving == "Yes") |>
  injury_case_count(Level) |>
  dplyr::mutate(
    n_label = prettyNum(n, big.mark = ","),
    percent = n / sum(n),
    percent_label = traumar::pretty_percent(percent, n_decimal = 1)
  )

# plot the count of incidents by definitive care facility level

{
  trauma_cases_by_def_care_level_plot <- trauma_cases_by_def_care_level |>
    ggplot2::ggplot(ggplot2::aes(
      x = reorder(Level, n),
      y = n,
      color = Level,
      label = paste0(n_label, " (", percent_label, ")")
    )) +
    ggplot2::geom_col(
      color = "white",
      fill = "darkslategray",
      width = 0.03,
      alpha = 0.75
    ) +
    ggplot2::geom_point(size = 8) +
    ggplot2::geom_text(
      ggplot2::aes(y = dplyr::if_else(Level == "I", n - 225, n + 275)),
      nudge_x = dplyr::if_else(
        trauma_cases_by_def_care_level$Level == "I",
        -0.175,
        0
      ),
      color = "black",
      family = "Work Sans",
      size = 8
    ) +
    ggplot2::labs(
      x = "",
      y = "",
      title = "Count of Receiving Facility Cases by Trauma Facility Verification Level",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024",
      caption = "These data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a patient enters\nan ED or hospital for treatment of an injury."
    ) +
    coord_flip() +
    ggplot2::guides(color = "none") +
    traumar::theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.5,
      vjust_subtitle = 1
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) traumar::pretty_number(x, n_decimal = 2)
    ) +
    ggthemes::scale_color_colorblind()

  # save the definitive care case count plot

  plot_save_params(
    filename = "trauma_cases_by_def_care_level_plot.png",
    plot = trauma_cases_by_def_care_level_plot,
    path = plot_path
  )
}
