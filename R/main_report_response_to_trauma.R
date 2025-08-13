###_____________________________________________________________________________
# Response to Trauma - Main report section ----
###_____________________________________________________________________________

####
# You must first run data_load.R and setup.R before running this script
####

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

###_____________________________________________________________________________
# Response to Trauma section ----
###_____________________________________________________________________________

# transport mode to facility df

{
  transport_mode_to_facility <- trauma_2024 |>
    dplyr::mutate(
      Transport_To_Your_Facility_By = dplyr::if_else(
        grepl(
          pattern = "not known|not applicable",
          x = Transport_To_Your_Facility_By,
          ignore.case = TRUE
        ),
        "Not Known/Not Recorded",
        dplyr::if_else(
          grepl(
            pattern = "ALS|BLS",
            x = Transport_To_Your_Facility_By,
            ignore.case = FALSE
          ),
          "Ground Ambulance",
          Transport_To_Your_Facility_By
        )
      )
    ) |>
    injury_case_count(Transport_To_Your_Facility_By) |>
    dplyr::arrange(desc(n)) |>
    dplyr::mutate(
      Transport_To_Your_Facility_By = str_wrap(
        Transport_To_Your_Facility_By,
        whitespace_only = FALSE,
        width = 20
      ),
      number_label = traumar::pretty_number(
        small_count_label(var = n, cutoff = 6, replacement = NA_integer_),
        n_decimal = 2
      ),
      percent = n / sum(n),
      percent_label = traumar::pretty_percent(percent)
    )
}

# transport mode to facility plot

{
  transport_mode_to_facility_plot <- transport_mode_to_facility |>
    ggplot2::ggplot(ggplot2::aes(
      x = reorder(Transport_To_Your_Facility_By, n),
      y = n,
      label = paste0(number_label, " (", percent_label, ")")
    )) +
    ggplot2::geom_col(color = "black", alpha = 0.5, fill = "dodgerblue1") +
    ggplot2::coord_flip() +
    ggplot2::geom_text(
      ggplot2::aes(y = dplyr::if_else(n > 5000, 0, n)),
      family = "Work Sans",
      size = 8,
      nudge_y = dplyr::if_else(
        transport_mode_to_facility$n > 5000,
        2100,
        dplyr::if_else(
          transport_mode_to_facility$n < 5000 &
            transport_mode_to_facility$n > 1000,
          transport_mode_to_facility$n + (transport_mode_to_facility$n * 0.35),
          1500
        )
      )
    ) +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(
      x = "",
      y = "\n",
      title = "Count of Cases by Transport Mode to Facility",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024",
      caption = "These data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a\npatient enters an ED or hospital for treatment of an injury."
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
    )

  # save the transport mode plot

  plot_save_params(
    filename = "transport_mode_to_facility_plot.png",
    plot = transport_mode_to_facility_plot,
    path = plot_path
  )
}

# transport mode to facility among receiving facilities df

{
  transport_mode_to_facility_receiving <- trauma_2024 |>
    dplyr::filter(Receiving == "Yes") |>
    dplyr::mutate(
      Transport_To_Your_Facility_By = dplyr::if_else(
        grepl(
          pattern = "not known|not applicable",
          x = Transport_To_Your_Facility_By,
          ignore.case = TRUE
        ),
        "Not Known/Not Recorded",
        dplyr::if_else(
          grepl(
            pattern = "ALS|BLS",
            x = Transport_To_Your_Facility_By,
            ignore.case = FALSE
          ),
          "Ground Ambulance",
          Transport_To_Your_Facility_By
        )
      )
    ) |>
    injury_case_count(Transport_To_Your_Facility_By) |>
    dplyr::arrange(desc(n)) |>
    dplyr::mutate(
      Transport_To_Your_Facility_By = str_wrap(
        Transport_To_Your_Facility_By,
        whitespace_only = FALSE,
        width = 20
      ),
      number_label = dplyr::if_else(
        n >= 6,
        prettyNum(
          small_count_label(var = n, cutoff = 6, replacement = NA_integer_),
          big.mark = ","
        ),
        "*"
      ),
      percent = n / sum(n),
      percent_label = dplyr::if_else(
        number_label == "*",
        "*",
        traumar::pretty_percent(percent)
      )
    )
}

# transport mode to facility among receiving facilities plot

{
  transport_mode_to_facility_receiving_plot <- transport_mode_to_facility_receiving |>
    ggplot2::ggplot(ggplot2::aes(
      x = reorder(Transport_To_Your_Facility_By, n),
      y = n,
      label = paste0(number_label, " (", percent_label, ")")
    )) +
    ggplot2::geom_col(color = "black", alpha = 0.5, fill = "coral") +
    ggplot2::coord_flip() +
    ggplot2::geom_text(
      ggplot2::aes(y = dplyr::if_else(n > 700, 0, n)),
      family = "Work Sans",
      size = 8,
      nudge_y = dplyr::if_else(
        transport_mode_to_facility_receiving$n > 1000,
        450,
        dplyr::if_else(
          transport_mode_to_facility_receiving$n < 1000 &
            transport_mode_to_facility_receiving$n > 700,
          350,
          dplyr::if_else(
            transport_mode_to_facility_receiving$n < 700 &
              transport_mode_to_facility_receiving$n > 10,
            325,
            200
          )
        )
      )
    ) +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(
      x = "",
      y = "\n",
      title = "Count of Cases by Transport Mode to Among Receiving Facilities",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024",
      caption = "These data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a\npatient enters an ED or hospital for treatment of an injury.\n'*' is used to mask counts < 6 to protect confidentiality."
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
    )

  # save the transport mode plot

  plot_save_params(
    filename = "transport_mode_to_facility_receiving_plot.png",
    plot = transport_mode_to_facility_receiving_plot,
    path = plot_path
  )
}

# case count by ISS range

{
  case_count_iss_range_df <- trauma_2024 |>
    dplyr::filter(!is.na(ISS_Range)) |>
    dplyr::mutate(
      ISS_Range = factor(
        ISS_Range,
        levels = c("1 - 8", "9 - 15", "16+"),
        labels = c("1-8", "9-15", "16+")
      ),
      Level = factor(Level, levels = c("I", "II", "III", "IV"))
    ) |>
    injury_case_count(ISS_Range, Level) |>
    dplyr::arrange(ISS_Range, Level) |>
    dplyr::mutate(
      number_label = prettyNum(n, big.mark = ","),
      percent = n / sum(n),
      percent_label = traumar::pretty_percent(percent),
      full_label = paste0(number_label, " (", percent_label, ")"),
      .by = ISS_Range
    )
}

# case count by ISS range plot

{
  case_count_iss_range_plot <- case_count_iss_range_df |>
    ggplot2::ggplot(ggplot2::aes(
      x = fct_rev(Level),
      y = n,
      fill = Level,
      label = full_label
    )) +
    ggplot2::geom_col(color = "black", alpha = 0.75) +
    ggplot2::geom_text(
      ggplot2::aes(y = dplyr::if_else(n > 1400, 0, n)),
      nudge_y = dplyr::if_else(case_count_iss_range_df$n > 1400, 525, 500),
      family = "Work Sans",
      size = 7,
      color = dplyr::if_else(
        case_count_iss_range_df$n > 1400,
        "white",
        "black"
      )
    ) +
    ggplot2::coord_flip() +
    ggplot2::facet_grid(rows = vars(ISS_Range), switch = "y") +
    ggplot2::labs(
      x = "",
      y = "\n",
      title = "Count of Cases by ISS Range and Trauma Facility Verification Level",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024",
      caption = "These data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a patient enters an ED or\nhospital for treatment of an injury.\nProportions in each ISS Range row sum to 100%."
    ) +
    ggplot2::guides(fill = "none") +
    traumar::theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 2,
      vjust_subtitle = 1.25,
      facet_text_size = 18,
      strip.placement = "outside",
      draw_panel_border = TRUE,
      facets = TRUE
    ) +
    ggthemes::scale_fill_colorblind() +
    ggplot2::scale_y_continuous(
      labels = function(x) traumar::pretty_number(x, n_decimal = 1)
    )

  plot_save_params(
    filename = "case_count_iss_range_plot.png",
    plot = case_count_iss_range_plot,
    path = plot_path
  )
}

# case count by ISS range and facility level at receiving facilities

{
  case_count_iss_range_receiving_df <- trauma_2024 |>
    dplyr::filter(!is.na(ISS_Range), Receiving == "Yes") |>
    dplyr::mutate(
      ISS_Range = factor(
        ISS_Range,
        levels = c("1 - 8", "9 - 15", "16+"),
        labels = c("1-8", "9-15", "16+")
      ),
      Level = factor(Level, levels = c("I", "II", "III", "IV"))
    ) |>
    injury_case_count(ISS_Range, Level) |>
    dplyr::arrange(ISS_Range, Level) |>
    dplyr::mutate(
      number_label = prettyNum(n, big.mark = ","),
      percent = n / sum(n),
      percent_label = traumar::pretty_percent(percent),
      full_label = paste0(number_label, " (", percent_label, ")"),
      .by = ISS_Range
    )
}


# case count by ISS range and facility level at receiving facilities plot

{
  case_count_iss_range_receiving_plot <- case_count_iss_range_receiving_df |>
    ggplot2::ggplot(ggplot2::aes(
      x = fct_rev(Level),
      y = n,
      fill = Level,
      label = full_label
    )) +
    ggplot2::geom_col(color = "black", alpha = 0.75) +
    ggplot2::geom_text(
      ggplot2::aes(y = dplyr::if_else(n > 200, 0, n)),
      nudge_y = dplyr::if_else(
        case_count_iss_range_receiving_df$n > 200,
        100,
        90
      ),
      family = "Work Sans",
      size = 7,
      color = dplyr::if_else(
        case_count_iss_range_receiving_df$n > 200,
        "white",
        "black"
      )
    ) +
    ggplot2::coord_flip() +
    ggplot2::facet_grid(rows = vars(ISS_Range), switch = "y") +
    ggplot2::labs(
      x = "",
      y = "\n",
      title = "Count of Cases by ISS Range and Receiving Trauma Facility Verification Level",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024",
      caption = "These data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a patient enters an ED or\nhospital for treatment of an injury.\nProportions in each ISS Range row sum to 100%."
    ) +
    ggplot2::guides(fill = "none") +
    traumar::theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 2,
      vjust_subtitle = 1.25,
      facet_text_size = 18,
      strip.placement = "outside",
      draw_panel_border = TRUE,
      facets = TRUE
    ) +
    ggthemes::scale_fill_colorblind() +
    ggplot2::scale_y_continuous(
      labels = function(x) traumar::pretty_number(x, n_decimal = 1)
    )

  # save the plot

  plot_save_params(
    filename = "case_count_iss_range_receiving_plot.png",
    plot = case_count_iss_range_receiving_plot,
    path = plot_path
  )
}

# cause of injury frequency collapsed categories

{
  cause_of_injury_freq <- trauma_2024 |>
    dplyr::mutate(Level = factor(Level, levels = c("I", "II", "III", "IV"))) |>
    dplyr::filter(Level %in% c("I", "II", "III", "IV")) |>
    injury_case_count(Level, CAUSE_OF_INJURY_AR_1) |>
    dplyr::arrange(Level, desc(n)) |>
    dplyr::mutate(number_label = prettyNum(n, big.mark = ",")) |>
    drop_na()
}

# cause of injury frequency collapsed categories plot

{
  cause_of_injury_freq_plot <- cause_of_injury_freq |>
    ggplot2::ggplot(ggplot2::aes(
      x = reorder(Level, n),
      y = n,
      fill = CAUSE_OF_INJURY_AR_1,
      label = dplyr::if_else(n < 200, "", number_label)
    )) +
    ggplot2::geom_col(color = "black", alpha = 0.75, position = "stack") +
    ggplot2::geom_text(
      position = position_stack(vjust = 0.5),
      size = 8,
      color = "black",
      family = "Work Sans",
      fontface = "bold",
      angle = dplyr::if_else(cause_of_injury_freq$n < 500, 90, 0)
    ) +
    ggplot2::labs(
      x = "",
      y = "Case Count\n",
      title = "Cause of Injury Frequency by Trauma Facility Verification Level",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024",
      caption = "- These data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a patient enters an ED or\n   hospital for treatment of an injury.\n- As the colors descend in the legend from top to bottom, so are the colors ordered in the bars from right to left."
    ) +
    ggplot2::coord_flip() +
    traumar::theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 2,
      vjust_subtitle = 1,
      legend_position = "inside",
      legend.position.inside = c(.75, .25)
    ) +
    theme(legend.title = element_blank()) +
    ggplot2::scale_fill_paletteer_d(palette = "colorblindr::OkabeIto_black") +
    ggplot2::scale_y_continuous(
      labels = function(x) traumar::pretty_number(x, n_decimal = 1)
    )

  plot_save_params(
    filename = "cause_of_injury_freq_plot.png",
    plot = cause_of_injury_freq_plot,
    path = plot_path
  )
}

# additional cause of injury frequency df

{
  injuries_not_needed_pattern <- c("fall|motor|mvc|firearm|struck")

  cause_of_injury_additional_freq <- trauma_2024 |>
    dplyr::filter(
      !is.na(LEVEL_FALL1_1),
      !grepl(
        pattern = injuries_not_needed_pattern,
        x = LEVEL_FALL1_1,
        ignore.case = TRUE
      )
    ) |>
    injury_case_count(LEVEL_FALL1_1, sort = TRUE) |>
    dplyr::mutate(
      LEVEL_FALL1_1 = case_when(
        LEVEL_FALL1_1 == "Other Specified, Unintentional" ~
          "Other-Unintentional",
        LEVEL_FALL1_1 == "Other Specified, Assault" ~ "Other-Assault",
        LEVEL_FALL1_1 == "Poisoning, Non-Drug" ~ "Poisoning Non-Drug",
        TRUE ~ LEVEL_FALL1_1
      ),
      number_label = small_count_label(var = n, cutoff = 6, replacement = "*"),
      full_label = dplyr::if_else(
        n < 50,
        paste0(LEVEL_FALL1_1, " (", number_label, ")"),
        paste0(LEVEL_FALL1_1, "\n(", number_label, ")")
      )
    )
}

# additional cause of injury frequency treemap using treemapify package

{
  cause_of_injury_additional_freq_plots <- cause_of_injury_additional_freq |>
    ggplot2::ggplot(ggplot2::aes(area = n, label = full_label, fill = n)) +
    treemapify::geom_treemap(
      color = "white",
      layout = "squarified",
      start = "bottomleft"
    ) +
    treemapify::geom_treemap_text(
      family = "Work Sans",
      size = 18,
      color = "white",
      fontface = "bold"
    ) +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(
      title = "Cause of Injury Frequency with Expanded Categories",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024",
      caption = "Read the order of factors by changing color and box area, signaling decreasing count, from the bottom left to top right.\nThese data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a patient enters an ED or hospital\nfor treatment of an injury."
    ) +
    traumar::theme_cleaner(
      title_text_size = 20,
      subtitle_text_size = 18,
      base_size = 15,
      vjust_title = 2,
      vjust_subtitle = 1
    ) +
    ggplot2::scale_fill_viridis(option = "cividis", direction = -1)

  # save the plot

  plot_save_params(
    filename = "cause_of_injury_additional_freq_plots.png",
    plot = cause_of_injury_additional_freq_plots,
    path = plot_path
  )
}

# transfers out by trauma level df

{
  transfers_out_by_trauma_lvl <- trauma_2024 |>
    dplyr::filter(Transfer_Out == "Yes") |>
    injury_case_count(Level) |>
    dplyr::mutate(
      number_label = prettyNum(n, big.mark = ","),
      full_label = paste0(Level, " (", number_label, ")"),
      size_mod = log(n)
    )
}

# transfers out by trauma level plot

{
  transfers_out_by_trauma_lvl_plot <- transfers_out_by_trauma_lvl |>
    ggplot2::ggplot(ggplot2::aes(Level, n, fill = Level, label = full_label)) +
    ggplot2::geom_point(
      shape = 21,
      color = "black",
      size = 6 * transfers_out_by_trauma_lvl$size_mod
    ) +
    ggplot2::geom_text(
      family = "Work Sans",
      size = 7,
      fontface = "bold",
      color = "white"
    ) +
    ggplot2::coord_flip() +
    ggplot2::guides(color = "none", size = "none", fill = "none") +
    ggplot2::labs(
      title = "Cases Transferring Out by Trauma Verification Level",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024",
      x = "",
      y = "Case Count",
      caption = "These data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a patient enters an ED or hospital\nfor treatment of an injury."
    ) +
    traumar::theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      axis.text.y = element_blank()
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) traumar::pretty_number(x, n_decimal = 1),
      limits = c(-100, 5000)
    ) +
    ggthemes::scale_fill_colorblind()

  # save the plot of transfer cases

  plot_save_params(
    filename = "transfers_out_by_trauma_lvl_plot.png",
    plot = transfers_out_by_trauma_lvl_plot,
    path = plot_path
  )
}

# transfer delays among patients being transferred out df

{
  # source df to get reference values

  transfer_delays_transfer_out <- trauma_2024 |>
    dplyr::filter(Transfer_Out == "Yes") |>
    tidyr::replace_na(list(Transfer_Delay_Reason = "Not Applicable")) |>
    dplyr::mutate(
      Transfer_Delay_Reason = dplyr::if_else(
        grepl(
          pattern = "select|not\\sknown|not\\sapplicable",
          x = Transfer_Delay_Reason,
          ignore.case = TRUE
        ),
        "Missing",
        Transfer_Delay_Reason
      )
    ) |>
    injury_case_count(Transfer_Delay_Reason, sort = TRUE) |>
    dplyr::mutate(
      number_label = prettyNum(n, big.mark = ","),
      size_mod = log(n)
    )

  # df for plotting

  transfer_delays_transfer_out_main <- transfer_delays_transfer_out |>
    dplyr::filter(Transfer_Delay_Reason %not_in% c("Missing", "Other"))
}

# get the 'missing' and 'other' values for the transfer delay reasons

{
  missing_transfer_delays <- transfer_delays_transfer_out |>
    dplyr::filter(Transfer_Delay_Reason == "Missing") |>
    dplyr::pull(n)

  other_transfer_delays <- transfer_delays_transfer_out |>
    dplyr::filter(Transfer_Delay_Reason == "Other") |>
    dplyr::pull(n)
}

# transfer delays among patients being transferred out plot

{
  transfer_delays_transfer_out_plot <- transfer_delays_transfer_out_main |>
    dplyr::mutate(
      Transfer_Delay_Reason = case_when(
        Transfer_Delay_Reason ==
          "Delayed identification that the patient needed trauma center resources" ~
          "Delayed identification of Pt. need",
        TRUE ~ Transfer_Delay_Reason
      ),
      Transfer_Delay_Reason = str_replace_all(
        Transfer_Delay_Reason,
        pattern = "[Pp]atient",
        replacement = "Pt."
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(
      reorder(Transfer_Delay_Reason, n),
      n,
      fill = n,
      label = small_count_label(var = n, cutoff = 6, replacement = "*")
    )) +
    ggplot2::geom_col(color = "black", width = 0.5) +
    ggplot2::geom_text(
      nudge_y = dplyr::if_else(
        transfer_delays_transfer_out_main$n > 100,
        transfer_delays_transfer_out_main$size_mod * 2.5,
        dplyr::if_else(
          transfer_delays_transfer_out_main$n < 100 &
            transfer_delays_transfer_out_main$n > 10,
          transfer_delays_transfer_out_main$size_mod * 3,
          8
        )
      ),
      family = "Work Sans",
      size = 8,
      fontface = "bold",
      color = "black"
    ) +
    ggplot2::coord_flip() +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(
      title = "Transfer Delay Reasons Among Patients Being Transferred Out",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024",
      caption = paste0(
        "- '*' indicates small counts that are masked to protect confidentiality\n- These data reflect cases.  Cases are defined as each distinct episode when a patient\n   enters an ED or hospital for treatment of an injury.",
        "\n- There were ",
        prettyNum(missing_transfer_delays, big.mark = ","),
        " cases that were not delayed or were missing a category, and ",
        other_transfer_delays,
        "\n   marked as 'other.'"
      ),
      x = "",
      y = "Case Count\n"
    ) +
    ggplot2::scale_fill_viridis(option = "magma", direction = -1) +
    ggplot2::scale_y_continuous(
      labels = function(x) traumar::pretty_number(x, n_decimal = 1)
    ) +
    traumar::theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1
    )

  # save the transfer delay reason plot

  plot_save_params(
    filename = "transfer_delays_transfer_out_plot.png",
    plot = transfer_delays_transfer_out_plot,
    path = plot_path
  )
}

# average ED stay prior to transfer by ISS range df

{
  avg_ed_stay_transfers_iss <- trauma_2024 |>
    dplyr::filter(Transfer_Out == "Yes") |>
    dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
    dplyr::mutate(
      Trauma_Team_Activated = factor(
        Trauma_Team_Activated,
        levels = c("Trauma Team Activated", "Trauma Team Not Activated"),
        labels = c("Activated", "Not Activated")
      ),
      ISS_Range = factor(
        ISS_Range,
        levels = c("1 - 8", "9 - 15", "16+"),
        labels = c("1-8", "9-15", "16+")
      ),
      Length_of_Stay = traumar::impute(
        Length_of_Stay,
        method = "winsorize",
        percentile = 0.90
      ),
      Length_of_Stay = traumar::impute(
        Length_of_Stay,
        focus = "missing",
        method = "mean"
      ),
      .by = ISS_Range
    ) |>
    dplyr::summarize(
      median_los = median(Length_of_Stay, na.rm = TRUE),
      avg_los = mean(Length_of_Stay, na.rm = TRUE),
      .by = c(Trauma_Team_Activated, ISS_Range)
    ) |>
    dplyr::mutate(mod = log(avg_los)) |>
    dplyr::arrange(Trauma_Team_Activated, ISS_Range)

  # get differences

  avg_ed_stay_transfers_iss_diff <- avg_ed_stay_transfers_iss |>
    dplyr::select(-c(median_los, mod)) |>
    pivot_wider(
      id_cols = ISS_Range,
      names_from = Trauma_Team_Activated,
      values_from = avg_los
    ) |>
    dplyr::mutate(diff = abs(Activated - `Not Activated`))

  # get overall avg diff

  avg_diff_ed_los <- avg_ed_stay_transfers_iss_diff |>
    dplyr::summarize(mean = round(mean(diff), digits = 1)) |>
    dplyr::pull(mean)
}

# average ED stay prior to transfer by ISS range plot

{
  avg_ed_stay_transfers_iss_plot <- avg_ed_stay_transfers_iss |>
    ggplot2::ggplot(ggplot2::aes(
      x = fct_relevel(ISS_Range, rev(levels(ISS_Range))),
      y = avg_los,
      fill = ISS_Range,
      label = traumar::pretty_number(avg_los, n_decimal = 1)
    )) +
    ggplot2::geom_col(width = 0.5, color = "black") +
    ggplot2::geom_text(
      nudge_y = avg_ed_stay_transfers_iss$avg_los /
        (avg_ed_stay_transfers_iss$mod * 2.5),
      family = "Work Sans",
      size = 8,
      color = "black"
    ) +
    ggplot2::facet_grid(rows = vars(Trauma_Team_Activated), switch = "y") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Average ED Length of Stay in Minutes Prior to Transfer by ISS Range",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024",
      caption = "- ED LOS calculated from datetime of patient arrival to datetime of physical discharge.\n- These data reflect cases, which include transfers.  Cases are defined as each distinct episode when a patient enters an ED\n   or hospital for treatment of an injury.\n- Imputation methods: Winsorization at 10th / 90th percentiles, then mean imputation on missing values.",
      x = "",
      y = "Case Count\n",
      fill = "ISS Range"
    ) +
    ggplot2::scale_fill_paletteer_d(
      palette = "colorBlindness::PairedColor12Steps"
    ) +
    traumar::theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      facet_text_size = 18,
      strip.placement = "outside",
      legend_position = "inside",
      legend.position.inside = c(0.9, 0.75),
      legend.direction = "vertical",
      legend.key.spacing.y = unit(1, "cm"),
      draw_panel_border = TRUE,
      facets = TRUE
    )

  # save the avg Ed stay prior to transfer by iss range plot

  plot_save_params(
    filename = "avg_ed_stay_transfers_iss_plot.png",
    plot = avg_ed_stay_transfers_iss_plot,
    path = plot_path
  )
}

# average ED stay in minutes prior to transfer by ISS range and trauma level df

{
  avg_ED_LOS_transfer_iss_level <- trauma_2024 |>
    dplyr::filter(Transfer_Out == "Yes") |>
    dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
    dplyr::mutate(
      ISS_Range = factor(
        ISS_Range,
        levels = c("1 - 8", "9 - 15", "16+"),
        labels = c("1-8", "9-15", "16+")
      ),
      Length_of_Stay = traumar::impute(
        Length_of_Stay,
        method = "winsorize",
        percentile = 0.90
      ),
      Length_of_Stay = traumar::impute(
        Length_of_Stay,
        focus = "missing",
        method = "mean"
      ),
      .by = c(ISS_Range, Level)
    ) |>
    dplyr::summarize(
      avg_los = mean(Length_of_Stay),
      median_los = median(Length_of_Stay),
      min_los = min(Length_of_Stay),
      max_los = max(Length_of_Stay),
      N = dplyr::n(),
      N_label = dplyr::if_else(N < 6, "*", prettyNum(N, big.mark = ",")),
      mod = log(avg_los),
      .by = c(Trauma_Team_Activated, ISS_Range, Level)
    ) |>
    dplyr::arrange(Trauma_Team_Activated, ISS_Range, Level)
}

# average ED stay in minutes prior to transfer by ISS range and trauma level plot

{
  avg_ED_LOS_transfer_iss_level_plot <- avg_ED_LOS_transfer_iss_level |>
    ggplot2::ggplot(ggplot2::aes(
      x = ISS_Range,
      y = avg_los,
      fill = ISS_Range
    )) +
    ggplot2::geom_col(color = "black") +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = round(avg_los, digits = 1)),
      nudge_y = dplyr::if_else(
        avg_ED_LOS_transfer_iss_level$avg_los > 500,
        -1,
        1
      ),
      direction = "y",
      segment.color = NA,
      size = 8,
      color = dplyr::if_else(
        avg_ED_LOS_transfer_iss_level$avg_los > 500,
        "white",
        "black"
      ),
      family = "Work Sans"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(y = 65, label = paste0("n = ", N_label)),
      size = 8,
      color = "white",
      family = "Work Sans"
    ) +
    ggplot2::facet_grid(
      rows = vars(Level),
      cols = vars(Trauma_Team_Activated),
      switch = "y"
    ) +
    ggplot2::labs(
      title = "Average ED Length of Stay in Minutes Prior to Transfer by ISS Range and Trauma Level",
      subtitle = "Source: Iowa ImageTrend patient Registry | 2024",
      caption = "- Top value = average ED LOS, bottom value = # cases\n- ED LOS calculated from datetime of patient arrival to datetime of physical discharge.\n- Imputation methods: Winsorization at 10th / 90th percentiles, then mean imputation on missing values.\n- These data reflect cases, which include transfers.  Cases are defined as each distinct episode when a patient enters an ED or\n   hospital for treatment of an injury.",
      x = "",
      y = "",
      fill = "ISS Range"
    ) +
    ggplot2::scale_fill_paletteer_d(
      palette = "colorBlindness::ModifiedSpectralScheme11Steps",
      direction = 1
    ) +
    traumar::theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      facet_text_size = 18,
      strip.placement = "outside",
      axis.text.y = element_blank(),
      draw_panel_border = TRUE,
      facets = TRUE
    )

  # save the average ED stay in minutes prior to transfer by ISS range and trauma level plot

  plot_save_params(
    filename = "avg_ED_LOS_transfer_iss_level_plot.png",
    plot = avg_ED_LOS_transfer_iss_level_plot,
    path = plot_path
  )
}

# longitudinal average ED stay prior to transfer

# years and activation status

{
  longitudinal_avg_ed_los <- trauma_data_clean |>
    dplyr::filter(Transfer_Out == "Yes", Year %in% 2019:2024) |>
    dplyr::group_by(Year) |>
    dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Length_of_Stay = traumar::impute(
        Length_of_Stay,
        method = "winsorize",
        percentile = 0.90
      ),
      Length_of_Stay = traumar::impute(
        Length_of_Stay,
        focus = "missing",
        method = "mean"
      ),
      .by = c(Year, Trauma_Team_Activated)
    ) |>
    dplyr::summarize(
      avg_los = round(mean(Length_of_Stay), digits = 1),
      .by = c(Year, Trauma_Team_Activated)
    ) |>
    dplyr::arrange(Year, Trauma_Team_Activated) |>
    pivot_wider(
      id_cols = Year,
      names_from = Trauma_Team_Activated,
      values_from = avg_los
    ) |>
    set_names(nm = c("Year", "Activated", "Not Activated")) |>
    dplyr::mutate(
      label_1 = dplyr::if_else(
        Year == min(Year) | Year == max(Year),
        Activated,
        NA_real_
      ),
      label_2 = dplyr::if_else(
        Year == min(Year) | Year == max(Year),
        `Not Activated`,
        NA_real_
      )
    )
}

# year with no activation status strata

{
  longitudinal_avg_ed_los_year <- trauma_data_clean |>
    dplyr::filter(Transfer_Out == "Yes", Year %in% 2019:2024) |>
    dplyr::group_by(Year) |>
    dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Length_of_Stay = traumar::impute(
        Length_of_Stay,
        method = "winsorize",
        percentile = 0.90
      ),
      Length_of_Stay = traumar::impute(
        Length_of_Stay,
        focus = "missing",
        method = "mean"
      ),
      .by = Year
    ) |>
    dplyr::summarize(
      avg_los = round(mean(Length_of_Stay), digits = 1),
      median_los = median(Length_of_Stay),
      min_los = min(Length_of_Stay),
      max_los = max(Length_of_Stay),
      N = dplyr::n(),
      N_label = dplyr::if_else(N < 6, "*", prettyNum(N, big.mark = ",")),
      mod = log(avg_los),
      .by = Year
    ) |>
    dplyr::arrange(Year)
}

# longitudinal average ED stay prior to transfer plot

{
  longitudinal_avg_ed_los_plot <- longitudinal_avg_ed_los_year |>
    ggplot2::ggplot(ggplot2::aes(factor(Year), avg_los, fill = factor(Year))) +
    ggplot2::geom_col(alpha = 0.1) +
    ggplot2::geom_text(
      ggplot2::aes(y = 20, label = paste0(avg_los, "\n", "n = ", N_label)),
      family = "Work Sans",
      size = 8,
      color = "black"
    ) +
    ggplot2::geom_line(
      data = longitudinal_avg_ed_los,
      ggplot2::aes(
        x = factor(Year),
        y = Activated,
        color = "dodgerblue",
        group = 1
      ),
      linewidth = 2.25,
      lineend = "round",
      linejoin = "round"
    ) +
    ggrepel::geom_text_repel(
      data = longitudinal_avg_ed_los,
      ggplot2::aes(x = factor(Year), y = Activated, label = label_1),
      family = "Work Sans",
      size = 8,
      color = "black",
      direction = "y",
      nudge_y = 1,
      max.iter = 30000,
      segment.color = NA
    ) +
    ggplot2::geom_line(
      data = longitudinal_avg_ed_los,
      ggplot2::aes(
        x = factor(Year),
        y = `Not Activated`,
        color = "orangered",
        group = 2
      ),
      linewidth = 2.25,
      lineend = "round",
      linejoin = "round"
    ) +
    ggrepel::geom_text_repel(
      data = longitudinal_avg_ed_los,
      ggplot2::aes(x = factor(Year), y = `Not Activated`, label = label_2),
      family = "Work Sans",
      size = 8,
      color = "black",
      direction = "y",
      nudge_y = 4,
      max.iter = 30000,
      segment.color = NA
    ) +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(
      title = "Longitudinal Average ED Length of Stay Prior to Transfer",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2019-2024",
      caption = "- Top value = average ED LOS, bottom value = # cases\n- ED LOS calculated from datetime of patient arrival to datetime of physical discharge.\n- Imputation methods: Winsorization at 10th / 90th percentiles, then mean imputation on missing values.\n- These data reflect cases.  Cases are defined as each distinct episode when a patient enters an ED or\n   hospital for treatment of an injury.",
      x = "",
      y = ""
    ) +
    ggplot2::scale_fill_viridis_d(option = "cividis", direction = -1) +
    ggplot2::scale_color_manual(
      name = "Activation Status",
      values = c("dodgerblue", "orangered"),
      labels = c("Activated", "Not Activated")
    ) +
    traumar::theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1
    )

  # save the longitudinal ED LOS plot

  plot_save_params(
    filename = "longitudinal_avg_ed_los_plot.png",
    plot = longitudinal_avg_ed_los_plot,
    path = plot_path
  )
}
