###_____________________________________________________________________________
# Hospital admissions ----
###_____________________________________________________________________________

####
# You must first run data_load.R and setup.R before running this script
####

# longitudinal cases

{
  ipop_longitudinal_cases <- ipop_data_clean |>
    ipop_case_count(Year, which = "Inpatient", descriptive_stats = TRUE) |>
    dplyr::select(-change_label) |>
    dplyr::rename(`Total Cases` = n, `% Change in Cases` = change) |>
    tidyr::pivot_longer(
      cols = `Total Cases`:`% Change in Cases`,
      names_to = "Category",
      values_to = "Value"
    ) |>
    tidyr::pivot_wider(
      id_cols = Category,
      names_from = Year,
      values_from = Value
    ) |>
    dplyr::mutate(
      `2020-2024 Trend` = list(c(
        `2020`,
        `2021`,
        `2022`,
        `2023`,
        `2024`
      )),
      .by = Category
    ) |>
    dplyr::select(-c(`2020`, `2019`, `2020`))
}

# longitudinal patients

{
  ipop_longitudinal_patients <- ipop_data_clean |>
    ipop_patient_count(Year, which = "Inpatient", descriptive_stats = TRUE) |>
    dplyr::filter(Year >= 2020, Year < 2024) |>
    dplyr::select(-change_label) |>
    rename(`Total Pts.` = n, `% Change in Pts.` = change) |>
    pivot_longer(
      cols = `Total Pts.`:`% Change in Pts.`,
      names_to = "Category",
      values_to = "Value"
    ) |>
    pivot_wider(id_cols = Category, names_from = Year, values_from = Value) |>
    dplyr::mutate(
      `2020-2024 Trend` = list(c(
        `2020`,
        `2019`,
        `2020`,
        `2021`,
        `2023`,
        `2024`
      )),
      .by = Category
    ) |>
    dplyr::select(-c(`2020`, `2019`, `2020`))
}

# join the case and patient count data

ipop_longitudinal_case_patient <- bind_rows(
  ipop_longitudinal_cases,
  ipop_longitudinal_patients
)

# illustrate with a gt() table

{
  ipop_longitudinal_case_patient_tbl <- ipop_longitudinal_case_patient |>
    gt() |>
    gt_plt_sparkline(
      column = `2020-2024 Trend`,
      type = "shaded",
      same_limit = FALSE,
      label = TRUE
    ) |>
    tab_header(
      title = "Count and Rate of Change in IPOP Inpatient Injury Hospitalizations",
      subtitle = "Source: Iowa Inpatient Outpatient Database 2020-2024"
    ) |>
    fmt_percent(
      columns = `2021`:`2024`,
      rows = c(2, 4),
      drop_trailing_zeros = TRUE
    ) |>
    fmt_number(
      columns = `2021`:`2024`,
      rows = c(1, 3),
      drop_trailing_zeros = TRUE
    ) |>
    tab_row_group(label = "Cases", rows = 1:2) |>
    tab_row_group(label = "Patients", rows = 3:4) |>
    row_group_order(groups = c("Cases", "Patients")) |>
    tab_source_note(
      source_note = md(
        paste0(
          fontawesome::fa("magnifying-glass"),
          " Patients meeting trauma registry inclusionary criteria only"
        )
      )
    ) |>
    tab_source_note(
      source_note = md(
        paste0(
          fontawesome::fa("sticky-note"),
          " These data reflect inpatient cases from the IPOP database, only."
        )
      )
    ) |>
    tab_style_hhs(border_cols = `2021`:`2020-2024 Trend`)
}

# age distribution of hospital admissions

ipop_age_dist <- ipop_data_clean |>
  dplyr::filter(Year == 2024) |>
  ipop_case_count(Census_Age_Group, which = "Inpatient") |>
  dplyr::mutate(
    full_label = paste0(
      Census_Age_Group,
      "\n(",
      traumar::pretty_number(n, n_decimal = 2),
      ")"
    ),
    font_color = dplyr::if_else(
      Census_Age_Group %not_in% c("0-4", "5-9", "10-14"),
      "white",
      hhs_palette_2$primary_2
    )
  )

# plot the age distribution within the IPOP database

{
  ipop_age_dist_bar <- ipop_age_dist |>
    ggplot2::ggplot(ggplot2::aes(area = n, label = full_label, fill = n)) +
    treemapify::geom_treemap(
      color = "white",
      layout = "squarified",
      start = "bottomleft"
    ) +
    treemapify::geom_treemap_text(
      family = "Work Sans",
      size = 18,
      color = ipop_age_dist$font_color,
      fontface = "bold"
    ) +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(
      title = "Age Distribution of Cases in the IPOP Database",
      subtitle = "Source: Iowa Inpatient Outpatient Database | 2024",
      caption = "Read the order of factors by changing color and box area, signaling decreasing count, from the bottom left to top right.\nThese data reflect inpatient cases from the IPOP database, only."
    ) +
    traumar::theme_cleaner(
      title_text_size = 20,
      subtitle_text_size = 18,
      base_size = 15,
      vjust_title = 1.75,
      vjust_subtitle = 1
    ) +
    ggplot2::scale_fill_viridis(option = "rocket", direction = -1)

  # save the treemap

  plot_save_params(
    filename = "ipop_age_dist_bar.png",
    plot = ipop_age_dist_bar,
    path = plot_path
  )
}

# IPOP nature of injury frequency

ipop_nature_injury_freq <- ipop_data_clean |>
  dplyr::filter(Year == 2024) |>
  dplyr::mutate(
    NATURE_OF_INJURY_DESCRIPTOR = dplyr::if_else(
      NATURE_OF_INJURY_DESCRIPTOR %in% c("Unspecified", "Other specified"),
      "Other",
      NATURE_OF_INJURY_DESCRIPTOR
    )
  ) |>
  ipop_case_count(
    NATURE_OF_INJURY_DESCRIPTOR,
    sort = TRUE,
    which = "Inpatient"
  ) |>
  tidyr::replace_na(list(NATURE_OF_INJURY_DESCRIPTOR = "Missing")) |>
  dplyr::mutate(
    mod = sqrt(n),
    angle = 2 * pi * rank(mod) / dplyr::n(),
    angle_mod = cos(angle)
  )

# plot nature of injury frequency via area chart
# if this plot saves dark, you can load in Paint, edit size down, and it will turn to white background

{
  ipop_nature_injury_freq_plot <- ipop_nature_injury_freq |>
    ggplot2::ggplot(ggplot2::aes(
      x = reorder(str_wrap(NATURE_OF_INJURY_DESCRIPTOR, width = 5), -mod),
      y = mod,
      fill = mod,
      label = traumar::pretty_number(n, n_decimal = 2)
    )) +
    ggplot2::geom_col(position = "dodge2", show.legend = TRUE, alpha = 0.9) +
    # First segment: from the top of the bar to just before the text
    ggplot2::geom_segment(
      ggplot2::aes(
        x = reorder(str_wrap(NATURE_OF_INJURY_DESCRIPTOR, 5), -mod),
        xend = reorder(str_wrap(NATURE_OF_INJURY_DESCRIPTOR, 5), -mod),
        y = mod, # Start at the top of the bar
        yend = dplyr::if_else(
          NATURE_OF_INJURY_DESCRIPTOR == "Fracture",
          mod,
          max(mod) - 2
        ) # End just before the text
      ),
      linetype = "dashed",
      color = hhs_palette_1$accent_2
    ) +
    ggplot2::geom_label(
      fill = dplyr::if_else(
        ipop_nature_injury_freq$NATURE_OF_INJURY_DESCRIPTOR == "Fracture",
        "transparent",
        "white"
      ),
      nudge_y = dplyr::if_else(
        ipop_nature_injury_freq$NATURE_OF_INJURY_DESCRIPTOR == "Fracture",
        -10,
        20
      ),
      color = dplyr::if_else(
        ipop_nature_injury_freq$NATURE_OF_INJURY_DESCRIPTOR == "Fracture",
        "transparent",
        "white"
      )
    ) +
    ggplot2::geom_text(
      nudge_y = dplyr::if_else(
        ipop_nature_injury_freq$NATURE_OF_INJURY_DESCRIPTOR == "Fracture",
        -12,
        20
      ),
      family = "Work Sans",
      fontface = "bold",
      color = dplyr::if_else(
        ipop_nature_injury_freq$NATURE_OF_INJURY_DESCRIPTOR == "Fracture",
        "white",
        "black"
      ),
      size = 8
    ) +
    ggplot2::coord_radial(clip = "off", inner.radius = 0.15) +
    ggplot2::labs(
      x = "Nature of Injury Description\n",
      y = "",
      fill = str_wrap("Orange to blue High to low count", width = 15),
      title = "Nature of Injury Frequency Among IPOP Trauma Cases",
      subtitle = "Source: Iowa Inpatient Outpatient Database | 2024",
      caption = "Note: Square-root transformation applied to the y-axis due to the 'Fracture' category outlier,\nthe labels are true case counts."
    ) +
    ggplot2::scale_fill_paletteer_c(
      palette = "ggthemes::Orange-Blue Diverging",
      direction = -1
    ) +
    traumar::theme_cleaner(
      base_size = 15,
      base_background = "white",
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      axis.text.y = element_blank(),
      legend_position = "right"
    ) +
    theme(
      legend.text = element_blank(),
      plot.background = element_rect(
        fill = "transparent",
        color = "transparent"
      )
    )

  # save the nature of injury frequency plot for the IPOP data

  plot_save_params(
    filename = "ipop_nature_injury_freq_plot.png",
    plot = ipop_nature_injury_freq_plot,
    path = plot_path,
    height = 9,
    width_ratio = 2
  )
}

# IPOP body region injury frequency table

ipop_body_region <- ipop_data_clean |>
  dplyr::filter(Year == 2024) |>
  tidyr::replace_na(list(
    BODY_REGION_CATEGORY_LEVEL_1 = "Unclassifiable by body region"
  )) |>
  ipop_case_count(
    BODY_REGION_CATEGORY_LEVEL_1,
    sort = TRUE,
    which = "Inpatient"
  ) |>
  dplyr::mutate(
    BODY_REGION_CATEGORY_LEVEL_1 = replace_symbol(BODY_REGION_CATEGORY_LEVEL_1),
    BODY_REGION_CATEGORY_LEVEL_1 = dplyr::if_else(
      grepl(
        pattern = "unclass",
        x = BODY_REGION_CATEGORY_LEVEL_1,
        ignore.case = TRUE
      ),
      "Unclassifiable",
      dplyr::if_else(
        grepl(
          pattern = "head and neck",
          x = BODY_REGION_CATEGORY_LEVEL_1,
          ignore.case = TRUE
        ),
        "Head and neck",
        BODY_REGION_CATEGORY_LEVEL_1
      )
    ),
    mod = sqrt(n),
    angle = 2 * pi * rank(mod) / dplyr::n(),
    angle_mod = cos(angle)
  )

# make the circular bar plot

{
  ipop_body_region_plot <- ipop_body_region |>
    ggplot2::ggplot(ggplot2::aes(
      x = reorder(BODY_REGION_CATEGORY_LEVEL_1, -mod),
      y = mod,
      fill = mod,
      label = traumar::pretty_number(n, n_decimal = 2)
    )) +
    ggplot2::geom_col(position = "dodge2", width = 0.5) +
    ggplot2::geom_text(
      family = "Work Sans",
      size = 8,
      color = c(rep("white", 2), rep("black", 4)),
      fontface = "bold",
      nudge_y = c(-10, -15, 8, 8, 10, 8)
    ) +
    #ylim(-1, 9.1) +
    ggplot2::labs(
      x = "",
      y = "",
      title = "Body Region of Injury Frequency Among Trauma Cases",
      subtitle = "Source: Iowa Inpatient Outpatient Database | 2024",
      caption = "Note: Square-root transformation applied to the y-axis due to the 'Extremities' category outlier,\nthe labels are true case counts.",
      fill = str_wrap("Dark to light red High to low count", width = 19)
    ) +
    ggplot2::scale_fill_paletteer_c(
      palette = "grDevices::Reds",
      direction = -1
    ) +
    ggplot2::coord_radial(start = 0, clip = "off") +
    traumar::theme_cleaner(
      base_size = 15,
      base_background = "white",
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      axis.text.y = element_blank(),
      legend_position = "right"
    ) +
    theme(
      legend.text = element_blank(),
      plot.background = element_rect(
        fill = "transparent",
        color = "transparent"
      )
    )

  # save the body region injury frequency plot

  plot_save_params(
    filename = "ipop_body_region_plot.png",
    plot = ipop_body_region_plot,
    path = plot_path,
    height = 9,
    width_ratio = 2
  )
}
