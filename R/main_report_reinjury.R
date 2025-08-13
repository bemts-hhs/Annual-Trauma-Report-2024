###_____________________________________________________________________________
# Reinjury - Main Report Section ----
###_____________________________________________________________________________

####
# You must first run data_load.R and setup.R before running this script
####

# Get reinjured patient identifiers

{
  reinjured_patients <- trauma_2024 |>
    dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE) |>
    dplyr::mutate(
      reinjury = dplyr::if_else(dplyr::n() > 1, TRUE, FALSE),
      .by = Unique_Patient_ID
    ) |>
    dplyr::filter(reinjury == T) |>
    dplyr::pull(Unique_Patient_ID)
}

# df giving patients and their injury category

{
  injury_category_patients <- trauma_2024 |>
    dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE) |>
    dplyr::mutate(
      reinjured = dplyr::if_else(
        Unique_Patient_ID %in% reinjured_patients,
        TRUE,
        FALSE
      )
    ) |>
    dplyr::mutate(
      injury_category = dplyr::if_else(
        dplyr::n() < 2,
        paste0(dplyr::n(), " injury event"),
        paste0(dplyr::n(), " injury events")
      ),
      .by = Unique_Patient_ID
    ) |>
    dplyr::filter(!is.na(Unique_Patient_ID)) |>
    dplyr::distinct(Unique_Patient_ID, injury_category)
}

# tidyr::complete df including reinjury category

{
  reinjured_trauma_2024 <- trauma_2024 |>
    dplyr::mutate(
      reinjured = dplyr::if_else(
        Unique_Patient_ID %in% reinjured_patients,
        TRUE,
        FALSE
      )
    ) |>
    dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE) |>
    dplyr::mutate(n_injuries = dplyr::n(), .by = Unique_Patient_ID) |>
    dplyr::mutate(
      n_injury_cat = dplyr::if_else(
        n_injuries < 2,
        paste0(n_injuries, " injury event"),
        paste0(n_injuries, " injury events")
      ),
      n_injury_cat = factor(n_injury_cat)
    )
}

# plot the reinjured patients

{
  reinjured_trauma_2024_plot <- reinjured_trauma_2024 |>
    dplyr::filter(!is.na(Unique_Patient_ID)) |>
    ggplot2::ggplot(ggplot2::aes(
      Unique_Patient_ID,
      n_injuries,
      size = n_injuries,
      fill = n_injuries
    )) +
    ggplot2::geom_point(
      shape = 21,
      color = "black",
      position = position_jitter(),
      alpha = 0.5
    ) +
    ggplot2::labs(
      x = "Reinjured Patients in Registry per Injury Date\n",
      y = paste0("# Injury Events in ", max(reinjured_trauma_2024$Year)),
      fill = "# Injury Events",
      title = "Iowa Trauma Patient Reinjury - All Patients",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024",
      caption = "Reinjured patients are identified by more than one injury date for any unique patient identifier.\nIncreasing color intensity and point size indicate higher injury counts"
    ) +
    ggplot2::guides(size = "none") +
    ggplot2::scale_fill_viridis(option = "turbo", direction = 1) +
    ggplot2::scale_y_continuous(breaks = 1:8, labels = 1:8) +
    traumar::theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      axis.text.x = element_blank(),
      legend_position = "inside",
      legend.position.inside = c(0.945, 0.9),
      legend.direction = "vertical"
    ) +
    theme(legend.title = element_text(vjust = 2.5))

  # save the plot of reinjured patients

  plot_save_params(
    filename = "reinjured_trauma_2024_plot.png",
    plot = reinjured_trauma_2024_plot,
    path = plot_path
  )
}

# table of reinjured patients and trend statistics

reinjured_patients_tbl <- trauma_data_clean |>
  reinjury_patient_count(descriptive_stats = TRUE)

# create the gt() table

{
  reinjured_patients_tbl_gt <- reinjured_patients_tbl |>
    dplyr::filter(Year < 2024) |>
    dplyr::select(-matches("min|max|_label")) |>
    rename(
      `# Reinjured Pts.` = Reinjury,
      `Total # Pts.` = n,
      `Avg # Injury Events per Pt.` = Avg_Injuries,
      `% Change in Reinjured Pts.` = change,
      `Proportion Reinjured Pts.` = prop
    ) |>
    pivot_longer(
      cols = `# Reinjured Pts.`:`Proportion Reinjured Pts.`,
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
    dplyr::select(Category, `2021`:`2020-2024 Trend`) |>
    gt() |>
    gt_plt_sparkline(
      column = `2020-2024 Trend`,
      type = "shaded",
      same_limit = FALSE,
      label = FALSE
    ) |>
    fmt_number(rows = 1:3, drop_trailing_zeros = TRUE) |>
    fmt_percent(rows = 4:5, drop_trailing_zeros = TRUE) |>
    tab_row_group(label = "Counts", rows = 1:2) |>
    tab_row_group(label = "Proportion and Change", rows = 3:5) |>
    row_group_order(groups = c("Counts", "Proportion and Change")) |>
    tab_header(
      title = "Summary: Trend of Reinjured Patients in Iowa",
      subtitle = "Patients Seen at a Trauma Center | Data: iowa Trauma Registry 2020-2024"
    ) |>
    tab_style_hhs(border_cols = `2021`:`2020-2024 Trend`)
}

# gender reinjuries

{
  gender_reinjury_events_tbl <- reinjured_trauma_2024 |>
    dplyr::filter(!is.na(Unique_Patient_ID)) |>
    dplyr::distinct(Unique_Patient_ID, Patient_Gender, n_injury_cat) |>
    dplyr::count(Patient_Gender, n_injury_cat) |>
    dplyr::mutate(
      number_label = prettyNum(
        x = small_count_label(var = n, cutoff = 6, replacement = NA_integer_),
        big.mark = ","
      ),
      mod = log(n)
    ) |>
    replace_with_na(list(number_label = "NA")) |>
    tidyr::replace_na(list(Patient_Gender = "Missing", number_label = "*"))

  # special gender category cases to be mentioned in the caption of the plot

  non_binary_reinjury <- gender_reinjury_events_tbl |>
    dplyr::filter(grepl(
      pattern = "binary",
      x = Patient_Gender,
      ignore.case = TRUE
    )) |>
    dplyr::pull(n)

  missing_gender_reinjury <- gender_reinjury_events_tbl |>
    dplyr::filter(grepl(
      pattern = "missing",
      x = Patient_Gender,
      ignore.case = TRUE
    )) |>
    dplyr::pull(n)
}

# plot gender reinjury patients

{
  gender_reinjury_events_plot <- gender_reinjury_events_tbl |>
    dplyr::filter(Patient_Gender %in% c("Male", "Female")) |>
    ggplot2::ggplot(ggplot2::aes(
      n_injury_cat,
      mod,
      fill = n_injury_cat,
      label = number_label
    )) +
    ggplot2::geom_col(width = 0.5, color = "black") +
    ggrepel::geom_text_repel(
      direction = "y",
      nudge_y = 0.25,
      max.iter = 30000,
      segment.color = NA,
      size = 8,
      family = "Work Sans",
      color = "black"
    ) +
    ggplot2::facet_wrap(Patient_Gender ~ .) +
    ggplot2::labs(
      x = "",
      y = "# Patients (log)",
      title = "Iowa Trauma Patient Reinjury by Gender",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024",
      caption = paste0(
        "- '*' indicates counts < 6 masked to protect confidentiality\n- Reinjured patients are identified by more than one injury date for any unique patient identifier.\n- Log scale used for y axis due to the '1 injury event' group outlier, labels are actual reinjured patient counts.\n",
        "- Non-binary/indeterminate gender pts. = ",
        non_binary_reinjury,
        " | Missing gender pts. = ",
        missing_gender_reinjury,
        ". All these patients had 1 injury event in 2024."
      ),
      fill = "Injury Count Category"
    ) +
    ggplot2::scale_fill_viridis_d(option = "turbo", direction = 1) +
    traumar::theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend_position = "inside",
      legend.position.inside = c(0.9, 0.8),
      legend.direction = "vertical",
      facet_text_size = 18,
      draw_panel_border = TRUE,
      facets = TRUE
    ) +
    theme(legend.title = element_text(vjust = 2.5))

  # save the plot of count of reinjured patients by injury count category

  plot_save_params(
    filename = "gender_reinjury_events_plot.png",
    plot = gender_reinjury_events_plot,
    path = plot_path
  )
}

# race and reinjuries

{
  # patient count by race and injury count category

  race_reinjury_events_tbl <- reinjured_trauma_2024 |>
    dplyr::filter(!is.na(Unique_Patient_ID)) |>
    dplyr::distinct(Unique_Patient_ID, .keep_all = TRUE) |>
    dplyr::mutate(
      Patient_Race = case_when(
        grepl(pattern = "indian", x = Patient_Race, ignore.case = TRUE) ~
          "AIAN",
        grepl(pattern = "black", x = Patient_Race, ignore.case = TRUE) ~
          "Black",
        grepl(pattern = "hawaiian", x = Patient_Race, ignore.case = TRUE) ~
          "NHOPI",
        grepl(
          pattern = "select|not\\s(known|applicable)",
          x = Patient_Race,
          ignore.case = TRUE
        ) ~
          "Missing",
        is.na(Patient_Race) ~ "Missing",
        TRUE ~ Patient_Race
      )
    ) |>
    dplyr::count(Patient_Race, n_injury_cat) |>
    dplyr::mutate(
      number_label = dplyr::if_else(
        n < 6,
        "*",
        traumar::pretty_number(n, n_decimal = 2)
      ),
      mod = log(n)
    ) |>
    replace_with_na(list(number_label = "NA")) |>
    tidyr::replace_na(list(number_label = "*"))

  # totals by race of reinjury

  race_reinjury_events_tbl_totals <- reinjured_trauma_2024 |>
    dplyr::filter(!is.na(Unique_Patient_ID)) |>
    dplyr::distinct(Unique_Patient_ID, .keep_all = TRUE) |>
    dplyr::mutate(
      Patient_Race = case_when(
        grepl(pattern = "indian", x = Patient_Race, ignore.case = TRUE) ~
          "AIAN",
        grepl(pattern = "black", x = Patient_Race, ignore.case = TRUE) ~
          "Black",
        grepl(pattern = "hawaiian", x = Patient_Race, ignore.case = TRUE) ~
          "NHOPI",
        grepl(
          pattern = "select|not\\s(known|applicable)",
          x = Patient_Race,
          ignore.case = TRUE
        ) ~
          "Missing",
        is.na(Patient_Race) ~ "Missing",
        TRUE ~ Patient_Race
      )
    ) |>
    dplyr::filter(n_injury_cat != "1 injury event") |>
    dplyr::count(Patient_Race) |>
    dplyr::mutate(
      percent = n / sum(n),
      percent_label = traumar::pretty_percent(percent, n_decimal = 1),
      number_label = dplyr::if_else(
        n < 6,
        "*",
        traumar::pretty_number(n, n_decimal = 2)
      ),
      mod = log(n)
    ) |>
    replace_with_na(list(number_label = "NA")) |>
    tidyr::replace_na(list(number_label = "*"))

  # save this file to use for reporting

  write_csv(
    x = race_reinjury_events_tbl_totals,
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/reference/race_reinjury_events_tbl_totals.csv"
  )

  # overall patient population race statistics

  trauma_reg_race_pop_stats <- trauma_2024 |>
    dplyr::mutate(
      Patient_Race = case_when(
        grepl(pattern = "indian", x = Patient_Race, ignore.case = TRUE) ~
          "AIAN",
        grepl(pattern = "black", x = Patient_Race, ignore.case = TRUE) ~
          "Black",
        grepl(pattern = "hawaiian", x = Patient_Race, ignore.case = TRUE) ~
          "NHOPI",
        grepl(
          pattern = "select|not\\s(known|applicable)",
          x = Patient_Race,
          ignore.case = TRUE
        ) ~
          "Missing",
        is.na(Patient_Race) ~ "Missing",
        TRUE ~ Patient_Race
      )
    ) |>
    injury_patient_count(Patient_Race) |>
    dplyr::mutate(
      percent = n / sum(n),
      percent_label = traumar::pretty_percent(percent)
    )

  # save this file to use for reporting

  write_csv(
    x = trauma_reg_race_pop_stats,
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/reference/trauma_reg_race_pop_stats.csv"
  )
}

# create the reinjured pt count by race plot

{
  race_reinjury_events_plot <- race_reinjury_events_tbl |>
    dplyr::filter(Patient_Race != "Missing") |>
    ggplot2::ggplot(ggplot2::aes(
      x = n_injury_cat,
      y = mod,
      fill = n_injury_cat,
      label = number_label,
    )) +
    ggplot2::geom_col(color = "black", position = "dodge") +
    ggplot2::geom_text(
      size = 7,
      family = "Work Sans",
      nudge_y = 0.75
    ) +
    ggplot2::facet_wrap(Patient_Race ~ .) +
    ggplot2::guides(color = "none") +
    ggplot2::labs(
      x = "",
      y = "# Patients (log)",
      fill = "Injury Count Category",
      title = "Count of Reinjured Patients by Race",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024",
      caption = paste0(
        "- '*' indicates counts < 6 masked to protect confidentiality.\n- Reinjured patients are identified by more than one injury date for any unique patient identifier.\n- Log scale used for y axis due to the '1 injury event' group outlier, labels are actual reinjured patient counts."
      )
    ) +
    ggplot2::scale_fill_viridis_d(option = "turbo", direction = 1) +
    traumar::theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      draw_panel_border = TRUE,
      facet_text_size = 18,
      legend_position = "inside",
      legend.position.inside = c(0.65, 0.05),
      legend.direction = "horizontal",
      facets = TRUE
    )

  # save the reinjured pt count by race plot

  plot_save_params(
    filename = "race_reinjury_events_plot.png",
    plot = race_reinjury_events_plot,
    path = plot_path
  )
}

# age group and reinjury

{
  # patient count by age and injury count category

  age_reinjury_events_tbl <- reinjured_trauma_2024 |>
    dplyr::filter(!is.na(Unique_Patient_ID)) |>
    dplyr::distinct(Unique_Patient_ID, .keep_all = TRUE) |>
    dplyr::mutate(
      Age_Range = tidyr::replace_na(Age_Range, "Missing"),
      Age_Range = factor(
        Age_Range,
        levels = c(
          "0-9",
          "10-19",
          "20-29",
          "30-39",
          "40-49",
          "50-59",
          "60-69",
          "70-79",
          "80-89",
          "90-99",
          "100+",
          "Missing"
        )
      )
    ) |>
    dplyr::count(Age_Range, n_injury_cat) |>
    dplyr::mutate(
      number_label = dplyr::if_else(
        n < 6,
        "*",
        traumar::pretty_number(n, n_decimal = 1)
      ),
      mod = log(n)
    ) |>
    replace_with_na(list(number_label = "NA")) |>
    tidyr::replace_na(list(number_label = "*"))

  # totals by age and reinjury

  age_reinjury_events_tbl_totals <- reinjured_trauma_2024 |>
    dplyr::filter(!is.na(Unique_Patient_ID)) |>
    dplyr::distinct(Unique_Patient_ID, .keep_all = TRUE) |>
    dplyr::mutate(
      Age_Range = tidyr::replace_na(Age_Range, "Missing"),
      Age_Range = factor(
        Age_Range,
        levels = c(
          "0-9",
          "10-19",
          "20-29",
          "30-39",
          "40-49",
          "50-59",
          "60-69",
          "70-79",
          "80-89",
          "90-99",
          "100+",
          "Missing"
        )
      )
    ) |>
    dplyr::filter(n_injury_cat != "1 injury event") |>
    dplyr::count(Age_Range) |>
    dplyr::mutate(
      percent = n / sum(n),
      percent_label = traumar::pretty_percent(percent, n_decimal = 1),
      cum_percent = cumsum(n / sum(n)),
      cum_percent_label = traumar::pretty_percent(cum_percent),
      number_label = dplyr::if_else(
        n < 6,
        "*",
        traumar::pretty_number(n, n_decimal = 2)
      ),
      mod = log(n)
    ) |>
    replace_with_na(list(number_label = "NA")) |>
    tidyr::replace_na(list(number_label = "*"))

  # save this file to use for reporting

  write_csv(
    x = age_reinjury_events_tbl_totals,
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/reference/age_reinjury_events_tbl_totals.csv"
  )

  # overall patient population age statistics

  trauma_reg_age_pop_stats <- trauma_2024 |>
    dplyr::mutate(
      Age_Range = tidyr::replace_na(Age_Range, "Missing"),
      Age_Range = factor(
        Age_Range,
        levels = c(
          "0-9",
          "10-19",
          "20-29",
          "30-39",
          "40-49",
          "50-59",
          "60-69",
          "70-79",
          "80-89",
          "90-99",
          "100+",
          "Missing"
        )
      )
    ) |>
    injury_patient_count(Age_Range) |>
    dplyr::mutate(
      percent = n / sum(n),
      percent_label = traumar::pretty_percent(percent)
    )

  # save this file to use for reporting

  write_csv(
    x = trauma_reg_age_pop_stats,
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/reference/trauma_reg_age_pop_stats.csv"
  )
}

# create the reinjured pt count by age plot

{
  age_reinjury_events_plot <- age_reinjury_events_tbl |>
    dplyr::filter(Age_Range != "Missing") |>
    ggplot2::ggplot(ggplot2::aes(
      x = n_injury_cat,
      y = mod,
      fill = n_injury_cat,
      label = number_label
    )) +
    ggplot2::geom_col(color = "black", position = "dodge") +
    ggplot2::geom_text(
      size = 7,
      family = "Work Sans",
      nudge_y = 0.75
    ) +
    ggplot2::facet_wrap(Age_Range ~ .) +
    ggplot2::guides(color = "none") +
    ggplot2::labs(
      x = "",
      y = "# Patients (log)",
      fill = "Injury Count Category",
      title = "Count of Reinjured Patients by Age Group",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024",
      caption = paste0(
        "- '*' indicates counts < 6 masked to protect confidentiality.\n- Reinjured patients are identified by more than one injury date for any unique patient identifier.\n- Log scale used for y axis due to the '1 injury event' group outlier, labels are actual reinjured patient counts."
      )
    ) +
    ggplot2::scale_fill_viridis_d(option = "turbo", direction = 1) +
    traumar::theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      draw_panel_border = TRUE,
      facet_text_size = 18,
      facets = TRUE
    ) +
    ylim(0, 10)

  # save the reinjured pt count by race plot

  plot_save_params(
    filename = "age_reinjury_events_plot.png",
    plot = age_reinjury_events_plot,
    path = plot_path
  )
}

###_____________________________________________________________________________
# Reinjury - Main Report Section continued ----
###_____________________________________________________________________________

# cause of injury among reinjured patients

{
  cause_of_injury_reinjury <- reinjured_trauma_2024 |>
    dplyr::filter(reinjured == T) |>
    injury_incident_count(MECHANISM_1, sort = TRUE) |>
    tidyr::replace_na(list(MECHANISM_1 = "Missing")) |>
    dplyr::mutate(
      percent = n / sum(n),
      percent_label = traumar::pretty_percent(percent),
      mod = log(n),
      color = dplyr::if_else(MECHANISM_1 == "Fall", "white", "black")
    )

  # df for plotting

  cause_of_injury_reinjury_complete <- cause_of_injury_reinjury |>
    dplyr::filter(MECHANISM_1 != "Missing") |>
    slice_max(n, n = 15)

  # cause of injury among reinjured patients plot

  cause_of_injury_reinjury_plot <- cause_of_injury_reinjury_complete |>
    ggplot2::ggplot(ggplot2::aes(
      x = reorder(MECHANISM_1, mod),
      y = n,
      fill = n,
      label = dplyr::if_else(n < 6, "*", prettyNum(n, big.mark = ","))
    )) +
    ggplot2::geom_col(color = "black", width = 0.75) +
    ggplot2::geom_text(
      nudge_y = dplyr::if_else(
        cause_of_injury_reinjury$n > 1000,
        -cause_of_injury_reinjury$n + 75,
        30
      ),
      size = 8,
      family = "Work Sans",
      color = dplyr::if_else(
        cause_of_injury_reinjury_complete$MECHANISM_1 == "Fall",
        "white",
        "black"
      )
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "",
      y = "\n# Injury Events among Reinjured Pts.\n",
      title = "Cause of Injury Among Reinjured Patients",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024",
      caption = "Top mechanisms of injury shown and their injury event counts",
      fill = "Higher count gives\ndarker color"
    ) +
    ggplot2::scale_fill_viridis(option = "viridis", direction = -1) +
    traumar::theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      axis.text.x = element_blank(),
      axis_lines = TRUE,
      legend_position = "inside",
      legend.position.inside = c(0.75, 0.25),
      legend.direction = "vertical"
    ) +
    theme(legend.text = element_blank())

  # save the reinjury cause of injury plot

  plot_save_params(
    filename = "cause_of_injury_reinjury_plot.png",
    plot = cause_of_injury_reinjury_plot,
    path = plot_path
  )
}

# urbanicity and reinjury

urbanicity_reinjury <- reinjured_trauma_2024 |>
  tidyr::replace_na(list(Designation_Patient = "Missing")) |>
  dplyr::filter(Designation_Patient != "Missing") |>
  dplyr::distinct(Unique_Patient_ID, .keep_all = TRUE) |>
  dplyr::count(Designation_Patient, reinjured) |>
  #dplyr::filter(reinjured == T) |>
  dplyr::mutate(percent = n / sum(n))

# plot the proportions of reinjured patients among rural / urban areas

{
  urbanicity_reinjury_plot <- reinjured_trauma_2024 |>
    tidyr::replace_na(list(Designation_Patient = "Missing")) |>
    dplyr::filter(Designation_Patient != "Missing") |>
    dplyr::distinct(Unique_Patient_ID, .keep_all = TRUE) |>
    dplyr::count(Designation_Patient, reinjured) |>
    dplyr::mutate(percent = n / sum(n), .by = Designation_Patient) |>
    ggplot2::ggplot(ggplot2::aes(
      x = Designation_Patient,
      y = percent,
      fill = reinjured,
      label = traumar::pretty_percent(percent)
    )) +
    ggplot2::geom_col(position = "fill", color = "dodgerblue") +
    ggplot2::geom_text(
      color = "white",
      family = "Work Sans",
      size = 8,
      nudge_y = -0.02
    ) +
    ggplot2::labs(
      x = "",
      y = "% Reinjured Patients\n",
      fill = "Patient Reinjured",
      title = "Proportions of Reinjured Patients by Urban/Rural Patient County",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024"
    ) +
    ggplot2::scale_y_continuous(labels = label_percent()) +
    ggplot2::scale_fill_paletteer_d(
      palette = "colorblindr::OkabeIto_black",
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

  # save the reinjured proportions plot

  plot_save_params(
    filename = "urbanicity_reinjury_plot.png",
    plot = urbanicity_reinjury_plot,
    path = plot_path
  )
}

# get the prior probability of reinjury among urban and rural residents

urbanicity_reinjury_phat <- reinjured_trauma_2024 |>
  tidyr::replace_na(list(Designation_Patient = "Missing")) |>
  dplyr::filter(Designation_Patient != "Missing") |>
  dplyr::distinct(Unique_Patient_ID, .keep_all = TRUE) |>
  dplyr::summarize(
    p_hat = mean(reinjured == T, na.rm = TRUE),
    .by = Designation_Patient
  )

# get the difference between the probabilities

{
  urbanicity_reinjury_phat_diff <- reinjured_trauma_2024 |>
    dplyr::mutate(
      reinjured_fct = factor(
        reinjured,
        levels = c("TRUE", "FALSE"),
        labels = c("yes", "no")
      )
    ) |>
    tidyr::replace_na(list(Designation_Patient = "Missing")) |>
    dplyr::filter(Designation_Patient != "Missing") |>
    dplyr::mutate(
      Designation_Patient = factor(
        Designation_Patient,
        levels = c("Urban", "Rural")
      )
    ) |>
    dplyr::distinct(Unique_Patient_ID, .keep_all = TRUE) |>
    specify(reinjured_fct ~ Designation_Patient, success = "yes") |>
    calculate(stat = "diff in props", order = c("Urban", "Rural")) |>
    dplyr::pull() # urban - rural
}

# tidyr::complete the test of equal proportions of reinjured patients between rural and urban Iowa locations

{
  urbanicity_reinjury_props <- reinjured_trauma_2024 |>
    tidyr::replace_na(list(Designation_Patient = "Missing")) |>
    dplyr::filter(Designation_Patient != "Missing") |>
    dplyr::mutate(
      reinjured = dplyr::if_else(reinjured == T, "yes", "no"),
      reinjured = factor(reinjured, levels = c("yes", "no"))
    ) |>
    prop_test(
      reinjured ~ Designation_Patient,
      success = "yes",
      order = c("Urban", "Rural"),
      alternative = "two-sided",
      correct = FALSE
    ) |>
    stat_sig(p_val_col = p_value)
}

# test alt hypothesis that the proportions of Iowans that are reinjured are different between urban / rural settings

{
  urbanicity_reinjury_model <- reinjured_trauma_2024 |>
    tidyr::replace_na(list(Designation_Patient = "Missing")) |>
    dplyr::filter(Designation_Patient != "Missing") |>
    dplyr::mutate(
      reinjured = dplyr::if_else(reinjured == T, "yes", "no"),
      reinjured = factor(reinjured, levels = c("yes", "no"))
    ) |>
    dplyr::distinct(Unique_Patient_ID, .keep_all = TRUE) |>
    specify(reinjured ~ Designation_Patient, success = "yes") |>
    hypothesise(null = "independence") |>
    generate(reps = 1000, type = "permute") |>
    calculate(stat = "diff in props", order = c("Urban", "Rural"))
}

# get critical values

urbanicity_reinjury_crit <- urbanicity_reinjury_model |>
  calculate_critical_values(stat_col = stat, alpha = 0.05)

lower_crit <- urbanicity_reinjury_crit$lower

upper_crit <- urbanicity_reinjury_crit$upper

# modify df to classify stat by critical values

urbanicity_reinjury_model_mod <- urbanicity_reinjury_model |>
  dplyr::mutate(
    area = dplyr::if_else(stat <= lower_crit | stat >= upper_crit, TRUE, FALSE)
  )

# Calculate density

density_data <- density(urbanicity_reinjury_model$stat)
density_df <- data.frame(x = density_data$x, y = density_data$y)

# plot the distribution of differences between proportions

{
  urbanicity_reinjury_diff_plot <- urbanicity_reinjury_model_mod |>
    ggplot2::ggplot(ggplot2::aes(
      x = stat,
      label = traumar::pretty_number(
        abs(urbanicity_reinjury_phat_diff),
        n_decimal = 5
      )
    )) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = after_stat(density)),
      bins = 15,
      fill = "lightgray",
      color = "black",
      alpha = 0.5
    ) +
    ggplot2::geom_line(
      data = density_df,
      ggplot2::aes(x = x, y = y),
      color = "blue",
      linewidth = 1.25,
      alpha = 0.5
    ) +
    ggplot2::geom_area(
      data = density_df |> dplyr::filter(x <= lower_crit),
      ggplot2::aes(x = x, y = y),
      fill = "coral1",
      alpha = 0.5
    ) +
    ggplot2::geom_area(
      data = density_df |> dplyr::filter(x >= upper_crit),
      aes(x = x, y = y),
      fill = "coral1",
      alpha = 0.5
    ) +
    ggplot2::geom_area(
      data = density_df |> dplyr::filter(x <= upper_crit, x >= lower_crit),
      ggplot2::aes(x = x, y = y),
      fill = "dodgerblue",
      alpha = 0.5
    ) +
    ggplot2::annotate(
      geom = "segment",
      x = urbanicity_reinjury_phat_diff,
      y = 0,
      xend = urbanicity_reinjury_phat_diff,
      yend = Inf,
      color = "coral1",
      linewidth = 2.25
    ) +
    ggplot2::annotate(
      geom = "text",
      x = urbanicity_reinjury_phat_diff * 1.1,
      y = 50,
      label = paste0(
        "diff = ",
        traumar::pretty_number(urbanicity_reinjury_phat_diff, n_decimal = 5)
      ),
      size = 8,
      family = "Work Sans",
      angle = 90
    ) +
    ggplot2::labs(
      x = "Differences in Proportions",
      y = "Count / Kernel Density",
      title = "Differences in Proportions of Reinjured Pts. Between Rural and Urban Areas in Iowa",
      subtitle = "Simulation-Based Null Distribution from 1,000 Permutated Samples",
      caption = paste0(
        "- p = ",
        round(urbanicity_reinjury_props$p_value, digits = 3),
        ", alt. hypothesis: proportions between groups are different",
        "\n- Shaded area under curve = critical values where observed statistic needs to fall to indicate a significant difference",
        "\n- Red line is the observed difference in proportions of reinjured patients in rural and urban areas in Iowa in the population\n- These data suggest that there is not a statistically significant difference between urban/rural areas regarding proportions of\n   reinjured patients."
      )
    ) +
    traumar::theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      axis_lines = TRUE
    )

  # save the difference plot showing statistical significance

  plot_save_params(
    filename = "urbanicity_reinjury_diff_plot.png",
    plot = urbanicity_reinjury_diff_plot,
    path = plot_path
  )
}

###_____________________________________________________________________________
# reinjury and mortality analysis ----
###_____________________________________________________________________________

# get patients that died in the year

dead_patients <- trauma_2024 |>
  dplyr::filter(Death == T) |>
  dplyr::distinct(Unique_Patient_ID) |>
  dplyr::pull()

# df for this analysis
reinjury_mortality_df <- trauma_2024 |>
  dplyr::mutate(
    reinjured = dplyr::if_else(
      Unique_Patient_ID %in% reinjured_patients,
      TRUE,
      FALSE
    ),
    dead = dplyr::if_else(Unique_Patient_ID %in% dead_patients, TRUE, FALSE) # this tells us if the pt. ever died
  ) |>
  dplyr::distinct(Unique_Patient_ID, .keep_all = TRUE) |>
  dplyr::left_join(injury_category_patients, by = "Unique_Patient_ID") |>
  dplyr::mutate(
    dead = factor(dead, levels = c("TRUE", "FALSE"), labels = c("yes", "no")),
    reinjured_fct = factor(
      reinjured,
      levels = c("TRUE", "FALSE"),
      labels = c("yes", "no")
    ),
    reinjured_rev = factor(
      reinjured,
      levels = c("FALSE", "TRUE"),
      labels = c("no", "yes")
    ),
    reinjured_binary = dplyr::if_else(reinjured == "yes", 1, 0),
    injury_category = factor(injury_category)
  ) |>
  dplyr::filter(!is.na(Unique_Patient_ID))

# observed probability

reinjury_mortality <- reinjury_mortality_df |>
  dplyr::summarize(
    N = dplyr::n(),
    mortality_rate = mean(dead == "yes"),
    .by = reinjured
  ) |>
  dplyr::filter(reinjured == T) |>
  dplyr::mutate(injury_category = NA_character_, .before = N) |>
  dplyr::arrange(reinjured)

###_____________________________________________________________________________
# Relationship between reinjury, risk definition, and death ----
###_____________________________________________________________________________

# describe the relationship between reinjury, risk definition, and death

reinjury_risk_death <- reinjury_mortality_df |>
  dplyr::summarize(
    N = dplyr::n(),
    mortality_rate = mean(dead == "yes"),
    .by = c(reinjured, injury_category)
  ) |>
  dplyr::arrange(reinjured, injury_category)

# create a gt() table to illustrate differences in mortality between reinjured / singularly injured groups
# and the different risk definitions groups

{
  reinjury_risk_death_tbl <- bind_rows(reinjury_risk_death, reinjury_mortality)

  reinjury_risk_death_gt <- reinjury_risk_death_tbl |>
    dplyr::select(-reinjured) |>
    dplyr::mutate(
      injury_category = dplyr::if_else(
        is.na(injury_category),
        "Total",
        injury_category
      ),
      N = small_count_label(var = N, cutoff = 6, replacement = NA_real_)
    ) |>
    gt() |>
    cols_label(
      injury_category = "Reinjury Category",
      mortality_rate = "Mortality Rate"
    ) |>
    fmt_number(columns = N, drop_trailing_zeros = TRUE) |>
    fmt_percent(columns = mortality_rate, drop_trailing_zeros = TRUE) |>
    tab_row_group(rows = 1, label = "Singularly Injured Pts.") |>
    tab_row_group(rows = c(2:7), label = "Reinjured Pts.") |>
    row_group_order(groups = c("Singularly Injured Pts.", "Reinjured Pts.")) |>
    sub_missing(columns = injury_category) |>
    sub_missing(columns = N) |>
    tab_header(
      title = "Differences in Mortality Rate Among Reinjured / Singularly Injured Pts.",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2024"
    ) |>
    tab_source_note(
      source_note = md(paste0(
        fontawesome::fa("magnifying-glass"),
        " Some patients could not be assigned a unique identifier due to key missing variables, and so the totals will not equal the sum of the counts for the injury event groups as records with a missing unique identifiers were omitted."
      ))
    ) |>
    tab_footnote(
      footnote = "These data reflect counts of patients.  Counts smaller than 6 are masked to protect confidentiality.",
      locations = cells_column_labels(columns = N)
    ) |>
    tab_footnote(
      footnote = "This proportion reflects the within group mortality rate.",
      locations = cells_column_labels(columns = mortality_rate)
    ) |>
    opt_footnote_marks(marks = "standard") |>
    tab_style_hhs(border_cols = N:mortality_rate)
}

# get difference between reinjured and singularly injured patients on death rates

reinjury_risk_death_diff <- reinjury_mortality_df |>
  specify(dead ~ reinjured_fct, success = "yes") |>
  calculate(stat = "diff in props", order = c("yes", "no")) |> # reinjured_yes - reinjured_no, in this order
  dplyr::pull()

# conduct the test of equal proportions - two-sided

{
  reinjury_risk_death_prop_test_two_side <- reinjury_mortality_df |>
    prop_test(
      dead ~ reinjured_fct,
      success = "yes",
      order = c("yes", "no"),
      alternative = "two-sided",
      correct = FALSE
    ) |>
    stat_sig(p_val_col = p_value) # alt. hypothesis is reinjured prop is less than non-reinjured prop and was statistically significant

  # conduct the test of equal proportions - reinjured less than non-reinjured

  reinjury_risk_death_prop_test_greater <- reinjury_mortality_df |>
    prop_test(
      dead ~ reinjured_fct,
      success = "yes",
      order = c("yes", "no"),
      alternative = "greater",
      correct = FALSE
    ) |>
    stat_sig(p_val_col = p_value) # alt. hypothesis is reinjured prop is less than non-reinjured prop and was statistically significant

  # union the models
  reinjury_risk_death_prop_test_both <- bind_rows(
    reinjury_risk_death_prop_test_two_side,
    reinjury_risk_death_prop_test_greater
  )
}

# utilize a simulation based model to understand the difference in proportions

reinjury_risk_death_model <- reinjury_mortality_df |>
  specify(dead ~ reinjured_fct, success = "yes") |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  calculate(stat = "diff in props", order = c("yes", "no"))

# get critical values

reinjury_risk_death_crit <- reinjury_risk_death_model |>
  calculate_critical_values(stat_col = stat, alpha = 0.05)

reinjury_risk_death_lower_crit <- reinjury_risk_death_crit$lower

reinjury_risk_death_upper_crit <- reinjury_risk_death_crit$upper

# get density curve

reinjury_risk_death_density <- density(reinjury_risk_death_model$stat)

reinjury_risk_death_density_df <- data.frame(
  x = reinjury_risk_death_density$x,
  y = reinjury_risk_death_density$y
)

# plot the model significance for reinjury risk death model

{
  reinjury_risk_death_diff_plot <- reinjury_risk_death_model |>
    ggplot2::ggplot(ggplot2::aes(
      x = stat,
      label = traumar::pretty_number(
        abs(reinjury_risk_death_diff),
        n_decimal = 5
      )
    )) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = after_stat(density)),
      bins = 15,
      fill = "lightgray",
      color = "black",
      alpha = 0.5
    ) +
    ggplot2::geom_line(
      data = reinjury_risk_death_density_df,
      ggplot2::aes(x = x, y = y),
      color = "blue",
      linewidth = 1.25,
      alpha = 0.5
    ) +
    ggplot2::geom_area(
      data = reinjury_risk_death_density_df |>
        dplyr::filter(x >= reinjury_risk_death_upper_crit),
      ggplot2::aes(x = x, y = y),
      fill = "coral1",
      alpha = 0.5
    ) +
    ggplot2::geom_area(
      data = reinjury_risk_death_density_df |>
        dplyr::filter(x <= reinjury_risk_death_upper_crit),
      ggplot2::aes(x = x, y = y),
      fill = "dodgerblue",
      alpha = 0.5
    ) +
    ggplot2::annotate(
      geom = "segment",
      x = reinjury_risk_death_diff,
      y = 0,
      xend = reinjury_risk_death_diff,
      yend = Inf,
      color = "coral1",
      linewidth = 2.25
    ) +
    ggplot2::annotate(
      geom = "text",
      x = reinjury_risk_death_diff - (reinjury_risk_death_diff * .5),
      y = 50,
      label = paste0(
        "diff = ",
        traumar::pretty_number(reinjury_risk_death_diff, n_decimal = 5)
      ),
      size = 8,
      family = "Work Sans",
      angle = 90
    ) +
    ggplot2::labs(
      x = "Differences in Proportions",
      y = "Count / Kernel Density",
      title = "Differences in Proportions of Deceased Pts. Between Reinjured and Singularly Injured Samples",
      subtitle = "Simulation-Based Null Distribution from 1,000 Permutated Samples",
      caption = paste0(
        "- p = ",
        traumar::pretty_number(
          reinjury_risk_death_prop_test_greater$p_value,
          n_decimal = 4
        ),
        ", alt. hypothesis: mortality rate is greater among reinjured patients compared to singularly injured patients",
        "\n- Shaded area under curve = critical values where observed statistic needs to fall to indicate a significant difference",
        "\n- Red line is the observed difference in proportions of reinjured patients in rural and urban areas in Iowa in the population\n- These data suggest there no statistically significant difference in mortality rate between singularly / reinjured patients."
      )
    ) +
    traumar::theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      axis_lines = TRUE
    )

  # save the difference plot showing statistical significance

  plot_save_params(
    filename = "reinjury_risk_death_diff_plot.png",
    plot = reinjury_risk_death_diff_plot,
    path = plot_path
  )
}
