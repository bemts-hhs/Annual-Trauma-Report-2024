###_____________________________________________________________________________
# Annual Trauma Report Executive Summary ----
###_____________________________________________________________________________

####
# You must first run data_load.R and setup.R before running this script
####

###_____________________________________________________________________________
# Get counts ----
###_____________________________________________________________________________

# patients

# get the current year's patient count ----
patient_count <- trauma_2024 |>
  injury_patient_count() |>
  dplyr::pull(n)

# get the trend ----
patient_count_years <- trauma_data_clean |>
  injury_patient_count(Year, descriptive_stats = TRUE)

# a gt table ----
patients_gt <- patient_count_years |>
  gt::gt() |>
  gt::cols_hide(prop_change) |>
  gt::cols_label(
    "n" ~ "Count",
    "change" ~ "Count Change",
    "prop_label" ~ "% Change"
  ) |>
  gt::sub_missing() |>
  gt::fmt_number(columns = 2:3, drop_trailing_zeros = TRUE) |>
  tab_style_hhs(
    message_text = NULL,
    border_cols = 2:last_col(),
    column_labels = 18,
    body = 16
  )

# save the table ----
gt::gtsave(
  patients_gt,
  filename = "patients_gt.png",
  path = plot_folder
)

# a line plot ----
patient_count_plot <- patient_count_years |>
  ggplot2::ggplot(ggplot2::aes(
    x = Year,
    y = n,
    label = ifelse(
      Year %in% c(2020, 2024),
      traumar::pretty_number(x = n, n_decimal = 2),
      NA
    )
  )) +
  ggplot2::geom_line(
    linewidth = 1.75,
    color = "lightblue",
    lineend = "round",
    linejoin = "round"
  ) +
  ggrepel::geom_text_repel(
    nudge_y = -250,
    segment.color = "transparent",
    seed = 8182023,
    direction = "both",
    color = "darkblue",
    size = 10,
    fontface = "bold",
    family = "Work Sans"
  ) +
  traumar::theme_cleaner(base_family = "Work Sans") +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 30)
  ) +
  ggplot2::scale_y_continuous(labels = function(x) {
    traumar::pretty_number(x, n_decimal = 2)
  })

# save the bar plot ----
ggplot2::ggsave(
  filename = "patient_count_plot.png",
  plot = patient_count_plot,
  path = plot_folder,
  height = 6,
  width = 6 * (16 / 9)
)

# injuries ----

# get the current year's count of events ----
incident_count <- trauma_2024 |>
  injury_incident_count() |>
  dplyr::pull(n)

# get the trend ----
incident_count_years <- trauma_data_clean |>
  injury_incident_count(Year, descriptive_stats = TRUE)

# a line graph ----
incident_count_plot <- incident_count_years |>
  ggplot2::ggplot(ggplot2::aes(
    x = Year,
    y = n,
    label = ifelse(
      Year %in% c(2020, 2024),
      traumar::pretty_number(x = n, n_decimal = 2),
      NA
    )
  )) +
  ggplot2::geom_line(
    linewidth = 1.75,
    color = "lightblue",
    lineend = "round",
    linejoin = "round"
  ) +
  ggrepel::geom_text_repel(
    nudge_y = ifelse(incident_count_years$Year == 2020, -300, 400),
    segment.color = "transparent",
    seed = 8182023,
    direction = "both",
    color = "darkblue",
    size = 15,
    fontface = "bold",
    family = "Work Sans"
  ) +
  traumar::theme_cleaner(base_family = "Work Sans", base_size = 30) +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank()
  ) +
  ggplot2::scale_y_continuous(labels = function(x) {
    traumar::pretty_number(x, n_decimal = 2)
  })

# save the incident plot ----
ggplot2::ggsave(
  filename = "incident_count_plot.png",
  plot = incident_count_plot,
  path = plot_folder,
  height = 6,
  width = 6 * (16 / 9)
)

# generate the incident table ----
incidents_gt <- incident_count_years |>
  gt::gt() |>
  gt::cols_hide(columns = c(prop_change, Min_Reinjury:Q75_Reinjury)) |>
  gt::cols_label(
    "n" ~ "Count",
    "change" ~ "Count Change",
    "prop_label" ~ "% Change"
  ) |>
  gt::sub_missing() |>
  gt::fmt_number(columns = 2:3, drop_trailing_zeros = TRUE) |>
  tab_style_hhs(message_text = NULL, border_cols = 2:last_col())

gt::gtsave(
  incidents_gt,
  filename = "incidents_gt.png",
  path = plot_folder
)


# cases ----
case_count <- trauma_2024 |>
  injury_case_count() |>
  dplyr::pull(n)

case_count_years <- trauma_data_clean |>
  injury_case_count(Year, descriptive_stats = TRUE)

cases_gt <- case_count_years |>
  gt::gt() |>
  gt::cols_hide(prop_change) |>
  gt::cols_label(
    "n" ~ "Count",
    "change" ~ "Count Change",
    "prop_label" ~ "% Change"
  ) |>
  gt::sub_missing() |>
  gt::fmt_number(columns = 2:3, drop_trailing_zeros = TRUE) |>
  tab_style_hhs(message_text = NULL, border_cols = 2:last_col())

gt::gtsave(
  cases_gt,
  filename = "cases_gt.png",
  path = plot_folder
)

###_____________________________________________________________________________
# Transfers and transfer delay ----
###_____________________________________________________________________________

# Compute transfer delay summaries for 2023–2024 using imputed Length_of_Stay ----
transfer_delays_2024 <- trauma_data_clean |>

  # Filter to only cases from 2023–2024 that were acute interfacility transfers ----
  dplyr::filter(Year %in% 2023:2024, Acute_Transfer_Out == "Yes") |>

  # Retain only one row per incident to avoid double-counting transfers ----
  dplyr::group_by(Year) |>
  dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
  dplyr::ungroup() |>

  # Apply two-stage imputation for Length_of_Stay ----
  dplyr::mutate(
    # Step 1: Handle extreme outliers using IQR-based capping within each year
    Length_of_Stay_trt = traumar::impute(
      Length_of_Stay,
      focus = "skew", # Targeting non-normal distribution tails
      method = "iqr" # IQR rule to cap values beyond upper fence
    ),

    # Step 2: Impute remaining missing values using the median within each year
    Length_of_Stay_imp = traumar::impute(
      Length_of_Stay_trt,
      focus = "missing", # Now targeting missing values only
      method = "median" # Robust central tendency for skewed data
    ),

    .by = Year # Apply both steps separately for each year
  ) |>

  # Summarize delay and timeliness indicators for each year ----
  dplyr::summarize(
    Delayed_2hr = sum(Length_of_Stay_imp > 120, na.rm = TRUE), # Delays over 2 hours
    Delayed_3hr = sum(Length_of_Stay_imp > 180, na.rm = TRUE), # Delays over 3 hours
    Timely_2hr = sum(Length_of_Stay_imp < 120, na.rm = TRUE), # Transfers within 2 hours
    Timely_3hr = sum(Length_of_Stay_imp < 180, na.rm = TRUE), # Transfers within 3 hours
    Total = dplyr::n(), # Total acute transfers
    Percent_Delay_2hr = Delayed_2hr / Total, # % delayed > 2 hours
    Percent_Delay_3hr = Delayed_3hr / Total, # % delayed > 3 hours
    Percent_Timely_2hr = Timely_2hr / Total, # % timely < 2 hours
    Percent_Timely_3hr = Timely_3hr / Total, # % timely < 3 hours
    .by = Year # Summary statistics by year
  ) |>

  # Join with total injury case counts to contextualize transfer volume ----
  dplyr::left_join(
    trauma_data_clean |>
      dplyr::filter(Year %in% 2023:2024) |>
      injury_case_count(Year, name = "Total cases"),
    by = "Year"
  )


###_____________________________________________________________________________
# Gender ----
###_____________________________________________________________________________

# get gender data ----
gender_group <- trauma_data_clean |>
  dplyr::filter(Year %in% 2023:2024) |>
  dplyr::mutate(
    Patient_Gender = dplyr::if_else(
      grepl(pattern = "not", x = Patient_Gender, ignore.case = TRUE),
      "Missing",
      Patient_Gender
    )
  ) |>
  tidyr::replace_na(list(Patient_Gender = "Missing")) |>
  injury_incident_count(Year, Patient_Gender) |>
  dplyr::mutate(Proportion = (n / sum(n)) * 100, .by = Year)

# create a table visualization using gt() ----
gender_group_tbl <-
  gender_group |>
  dplyr::mutate(
    n = traumar::small_count_label(
      var = n,
      cutoff = 6,
      replacement = NA_integer_
    )
  ) |>
  gt::gt(groupname_col = "Year", rowname_col = "Patient_Gender") |>
  gt::sub_missing(columns = n) |>
  gt::fmt_number(columns = n, drop_trailing_zeros = TRUE) |>
  gt::tab_header(
    title = "Summary: Injury Events by Gender",
    subtitle = "Data: Iowa Trauma Registry 2023-2024"
  ) |>
  gt::cols_label(n = "# Injury Events") |>
  gtExtras::gt_plt_bar_pct(
    column = Proportion,
    scaled = TRUE,
    labels = TRUE,
    decimals = 2,
    height = 20,
    width = 125
  ) |>
  gt::row_group_order(groups = c("2023", "2024")) |>
  gt::tab_footnote(
    footnote = patient_count_message,
    locations = gt::cells_row_groups()
  ) |>
  gt::tab_footnote(
    footnote = small_count_message,
    locations = gt::cells_body(columns = n, rows = 6)
  ) |>
  tab_style_hhs(border_cols = n:Proportion, message_text = NULL)

# save the table viz ----
gt::gtsave(
  data = gender_group_tbl,
  filename = "gender_group_tbl.png",
  path = plot_folder
)

###_____________________________________________________________________________
# Age ----
###_____________________________________________________________________________

# source df ----
age_group <- trauma_data_clean |>
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
  injury_patient_count(Year, Age_Range) |>
  dplyr::mutate(
    percent = n / sum(n),
    percent_label = traumar::pretty_percent(n / sum(n), n_decimal = 1),
    .by = Year
  ) |>
  dplyr::arrange(Age_Range, Year) |>
  dplyr::mutate(
    change = (n - dplyr::lag(n)) / dplyr::lag(n),
    change_label = ifelse(
      !is.na(change),
      traumar::pretty_percent(change, n_decimal = 1),
      NA_character_
    ),
    trend = ifelse(change > 0, "up", ifelse(change < 0, "down", "flat")),
    trend = ifelse(Year == 2020, "start", trend),
    .by = Age_Range
  ) |>
  dplyr::mutate(
    trend = factor(trend, levels = c("up", "down", "flat", "start"))
  )


# df for printing ----
age_group_filtered <- age_group |>
  dplyr::filter(Year %in% 2023:2024)

# df for the plot ----
age_group_plot_df <- age_group |>
  dplyr::filter(Year %in% 2021:2024, Age_Range != "Missing") |>
  dplyr::mutate(
    category = dplyr::if_else(
      change >= 0,
      "Increase",
      dplyr::if_else(change < 0, "Decrease", "Neutral")
    )
  )

# produce the plot ----
age_group_plot <- age_group_plot_df |>
  ggplot2::ggplot(ggplot2::aes(
    factor(Year),
    change,
    group = Age_Range,
    fill = category,
    label = change_label
  )) +
  ggplot2::geom_col(color = "black", width = 0.8) +
  ggrepel::geom_text_repel(
    size = 6,
    direction = "y",
    family = "Work Sans SemiBold",
    nudge_y = dplyr::if_else(age_group_plot_df$change < 0, -0.035, 0.025),
    segment.color = NA
  ) +
  ggplot2::labs(
    title = "Percent Change in Injury Events by Age Group",
    subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2021-2024",
    caption = "Injury event refers to the number of unique injury incidents that led to evaluation/treatment at a verified trauma center.\nEach injury event could involve multiple cases, and each patient may have one or more injury events in a specified timespan.",
    x = "",
    y = "% Change",
    fill = "Change Type"
  ) +
  ggplot2::facet_wrap(~Age_Range) +
  ggplot2::guides(color = "none") +
  ggplot2::scale_y_continuous(
    labels = function(x) traumar::pretty_percent(x)
  ) +
  ggplot2::scale_fill_viridis_d(option = "inferno") +
  traumar::theme_cleaner(
    draw_panel_border = TRUE,
    facets = TRUE
  )

# save the age group column plot
ggplot2::ggsave(
  filename = paste0(plot_folder, "/age_group_plot.png"),
  plot = age_group_plot,
  height = 7,
  width = 7 * (16 / 9)
)


# age group plot line giving counts ----
age_group_lines <- age_group_plot_df |>
  ggplot2::ggplot(ggplot2::aes(
    factor(Year),
    n,
    group = Age_Range,
    color = Age_Range,
    label = traumar::pretty_number(x = n, n_decimal = 2)
  )) +
  ggplot2::geom_line(linewidth = 2, lineend = "round", linejoin = "round") +
  ggplot2::geom_point(size = 3, color = "black") +
  ggrepel::geom_text_repel(
    size = 6,
    segment.color = NA,
    direction = "y",
    color = "black",
    family = "Work Sans SemiBold"
  ) +
  ggplot2::labs(
    title = "Count of Unique Injury Events by Age Group",
    subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2021-2024",
    caption = "Injury event refers to the number of unique injury incidents that led to evaluation/treatment at a verified trauma center.\nEach injury event could involve multiple cases, and each patient may have one or more injury events in a specified timespan.",
    x = "",
    y = "Injury Event Count",
    fill = "Change Type"
  ) +
  ggplot2::facet_wrap(~Age_Range) +
  ggplot2::guides(color = "none", fill = "none") +
  ggplot2::scale_y_continuous(
    breaks = ggplot2::waiver(),
    labels = function(x) traumar::pretty_number(x, n_decimal = 2)
  ) +
  paletteer::scale_color_paletteer_d("colorBlindness::Blue2Orange12Steps") +
  traumar::theme_cleaner(facets = TRUE)

# save the age group line plot
ggplot2::ggsave(
  filename = paste0(output_folder, "/age_group_lines.png"),
  plot = age_group_lines
)


###_____________________________________________________________________________
# Race ----
###_____________________________________________________________________________

# source df ----
race_group <- trauma_data_clean |>
  dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
  dplyr::mutate(
    Patient_Race = dplyr::if_else(
      grepl(pattern = "not|select", x = Patient_Race, ignore.case = TRUE),
      "Missing",
      dplyr::if_else(
        grepl(
          pattern = "american indian",
          x = Patient_Race,
          ignore.case = TRUE
        ),
        "AIAN",
        dplyr::if_else(
          grepl(
            pattern = "native hawaiian",
            x = Patient_Race,
            ignore.case = TRUE
          ),
          "NHOPI",
          Patient_Race
        )
      )
    ),
    Patient_Race = tidyr::replace_na(Patient_Race, "Missing")
  ) |>
  injury_patient_count(Year, Patient_Race) |>
  dplyr::rename(Patients = n) |>
  tidyr::complete(Year, Patient_Race, fill = list(Patients = 0)) |>
  injury_mutate(col = Patients, group = Year) |>
  dplyr::arrange(Patient_Race) |>
  dplyr::mutate(
    change = (Patients - dplyr::lag(Patients)) /
      dplyr::lag(Patients),
    change_label = traumar::pretty_percent(change, n_decimal = 2),
    .by = Patient_Race
  ) |>
  dplyr::mutate(
    change = dplyr::if_else(change == Inf, 0, change),
    change_label = dplyr::if_else(change_label == Inf, "0%", change_label)
  ) |>
  tidyr::replace_na(list(change = 0, change_label = "0%"))


# df for plotting ----
race_group_plot_df <- race_group |>
  dplyr::filter(Year %in% 2021:2024) |>
  dplyr::mutate(
    normal_patients = traumar::normalize(Patients, method = "z_score"),
    .by = Year
  )

# df for printing ----
race_group_filtered <- race_group |>
  dplyr::filter(Year %in% 2023:2024)

# build the race group column plot ----
race_group_plot <- race_group_plot_df |>
  dplyr::filter(Patient_Race != "Not Known/Not Recorded") |>
  dplyr::mutate(
    category = dplyr::if_else(change > 0, "Increase", "Decrease")
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    factor(Year),
    change,
    group = Patient_Race,
    fill = category,
    label = change_label
  )) +
  ggplot2::geom_col(color = "black", width = 0.5) +
  ggrepel::geom_text_repel(
    size = 5,
    segment.color = NA,
    nudge_y = dplyr::if_else(race_group_plot_df$change < 0, -0.1, 0.1),
    direction = "y",
    family = "Work Sans SemiBold",
    max.iter = 30000
  ) +
  ggplot2::labs(
    title = "Percent Change in Patients by Patient Race",
    subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2021-2024",
    x = "",
    y = "% Change",
    fill = "Change Type"
  ) +
  ggplot2::facet_wrap(~Patient_Race) +
  ggplot2::guides(color = "none") +
  ggplot2::scale_y_continuous(
    labels = function(x) traumar::pretty_percent(x)
  ) +
  ggplot2::scale_fill_viridis_d(option = "inferno") +
  traumar::theme_cleaner(
    draw_panel_border = TRUE,
    facets = TRUE
  )

# save the race group column plot ----
ggplot2::ggsave(
  filename = "race_group_plot.png",
  plot = race_group_plot,
  path = plot_folder,
  height = 7,
  width = 7 * (16 / 9)
)


# race group line graph ----
race_group_line_plot <- race_group_plot_df |>
  dplyr::filter(Patient_Race != "More than One Category") |>
  ggplot2::ggplot(ggplot2::aes(
    factor(Year),
    Patients,
    group = Patient_Race,
    color = Patient_Race,
    label = traumar::pretty_number(x = Patients, n_decimal = 2)
  )) +
  ggplot2::geom_line(
    linewidth = 2,
    lineend = "round",
    linejoin = "round",
    alpha = 0.9
  ) +
  ggplot2::geom_point(size = 4, color = "black") +
  ggrepel::geom_text_repel(
    size = 6,
    nudge_y = dplyr::if_else(
      race_group_plot_df$Patient_Race == "Asian",
      3,
      8
    ),
    max.iter = 30000,
    direction = "x",
    segment.color = NA,
    color = "black",
    family = "Work Sans SemiBold"
  ) +
  ggplot2::labs(
    title = "Count of Distinct Patients by Patient Race",
    subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2021-2024",
    caption = "Note: Scale does not reach zero and each subplot may have a different scale.",
    x = "",
    y = "Patient Count",
  ) +
  ggplot2::facet_wrap(~Patient_Race, scales = "free_y") +
  ggplot2::guides(color = "none", fill = "none") +
  ggplot2::scale_y_continuous(
    breaks = ggplot2::waiver(),
    labels = function(x) traumar::pretty_number(x, n_decimal = 2)
  ) +
  paletteer::scale_color_paletteer_d(
    "colorBlindness::Blue2DarkOrange12Steps"
  ) +
  traumar::theme_cleaner(
    vjust_title = 2,
    vjust_subtitle = 1.25,
    facet_text_size = 16,
    title_text_size = 20,
    subtitle_text_size = 18,
    draw_panel_border = TRUE,
    facets = TRUE
  )

# save the race group line plot ----
ggplot2::ggsave(
  filename = "race_group_line_plot.png",
  plot = race_group_line_plot,
  path = plot_folder,
  height = 7,
  width = 7 * (16 / 9)
)


###_____________________________________________________________________________
# Leading causes of injury ----
###_____________________________________________________________________________

# leading causes of injury - cases ----
leading_causes_cases <- trauma_data_clean |>
  injury_case_count(Year, CAUSE_OF_INJURY_AR_1) |>
  dplyr::group_by(Year) |>
  dplyr::arrange(desc(n), .by_group = TRUE) |>
  dplyr::ungroup() |>
  na.omit() |>
  dplyr::mutate(percent = n / sum(n, na.rm = TRUE), .by = Year) |>
  dplyr::mutate(
    label_num = traumar::small_count_label(
      var = n,
      cutoff = 6,
      replacement = "*"
    ),
    percent_label = dplyr::if_else(
      label_num == "*",
      "*",
      dplyr::if_else(
        n == 6,
        traumar::pretty_percent(percent, n_decimal = 2),
        traumar::pretty_percent(percent, n_decimal = 1)
      )
    )
  )

# leading causes of injury events ----
leading_causes_years <- trauma_data_clean |>
  injury_incident_count(Year, CAUSE_OF_INJURY_AR_1) |>
  dplyr::group_by(Year) |>
  dplyr::arrange(desc(n), .by_group = TRUE) |>
  dplyr::ungroup() |>
  na.omit() |>
  dplyr::mutate(percent = n / sum(n, na.rm = TRUE), .by = Year) |>
  dplyr::mutate(
    label_num = traumar::small_count_label(
      var = n,
      cutoff = 6,
      replacement = "*"
    ),
    percent_label = dplyr::if_else(
      label_num == "*",
      "*",
      dplyr::if_else(
        n == 6,
        traumar::pretty_percent(percent, n_decimal = 2),
        traumar::pretty_percent(percent, n_decimal = 1)
      )
    )
  )

# for printing ----
leading_causes_recent <- leading_causes_years |>
  dplyr::filter(Year %in% 2023:2024)

# get color order ----
legend_order = leading_causes_years |>
  dplyr::filter(Year > 2020, Year < 2024) |>
  dplyr::distinct(CAUSE_OF_INJURY_AR_1) |>
  dplyr::pull()

# plot of leading causes by year ----
leading_causes_plot <- leading_causes_years |>
  ggplot2::ggplot(ggplot2::aes(
    reorder(CAUSE_OF_INJURY_AR_1, -percent),
    percent,
    fill = CAUSE_OF_INJURY_AR_1,
    label = percent_label
  )) +
  ggplot2::geom_col(
    alpha = 0.75,
    position = "dodge",
    width = 0.5,
    color = "black"
  ) +
  ggrepel::geom_text_repel(
    size = 5.5,
    direction = "y",
    segment.color = NA,
    color = "#03617A",
    fontface = "bold",
    nudge_y = 0.01
  ) +
  ggplot2::labs(
    fill = "Cause of Injury",
    title = "Comparing Leading Causes of Injury by Year",
    subtitle = "Proportion of Total Injury Events is Column Height and Label | Years: 2019-2024",
    caption = "\nAll facets are scaled the same, starting at 0.\n'*' indicates small counts that are masked to protect confidentiality\nData: Iowa Trauma Registry | Bureau of Emergency Medical and Trauma Services | Division of Public Health | Iowa HHS",
    x = "",
    y = ""
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1), color = "none") +
  ggplot2::scale_y_continuous(
    labels = function(x) traumar::pretty_percent(x)
  ) +
  paletteer::scale_fill_paletteer_d(
    palette = "colorBlindness::PairedColor12Steps",
    breaks = legend_order
  ) +
  traumar::theme_cleaner(
    axis.text.x = ggplot2::element_blank(),
    legend_position = "bottom",
    vjust_title = 2,
    vjust_subtitle = 1,
    title_text_size = 20,
    subtitle_text_size = 18,
    base_size = 15,
    facet_text_size = 15,
    draw_panel_border = TRUE,
    facets = TRUE
  ) +
  ggplot2::facet_wrap(~Year)

# save the plot on leading causes ----
ggplot2::ggsave(
  filename = "leading_causes_injury_years.png",
  plot = leading_causes_plot,
  path = plot_folder,
  height = 9,
  width = 9 * (16 / 9)
)

###_____________________________________________________________________________
# Falls ----
###_____________________________________________________________________________

# fall related hospital vistis ----
fall_related_cases <-
  trauma_data_clean |>
  dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
  dplyr::summarize(
    Falls = sum(
      grepl(pattern = "fall", x = CAUSE_OF_INJURY_AR_1, ignore.case = TRUE),
      na.rm = TRUE
    ),
    cases = dplyr::n(),
    percent = round(Falls / cases, digits = 3),
    label = traumar::pretty_percent(Falls / cases, n_decimal = 1),
    .by = Year
  ) |>
  dplyr::mutate(
    increase = round(
      (Falls - dplyr::lag(Falls)) / dplyr::lag(Falls),
      digits = 3
    ),
    increase_label = traumar::pretty_percent(
      (Falls - dplyr::lag(Falls)) / dplyr::lag(Falls),
      n_decimal = 2
    )
  )

# fall related injury events ----
fall_related_injuries <-
  trauma_data_clean |>
  dplyr::distinct(Unique_Patient_ID, Incident_Date, .keep_all = TRUE) |>
  dplyr::summarize(
    Falls = sum(
      grepl(pattern = "fall", x = CAUSE_OF_INJURY_AR_1, ignore.case = TRUE),
      na.rm = TRUE
    ),
    Injuries = dplyr::n(),
    percent = round(Falls / Injuries, digits = 3),
    label = traumar::pretty_percent(Falls / Injuries, n_decimal = 1),
    .by = Year
  ) |>
  dplyr::mutate(
    increase = round(
      (Falls - dplyr::lag(Falls)) / dplyr::lag(Falls),
      digits = 3
    ),
    increase_label = traumar::pretty_percent(
      (Falls - dplyr::lag(Falls)) / dplyr::lag(Falls),
      n_decimal = 2
    )
  )

###_____________________________________________________________________________
# Motor vehicle, boating, and air incidents ----
###_____________________________________________________________________________

# MVC-related hospital visits ----
motor_vehicle_related_cases <-
  trauma_data_clean |>
  dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
  dplyr::summarize(
    MVC = sum(
      grepl(
        pattern = "mvc/transport",
        x = CAUSE_OF_INJURY_AR_1,
        ignore.case = TRUE
      ),
      na.rm = TRUE
    ),
    cases = dplyr::n(),
    percent = round(MVC / cases, digits = 4),
    .by = Year
  ) |>
  dplyr::mutate(
    increase = round((MVC - dplyr::lag(MVC)) / dplyr::lag(MVC), digits = 3),
    increase_label = traumar::pretty_percent(
      (MVC - dplyr::lag(MVC)) / dplyr::lag(MVC),
      n_decimal = 2
    )
  )

# MVC related injuries ----
motor_vehicle_related_injuries <-
  trauma_data_clean |>
  dplyr::distinct(Unique_Patient_ID, Incident_Date, .keep_all = TRUE) |>
  dplyr::summarize(
    MVC = sum(
      grepl(
        pattern = "mvc/transport",
        x = CAUSE_OF_INJURY_AR_1,
        ignore.case = TRUE
      ),
      na.rm = TRUE
    ),
    Patients = dplyr::n(),
    percent = round(MVC / Patients, digits = 4),
    .by = Year
  ) |>
  dplyr::mutate(
    increase = round((MVC - dplyr::lag(MVC)) / dplyr::lag(MVC), digits = 3),
    increase_label = traumar::pretty_percent(
      (MVC - dplyr::lag(MVC)) / dplyr::lag(MVC),
      n_decimal = 2
    )
  )


# table for motor vehicle injuries ----
mvc_injury_transpose <-
  motor_vehicle_related_injuries |>
  dplyr::select(-increase_label) |>
  t() |>
  as.data.frame() |>
  tibble::rownames_to_column(var = "Category") |>
  dplyr::filter(Category != "Year") |>
  purrr::set_names(nm = c("Category", 2020:2024)) |>
  dplyr::mutate(
    Category = c(
      "MVC Injury Event Count",
      "Total Injury Events",
      "Proportion of Injuries",
      "% Change in MVC Injuries"
    )
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
  dplyr::select(-c(`2020`, `2021`, `2022`))


# create the table ----
mvc_injury_table <-
  mvc_injury_transpose |>
  gt::gt() |>
  gt::cols_label(Category = "") |>
  gtExtras::gt_plt_sparkline(
    column = `2020-2024 Trend`,
    type = "shaded",
    same_limit = FALSE,
    label = FALSE
  ) |>
  gt::fmt_percent(rows = 3:4) |>
  gt::fmt_number(rows = 1:2, drop_trailing_zeros = TRUE) |>
  gt::tab_header(
    title = "Summary: Trend of Motor Vehicle Injury Events ",
    subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2020-2024"
  ) |>
  gt::tab_row_group(label = "Counts", rows = 1:2) |>
  gt::tab_row_group(label = "Proportion and Change", rows = 3:4) |>
  gt::row_group_order(groups = c("Counts", "Proportion and Change")) |>
  tab_style_hhs(border_cols = 2:4)

# save the gt table on MVCs
gt::gtsave(
  data = mvc_injury_table,
  filename = "mvc_injury_table.png",
  path = plot_folder
)


###_____________________________________________________________________________
# Reinjury ----
###_____________________________________________________________________________

# patients ----
reinjuries_stats_patients <- trauma_data_clean |>
  reinjury_patient_count(Year, descriptive_stats = TRUE)

# cases ----
reinjuries_stats_cases <- trauma_data_clean |>
  reinjury_case_count(Year, descriptive_stats = TRUE)

# injury events ----
reinjury_stats_injuries <- trauma_data_clean |>
  reinjury_injury_count(Year, descriptive_stats = TRUE)

# create a gt() table to be explored in the report ----
reinjury_stat_tbl_object <- reinjury_stats_injuries |>
  dplyr::select(-matches("_label")) |>
  tidyr::replace_na(list(change = 0)) |>
  t() |>
  as.data.frame() |>
  purrr::set_names(nm = c(2020:2024)) |>
  tibble::rownames_to_column(var = "Category") |>
  dplyr::filter(Category != "Year") |>
  dplyr::rowwise() |>
  dplyr::mutate(
    `2020-2024 Trend` = list(c(`2020`, `2021`, `2022`, `2023`, `2024`))
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-c(`2020`, `2021`, `2022`))

# generate the gt() table ----
reinjury_stat_tbl <-
  reinjury_stat_tbl_object |>
  dplyr::filter(Category %in% c("Reinjury", "n", "prop", "change")) |>
  dplyr::mutate(
    Category = dplyr::case_when(
      Category == "Reinjury" ~ "Injury Event Count (Reinjured Pts Only)",
      Category == "n" ~ "Total Injury Events",
      Category == "prop" ~ "% Reinjury",
      Category == "change" ~ "% Change in Injury Events"
    )
  ) |>
  gt::gt() |>
  gt::cols_label(Category = "") |>
  gtExtras::gt_plt_sparkline(
    column = `2020-2024 Trend`,
    same_limit = FALSE,
    type = "shaded",
    label = FALSE
  ) |>
  gt::fmt_number(
    columns = tidyselect::everything(),
    rows = 1:2,
    drop_trailing_zeros = TRUE
  ) |>
  gt::fmt_percent(
    columns = tidyselect::everything(),
    rows = 3:4,
    drop_trailing_zeros = TRUE
  ) |>
  gt::tab_header(
    title = "Summary: Trend of Reinjury in Iowa",
    subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2020-2024"
  ) |>
  gt::tab_row_group(label = "Counts", rows = 1:2) |>
  gt::tab_row_group(label = "Proportion and Change", rows = 3:4) |>
  gt::row_group_order(groups = c("Counts", "Proportion and Change")) |>
  tab_style_hhs(border_cols = 2:4)

# save the reinjury table
gt::gtsave(
  data = reinjury_stat_tbl,

  filename = "reinjury_stat_tbl.png",
  path = plot_folder
)


###_____________________________________________________________________________
# work related ----
###_____________________________________________________________________________

# work related hospital visits ----
work_related_cases <-
  trauma_data_clean |>
  dplyr::mutate(
    Financial_Work_Related = dplyr::if_else(
      Financial_Work_Related != "Yes" | is.na(Financial_Work_Related),
      "No",
      Financial_Work_Related
    )
  ) |>
  injury_case_count(Year, Financial_Work_Related) |>
  dplyr::rename(cases = n) |>
  case_mutate(col = cases, group = Year) |>
  dplyr::filter(Financial_Work_Related == "Yes") |>
  dplyr::mutate(
    increase = round(
      (cases - dplyr::lag(cases)) / dplyr::lag(cases),
      digits = 3
    ),
    increase_label = traumar::pretty_percent(
      (cases - dplyr::lag(cases)) / dplyr::lag(cases),
      n_decimal = 2
    )
  )

# work related injury events ----
work_related_injuries <-
  trauma_data_clean |>
  dplyr::mutate(
    Financial_Work_Related = dplyr::if_else(
      Financial_Work_Related != "Yes" | is.na(Financial_Work_Related),
      "No",
      Financial_Work_Related
    )
  ) |>
  injury_incident_count(
    Year,
    Financial_Work_Related
  ) |>
  dplyr::rename(injuries = n) |>
  injury_mutate(col = injuries, group = Year) |>
  dplyr::filter(Financial_Work_Related == "Yes") |>
  dplyr::mutate(
    increase = round(
      (injuries - dplyr::lag(injuries)) / dplyr::lag(injuries),
      digits = 3
    ),
    increase_label = traumar::pretty_percent(
      (injuries - dplyr::lag(injuries)) / dplyr::lag(injuries),
      n_decimal = 2
    )
  )

###_____________________________________________________________________________
# farm related ----
###_____________________________________________________________________________

# farm-related hospital visits ----
farm_related_cases <-
  trauma_data_clean |>
  dplyr::mutate(
    Farm_Ag_Related = dplyr::if_else(
      Farm_Ag_Related != "Yes" | is.na(Farm_Ag_Related),
      "No",
      Farm_Ag_Related
    )
  ) |>
  injury_case_count(Year, Farm_Ag_Related) |>
  dplyr::rename(cases = n) |>
  case_mutate(col = cases, group = Year) |>
  dplyr::filter(Farm_Ag_Related == "Yes") |>
  dplyr::mutate(
    increase = round(
      (cases - dplyr::lag(cases)) / dplyr::lag(cases),
      digits = 3
    ),
    increase_label = traumar::pretty_percent(
      (cases - dplyr::lag(cases)) / dplyr::lag(cases),
      n_decimal = 2
    )
  )

# farm-related injury events ----
farm_related_injuries <-
  trauma_data_clean |>
  dplyr::mutate(
    Farm_Ag_Related = dplyr::if_else(
      Farm_Ag_Related != "Yes" | is.na(Farm_Ag_Related),
      "No",
      Farm_Ag_Related
    )
  ) |>
  injury_incident_count(Year, Farm_Ag_Related) |>
  dplyr::rename(Injury_Events = n) |>
  injury_mutate(col = Injury_Events, group = Year) |>
  dplyr::filter(Farm_Ag_Related == "Yes") |>
  dplyr::mutate(
    increase = round(
      (Injury_Events - dplyr::lag(Injury_Events)) / dplyr::lag(Injury_Events),
      digits = 3
    ),
    increase_label = traumar::pretty_percent(
      (Injury_Events - dplyr::lag(Injury_Events)) / dplyr::lag(Injury_Events),
      n_decimal = 2
    )
  )

###_____________________________________________________________________________
# Intentionality ----
###_____________________________________________________________________________

# source df ----
intentionality_of_injury <- trauma_data_clean |>
  tidyr::replace_na(list(INTENTIONALITY_1 = "Categorization Not Possible")) |>
  dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE) |>
  dplyr::summarize(
    `Intentional Injury Events` = sum(
      !grepl(
        pattern = "categorization|unintentional",
        x = INTENTIONALITY_1,
        ignore.case = TRUE
      ),
      na.rm = TRUE
    ),
    `Unintentional Injury Events` = sum(
      grepl(
        pattern = "unintentional",
        x = INTENTIONALITY_1,
        ignore.case = TRUE
      ),
      na.rm = TRUE
    ),
    `Categorization Not Possible` = sum(
      grepl(
        pattern = "categorization",
        x = INTENTIONALITY_1,
        ignore.case = TRUE
      ),
      na.rm = TRUE
    ),
    `Total Categorized Injury Events` = `Intentional Injury Events` +
      `Unintentional Injury Events`,
    `Total Injury Events` = dplyr::n(),
    `% Intentional Injury Events` = `Intentional Injury Events` /
      `Total Categorized Injury Events`,
    `% Unintentional Injury Events` = `Unintentional Injury Events` /
      `Total Categorized Injury Events`,
    .by = Year
  ) |>
  dplyr::mutate(
    `% Change Intentional Injury Events` = (`Intentional Injury Events` -
      dplyr::lag(`Intentional Injury Events`)) /
      dplyr::lag(`Intentional Injury Events`),
    `% Change Unintentional Injury Events` = (`Unintentional Injury Events` -
      dplyr::lag(`Unintentional Injury Events`)) /
      dplyr::lag(`Unintentional Injury Events`)
  )

# pivot the df for a gt() table ----
intentionality_of_injury_pivot <- intentionality_of_injury |>
  tidyr::pivot_longer(
    cols = 2:length(intentionality_of_injury),
    names_to = "Category",
    values_to = "Vals"
  ) |>
  tidyr::pivot_wider(
    id_cols = Category,
    names_from = Year,
    values_from = Vals
  ) |>
  tidyr::replace_na(list(`2020` = 0)) |>
  dplyr::mutate(
    `2020-2024 Trend` = list(c(`2020`, `2021`, `2022`, `2023`, `2024`)),
    .by = Category
  ) |>
  dplyr::select(-c(`2020`:`2022`))

# develop the gt() table for intentionality of injury ----
intentionality_of_injury_tbl <-
  intentionality_of_injury_pivot |>
  gt::gt() |>
  gt::cols_label(Category = "") |>
  gtExtras::gt_plt_sparkline(
    column = `2020-2024 Trend`,
    type = "shaded",
    same_limit = FALSE,
    label = FALSE
  ) |>
  gt::fmt_number(
    columns = tidyselect::everything(),
    rows = 1:5,
    drop_trailing_zeros = TRUE
  ) |>
  gt::fmt_percent(
    columns = tidyselect::everything(),
    rows = 6:9,
    drop_trailing_zeros = TRUE
  ) |>
  gt::tab_header(
    title = "Intentionality of Injury Events in Iowa",
    subtitle = "Data: Iowa Trauma Registry 2020-2024"
  ) |>
  gt::tab_row_group(label = "Counts", rows = 1:5) |>
  gt::tab_row_group(label = "Proportion and Change", rows = 6:9) |>
  gt::row_group_order(groups = c("Counts", "Proportion and Change")) |>
  tab_style_hhs(border_cols = 2:4)

# save the intentionality table ----
gt::gtsave(
  data = intentionality_of_injury_tbl,
  filename = "intentionality_of_injury_tbl.png",
  path = plot_folder
)

###_____________________________________________________________________________
# Trauma activations ----
###_____________________________________________________________________________

# source df ----
trauma_activation_cases <- trauma_data_clean |>
  dplyr::mutate(
    Trauma_Team_Activation_Level = dplyr::if_else(
      !Trauma_Team_Activation_Level %in%
        c(
          "Consultation",
          "Level 1",
          "Level 2",
          "Level 3"
        ),
      "Not Activated",
      Trauma_Team_Activation_Level,
      missing = "Not Activated"
    )
  ) |>
  injury_case_count(Year, Trauma_Team_Activation_Level) |>
  tidyr::complete(Year, Trauma_Team_Activation_Level, fill = list(n = 0)) |>
  dplyr::mutate(
    percent = n / sum(n, na.rm = TRUE),
    percent_label = traumar::pretty_percent(percent, n_decimal = 1),
    .by = Year
  ) |>
  dplyr::arrange(Trauma_Team_Activation_Level) |>
  dplyr::mutate(
    change = (n - dplyr::lag(n)) / dplyr::lag(n),
    change_label = traumar::pretty_percent(change),
    .by = Trauma_Team_Activation_Level
  )


# df for printing ----
trauma_activation_cases_recent <- trauma_activation_cases |>
  dplyr::filter(Year %in% 2023:2024)

# overall activations ----
trauma_activation_cases_binary <- trauma_data_clean |>
  dplyr::mutate(
    Trauma_Team_Activation_Level = dplyr::if_else(
      !Trauma_Team_Activation_Level %in%
        c(
          "Consultation",
          "Level 1",
          "Level 2",
          "Level 3"
        ),
      "Not Activated",
      Trauma_Team_Activation_Level,
      missing = "Not Activated"
    )
  ) |>
  dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
  dplyr::summarize(
    Activations = sum(
      Trauma_Team_Activation_Level %in% c("Level 1", "Level 2", "Level 3"),
      na.rm = TRUE
    ),
    `Not Activated` = sum(
      Trauma_Team_Activation_Level %in%
        c("Consultation", "Not Activated", "Non-Trauma"),
      na.rm = TRUE
    ),
    cases = dplyr::n(),
    .by = Year
  ) |>
  dplyr::mutate(
    percent_activations = Activations / cases,
    percent_label = traumar::pretty_percent(percent_activations)
  ) |>
  dplyr::mutate(
    change_activations = (Activations - dplyr::lag(Activations)) /
      dplyr::lag(Activations),
    change_activations_label = traumar::pretty_percent(change_activations),
    change_non_activations = (`Not Activated` - dplyr::lag(`Not Activated`)) /
      dplyr::lag(`Not Activated`),
    change_non_activations_label = traumar::pretty_percent(
      change_non_activations
    )
  )


# df for gt() table for overall trauma activation statistics ----
trauma_activation_cases_binary_pivot <- trauma_activation_cases_binary |>
  dplyr::select(-tidyselect::matches("_label")) |>
  tidyr::pivot_longer(
    cols = c(Activations:change_non_activations),
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
  dplyr::select(-c(`2020`, `2021`, `2022`))


# df for gt() graphics for detailed table ----
trauma_activation_cases_pivot <- trauma_activation_cases |>
  dplyr::select(-matches("label")) |>
  tidyr::pivot_wider(
    id_cols = Trauma_Team_Activation_Level,
    names_from = Year,
    values_from = n:change,
    values_fill = 0
  ) |>
  dplyr::mutate(
    `2020-2024 Trend` = list(c(
      change_2020,
      change_2021,
      change_2022,
      change_2023,
      change_2024
    )),
    .by = Trauma_Team_Activation_Level
  ) |>
  dplyr::select(-matches("_202[012]")) |>
  dplyr::filter(Trauma_Team_Activation_Level != "Level 3")


# gt() table for overall trauma activation stats ----
trauma_activation_cases_overall_tbl <-
  trauma_activation_cases_binary_pivot |>
  dplyr::mutate(
    Category = dplyr::case_when(
      Category == "cases" ~ "Trauma Cases",
      Category == "percent_activations" ~ "% Activations",
      Category == "change_activations" ~ "% Change Activations",
      Category == "change_non_activations" ~ "% Change Non-Activations",
      TRUE ~ Category
    )
  ) |>
  gt::gt() |>
  gt::cols_label(Category = "") |>
  gtExtras::gt_plt_sparkline(
    column = `2020-2024 Trend`,
    type = "shaded",
    same_limit = FALSE,
    label = FALSE
  ) |>
  gt::fmt_percent(rows = 4:6) |>
  gt::fmt_number(rows = 1:3, drop_trailing_zeros = TRUE) |>
  gt::tab_header(
    title = "Trauma Team Activation Statistics",
    subtitle = "Data: Iowa Trauma Registry 2020-2024"
  ) |>
  gt::tab_row_group(label = "Counts", rows = 1:3) |>
  gt::tab_row_group(label = "Proportion and Change", rows = 4:6) |>
  gt::row_group_order(groups = c("Counts", "Proportion and Change")) |>
  tab_style_hhs(border_cols = 2:4)

# save the activations table ----
gt::gtsave(
  data = trauma_activation_cases_overall_tbl,
  filename = "trauma_activation_cases_overall_tbl.png",
  path = plot_folder
)

# gt() table for detailed trauma team activation statistics ----
trauma_activation_cases_tbl <-
  trauma_activation_cases_pivot |>
  gt::gt() |>
  gt::cols_label(
    Trauma_Team_Activation_Level = "",
    n_2023 = "Count",
    n_2024 = "Count",
    percent_2023 = "% Cases",
    percent_2024 = "% Cases",
    change_2023 = "% Change",
    change_2024 = "% Change"
  ) |>
  gtExtras::gt_plt_sparkline(
    column = `2020-2024 Trend`,
    type = "shaded",
    same_limit = FALSE,
    label = FALSE
  ) |>
  gt::tab_spanner(label = "2021", columns = matches("_2021")) |>
  gt::tab_spanner(label = "2023", columns = matches("_2023")) |>
  gt::tab_spanner(label = "2024", columns = matches("_2024")) |>
  gt::fmt_percent(columns = matches("percent|change")) |>
  gt::fmt_number(columns = matches("n_"), drop_trailing_zeros = TRUE) |>
  gt::tab_header(
    title = "Trend of Trauma Team Activations",
    subtitle = "Data: Iowa Trauma Registry 2020-2024"
  ) |>
  gt::tab_row_group(label = "Non-Activation", rows = c(1, 4)) |>
  gt::tab_row_group(label = "Activation", rows = 2:3) |>
  gt::row_group_order(groups = c("Activation", "Non-Activation")) |>
  tab_style_hhs(border_cols = matches("n_\\d|trend"))

# save the TTA trends table ----
gt::gtsave(
  data = trauma_activation_cases_tbl,
  filename = "trauma_activation_cases_tbl.png",
  path = plot_folder
)


###_____________________________________________________________________________
# EMS Reporting - Utilize the Elite report Annual report - Main Report (Bulk) ----
# Using the bulk report is much faster and can easily be referenced again by
# download.
###_____________________________________________________________________________

###_____________________________________________________________________________
# Calculate the Executive Summary EMS data ----
###_____________________________________________________________________________

# Get unique count of incidents ----
ems_incidents <- ems_data_clean |>
  dplyr::filter(Scene_First_EMS_Unit_On_Scene == "Yes") |>
  dplyr::distinct(Unique_Run_ID, .keep_all = TRUE) |>
  dplyr::count(Year, name = "Incidents") |>
  dplyr::mutate(
    change_incident = (Incidents - dplyr::lag(Incidents)) /
      dplyr::lag(Incidents),
    change_incident_label = ifelse(
      is.na(change_incident),
      NA_character_,
      traumar::pretty_percent(change_incident)
    )
  )


# get a unique count of total resources used ----
ems_runs <- ems_data_clean |>
  dplyr::distinct(Unique_Run_ID, .keep_all = TRUE) |>
  dplyr::count(Year, name = "Runs") |>
  dplyr::mutate(
    change_runs = (Runs - dplyr::lag(Runs)) / dplyr::lag(Runs),
    change_runs_label = ifelse(
      is.na(change_runs),
      NA_character_,
      traumar::pretty_percent(change_runs)
    )
  )


# bind columns of the incidents and runs table ----
ems_incidents_runs <- ems_incidents |>
  dplyr::left_join(ems_runs, by = "Year") |>
  dplyr::mutate(incident_run_ratio = Runs / Incidents)

# ems incidents for printing ----
ems_incidents_runs_recent <- ems_incidents_runs |>
  dplyr::filter(Year %in% 2023:2024)

# Get All Transports count by filtering Disposition Incident Patient Disposition
# by only values in disposition_incident_patient_disposition ----

# transport incidents ----
transport_incidents <- ems_data_clean |>
  dplyr::filter(Scene_First_EMS_Unit_On_Scene == "Yes") |>
  dplyr::distinct(Unique_Run_ID, .keep_all = TRUE) |>
  dplyr::count(Year, Patient_Transported) |>
  dplyr::filter(Patient_Transported == T) |>
  dplyr::left_join(
    ems_incidents |>
      dplyr::select(Year, Incidents),
    by = "Year"
  ) |>
  dplyr::mutate(
    percent_incidents = n / Incidents,
    change_incidents = (n - dplyr::lag(n)) / dplyr::lag(n)
  )


# transport runs ----
transport_runs <- ems_data_clean |>
  dplyr::distinct(Unique_Run_ID, .keep_all = TRUE) |>
  dplyr::count(Year, Patient_Transported) |>
  dplyr::filter(Patient_Transported == T) |>
  dplyr::left_join(
    ems_runs |> dplyr::select(Year, Runs),
    by = "Year"
  ) |>
  dplyr::mutate(
    percent_runs = n / Runs,
    change_runs = (n - dplyr::lag(n)) / dplyr::lag(n)
  )


# join the transport runs and incidents table ----
transport_incidents_runs <- transport_incidents |>
  dplyr::rename(Transport_Incidents = n) |>
  dplyr::left_join(
    transport_runs |> dplyr::rename(Transport_Runs = n),
    by = c("Patient_Transported", "Year")
  ) |>
  dplyr::select(-Patient_Transported)

# transports for printing ----
transport_incidents_runs_recent <- transport_incidents_runs |>
  dplyr::filter(Year %in% 2023:2024)

# Get trauma related ems responses ----
ems_trauma_incidents <- ems_data_clean |>
  dplyr::filter(
    Trauma_Flag == "Yes",
    Scene_First_EMS_Unit_On_Scene == "Yes"
  ) |>
  dplyr::distinct(Unique_Run_ID, .keep_all = TRUE) |>
  dplyr::count(Year, name = "Incidents") |>
  dplyr::mutate(
    change_incident = (Incidents - dplyr::lag(Incidents)) /
      dplyr::lag(Incidents),
    change_incident_label = traumar::pretty_percent(change_incident)
  )


# Get n trauma runs ----
ems_trauma_runs <- ems_data_clean |>
  dplyr::filter(Trauma_Flag == "Yes") |>
  dplyr::distinct(Unique_Run_ID, .keep_all = TRUE) |>
  dplyr::count(Year, name = "Runs") |>
  dplyr::mutate(
    change_runs = (Runs - dplyr::lag(Runs)) / dplyr::lag(Runs),
    change_runs_label = traumar::pretty_percent(change_runs)
  )


# ems trauma join incidents and runs ----
ems_trauma_incidents_runs <- ems_trauma_incidents |>
  dplyr::left_join(ems_trauma_runs, by = "Year")

# ems trauma stats for printing ----
ems_trauma_incidents_runs_recent <- ems_trauma_incidents_runs |>
  dplyr::filter(Year %in% 2023:2024)

# Get trauma related ems transport runs ----
ems_trauma_transport_incidents <- ems_data_clean |>
  dplyr::filter(
    Trauma_Flag == "Yes",
    Scene_First_EMS_Unit_On_Scene == "Yes"
  ) |>
  dplyr::distinct(Unique_Run_ID, .keep_all = TRUE) |>
  dplyr::count(Year, Patient_Transported) |>
  dplyr::filter(Patient_Transported == T) |>
  dplyr::left_join(
    ems_trauma_incidents |> dplyr::select(Year, Incidents),
    by = "Year"
  ) |>
  dplyr::left_join(
    transport_incidents |> dplyr::select(Year, n_transports = n),
    by = dplyr::join_by(Year),
    suffix = c("_trauma", "_transport")
  ) |>
  dplyr::mutate(
    percent_trauma_transport_incidents = n / Incidents,
    change_trauma_transport_incidents = (Incidents - dplyr::lag(Incidents)) /
      dplyr::lag(Incidents),
    percent_trauma_total_transports = n / n_transports
  )

# Get trauma related ems transport incidents ----
ems_trauma_transport_runs <- ems_data_clean |>
  dplyr::filter(Trauma_Flag == "Yes") |>
  dplyr::distinct(Unique_Run_ID, .keep_all = TRUE) |>
  dplyr::count(Year, Patient_Transported) |>
  dplyr::filter(Patient_Transported == T) |>
  dplyr::left_join(
    ems_trauma_runs |> dplyr::select(Year, Runs),
    by = "Year"
  ) |>
  dplyr::left_join(
    transport_runs |> dplyr::select(Year, n_transports = n),
    by = dplyr::join_by(Year)
  ) |>
  dplyr::mutate(
    percent_trauma_transport_runs = n / Runs,
    change_trauma_transport_runs = (Runs - dplyr::lag(Runs)) /
      dplyr::lag(Runs),
    percent_trauma_total_transport_runs = n / n_transports
  )


# join the ems trauma transport incident and runs tables ----
ems_trauma_transport_incidents_runs <- ems_trauma_transport_incidents |>
  dplyr::rename(Trauma_Transport_Incidents = n) |>
  dplyr::left_join(
    ems_trauma_transport_runs |> dplyr::rename(Trauma_Transport_Runs = n),
    by = c("Patient_Transported", "Year"),
    suffix = c("_incidents", "_runs")
  ) |>
  dplyr::select(-Patient_Transported)

# trauma transports for printing ----
ems_trauma_transport_incidents_runs_recent <- ems_trauma_transport_incidents_runs |>
  dplyr::filter(Year %in% 2023:2024)
