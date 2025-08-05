####
# Script to create the Annual Trauma Report ----
####

###_____________________________________________________________________________
# use the Patient Registry report Annual Trauma Report (Bulk Export - State Incidents)
# The bulk report is much more efficient with larger numbers of records
# This report is under My Reports, Annual Trauma Reports
# Use the report for the date range for the current report year and then for the
# previous five years
# This is done by setting criteria to the appropriate date range in the criteria tab
###_____________________________________________________________________________

###_____________________________________________________________________________
# For the SEQIC indicators and OOH indicators, use the functions
# created from the custom_functions script to tidyr::complete the needed operations.
# It is not necessary to copy those scripts into this one, they can be run separately,
# simply signal that you are doing the SEQIC or OOH portion of the analysis
# at the appropriate time
###_____________________________________________________________________________

###_____________________________________________________________________________
# Get the unique responses for the ICD-10 causes of injury to make categories ----
###_____________________________________________________________________________

cause_of_injury_trauma <- trauma_2024 |>
  injury_case_count(CAUSE_OF_INJURY_AR_1, sort = TRUE) |>
  dplyr::mutate(
    percent = traumar::pretty_percent(variable = n / sum(n)),
    CAUSE_OF_INJURY_AR_1 = ifelse(
      is.na(CAUSE_OF_INJURY_AR_1),
      "Missing",
      CAUSE_OF_INJURY_AR_1
    )
  )

# check missingness in the cause of injury annual report field
cause_of_injury_missing <- trauma_2024 |>
  dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
  dplyr::filter(is.na(CAUSE_OF_INJURY_AR_1))

# find unique injury codes that are missing
injury_codes_missing <- cause_of_injury_missing |>
  dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
  dplyr::count(Mechanism_Injury_1_code, sort = TRUE)

# among the codes missing the cause of injury category for the annual report
# can these codes be found in any of the matrices we use for categorization?
injury_codes_missing_flagged <- injury_codes_missing |>
  dplyr::mutate(
    can_categorize_mech = stringr::str_sub(
      Mechanism_Injury_1_code,
      start = 1,
      end = 4
    ) %in%
      unique(stringr::str_sub(
        mechanism_injury_mapping$UPPER_CODE,
        start = 1,
        end = 4
      )),
    can_categorize_cause = stringr::str_sub(
      Mechanism_Injury_1_code,
      start = 1,
      end = 4
    ) %in%
      unique(stringr::str_sub(
        nature_injury_mapping$ICD_10_CODE_TRIM,
        start = 1,
        end = 4
      ))
  )

###_____________________________________________________________________________
# Get counts ----
###_____________________________________________________________________________

# patients

{
  patient_count <- trauma_2024 |>
    injury_patient_count() |>
    dplyr::pull(n)

  patient_count_years <- trauma_data_clean |>
    injury_patient_count(Year, descriptive_stats = TRUE)
}

# injuries

{
  incident_count <- trauma_2024 |>
    injury_incident_count() |>
    dplyr::pull(n)

  incident_count_years <- trauma_data_clean |>
    injury_incident_count(Year, descriptive_stats = TRUE)
}

# cases

{
  case_count <- trauma_2024 |>
    injury_case_count() |>
    dplyr::pull(n)

  case_count_years <- trauma_data_clean |>
    injury_case_count(Year, descriptive_stats = TRUE)
}

###_____________________________________________________________________________
# Transfers and transfer delay ----
###_____________________________________________________________________________

{
  # Compute transfer delay summaries for 2023–2024 using imputed Length_of_Stay
  transfer_delays_2024 <- trauma_data_clean |>

    # Filter to only cases from 2023–2024 that were acute interfacility transfers
    dplyr::filter(Year %in% 2023:2024, Acute_Transfer_Out == "Yes") |>

    # Retain only one row per incident to avoid double-counting transfers
    dplyr::group_by(Year) |>
    dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
    dplyr::ungroup() |>

    # Apply two-stage imputation for Length_of_Stay
    dplyr::mutate(
      # Step 1: Handle extreme outliers using IQR-based capping within each year
      Length_of_Stay_imp = traumar::impute(
        Length_of_Stay,
        focus = "skew", # Targeting non-normal distribution tails
        method = "iqr" # IQR rule to cap values beyond upper fence
      ),

      # Step 2: Impute remaining missing values using the median within each year
      Length_of_Stay_imp = traumar::impute(
        Length_of_Stay,
        focus = "missing", # Now targeting missing values only
        method = "median" # Robust central tendency for skewed data
      ),

      .by = Year # Apply both steps separately for each year
    ) |>

    # Summarize delay and timeliness indicators for each year
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

    # Join with total injury case counts to contextualize transfer volume
    dplyr::left_join(
      trauma_data_clean |>
        dplyr::filter(Year %in% 2023:2024) |>
        injury_case_count(Year, name = "Total cases"),
      by = "Year"
    )
}

###_____________________________________________________________________________
# Gender ----
###_____________________________________________________________________________

# get gender data

gender_group <- trauma_data_clean |>
  dplyr::filter(Year %in% 2023:2024) |>
  injury_incident_count(Year, Patient_Gender) |>
  tidyr::replace_na(list(Patient_Gender = "Missing")) |>
  dplyr::mutate(Proportion = (n / sum(n)) * 100, .by = Year) |>
  dplyr::mutate(
    Patient_Gender = dplyr::if_else(
      grepl(pattern = "not", x = Patient_Gender, ignore.case = TRUE),
      "Missing",
      Patient_Gender
    )
  )

# create a table visualization using gt()

gender_group_tbl <- {
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
      subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2023-2024"
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
      footnote = "All counts and other measures are related to reinjured patients, only.",
      locations = gt::cells_row_groups()
    ) |>
    gt::tab_footnote(
      footnote = "Injury event refers to the number of unique injury incidents that led to evaluation/treatment at a verified trauma center.  Each injury event could involve multiple cases, and each patient may have one or more injury events in a specified timespan.",
      locations = gt::cells_column_labels(columns = n)
    ) |>
    gt::tab_footnote(
      footnote = "Refers to the proportion of injuries attributed to reinjured patients in a given year",
      locations = gt::cells_column_labels(columns = Proportion)
    ) |>
    gt::tab_footnote(
      footnote = "Small counts < 6 are masked to protect confidentiality",
      locations = gt::cells_body(columns = n, rows = 3)
    ) |>
    gt::opt_footnote_marks(marks = "extended") |>
    tab_style_hhs(border_cols = n:Proportion, message_text = NULL)
}

###_____________________________________________________________________________
# Age ----
###_____________________________________________________________________________

# source df

{
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
    injury_incident_count(Year, Age_Range) |>
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
}

# exploratory age plot
age_group |>
  dplyr::filter(Age_Range != "Missing") |>
  ggplot2::ggplot(ggplot2::aes(x = Year, y = n, fill = trend)) +
  ggplot2::geom_col(position = "dodge", width = 0.5) +
  ggplot2::facet_wrap(~Age_Range, scales = "fixed") +
  ggplot2::scale_y_continuous(
    n.breaks = 4,
    labels = function(x) traumar::pretty_number(x)
  ) +
  paletteer::scale_fill_paletteer_d(
    palette = "colorblindr::OkabeIto_black",
    direction = -1
  ) +
  traumar::theme_cleaner(
    axis.text.x = ggplot2::element_text(angle = 90),
    facets = TRUE
  ) +
  ggplot2::labs(x = "")

# df for printing
age_group_filtered <- age_group |>
  dplyr::filter(Year %in% 2023:2024)

# df for the plot
age_group_plot_df <- age_group |>
  dplyr::filter(Year %in% 2021:2024, Age_Range != "Missing") |>
  dplyr::mutate(
    category = dplyr::if_else(
      change > 0,
      "Increase",
      dplyr::if_else(change < 0, "Decrease", "Neutral")
    )
  )

# produce the plot

{
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
      base_size = 15,
      vjust_title = 2,
      vjust_subtitle = 1.25,
      facet_text_size = 16,
      title_text_size = 20,
      subtitle_text_size = 18,
      draw_panel_border = TRUE,
      facets = TRUE
    )

  # save the age group column plot

  plot_save_params(
    filename = "age_group_plot.png",
    plot = age_group_plot,
    path = plot_path,
    height = 9
  )
}

# age group plot line giving counts

{
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
      breaks = waiver(),
      labels = function(x) traumar::pretty_number(x, n_decimal = 2)
    ) +
    ggplot2::scale_color_paletteer_d("colorBlindness::Blue2Orange12Steps") +
    traumar::theme_cleaner(
      base_size = 15,
      vjust_title = 2,
      vjust_subtitle = 1.25,
      facet_text_size = 16,
      title_text_size = 20,
      subtitle_text_size = 18,
      draw_panel_border = TRUE,
      facets = TRUE
    )

  # save the age group line plot

  plot_save_params(
    filename = "age_group_lines.png",
    plot = age_group_lines,
    path = plot_path,
    height = 9
  )
}

###_____________________________________________________________________________
# Race ----
###_____________________________________________________________________________

# source df

{
  race_group <- trauma_data_clean |>
    dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
    dplyr::mutate(
      Patient_Race = dplyr::if_else(
        grepl(pattern = "not|select", x = Patient_Race, ignore.case = TRUE),
        "Not Known/Not Recorded",
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
      Patient_Race = tidyr::replace_na(Patient_Race, "Not Known/Not Recorded")
    ) |>
    injury_incident_count(Year, Patient_Race, name = "Injury_Events") |>
    tidyr::complete(Year, Patient_Race, fill = list(Injury_Events = 0)) |>
    injury_mutate() |>
    dplyr::arrange(Patient_Race) |>
    dplyr::mutate(
      change = (Injury_Events - dplyr::lag(Injury_Events)) /
        dplyr::lag(Injury_Events),
      change_label = traumar::pretty_percent(change, n_decimal = 2),
      .by = Patient_Race
    ) |>
    dplyr::mutate(
      change = dplyr::if_else(change == Inf, 0, change),
      change_label = dplyr::if_else(change_label == Inf, "0%", change_label)
    ) |>
    tidyr::replace_na(list(change = 0, change_label = "0%"))
}

# df for plotting

race_group_plot_df <- race_group |>
  dplyr::filter(Year %in% 2021:2024) |>
  dplyr::mutate(normal_events = normalize(Injury_Events), .by = Patient_Race)

# df for printing

race_group_filtered <- race_group |>
  dplyr::filter(Year %in% 2023:2024)

# build the race group column plot

{
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
    ggplot2::geom_col(color = "black", width = 0.8) +
    ggrepel::geom_text_repel(
      size = 6,
      segment.color = NA,
      nudge_y = dplyr::if_else(race_group_plot_df$change < 0, -0.1, 0.1),
      direction = "y",
      family = "Work Sans SemiBold",
      max.iter = 30000
    ) +
    ggplot2::labs(
      title = "Percent Change in Injury Events by Patient Race",
      subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2023-2024",
      caption = "Injury event refers to the number of unique injury incidents that led to evaluation/treatment at a verified trauma center.\nEach injury event could involve multiple cases, and each patient may have one or more injury events in a specified timespan.",
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
      base_size = 15,
      vjust_title = 2,
      vjust_subtitle = 1.25,
      facet_text_size = 16,
      title_text_size = 20,
      subtitle_text_size = 18,
      draw_panel_border = TRUE,
      facets = TRUE
    )

  # save the race group column plot

  plot_save_params(
    filename = "race_group_plot.png",
    plot = race_group_plot,
    path = plot_path,
    height = 9
  )
}

# race group line graph

{
  race_group_line_plot <- race_group_plot_df |>
    dplyr::filter(Patient_Race != "More than One Category") |>
    ggplot2::ggplot(ggplot2::aes(
      factor(Year),
      Injury_Events,
      group = Patient_Race,
      color = Patient_Race,
      label = traumar::pretty_number(x = Injury_Events, n_decimal = 2)
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
      title = "Count of Unique Injury Events by Patient Race",
      subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2021-2024",
      caption = "Note: Scale does not reach zero and each subplot may have a different scale.\nInjury event refers to the number of unique injury incidents that led to evaluation/treatment at a verified trauma center.\nEach injury event could involve multiple cases, and each patient may have one or more injury events in a specified timespan.",
      x = "",
      y = "Injury Event Count",
    ) +
    ggplot2::facet_wrap(~Patient_Race, scales = "free_y") +
    ggplot2::guides(color = "none", fill = "none") +
    ggplot2::scale_y_continuous(
      breaks = waiver(),
      labels = function(x) traumar::pretty_number(x, n_decimal = 2)
    ) +
    ggplot2::scale_color_paletteer_d("colorBlindness::Blue2DarkOrange12Steps") +
    traumar::theme_cleaner(
      vjust_title = 2,
      vjust_subtitle = 1.25,
      facet_text_size = 16,
      title_text_size = 20,
      subtitle_text_size = 18,
      draw_panel_border = TRUE,
      facets = TRUE
    )

  # save the race group line plot

  plot_save_params(
    filename = "race_group_line_plot.png",
    plot = race_group_line_plot,
    path = plot_path,
    height = 9
  )
}

###_____________________________________________________________________________
# Leading causes of injury ----
###_____________________________________________________________________________

# leading causes of injury - cases

leading_causes_cases <- trauma_data_clean |>
  injury_case_count(Year, CAUSE_OF_INJURY_AR_1) |>
  dplyr::group_by(Year) |>
  dplyr::arrange(desc(n), .by_group = TRUE) |>
  dplyr::ungroup() |>
  na.omit() |>
  dplyr::mutate(percent = n / sum(n, na.rm = TRUE), .by = Year) |>
  dplyr::mutate(
    label_num = small_count_label(var = n, cutoff = 6, replacement = "*"),
    percent_label = dplyr::if_else(
      label_num == "*",
      "*",
      dplyr::if_else(
        n == 6,
        traumar::pretty_percent(percent, n_decimal = 2),
        traumar::pretty_percent(percent, n_decimal = 1)
      )
    )
  ) |>
  dplyr::filter(Year > 2020, Year < 2024)

# leading causes of injury events
leading_causes_years <- trauma_data_clean |>
  injury_incident_count(Year, CAUSE_OF_INJURY_AR_1) |>
  dplyr::group_by(Year) |>
  dplyr::arrange(desc(n), .by_group = TRUE) |>
  dplyr::ungroup() |>
  na.omit() |>
  dplyr::mutate(percent = n / sum(n, na.rm = TRUE), .by = Year) |>
  dplyr::mutate(
    label_num = small_count_label(var = n, cutoff = 6, replacement = "*"),
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

# for printing

leading_causes_recent <- leading_causes_years |>
  dplyr::filter(Year %in% 2023:2024)

# get color order

legend_order = leading_causes_years |>
  dplyr::filter(Year > 2020, Year < 2024) |>
  dplyr::distinct(CAUSE_OF_INJURY_AR_1) |>
  dplyr::pull()

# plot of leading causes by year

{
  leading_causes_plot <- leading_causes_years |>
    dplyr::filter(Year > 2020, Year < 2024) |>
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
      color = hhs_palette_1$primary_1,
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
    ggplot2::guides(fill = guide_legend(nrow = 1), color = "none") +
    ggplot2::scale_y_continuous(
      labels = function(x) traumar::pretty_percent(x)
    ) +
    ggplot2::scale_fill_paletteer_d(
      palette = "colorBlindness::PairedColor12Steps",
      breaks = legend_order
    ) +
    traumar::theme_cleaner(
      axis.text.x = element_blank(),
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

  # save the plot on leading causes

  plot_save_params(
    filename = "leading_causes_injury_years.png",
    plot = leading_causes_plot,
    path = plot_path,
    height = 9
  )
}

###_____________________________________________________________________________
# Falls ----
###_____________________________________________________________________________

fall_related_cases <- {
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
}

fall_related_injuries <- {
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
}

###_____________________________________________________________________________
# Motor vehicle, boating, and air incidents ----
###_____________________________________________________________________________

motor_vehicle_related_cases <- {
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
}

motor_vehicle_related_injuries <- {
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
      Injury_Events = dplyr::n(),
      percent = round(MVC / Injury_Events, digits = 4),
      .by = Year
    ) |>
    dplyr::mutate(
      increase = round((MVC - dplyr::lag(MVC)) / dplyr::lag(MVC), digits = 3),
      increase_label = traumar::pretty_percent(
        (MVC - dplyr::lag(MVC)) / dplyr::lag(MVC),
        n_decimal = 2
      )
    )
}

# table for motor vehicle injuries

mvc_injury_transpose <- {
  motor_vehicle_related_injuries |>
    dplyr::filter(Year < 2024) |>
    dplyr::select(-increase_label) |>
    t() |>
    as.data.frame() |>
    rownames_to_column(var = "Category") |>
    dplyr::filter(Category != "Year") |>
    set_names(nm = c("Category", 2018:2024)) |>
    dplyr::mutate(
      Category = c(
        "MVC Injury Event Count",
        "Total Injury Events",
        "Proportion of Injuries",
        "% Change in MVC Injuries"
      )
    ) |>
    dplyr::mutate(
      `2018-2024 Trend` = list(c(
        `2018`,
        `2019`,
        `2020`,
        `2021`,
        `2023`,
        `2024`
      )),
      .by = Category
    ) |>
    dplyr::select(-c(`2018`, `2019`, `2020`))
}

# create the table

mvc_injury_table <- {
  mvc_injury_transpose |>
    gt() |>
    cols_label(Category = "") |>
    gt_plt_sparkline(
      column = `2018-2024 Trend`,
      type = "shaded",
      same_limit = FALSE,
      label = FALSE
    ) |>
    fmt_percent(rows = 3:4) |>
    fmt_number(rows = 1:2, drop_trailing_zeros = TRUE) |>
    tab_header(
      title = "Summary: Trend of Motor Vehicle Injury Events ",
      subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2018-2024"
    ) |>
    tab_row_group(label = "Counts", rows = 1:2) |>
    tab_row_group(label = "Proportion and Change", rows = 3:4) |>
    row_group_order(groups = c("Counts", "Proportion and Change")) |>
    tab_footnote(
      footnote = "Injury event refers to the number of unique injury incidents that led to evaluation/treatment at a verified trauma center.  Each injury event could involve multiple cases, and each patient may have one or more injury events in a specified timespan.",
      locations = cells_body(columns = 1, rows = 1:2)
    ) |>
    opt_footnote_marks(marks = "standard") |>
    tab_style_hhs(border_cols = 2:5)
}

###_____________________________________________________________________________
# Reinjury ----
###_____________________________________________________________________________

# patients

reinjuries_stats_patients <- trauma_data_clean |>
  reinjury_patient_count(descriptive_stats = TRUE)

# cases

reinjuries_stats_cases <- trauma_data_clean |>
  reinjury_case_count(descriptive_stats = TRUE)

# injury events

reinjury_stats_injuries <- trauma_data_clean |>
  reinjury_injury_count(descriptive_stats = TRUE)

# create a gt() table to be explored in the report

reinjury_stat_tbl_object <- reinjury_stats_injuries |>
  dplyr::select(-matches("_label")) |>
  dplyr::filter(Year < 2024) |>
  tidyr::replace_na(list(change = 0)) |>
  t() |>
  as.data.frame() |>
  set_names(nm = c(2018:2024)) |>
  rownames_to_column(var = "Category") |>
  dplyr::filter(Category != "Year") |>
  rowwise() |>
  dplyr::mutate(
    `2018-2024 Trend` = list(c(`2018`, `2019`, `2020`, `2021`, `2023`, `2024`))
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-c(`2018`, `2019`, `2020`))

# generate the gt() table

reinjury_stat_tbl <- {
  reinjury_stat_tbl_object |>
    dplyr::mutate(
      Category = case_when(
        Category == "Reinjury" ~ "Injury Event Count (Reinjured Pts Only)",
        Category == "n" ~ "Total Injury Events",
        Category == "prop" ~ "% Reinjury",
        Category == "change" ~ "% Change in Injury Events",
        Category == "Avg_Injuries" ~ "Average Injury Events",
        Category == "Min_Injuries" ~ "Minimum  Injury Events",
        Category == "Max_Injuries" ~ "Max  Injury Events"
      )
    ) |>
    gt() |>
    cols_label(Category = "") |>
    gt_plt_sparkline(
      column = `2018-2024 Trend`,
      same_limit = FALSE,
      type = "shaded",
      label = FALSE
    ) |>
    fmt_number(
      columns = everything(),
      rows = c(1:2, 5:7),
      drop_trailing_zeros = TRUE
    ) |>
    fmt_percent(
      columns = everything(),
      rows = 3:4,
      drop_trailing_zeros = TRUE
    ) |>
    tab_header(
      title = "Summary: Trend of Reinjury in Iowa",
      subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2018-2024"
    ) |>
    tab_row_group(label = "Counts", rows = 1:2) |>
    tab_row_group(label = "Proportion and Change", rows = 3:4) |>
    tab_row_group(label = "Measures of Spread", rows = 5:7) |>
    row_group_order(groups = c("Counts", "Proportion and Change")) |>
    tab_footnote(
      footnote = "All counts and other measures are related to reinjured patients, only.",
      locations = cells_row_groups()
    ) |>
    tab_footnote(
      footnote = "Injury event refers to the number of unique injury incidents that led to evaluation/treatment at a verified trauma center.  Each injury event could involve multiple cases, and each patient may have one or more injury events in a specified timespan.",
      locations = cells_body(columns = 1, rows = 2)
    ) |>
    tab_footnote(
      footnote = "Refers to the proportion of injuries, or change in count (from previous year), attributed to reinjured patients in a given year",
      locations = cells_body(columns = 1, rows = 3:4)
    ) |>
    opt_footnote_marks(marks = "extended") |>
    tab_style_hhs(border_cols = 2:5)
}

###_____________________________________________________________________________
# work related ----
###_____________________________________________________________________________

work_related_cases <- {
  trauma_data_clean |>
    dplyr::mutate(
      Financial_Work_Related = dplyr::if_else(
        Financial_Work_Related != "Yes" | is.na(Financial_Work_Related),
        "No",
        Financial_Work_Related
      )
    ) |>
    injury_case_count(Year, Financial_Work_Related, name = "cases") |>
    case_mutate() |>
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
}

work_related_injuries <- {
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
      Financial_Work_Related,
      name = "Injury_Events"
    ) |>
    injury_mutate() |>
    dplyr::filter(Financial_Work_Related == "Yes") |>
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
}

###_____________________________________________________________________________
# farm related ----
###_____________________________________________________________________________

farm_related_cases <- {
  trauma_data_clean |>
    dplyr::mutate(
      Farm_Ag_Related = dplyr::if_else(
        Farm_Ag_Related != "Yes" | is.na(Farm_Ag_Related),
        "No",
        Farm_Ag_Related
      )
    ) |>
    injury_case_count(Year, Farm_Ag_Related, name = "cases") |>
    case_mutate() |>
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
}

farm_related_injuries <- {
  trauma_data_clean |>
    dplyr::mutate(
      Farm_Ag_Related = dplyr::if_else(
        Farm_Ag_Related != "Yes" | is.na(Farm_Ag_Related),
        "No",
        Farm_Ag_Related
      )
    ) |>
    injury_incident_count(Year, Farm_Ag_Related, name = "Injury_Events") |>
    injury_mutate() |>
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
}

###_____________________________________________________________________________
# Intentionality ----
###_____________________________________________________________________________

# source df

{
  intentionality_of_injury <- trauma_data_clean |>
    dplyr::filter(Year < 2024) |>
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
}

# pivot the df for a gt() table

intentionality_of_injury_pivot <- intentionality_of_injury |>
  pivot_longer(
    cols = 2:length(intentionality_of_injury),
    names_to = "Category",
    values_to = "Vals"
  ) |>
  pivot_wider(id_cols = Category, names_from = Year, values_from = Vals) |>
  tidyr::replace_na(list(`2018` = 0)) |>
  dplyr::mutate(
    `2018-2024 Trend` = list(c(`2018`, `2019`, `2020`, `2021`, `2023`, `2024`)),
    .by = Category
  ) |>
  dplyr::select(-c(`2018`:`2020`))

# develop the gt() table for intentionality of injury

intentionality_of_injury_tbl <- {
  intentionality_of_injury_pivot |>
    gt() |>
    cols_label(Category = "") |>
    gt_plt_sparkline(
      column = `2018-2024 Trend`,
      type = "shaded",
      same_limit = FALSE,
      label = FALSE
    ) |>
    fmt_number(
      columns = everything(),
      rows = 1:5,
      drop_trailing_zeros = TRUE
    ) |>
    fmt_percent(
      columns = everything(),
      rows = 6:9,
      drop_trailing_zeros = TRUE
    ) |>
    tab_header(
      title = "Summary: Trend of Intentional/Unintentional Injury Events in Iowa",
      subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2018-2024"
    ) |>
    tab_row_group(label = "Counts", rows = 1:5) |>
    tab_row_group(label = "Proportion and Change", rows = 6:9) |>
    row_group_order(groups = c("Counts", "Proportion and Change")) |>
    tab_footnote(
      footnote = "Injury event refers to the number of unique injury incidents that led to evaluation/treatment at a verified trauma center.  Each injury event could involve multiple cases, and each patient may have one or more injury events in a specified timespan.",
      locations = cells_row_groups(groups = "Counts")
    ) |>
    tab_footnote(
      footnote = "Refers to the proportion of injuries, or change in count (from previous year), attributed to patients in a given year.",
      locations = cells_row_groups(groups = "Proportion and Change")
    ) |>
    opt_footnote_marks(marks = "extended") |>
    tab_style_hhs(border_cols = 2:5)
}

###_____________________________________________________________________________
# Trauma activations ----
###_____________________________________________________________________________

# source df

{
  trauma_activation_cases <- trauma_data_clean |>
    dplyr::filter(Year < 2024) |>
    dplyr::mutate(
      Trauma_Team_Activation_Level = dplyr::if_else(
        Trauma_Team_Activation_Level %in%
          c(
            "Consultation",
            "Level 1",
            "Level 2",
            "Level 3",
            "Not Activated",
            "Non-Trauma"
          ),
        Trauma_Team_Activation_Level,
        "Missing"
      ),
      Trauma_Team_Activation_Level = dplyr::if_else(
        Trauma_Team_Activation_Level == "Level 3",
        "Level 2",
        Trauma_Team_Activation_Level
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
    ) |>
    dplyr::mutate(
      change = dplyr::if_else(
        Year > 2018 &
          Year <= 2021 &
          Trauma_Team_Activation_Level == "Non-Trauma",
        0,
        change
      ),
      change_label = dplyr::if_else(
        Year > 2018 & Trauma_Team_Activation_Level == "Non-Trauma",
        "0%",
        change_label
      )
    )
}

# df for printing

trauma_activation_cases_recent <- trauma_activation_cases |>
  dplyr::filter(Year %in% 2023:2024)

# overall activations

{
  trauma_activation_cases_binary <- trauma_data_clean |>
    dplyr::filter(Year < 2024) |>
    dplyr::mutate(
      Trauma_Team_Activation_Level = dplyr::if_else(
        Trauma_Team_Activation_Level %in%
          c(
            "Consultation",
            "Level 1",
            "Level 2",
            "Level 3",
            "Not Activated",
            "Non-Trauma"
          ),
        Trauma_Team_Activation_Level,
        "Missing"
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
      Missing = sum(Trauma_Team_Activation_Level == "Missing", na.rm = TRUE),
      `Records Not Missing Activation Data` = Activations + `Not Activated`,
      cases = dplyr::n(),
      .by = Year
    ) |>
    dplyr::mutate(
      percent_activations = Activations / `Records Not Missing Activation Data`,
      percent_label = traumar::pretty_percent(percent_activations),
      .by = Year
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
}

# df for gt() table for overall trauma activation statistics

{
  trauma_activation_cases_binary_pivot <- trauma_activation_cases_binary |>
    dplyr::select(-matches("_label")) |>
    pivot_longer(
      cols = c(Activations:change_non_activations),
      names_to = "Category",
      values_to = "Value"
    ) |>
    pivot_wider(id_cols = Category, names_from = Year, values_from = Value) |>
    dplyr::mutate(
      `2018-2024 Trend` = list(c(
        `2018`,
        `2019`,
        `2020`,
        `2021`,
        `2023`,
        `2024`
      )),
      .by = Category
    ) |>
    dplyr::select(-c(`2018`, `2019`, `2020`))
}

# df for gt() graphics for detailed table

{
  trauma_activation_cases_pivot <- trauma_activation_cases |>
    dplyr::select(-matches("label")) |>
    pivot_wider(
      id_cols = Trauma_Team_Activation_Level,
      names_from = Year,
      values_from = n:change,
      values_fill = 0
    ) |>
    dplyr::mutate(
      `2018-2024 Trend` = list(c(
        change_2018,
        change_2019,
        change_2020,
        change_2021,
        change_2023,
        change_2024
      )),
      .by = Trauma_Team_Activation_Level
    ) |>
    dplyr::select(-matches("_20(18|19|20)"))
}

# gt() table for overall trauma activation stats

trauma_activation_cases_overall_tbl <- {
  trauma_activation_cases_binary_pivot |>
    dplyr::mutate(
      Category = case_when(
        Category == "percent_activations" ~ "% Activations",
        Category == "change_activations" ~ "% Change Activations",
        Category == "change_non_activations" ~ "% Change Non-Activations",
        TRUE ~ Category
      )
    ) |>
    gt() |>
    cols_label(Category = "") |>
    gt_plt_sparkline(
      column = `2018-2024 Trend`,
      type = "shaded",
      same_limit = FALSE,
      label = FALSE
    ) |>
    fmt_percent(rows = 6:8) |>
    fmt_number(rows = 1:5, drop_trailing_zeros = TRUE) |>
    tab_header(
      title = "Summary: Trend of Overall Trauma Team Activation Statistics",
      subtitle = "Trauma Center case Data | Data: Iowa Trauma Registry 2018-2024"
    ) |>
    tab_row_group(label = "Counts", rows = 1:5) |>
    tab_row_group(label = "Proportion and Change", rows = 6:8) |>
    row_group_order(groups = c("Counts", "Proportion and Change")) |>
    tab_source_note(
      source_note = md(paste0(
        fontawesome::fa("magnifying-glass"),
        " These data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a patient enters an ED or hospital for treatment of an injury."
      ))
    ) |>
    tab_style_hhs(border_cols = 2:5)
}

# gt() table for detailed trauma team activation statistics

trauma_activation_cases_tbl <- {
  trauma_activation_cases_pivot |>
    gt() |>
    cols_label(
      Trauma_Team_Activation_Level = "",
      n_2021 = "Count",
      n_2023 = "Count",
      n_2024 = "Count",
      percent_2021 = "% Cases",
      percent_2023 = "% Cases",
      percent_2024 = "% Cases",
      change_2021 = "% Change",
      change_2023 = "% Change",
      change_2024 = "% Change"
    ) |>
    gt_plt_sparkline(
      column = `2018-2024 Trend`,
      type = "shaded",
      same_limit = FALSE,
      label = FALSE
    ) |>
    tab_spanner(label = "2021", columns = matches("_2021")) |>
    tab_spanner(label = "2023", columns = matches("_2023")) |>
    tab_spanner(label = "2024", columns = matches("_2024")) |>
    fmt_percent(columns = matches("percent|change")) |>
    fmt_number(columns = matches("n_"), drop_trailing_zeros = TRUE) |>
    tab_header(
      title = "Summary: Trend of Trauma Team Activation Level Statistics",
      subtitle = "Trauma Center Case Data | Data: Iowa Trauma Registry 2018-2024"
    ) |>
    tab_row_group(label = "Non-Activation", rows = c(1, 4:6)) |>
    tab_row_group(label = "Activation", rows = 2:3) |>
    row_group_order(groups = c("Activation", "Non-Activation")) |>
    tab_source_note(
      source_note = md(paste0(
        fontawesome::fa("magnifying-glass"),
        " These data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a patient enters an ED or hospital for treatment of an injury."
      ))
    ) |>
    tab_footnote(
      footnote = "Reflects trend in % change of case count for each activation level category from 2018-2024.",
      locations = cells_column_labels(columns = `2018-2024 Trend`)
    ) |>
    opt_footnote_marks(marks = "standard") |>
    tab_style_hhs(border_cols = matches("n_\\d|trend"))
}

###_____________________________________________________________________________
# EMS Reporting - Utilize the Elite report Annual report - Main Report (Bulk) ----
# Using the bulk report is much faster and can easily be referenced again by
# download.
###_____________________________________________________________________________

###_____________________________________________________________________________
# Calculate the Executive Summary EMS data ----
###_____________________________________________________________________________

# Get unique count of incidents

{
  ems_incidents <- ems_data_clean |>
    dplyr::filter(Scene_First_EMS_Unit_On_Scene == "Yes", Year < 2024) |>
    dplyr::distinct(Unique_Run_ID, .keep_all = TRUE) |>
    dplyr::count(Year, name = "Incidents") |>
    dplyr::mutate(
      change_incident = (Incidents - dplyr::lag(Incidents)) /
        dplyr::lag(Incidents),
      change_incident_label = traumar::pretty_percent(change_incident)
    )
}

# get a unique count of total resources used

{
  ems_runs <- ems_data_clean |>
    dplyr::filter(Year < 2024) |>
    dplyr::distinct(Unique_Run_ID, .keep_all = TRUE) |>
    dplyr::count(Year, name = "Runs") |>
    dplyr::mutate(
      change_runs = (Runs - dplyr::lag(Runs)) / dplyr::lag(Runs),
      change_runs_label = traumar::pretty_percent(change_runs)
    )
}

# bind columns of the incidents and runs table

ems_incidents_runs <- ems_incidents |>
  dplyr::left_join(ems_runs, by = "Year")

# ems incidents for printing

ems_incidents_runs_recent <- ems_incidents_runs |>
  dplyr::filter(Year %in% 2023:2024)

# Get All Transports count by filtering Disposition Incident Patient Disposition by only values in disposition_incident_patient_disposition

# transport incidents

{
  transport_incidents <- ems_data_clean |>
    dplyr::filter(Scene_First_EMS_Unit_On_Scene == "Yes", Year < 2024) |>
    dplyr::count(Year, Patient_Transported) |>
    dplyr::filter(Patient_Transported == T) |>
    dplyr::left_join(
      ems_incidents |>
        dplyr::filter(Year < 2024) |>
        dplyr::select(Year, Incidents),
      by = "Year"
    ) |>
    dplyr::mutate(
      percent_incidents = n / Incidents,
      change_incidents = (n - dplyr::lag(n)) / dplyr::lag(n)
    )
}

# transport runs

{
  transport_runs <- ems_data_clean |>
    dplyr::filter(Year < 2024) |>
    dplyr::distinct(Unique_Run_ID, .keep_all = TRUE) |>
    dplyr::count(Year, Patient_Transported) |>
    dplyr::filter(Patient_Transported == T) |>
    dplyr::left_join(
      ems_runs |> dplyr::filter(Year < 2024) |> dplyr::select(Year, Runs),
      by = "Year"
    ) |>
    dplyr::mutate(
      percent_runs = n / Runs,
      change_runs = (n - dplyr::lag(n)) / dplyr::lag(n)
    )
}

# join the transport runs and incidents table

transport_incidents_runs <- transport_incidents |>
  rename(Transport_Incidents = n) |>
  dplyr::left_join(
    transport_runs |> rename(Transport_Runs = n),
    by = c("Patient_Transported", "Year")
  ) |>
  dplyr::select(-Patient_Transported)

# transports for printing

transport_incidents_runs_recent <- transport_incidents_runs |>
  dplyr::filter(Year %in% 2023:2024)

# Get trauma related ems responses

{
  ems_trauma_incidents <- ems_data_clean |>
    dplyr::filter(
      Trauma_Flag == "Yes",
      Scene_First_EMS_Unit_On_Scene == "Yes",
      Year < 2024
    ) |>
    dplyr::distinct(Unique_Run_ID, .keep_all = TRUE) |>
    dplyr::count(Year, name = "Incidents") |>
    dplyr::mutate(
      change_incident = (Incidents - dplyr::lag(Incidents)) /
        dplyr::lag(Incidents),
      change_incident_label = traumar::pretty_percent(change_incident)
    )
}

# Get n trauma runs

{
  ems_trauma_runs <- ems_data_clean |>
    dplyr::filter(Trauma_Flag == "Yes", Year < 2024) |>
    dplyr::distinct(Unique_Run_ID, .keep_all = TRUE) |>
    dplyr::count(Year, name = "Runs") |>
    dplyr::mutate(
      change_runs = (Runs - dplyr::lag(Runs)) / dplyr::lag(Runs),
      change_runs_label = traumar::pretty_percent(change_runs)
    )
}

# ems trauma join incidents and runs

ems_trauma_incidents_runs <- ems_trauma_incidents |>
  dplyr::left_join(ems_trauma_runs, by = "Year")

# ems trauma stats for printing

ems_trauma_incidents_runs_recent <- ems_trauma_incidents_runs |>
  dplyr::filter(Year %in% 2023:2024)

# Get trauma related ems transport runs

{
  ems_trauma_transport_incidents <- ems_data_clean |>
    dplyr::filter(
      Trauma_Flag == "Yes",
      Scene_First_EMS_Unit_On_Scene == "Yes",
      Year < 2024
    ) |>
    dplyr::distinct(Unique_Run_ID, .keep_all = TRUE) |>
    dplyr::count(Year, Patient_Transported) |>
    dplyr::filter(Patient_Transported == T) |>
    dplyr::left_join(
      ems_trauma_incidents |> dplyr::select(Year, Incidents),
      by = "Year"
    ) |>
    dplyr::mutate(
      percent_trauma_transport_incidents = n / Incidents,
      change_trauma_transport_incidents = (Incidents - dplyr::lag(Incidents)) /
        dplyr::lag(Incidents)
    )
}

# Get trauma related ems transport incidents

{
  ems_trauma_transport_runs <- ems_data_clean |>
    dplyr::filter(Trauma_Flag == "Yes", Year < 2024) |>
    dplyr::distinct(Unique_Run_ID, .keep_all = TRUE) |>
    dplyr::count(Year, Patient_Transported) |>
    dplyr::filter(Patient_Transported == T) |>
    dplyr::left_join(
      ems_trauma_runs |> dplyr::select(Year, Runs),
      by = "Year"
    ) |>
    dplyr::mutate(
      percent_trauma_transport_runs = n / Runs,
      change_trauma_transport_runs = (Runs - dplyr::lag(Runs)) /
        dplyr::lag(Runs)
    )
}

# join the ems trauma transport incident and runs tables

ems_trauma_transport_incidents_runs <- ems_trauma_transport_incidents |>
  rename(Trauma_Transport_Incidents = n) |>
  dplyr::left_join(
    ems_trauma_transport_runs |> rename(Trauma_Transport_Runs = n),
    by = c("Patient_Transported", "Year")
  ) |>
  dplyr::select(-Patient_Transported)

# trauma transports for printing

ems_trauma_transport_incidents_runs_recent <- ems_trauma_transport_incidents_runs |>
  dplyr::filter(Year %in% 2023:2024)

####
# Main report section ----
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

###_____________________________________________________________________________
# Reinjury - Main Report Section ----
###_____________________________________________________________________________

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
      `2018-2024 Trend` = list(c(
        `2018`,
        `2019`,
        `2020`,
        `2021`,
        `2023`,
        `2024`
      )),
      .by = Category
    ) |>
    dplyr::select(Category, `2021`:`2018-2024 Trend`) |>
    gt() |>
    gt_plt_sparkline(
      column = `2018-2024 Trend`,
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
      subtitle = "Patients Seen at a Trauma Center | Data: iowa Trauma Registry 2018-2024"
    ) |>
    tab_style_hhs(border_cols = `2021`:`2018-2024 Trend`)
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
# fit a logistic regression model for explanatory data analysis
###_____________________________________________________________________________

# use this section from the multi_year_injury_ML.R file to conduct the analysis

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

###_____________________________________________________________________________
# Hospital admissions ----
###_____________________________________________________________________________

# longitudinal cases

{
  ipop_longitudinal_cases <- ipop_data_clean |>
    ipop_case_count(Year, which = "Inpatient", descriptive_stats = TRUE) |>
    dplyr::filter(Year >= 2018, Year < 2024) |>
    dplyr::select(-change_label) |>
    rename(`Total Cases` = n, `% Change in Cases` = change) |>
    pivot_longer(
      cols = `Total Cases`:`% Change in Cases`,
      names_to = "Category",
      values_to = "Value"
    ) |>
    pivot_wider(id_cols = Category, names_from = Year, values_from = Value) |>
    dplyr::mutate(
      `2018-2024 Trend` = list(c(
        `2018`,
        `2019`,
        `2020`,
        `2021`,
        `2023`,
        `2024`
      )),
      .by = Category
    ) |>
    dplyr::select(-c(`2018`, `2019`, `2020`))
}

# longitudinal patients

{
  ipop_longitudinal_patients <- ipop_data_clean |>
    ipop_patient_count(Year, which = "Inpatient", descriptive_stats = TRUE) |>
    dplyr::filter(Year >= 2018, Year < 2024) |>
    dplyr::select(-change_label) |>
    rename(`Total Pts.` = n, `% Change in Pts.` = change) |>
    pivot_longer(
      cols = `Total Pts.`:`% Change in Pts.`,
      names_to = "Category",
      values_to = "Value"
    ) |>
    pivot_wider(id_cols = Category, names_from = Year, values_from = Value) |>
    dplyr::mutate(
      `2018-2024 Trend` = list(c(
        `2018`,
        `2019`,
        `2020`,
        `2021`,
        `2023`,
        `2024`
      )),
      .by = Category
    ) |>
    dplyr::select(-c(`2018`, `2019`, `2020`))
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
      column = `2018-2024 Trend`,
      type = "shaded",
      same_limit = FALSE,
      label = TRUE
    ) |>
    tab_header(
      title = "Count and Rate of Change in IPOP Inpatient Injury Hospitalizations",
      subtitle = "Source: Iowa Inpatient Outpatient Database 2018-2024"
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
    tab_style_hhs(border_cols = `2021`:`2018-2024 Trend`)
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

###_____________________________________________________________________________
# Deaths ----
###_____________________________________________________________________________

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
# CDC WISQARS data can be accessed here:
# https://www.cdc.gov/injury/wisqars/animated-leading-causes.html
# utilize this link to download the text file that can subsequently be loaded
# into ChatGPT to create the dataset as below:
# https://www.cdc.gov/injury/wisqars/data/Top-Ten-Leading-Causes-of-Death-in-the-U.S.-for-Ages-1-44.txt
# If that link gets broken, the link can be found below the first chart on the first
# WISQARS URL above
# unintentional injury trends can be accessed at the same animated leading causes link above
# and loaded into ChatGPT, it is the second chart on that page
# For specific tables at the end of the section, those are provided direction from
# Health Statistics via the dashboard that Josh Jungling has in Tableau.
###_____________________________________________________________________________

###
# Health statistics annual report data
###

{
  # compile 2023 data

  death_iowa_state_2023 <- data.frame(
    Year = 2023,
    Cause_of_Death = c(
      "Diseases of the Heart",
      "Malignant Neoplasms",
      "Unintentional Injuries",
      "Chronic Lower Respiratory Diseases",
      "COVID-19",
      "Cerebrovascular Diseases",
      "Alzheimer's Disease",
      "Diabetes Mellitus",
      "All Infective and Parasitic Diseases",
      "Suicides"
    ),
    Deaths = c(7725, 6252, 1815, 1722, 1693, 1399, 1343, 998, 614, 582)
  ) |>
    dplyr::mutate(Percent = Deaths / sum(Deaths))

  # Create a tibble with the provided CDC data for 2024
  death_iowa_state_2024 <- tibble(
    Year = 2024,
    Cause_of_Death = c(
      "Diseases of the Heart",
      "Malignant Neoplasms",
      "Unintentional Injuries",
      "Chronic Lower Respiratory Diseases",
      "Cerebrovascular Diseases",
      "Alzheimer's Disease",
      "Diabetes Mellitus",
      "All Infective and Parasitic Diseases",
      "Essential Hypertension and Hypertensive Renal Disease",
      "COVID-19"
    ),
    Deaths = c(7619, 6389, 1839, 1716, 1382, 1351, 949, 628, 623, 524)
  ) |>
    dplyr::mutate(Percent = Deaths / sum(Deaths))

  # union 2023 and 2024

  death_iowa_state <- bind_rows(death_iowa_state_2023, death_iowa_state_2024)

  # export the file

  write_csv(
    death_iowa_state,
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/iowa_deaths.csv"
  )
}

###
# CDC WONDER Data
# all age groups and deaths
###

# CDC WONDER all ages

{
  # 2018

  death_cdc_wonder_nation_all_2018 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States All Ages 2018.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    dplyr::mutate(Year = 2018)

  # 2019

  death_cdc_wonder_nation_all_2019 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States All Ages 2019.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    dplyr::mutate(Year = 2019)

  # 2020

  death_cdc_wonder_nation_all_2020 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States All Ages 2020.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    dplyr::mutate(Year = 2020)

  # 2021

  death_cdc_wonder_nation_all_2021 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States All Ages 2021.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    dplyr::mutate(Year = 2021)

  # 2023

  death_cdc_wonder_nation_all_2023 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States All Ages 2023.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    dplyr::mutate(Year = 2023)

  # 2024

  death_cdc_wonder_nation_all_2024 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States All Ages 2024.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    dplyr::mutate(Year = 2024)

  # 2018-2024

  death_cdc_wonder_nation_all_2018_2024_aggregate <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States All Ages 2018-2024.txt",
    n_max = 10
  ) |>
    dplyr::select(-Notes) |>
    dplyr::mutate(
      `UCD - 15 Leading Causes of Death` = str_replace_all(
        `UCD - 15 Leading Causes of Death`,
        pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
        ""
      )
    )

  # All US deaths 2018 - 2024

  death_cdc_wonder_nation_all_2018_2024_detail <- bind_rows(
    death_cdc_wonder_nation_all_2018,
    death_cdc_wonder_nation_all_2019,
    death_cdc_wonder_nation_all_2020,
    death_cdc_wonder_nation_all_2021,
    death_cdc_wonder_nation_all_2023,
    death_cdc_wonder_nation_all_2024
  ) |>
    dplyr::mutate(
      `UCD - 15 Leading Causes of Death` = str_replace_all(
        `UCD - 15 Leading Causes of Death`,
        pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
        ""
      )
    )

  # Download top 10 deaths yearly detail file to .csv for future reference

  write_csv(
    death_cdc_wonder_nation_all_2018_2024_detail,
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/death_cdc_wonder_nation_all_2018_2024_detail.csv"
  )

  # Download top 10 deaths aggregate file to .csv for future reference

  write_csv(
    death_cdc_wonder_nation_all_2018_2024_aggregate,
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/death_cdc_wonder_nation_all_2018_2024_aggregate.csv"
  )
}

# CDC WONDER ages 1-44

{
  # 2018

  death_cdc_wonder_nation_1_44_2018 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States Ages 1-44 2018.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    dplyr::mutate(Year = 2018)

  # 2019

  death_cdc_wonder_nation_1_44_2019 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States Ages 1-44 2019.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    dplyr::mutate(Year = 2019)

  # 2020

  death_cdc_wonder_nation_1_44_2020 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States Ages 1-44 2020.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    dplyr::mutate(Year = 2020)

  # 2021

  death_cdc_wonder_nation_1_44_2021 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States Ages 1-44 2021.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    dplyr::mutate(Year = 2021)

  # 2023

  death_cdc_wonder_nation_1_44_2023 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States Ages 1-44 2023.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    dplyr::mutate(Year = 2023)

  # 2024

  death_cdc_wonder_nation_1_44_2024 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States Ages 1-44 2024.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    dplyr::mutate(Year = 2024)

  # 2018-2024

  death_cdc_wonder_nation_1_44_2018_2024_aggregate <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/Top 15 Causes of Death in the United States Ages 1-44 2018-2024.txt",
    n_max = 10
  ) |>
    dplyr::select(-Notes) |>
    dplyr::mutate(
      `UCD - 15 Leading Causes of Death` = str_replace_all(
        `UCD - 15 Leading Causes of Death`,
        pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
        ""
      )
    )

  # All US deaths 2018 - 2024

  death_cdc_wonder_nation_1_44_2018_2024_detail <- bind_rows(
    death_cdc_wonder_nation_1_44_2018,
    death_cdc_wonder_nation_1_44_2019,
    death_cdc_wonder_nation_1_44_2020,
    death_cdc_wonder_nation_1_44_2021,
    death_cdc_wonder_nation_1_44_2023,
    death_cdc_wonder_nation_1_44_2024
  ) |>
    dplyr::mutate(
      `UCD - 15 Leading Causes of Death` = str_replace_all(
        `UCD - 15 Leading Causes of Death`,
        pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
        ""
      )
    )

  # Download top 10 deaths yearly detail file to .csv for future reference

  write_csv(
    death_cdc_wonder_nation_1_44_2018_2024_detail,
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/death_cdc_wonder_nation_1_44_2018_2024_detail.csv"
  )

  # Download top 10 deaths aggregate file to .csv for future reference

  write_csv(
    death_cdc_wonder_nation_1_44_2018_2024_aggregate,
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/death_cdc_wonder_nation_1_44_2018_2024_aggregate.csv"
  )
}

###
# CDC WISQARS Data
# Show how injury remains the #1 cause of death among individuals ages 1-44 per CDC WISQARS
###

{
  # 2018 - 2023

  death_cdc_wisqars_all <- tibble(
    Year = rep(2018:2023, each = 10),
    Cause_of_Death = c(
      "Unintentional Injury",
      "Suicide",
      "Malignant Cancer",
      "Heart Disease",
      "Homicide",
      "Liver Disease",
      "Diabetes",
      "Stroke",
      "Birth Defects",
      "Influenza & Pneumonia",
      "Unintentional Injury",
      "Suicide",
      "Malignant Cancer",
      "Heart Disease",
      "Homicide",
      "Liver Disease",
      "Diabetes",
      "Stroke",
      "Birth Defects",
      "Influenza & Pneumonia",
      "Unintentional Injury",
      "Suicide",
      "Homicide",
      "Heart Disease",
      "Malignant Cancer",
      "COVID-19",
      "Liver Disease",
      "Diabetes",
      "Stroke",
      "Influenza & Pneumonia",
      "Unintentional Injury",
      "Suicide",
      "COVID-19",
      "Homicide",
      "Heart Disease",
      "Malignant Cancer",
      "Liver Disease",
      "Diabetes",
      "Stroke",
      "Birth Defects",
      "Unintentional Injury",
      "Suicide",
      "Homicide",
      "Malignant Neoplasms",
      "Heart Disease",
      "Liver Disease",
      "COVID-19",
      "Diabetes",
      "Stroke",
      "Birth Defects"
    ),
    Deaths = c(
      61977,
      22357,
      16864,
      15282,
      13787,
      4151,
      3401,
      2550,
      1994,
      1857,
      62982,
      22084,
      16720,
      15177,
      14191,
      4565,
      3391,
      2621,
      2017,
      1835,
      80208,
      22431,
      18838,
      17310,
      16708,
      8902,
      6620,
      4445,
      2927,
      2100,
      89729,
      23859,
      23736,
      19864,
      18167,
      17210,
      7729,
      4641,
      3146,
      2153,
      87639,
      23390,
      18628,
      17340,
      17216,
      7349,
      6160,
      4446,
      3055,
      2331
    )
  )

  # download WISQARS data to .csv

  write_csv(death_cdc_wisqars_all, file = "death_cdc_wisqars_all.csv")

  ###
  # Create a tibble with the provided CDC WISQARS data for 2018:2023
  ###

  unintentional_injury_data <- tibble(
    Year = rep(2018:2023, each = 5),
    Cause_of_Death = c(
      "Unintentional Poisoning",
      "Unintentional MV Traffic",
      "Unintentional Drowning",
      "Unintentional Fall",
      "All Other Unintentional",
      "Unintentional Poisoning",
      "Unintentional MV Traffic",
      "Unintentional Drowning",
      "Unintentional Fall",
      "All Other Unintentional",
      "Unintentional Poisoning",
      "Unintentional MV Traffic",
      "Unintentional Drowning",
      "Unintentional Fall",
      "All Other Unintentional",
      "Unintentional Poisoning",
      "Unintentional MV Traffic",
      "Unintentional Drowning",
      "Unintentional Fall",
      "All Other Unintentional",
      "Unintentional Poisoning",
      "Unintentional MV Traffic",
      "Unintentional Drowning",
      "Unintentional Fall",
      "All Other Unintentional"
    ),
    Deaths = c(
      34634,
      19255,
      1986,
      1032,
      5070,
      36064,
      18799,
      1892,
      1096,
      5131,
      49643,
      21780,
      2232,
      1176,
      5377,
      56280,
      24165,
      2442,
      1324,
      5476,
      55441,
      23130,
      2275,
      1256,
      5490
    )
  )

  write_csv(
    unintentional_injury_data,
    file = "death_cdc_wisqars_unintentional_injury.csv"
  )
}


###
# Data for section Trends in Causes of Death
###

{
  # for the Iowa trauma deaths by intentionality plot

  iowa_deaths_intentionality <- read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/P15_data.csv"
  ) |>
    rename(
      Intentionality = `_TR P15`,
      Year = `The Year`,
      Deaths = `# of Deaths`
    )

  # for the Iowa unintentional trauma deaths by cause plot

  iowa_deaths_cause <- read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/P16-1_data.csv"
  ) |>
    rename(Cause = `_TR P16-1`, Year = `The Year`, Deaths = `# of Deaths`)

  # for the Iowa trauma suicides by cause plot

  iowa_suicides_cause <- read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/P16-2_data.csv"
  ) |>
    rename(Cause = `_TR P16-2`, Year = `The Year`, Deaths = `# of Deaths`)

  # for the trends in causes of death table

  iowa_death_trends_cause <- read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/P17_data.csv"
  ) |>
    rename(Cause = `_TR P17`, Year = `The Year`, Deaths = `# of Deaths`)

  # for Iowa unintentional falls trends plot

  iowa_death_unintentional_falls <- read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/P30_data.csv"
  ) |>
    rename(Cause = `_TR P30`, Year = `The Year`, Deaths = `# of Deaths`)

  # for Iowa poisoning death trends plot

  # unintentional poisoning
  iowa_death_unintentional_poisoning <- read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/P33-1_data.csv"
  ) |>
    rename(Cause = `_TR P33-1`, Year = `The Year`, Deaths = `# of Deaths`)

  # suicide poisoning
  iowa_death_suicide_poisoning <- read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2024/data/death/P33-2_data.csv"
  ) |>
    rename(Cause = `_TR P33-2`, Year = `The Year`, Deaths = `# of Deaths`)

  #union the poisoning tables

  iowa_death_poisoning <- bind_rows(
    iowa_death_unintentional_poisoning,
    iowa_death_suicide_poisoning
  )
}

###_____________________________________________________________________________
# Plots for the death data
###_____________________________________________________________________________

# top 10 causes of death in the US

{
  top_10_causes_us_plot <- death_cdc_wonder_nation_all_2018_2024_aggregate |>
    ggplot2::ggplot(ggplot2::aes(
      x = reorder(
        str_wrap(`UCD - 15 Leading Causes of Death`, width = 20),
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
        death_cdc_wonder_nation_all_2018_2024_aggregate$Deaths < 500000,
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
      arrow = arrow(type = "closed")
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "",
      y = "",
      title = "Top 10 Causes of Death Among All Age Groups in the U.S.",
      subtitle = "Source: CDC WONDER | 2018-2024",
      caption = "Note: 2024 data used in this report via CDC WONDER are provisional.",
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
}

# gt() tbl of deaths among 1-44 age population in the U.S.

{
  death_cdc_wisqars_all_1_44_tbl <- death_cdc_wonder_nation_1_44_2018_2024_aggregate |>
    dplyr::select(
      -matches("crude|population|code"),
      `UCD - 15 Leading Causes of Death`,
      Deaths,
      `Age Adjusted Rate`,
      `Age Adjusted Rate Lower 95% Confidence Interval`,
      `Age Adjusted Rate Upper 95% Confidence Interval`
    ) |>
    gt() |>
    tab_header(
      title = "Top 10 Causes of Death Among Persons Ages 1-44 in the U.S.",
      subtitle = "Source: CDC WONDER | 2018-2024"
    ) |>
    tab_source_note(
      source_note = md(paste0(
        fontawesome::fa("magnifying-glass"),
        " 2024 Data included here are provisional."
      ))
    ) |>
    tab_source_note(
      source_note = md(paste0(
        fontawesome::fa("sticky-note"),
        " Injuries remain in the leading causes of death for the years 2018-2024 in the U.S."
      ))
    ) |>
    tab_footnote(
      footnote = "Rate per 100,000 population.",
      locations = cells_column_labels(columns = `Age Adjusted Rate`)
    ) |>
    opt_footnote_marks(marks = "standard") |>
    fmt_number(columns = Deaths, drop_trailing_zeros = TRUE) |>
    gt_duplicate_column(
      column = `Age Adjusted Rate`,
      after = `Age Adjusted Rate`,
      dupe_name = "Rate_Bar"
    ) |>
    gt_plt_bar(column = Rate_Bar, color = "coral") |>
    cols_label(
      `Age Adjusted Rate` = "Age Adjusted Rate (95% CI)",
      Rate_Bar = "",
      `UCD - 15 Leading Causes of Death` = "Cause of Death"
    ) |>
    cols_merge(
      columns = c(
        `Age Adjusted Rate`,
        `Age Adjusted Rate Lower 95% Confidence Interval`,
        `Age Adjusted Rate Upper 95% Confidence Interval`
      ),
      pattern = "{1} ({2}&mdash;{3})"
    ) |>
    tab_style_hhs(border_cols = c(Deaths, `Age Adjusted Rate`))
}

# top 10 causes of death in Iowa

{
  top_10_causes_iowa_plot <- death_iowa_state |>
    dplyr::filter(Year == 2024) |>
    ggplot2::ggplot(ggplot2::aes(
      x = reorder(str_wrap(Cause_of_Death, width = 10), -Deaths),
      y = Deaths,
      fill = Deaths,
      label = traumar::pretty_number(Deaths)
    )) +
    ggplot2::geom_col(width = 0.75, position = position_dodge(width = 1)) +
    ggplot2::geom_text(
      family = "Work Sans",
      size = 8,
      fontface = "bold",
      nudge_y = 200
    ) +
    ggplot2::annotate(
      geom = "segment",
      x = str_wrap("Unintentional Injuries", width = 10),
      xend = str_wrap("Unintentional Injuries", width = 10),
      y = 3500,
      yend = 2300,
      arrow = arrow(type = "closed")
    ) +
    ggplot2::labs(
      x = "",
      y = "",
      title = "Top 10 Causes of Death in Iowa Among All Age Groups",
      subtitle = "Source: Iowa Death Certificate Data | 2024"
    ) +
    ggplot2::scale_fill_paletteer_c(
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
}

# Iowa trauma deaths by intentionality

{
  iowa_deaths_intentionality_plot <- iowa_deaths_intentionality |>
    dplyr::mutate(
      Intentionality = str_remove_all(Intentionality, pattern = "^\\d{2}-"),
      labels = dplyr::if_else(
        Year %in% c(2018, 2024) & Deaths >= 6,
        traumar::pretty_number(Deaths),
        dplyr::if_else(
          Year %in% c(2018, 2024) & Deaths < 6,
          small_count_label(var = Deaths, cutoff = 6, replacement = "*"),
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
      subtitle = "Source: Iowa Death Certificate Data | 2018-2024",
      color = "Intentionality",
      caption = "Note: Order of color legend follows descending order of lines.\n'*' indicates a masked value < 6 to protect confidentiality."
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

  # save the Iowa trauma deaths by intentionality plot

  plot_save_params(
    filename = "iowa_deaths_intentionality_plot.png",
    plot = iowa_deaths_intentionality_plot,
    path = plot_path
  )
}

# unintentional trauma deaths by cause in Iowa plot

{
  iowa_deaths_cause_plot <- iowa_deaths_cause |>
    dplyr::mutate(
      Cause = str_remove_all(Cause, pattern = "^\\d{2}-"),
      labels = dplyr::if_else(
        Year %in% c(2018, 2024) & Deaths >= 6,
        traumar::pretty_number(Deaths),
        dplyr::if_else(
          Year %in% c(2018, 2024) & Deaths < 6,
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
      direction = "both"
    ) +
    ggplot2::labs(
      x = "",
      y = "",
      title = "Iowa Unintentional Trauma Deaths by Cause",
      subtitle = "Source: Iowa Death Certificate Data | 2018-2024",
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
}

# trauma suicides by cause plot

{
  iowa_suicides_cause_plot <- iowa_suicides_cause |>
    dplyr::mutate(
      Cause = str_remove_all(Cause, pattern = "^\\d{2}-"),
      labels = dplyr::if_else(
        Year %in% c(2018, 2024) & Deaths >= 6,
        traumar::pretty_number(Deaths),
        dplyr::if_else(
          Year %in% c(2018, 2024) & Deaths < 6,
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
      subtitle = "Source: Iowa Death Certificate Data | 2018-2024",
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
}

# trends in causes of death with 5-year avg

{
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
      `Five Year Avg.` = mean(`2018`:`2024`, na.rm = TRUE),
      `% Diff. from Five Year Avg.` = (`2024` - `Five Year Avg.`) /
        `Five Year Avg.`,
      `2018-2024 Trend` = list(c(
        `2018`,
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
      subtitle = "Source: Iowa Death Certificate Data | 2018-2024"
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
      footnote = "Five year avg. calculated from counts of each cause of death from 2018 through 2024.",
      locations = cells_column_labels(columns = `Five Year Avg.`)
    ) |>
    opt_footnote_marks(marks = "standard") |>
    gt_plt_dot(
      column = `2024`,
      category_column = Cause,
      palette = "colorblindr::OkabeIto"
    ) |>
    gt_plt_sparkline(
      column = `2018-2024 Trend`,
      type = "shaded",
      same_limit = FALSE,
      label = TRUE,
      fig_dim = c(8, 30)
    ) |>
    tab_style_hhs(border_cols = `2024`:`2018-2024 Trend`)
}

# trends in unintentional and suicide poisonings

{
  iowa_death_poisoning_plot <- iowa_death_poisoning |>
    dplyr::arrange(Cause, Year) |>
    dplyr::mutate(
      labels = dplyr::if_else(
        Year %in% c(2018, 2024),
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
      x = 2018,
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
      subtitle = "Source: Iowa Death Certificate Data | 2018-2024",
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
}

###_____________________________________________________________________________
# Performance Indicators Section ----
###_____________________________________________________________________________

# calculate indicators for trauma verification levels using a custom function
level_indicators <- trauma_2024 |>
  seqic_level(
    toggle = TRUE,
    pivot = TRUE,
    bind_goals = TRUE,
    include_state = TRUE
  )

# get a tidy table for the verification level indicators using the gt package
level_indicators_gt <- level_indicators |>
  seqic_gt(type = "level", target_name = "State") |>
  sub_missing() |>
  cols_hide(columns = Year) |>
  tab_style_hhs(border_cols = 3:8) |>
  tab_style(
    locations = cells_body(columns = 2),
    cell_text(weight = "bold", align = "left")
  )

# calculate indicators for Emergency Preparedness regions using a custom function
region_indicators <- trauma_2024 |>
  seqic_region(
    toggle = TRUE,
    pivot = TRUE,
    bind_goals = TRUE,
    include_state = TRUE,
    region_randomize = TRUE
  )

# get a tidy table for the region indicators using the gt package
region_indicators_gt <- region_indicators |>
  seqic_gt(type = "region", target_name = "State") |>
  sub_missing() |>
  cols_hide(columns = Year) |>
  tab_style_hhs(border_cols = 3:14) |>
  tab_style(
    locations = cells_body(columns = 2),
    cell_text(weight = "bold", align = "left")
  )

####
### End ----
####
