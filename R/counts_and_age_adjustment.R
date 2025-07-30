###_____________________________________________________________________________
# Estimate age adjusted rates of injury prevalence resulting in inpatient hospitalization ----
# based on the trauma registry
###_____________________________________________________________________________

# total trauma cases
trauma_cases_years <- trauma_data_clean |>
  injury_case_count(Year, descriptive_stats = TRUE)

# check overall trauma case counts with injury location of Iowa

trauma_years_iowa <- trauma_data_clean |>
  dplyr::filter(
    grepl(pattern = "^ia$|iowa", x = Injury_State, ignore.case = T),
    !is.na(Injury_County),
    !Injury_County %in%
      c(
        "Not Applicable",
        "Not Known",
        "Not Known / Not Recorded",
        "Not Known/Not Recorded",
        "Rock Island"
      )
  ) |>
  injury_case_count(Year) # close to the annual trauma report, but is a different file if not filtering out non-Iowa incidents

# get injury counts and case counts by year, county, and age group

{
  # injuries
  injury_counts <- trauma_data_clean |>
    dplyr::filter(
      grepl(pattern = "^ia$|iowa", x = Injury_State, ignore.case = T),
      !is.na(Injury_County),
      !Injury_County %in%
        c(
          "Not Applicable",
          "Not Known",
          "Not Known / Not Recorded",
          "Not Known/Not Recorded",
          "Rock Island"
        )
    ) |>
    # it seems that registrars may enter incident_state as the state the patient is from
    # observed Iowa counties paired with other states, which is improbable that it was meant as the county of another state
    injury_incident_count(Year, Injury_County, Age_Group) |> # as the states in this dataset seem to have a very, very good spelling match with Iowa county names, so assumption is county and zip code are better sources
    tidyr::complete(Year, Injury_County, Age_Group, fill = list(n = 0L)) |>
    dplyr::arrange(Year, Injury_County, Age_Group) |> # returns 99 counties, and more accurate counts
    dplyr::left_join(
      age_group_pops_final,
      by = c("Injury_County" = "County", "Age_Group", "Year")
    ) |>
    dplyr::rename(Count = n) |>
    dplyr::filter(Age_Group != "Missing") |>
    dplyr::left_join(us_age_pops_clean, by = "Age_Group") |>
    dplyr::mutate(Rate_Type = "Injury_Count")

  # cases
  # often mentioned as 'incidents'
  case_counts <- trauma_data_clean |>
    injury_case_count(Year, County, Age_Group) |>
    tidyr::complete(Year, County, Age_Group, fill = list(n = 0L)) |>
    dplyr::arrange(Year, County, Age_Group) |> # returns 99 counties, and more accurate counts
    dplyr::left_join(
      age_group_pops_final,
      by = c("County", "Age_Group", "Year")
    ) |>
    dplyr::rename(Count = n) |>
    dplyr::filter(Age_Group != "Missing") |>
    dplyr::left_join(us_age_pops_clean, by = "Age_Group") |>
    dplyr::mutate(Rate_Type = "Case_Count")
}

# get injury counts and case counts by year and age group

{
  # injury counts by age group
  iowa_injury_counts_age <- trauma_data_clean |>
    dplyr::filter(
      grepl(pattern = "^ia$|iowa", x = Injury_State, ignore.case = T),
      !is.na(Injury_County),
      !Injury_County %in%
        c(
          "Not Applicable",
          "Not Known",
          "Not Known / Not Recorded",
          "Not Known/Not Recorded",
          "Rock Island"
        )
    ) |>
    # it seems that registrars may enter incident_state as the state the patient is from
    # observed Iowa counties paired with other states, which is improbable that it was meant as the county of another state
    # as the states in this dataset seem to have a very, very good spelling match with Iowa county names, so assumption is county and zip code are better sources
    injury_incident_count(Year, Age_Group) |>
    tidyr::complete(Year, Age_Group, fill = list(n = 0L)) |>
    dplyr::arrange(Year, Age_Group) |> # returns 99 counties, and more accurate counts
    dplyr::filter(Age_Group != "Missing") |>
    dplyr::left_join(state_age_group_pops, by = c("Age_Group", "Year")) |>
    dplyr::left_join(
      us_age_pops_clean,
      by = "Age_Group"
    ) |>
    dplyr::rename(Count = n)

  # get case counts

  iowa_case_counts_age <- trauma_data_clean |>
    injury_case_count(Year, Age_Group) |>
    tidyr::complete(Year, Age_Group, fill = list(n = 0L)) |>
    dplyr::arrange(Year, Age_Group) |> # returns 99 counties, and more accurate counts
    dplyr::filter(Age_Group != "Missing") |>
    dplyr::left_join(state_age_group_pops, by = c("Age_Group", "Year")) |>
    dplyr::left_join(
      us_age_pops_clean,
      by = "Age_Group"
    ) |>
    dplyr::rename(Count = n)
}

# check for missingness
# if this check produces many missing age_groups
# go back to the trauma_data_final file and
# review the DOB / Incident dates to see what is driving
# the missingness
trauma_counts_na <- injury_counts |>
  dplyr::filter(dplyr::if_any(tidyselect::everything(), ~ is.na(.)))

# check the trauma_data_final file for missing DOB / Incident_Date
trauma_data_na <- trauma_data_clean |>
  dplyr::filter(dplyr::if_any(c(Patient_DOB, Incident_Date), ~ is.na(.)))

# run this to get a big picture of where the NAs are at and if it seems to be truly unusual before
trauma_data_clean |>
  dplyr::distinct(Unique_Incident_ID, .keep_all = T) |>
  dplyr::count(Year, Age_Group) |>
  dplyr::filter(Age_Group == "Missing") |>
  print(n = Inf)

###_____________________________________________________________________________
# Trauma data age adjustments ----
###_____________________________________________________________________________

# rates summarized by year and county

{
  # injuries
  injury_rates <- injury_counts |>
    calc_age_adjusted_rate(
      count = Count,
      local_population = County_Age_Population,
      standard_population_weight = Weight,
      .by = c("Year", "Injury_County", "Rate_Type")
    ) |>
    dplyr::mutate(
      pretty_label = ifelse(
        Year %in% c(2020, 2023, 2024),
        traumar::pretty_number(
          x = Age_Adjusted_Rate,
          n_decimal = 2
        ),
        ""
      ),
      .by = Injury_County
    ) |>
    dplyr::left_join(location_data, by = c("Injury_County" = "County"))

  # cases / incidents
  case_rates <- case_counts |>
    calc_age_adjusted_rate(
      count = Count,
      local_population = County_Age_Population,
      standard_population_weight = Weight,
      .by = c("Year", "County", "Rate_Type")
    ) |>
    dplyr::mutate(
      pretty_label = ifelse(
        Year %in% c(2020, 2023, 2024),
        traumar::pretty_number(
          x = Age_Adjusted_Rate,
          n_decimal = 2
        ),
        ""
      ),
      .by = County
    ) |>
    dplyr::left_join(location_data, by = "County")

  # union the by year and county age adjusted rates
  injury_case_rates <- dplyr::bind_rows(
    injury_rates |> dplyr::rename(County = Injury_County),
    case_rates
  )
}

###_____________________________________________________________________________
# Plot the by county, by year rates ----
###_____________________________________________________________________________

# injuries by county and year - urban

{
  injury_rates_plot_urban <- injury_rates |>
    dplyr::filter(Designation == "Urban") |>
    ggplot2::ggplot(ggplot2::aes(
      x = Year,
      y = Age_Adjusted_Rate,
      color = "gold",
      label = pretty_label
    )) +
    ggplot2::geom_line(
      linewidth = 1.5,
      alpha = 0.4,
      lineend = "round",
      linejoin = "round"
    ) +
    # ggrepel::geom_text_repel(
    #   angle = 90,
    #   color = "black",
    #   nudge_y = 200,
    #   family = "Work Sans",
    #   direction = "y",
    #   segment.color = "transparent"
    # ) +
    ggplot2::facet_wrap(~Injury_County, scales = "fixed") +
    traumar::theme_cleaner(
      title_text_size = 20,
      subtitle_text_size = 18,
      base_size = 15,
      axis.text.x = ggplot2::element_text(angle = 90),
      vjust_title = 2.5,
      vjust_subtitle = 1.5,
      facets = TRUE
    ) +
    ggplot2::labs(
      x = "Year",
      y = "",
      title = "Urban Age Adjusted Injury* Rates by County and Year",
      subtitle = "Bureau of Emergency Medical and Trauma Services | Division of Public Health | Iowa HHS",
      caption = "- Data: Iowa Trauma Registry via ImageTrend\n- All rates are age adjusted per 100,000 population using US 2000 standard pops\n- *Injury incidence rate refers to the incidence of injury events resulting in evaluation/treatment at a verified trauma center based on the injury county"
    ) +
    ggplot2::guides(color = "none") +
    ggplot2::scale_x_continuous(labels = 2020:2024) +
    ggplot2::scale_color_viridis_d(option = "viridis")

  # save the plot
  ggplot2::ggsave(
    filename = "injury_rates_plot_urban.png",
    path = plot_folder,
    plot = injury_rates_plot_urban,
    height = 9,
    width = 9 * (16 / 9),
    units = "in"
  )
}

# injuries by county and year - rural

{
  injury_rates_plot_rural <- injury_rates |>
    dplyr::filter(Designation == "Rural") |>
    ggplot(aes(
      x = Year,
      y = Age_Adj_Injury_Rate,
      color = "darkred",
      label = pretty_label
    )) +
    geom_line(
      linewidth = 1.5,
      alpha = 0.4,
      lineend = "round",
      linejoin = "round"
    ) +
    geom_text_repel(
      angle = 45,
      color = "black",
      nudge_y = injury_rates$Age_Adj_Injury_Rate,
      family = "Work Sans"
    ) +
    facet_wrap(~Injury_County) +
    theme_cleaner_facet(
      title_text_size = 20,
      subtitle_text_size = 18,
      base_size = 15,
      axis.text.x = element_text(angle = 90),
      axis.text.y = element_blank(),
      vjust_title = 2.5,
      vjust_subtitle = 1.5
    ) +
    labs(
      x = "Year",
      y = "",
      title = "Rural Age Adjusted Injury* Rates by County and Year",
      subtitle = "Bureau of Emergency Medical and Trauma Services | Division of Public Health | Iowa HHS",
      caption = "- Data: Iowa Trauma Registry via ImageTrend\n- All rates are age adjusted per 100,000 population using US 2000 standard pops\n- Injury incidence rate refers to the incidence of injury events resulting in evaluation/treatment at a verified trauma center based on the injury county"
    ) +
    guides(color = "none") +
    scale_x_continuous(labels = 2018:2023) +
    scale_color_viridis_d(option = "viridis")

  plot_save_params(
    filename = "injury_rates_plot_rural.png",
    plot = injury_rates_plot_rural,
    path = plot_path
  )
}

# cases by county and year - urban

{
  case_rates_plot_urban <- case_rates |>
    dplyr::filter(Designation == "Urban") |>
    ggplot(aes(
      x = Year,
      y = Age_Adj_Trauma_case_Rate,
      color = "gold",
      label = pretty_label
    )) +
    geom_line(
      linewidth = 1.5,
      alpha = 0.4,
      lineend = "round",
      linejoin = "round"
    ) +
    geom_text_repel(
      angle = 45,
      color = "black",
      nudge_y = case_rates$Age_Adj_Trauma_case_Rate * 0.5,
      family = "Work Sans"
    ) +
    facet_wrap(~County) +
    theme_cleaner_facet(
      title_text_size = 20,
      subtitle_text_size = 18,
      base_size = 15,
      axis.text.x = element_text(angle = 90),
      axis.text.y = element_blank(),
      vjust_title = 2.5,
      vjust_subtitle = 1.5
    ) +
    labs(
      x = "Year",
      y = "",
      title = "Urban Age Adjusted Trauma Case* Rates by County and Year",
      subtitle = "Bureau of Emergency Medical and Trauma Services | Division of Public Health | Iowa HHS",
      caption = "- Data: Iowa Trauma Registry via ImageTrend\n- All rates are age adjusted per 100,000 population using US 2000 standard pops\n- *Trauma case rate refers to the number of times patients in the registry were evaluated/treated at a verified trauma center based on the hospital county"
    ) +
    guides(color = "none") +
    scale_x_continuous(labels = 2018:2023) +
    scale_color_viridis_d(option = "viridis")

  plot_save_params(
    filename = "case_rates_plot_urban.png",
    plot = case_rates_plot_urban,
    path = plot_path
  )
}

# cases by county and year - rural

{
  case_rates_plot_rural <- case_rates |>
    dplyr::filter(Designation == "Rural") |>
    ggplot(aes(
      x = Year,
      y = Age_Adj_Trauma_case_Rate,
      color = "darkred",
      label = pretty_label
    )) +
    geom_line(
      linewidth = 1.5,
      alpha = 0.4,
      lineend = "round",
      linejoin = "round"
    ) +
    geom_text_repel(
      angle = 45,
      color = "black",
      nudge_y = case_rates$Age_Adj_Trauma_case_Rate * 0.5,
      family = "Work Sans"
    ) +
    facet_wrap(~County) +
    theme_cleaner_facet(
      title_text_size = 20,
      subtitle_text_size = 18,
      base_size = 15,
      axis.text.x = element_text(angle = 90),
      axis.text.y = element_blank(),
      vjust_title = 2.5,
      vjust_subtitle = 1.5
    ) +
    labs(
      x = "Year",
      y = "",
      title = "Rural Age Adjusted Trauma Case* Rates by County and Year",
      subtitle = "Bureau of Emergency Medical and Trauma Services | Division of Public Health | Iowa HHS",
      caption = "- Data: Iowa Trauma Registry via ImageTrend\n- All rates are age adjusted per 100,000 population using US 2000 standard pops\n- *Trauma case rate refers to the number of times patients in the registry were evaluated/treated at a verified trauma center based on the hospital county"
    ) +
    guides(color = "none") +
    scale_x_continuous(labels = 2018:2023) +
    scale_color_viridis_d(option = "viridis")

  plot_save_params(
    filename = "case_rates_plot_rural.png",
    plot = case_rates_plot_rural,
    path = plot_path
  )
}

###_____________________________________________________________________________
# rates at the state level ----
###_____________________________________________________________________________

{
  # injuries
  iowa_injury_rate <- iowa_injury_counts_age |>
    summarize(
      Count = sum(Count, na.rm = T),
      Crude_Rate = round(
        (sum(Count, na.rm = T) / sum(Population_state, na.rm = T)) * 100000,
        digits = 1
      ),
      Age_Adj_Injury_Rate = round(
        sum(Adjusted_Rate_Component, na.rm = TRUE) * 100000,
        digits = 1
      ),
      Population = sum(Population_state, na.rm = TRUE),
      .by = Year
    ) |>
    pivot_longer(
      cols = Crude_Rate:Age_Adj_Injury_Rate,
      names_to = "Rate_Type",
      values_to = "Rate"
    ) |>
    mutate(Rate_Category = "Injury_Count", .before = Rate_Type)

  # cases / incidents
  iowa_case_rate <- iowa_case_counts_age |>
    summarize(
      Count = sum(Count, na.rm = T),
      Crude_Rate = round(
        (sum(Count, na.rm = T) / sum(Population_state, na.rm = T)) * 100000,
        digits = 1
      ),
      Age_Adj_Trauma_case_Rate = round(
        sum(Adjusted_Rate_Component, na.rm = TRUE) * 100000,
        digits = 1
      ),
      Population = sum(Population_state, na.rm = TRUE),
      .by = Year
    ) |>
    pivot_longer(
      cols = Crude_Rate:Age_Adj_Trauma_case_Rate,
      names_to = "Rate_Type",
      values_to = "Rate"
    ) |>
    mutate(Rate_Category = "case_Count", .before = Rate_Type)

  # union the state rates

  iowa_injury_case_rates <- bind_rows(iowa_injury_rate, iowa_case_rate)
}

# state-level plots

{
  # injury hospitalization rate
  iowa_injury_rate_plot <- iowa_injury_rate |>
    mutate(
      Rate_Type = factor(
        Rate_Type,
        levels = c("Age_Adj_Injury_Rate", "Crude_Rate"),
        labels = c("Age Adj Injury Rate", "Crude Rate")
      ),
      Label = if_else(
        Year %in% c(2018, 2022, 2023),
        pretty_number(Rate, n_decimal = 2),
        ""
      )
    ) |>
    ggplot(aes(Year, Rate, color = Rate_Type, label = Label)) +
    geom_line(linewidth = 2, lineend = "round", linejoin = "round") +
    theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.5
    ) +
    geom_text(
      family = "Work Sans",
      size = 8,
      color = "black",
      nudge_y = c(35, -15, rep(20, 10))
    ) +
    labs(
      x = "",
      y = "",
      title = "State-level Age Adjusted Injury* Rate by Year: Iowa",
      caption = "- *Injury incidence rate refers to the incidence of injury events resulting in evaluation/treatment at a verified trauma center based\n    on the injury county\n- Data: Iowa Trauma Registry via ImageTrend | Age adjusted rates are per 100,000 population using US 2000 standard pops\n- Bureau of Emergency Medical and Trauma Services | Division of Public Health | Iowa HHS\n- Scale does not reach zero to highlight the trend.",
      color = "Rate Type: "
    ) +
    scale_color_viridis_d(option = "cividis")

  plot_save_params(
    filename = "iowa_injury_rate_plot.png",
    plot = iowa_injury_rate_plot,
    path = plot_path
  )
}

# injury case rate overall

{
  iowa_case_rate_plot <- iowa_case_rate |>
    mutate(
      Rate_Type = factor(
        Rate_Type,
        levels = c("Age_Adj_Trauma_case_Rate", "Crude_Rate"),
        labels = c("Age Adj Trauma case Rate", "Crude Rate")
      ),
      Label = if_else(
        Year %in% c(2018, 2022, 2023),
        pretty_number(Rate, n_decimal = 2),
        ""
      )
    ) |>
    ggplot(aes(Year, Rate, color = Rate_Type, label = Label)) +
    geom_line(linewidth = 2, lineend = "round", linejoin = "round") +
    theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.5
    ) +
    geom_text(
      family = "Work Sans",
      size = 8,
      color = "black",
      nudge_y = c(-10, -10, rep(20, 10))
    ) +
    labs(
      x = "",
      y = "",
      title = "State-level Age Adjusted Trauma Case* Rate by Year: Iowa",
      caption = "- Data: Iowa Trauma Registry via ImageTrend | Age adjusted rates are per 100,000 population using US 2000 standard pops\n- *Trauma case rate refers to the number of times patients in the registry were evaluated/treated at a verified trauma center\n    based on the hospital county\n- Bureau of Emergency Medical and Trauma Services | Division of Public Health | Iowa HHS\n- Scale does not reach zero to highlight the trend.",
      color = "Rate Type:  "
    ) +
    scale_color_viridis_d(option = "cividis")

  plot_save_params(
    filename = "iowa_case_rate_plot.png",
    plot = iowa_case_rate_plot,
    path = plot_path
  )
}
