###_____________________________________________________________________________
### Custom functions and other utilities for the Annual Trauma Report 2024 ----
### For any analyses, these data must be loaded for the applicable section
### This script should be reviewed and ran first before going to any other
### scripts in the project
###_____________________________________________________________________________

# Utilize the air package for code formatting

### packages ----

# these packages are utilized in this project and must be loaded
# install.packages(c(
#   'renv',
#   'usethis',
#   'devtools',
#   'tidyverse',
#   'tidymodels',
#   'traumar',
#   'nemsqar',
#   'naniar',
#   'cli'
# ))

###_____________________________________________________________________________
# Get palettes ----
###_____________________________________________________________________________

# easy to access list of qualitative palettes
good_palettes <- paletteer::palettes_d_names |>
  dplyr::filter(grepl(pattern = "blind", x = package, ignore.case = T)) |>
  dplyr::arrange(package, desc(length))

# palettes for continuous data
quant_palettes <- paletteer::palettes_c_names

###_____________________________________________________________________________
# Custom functions for counts ----
###_____________________________________________________________________________

{
  # custom function to get unique count of injury events in IPOP

  ipop_injury_count <- function(
    df,
    ...,
    which = c("Inpatient", "Outpatient", "IPOP"),
    descriptive_stats = F
  ) {
    # where ... will always be one bare column name.  The function will fail with more than one grouping at this time

    # get inpatient counts
    if (which == "Inpatient") {
      # filter down to Inpatient and then unique injury counts by only looking at unique
      # combinations of patient and date of service

      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "Inpatient") |>
        dplyr::distinct(Patient_Linking, Date_of_Service, .keep_all = TRUE) |>
        dplyr::count(...)

      cli::cli_alert_success(
        "Returning the count(s) of unique injury events that led to inpatient hospitalization."
      )
    } else if (which == "Outpatient") {
      # get oupatient counts
      # filter down to Inpatient and then unique injury counts by only looking at unique
      # combinations of patient and date of service

      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "Outpatient") |>
        dplyr::distinct(Patient_Linking, Date_of_Service, .keep_all = TRUE) |>
        dplyr::count(...)

      cli::cli_alert_success(
        "Returning the count(s) of unique injury events that led to outpatient visits."
      )
    } else if (which == "IPOP") {
      # get oupatient and inpatient counts
      # filter down to Inpatient and then unique injury counts by only looking at unique
      # combinations of patient and date of service

      temp <- df |>
        dplyr::distinct(Patient_Linking, Date_of_Service, .keep_all = TRUE) |>
        dplyr::count(...)

      cli::cli_alert_success(
        "Returning the count(s) of unique injury events that led to inpatient hospitalization and outpatient visits."
      )
    }

    if (descriptive_stats == FALSE) {
      return(temp)
    } else {
      out <- temp |>
        dplyr::mutate(
          change = (n - dplyr::lag(n)) / dplyr::lag(n),
          change_label = traumar::pretty_percent(change, n_decimal = 2)
        )

      return(out)
    }
  }

  # custom function to get unique count of cases in IPOP

  ipop_case_count <- function(
    df,
    ...,
    which = c("Inpatient", "Outpatient", "IPOP"),
    descriptive_stats = F
  ) {
    # where ... will always be one bare column name.  The function will fail with more than one grouping at this time

    # get inpatient counts

    if (which == "Inpatient") {
      # filter down to Inpatient and then unique case counts by only looking at unique
      # combinations of Patient_Linking, Record_ID, Hospital_Number, Date_of_Service

      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "Inpatient") |>
        dplyr::distinct(
          Patient_Linking,
          Hospital_Number,
          Date_of_Service,
          .keep_all = TRUE
        ) |>
        dplyr::count(...)

      cli::cli_alert_success(
        "Returning the count(s) of total unique inpatient injury cases."
      )
    } else if (which == "Outpatient") {
      # get outpatient case record counts
      # filter down to Inpatient and then unique case counts by only looking at unique
      # combinations of Patient_Linking, Record_ID, Hospital_Number, Date_of_Service

      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "Outpatient") |>
        dplyr::distinct(
          Patient_Linking,
          Hospital_Number,
          Date_of_Service,
          .keep_all = TRUE
        ) |>
        dplyr::count(...)

      cli::cli_alert_success(
        "Returning the count(s) of total unique injury outpatient visits."
      )
    } else if (which == "IPOP") {
      # get both outpatient and inpatient case record counts
      # filter down to Inpatient and then unique case counts by only looking at unique
      # combinations of Patient_Linking, Record_ID, Hospital_Number, Date_of_Service

      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "IPOP") |>
        dplyr::distinct(
          Patient_Linking,
          Hospital_Number,
          Date_of_Service,
          .keep_all = TRUE
        ) |>
        dplyr::count(...)

      cli::cli_alert_success(
        "Returning the count(s) of total unique injury inpatient cases and injury outpatient visits."
      )
    }

    if (descriptive_stats == FALSE) {
      return(temp)
    } else {
      out <- temp |>
        dplyr::mutate(
          change = (n - dplyr::lag(n)) / dplyr::lag(n),
          change_label = traumar::pretty_percent(change, n_decimal = 2)
        )

      return(out)
    }
  }

  # custom function to get unique count of cases in IPOP
  # this function runs by grouping first using group_by() and then running the dplyr::distinct() function
  # running the count grouped by the column supplied, usually Year

  ipop_patient_count <- function(
    df,
    ...,
    which = c("Inpatient", "Outpatient", "IPOP"),
    descriptive_stats = F
  ) {
    # where ... will always be one bare column name.  The function will fail with more than one grouping at this time

    # get inpatient counts

    if (which == "Inpatient") {
      # filter down to Inpatient and then unique patient counts by only looking at unique
      # instances of Patient_Linking

      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "Inpatient") |>
        dplyr::group_by(Year) |>
        dplyr::distinct(Patient_Linking, .keep_all = TRUE) |>
        ungroup() |>
        dplyr::count(...)

      cli::cli_alert_success(
        "Returning the count(s) of total unique injured patients who had an inpatient case event."
      )
    } else if (which == "Outpatient") {
      # get outpatient counts
      # filter down to Inpatient and then unique patient counts by only looking at unique
      # instances of Patient_Linking

      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "Outpatient") |>
        dplyr::group_by(Year) |>
        dplyr::distinct(Patient_Linking, .keep_all = TRUE) |>
        ungroup() |>
        dplyr::count(...)

      cli::cli_alert_success(
        "Returning the count(s) of total unique injured patients who had an outpatient visit."
      )
    } else if (which == "IPOP") {
      # get both outpatient and inpatient counts
      # filter down to Inpatient and then unique patient counts by only looking at unique
      # instances of Patient_Linking

      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "IPOP") |>
        dplyr::group_by(Year) |>
        dplyr::distinct(Patient_Linking, .keep_all = TRUE) |>
        ungroup() |>
        dplyr::count(...)

      cli::cli_alert_success(
        "Returning the count(s) of total unique injured patients who had an inpatient case event or outpatient visit."
      )
    }

    if (descriptive_stats == FALSE) {
      return(temp)
    } else {
      out <- temp |>
        dplyr::mutate(
          change = (n - dplyr::lag(n)) / dplyr::lag(n),
          change_label = traumar::pretty_percent(change, n_decimal = 2)
        )

      return(out)
    }
  }

  # custom function to get unique count of cases from Patient Registry
  # synonymous with "incident count" from the past

  injury_case_count <- function(df, ..., descriptive_stats = F) {
    # set up a temp file so dplyr::distinct() does not have to be ran more than once
    # this function will get the distinct rows based on Unique Incident ID,
    # which is equal to # of inpatient visits overall
    # in all cases the function will return # cases per group, depending
    # on the grouping variable provided by the user

    temp <- df |>
      dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE)

    if (descriptive_stats == F) {
      # provide only the counts, user can define grouping based on dynamic dots

      out <- temp |>
        dplyr::count(...)

      cli::cli_alert_success(
        "Returning the count(s) of total unique inpatient injury cases."
      )

      return(out)
    } else if (descriptive_stats == T) {
      # provide descriptive statistics about % change and # change
      # count based on user input, user can define grouping based on dynamic dots

      out <- temp |>
        dplyr::count(...) |>
        dplyr::mutate(
          change = n - dplyr::lag(n),
          prop_change = (n - dplyr::lag(n)) / dplyr::lag(n),
          prop_label = traumar::pretty_percent(
            prop_change,
            n_decimal = 2
          )
        )

      cli::cli_alert_success(
        "Returning the count(s) of total unique inpatient injury cases and descriptive statistics."
      )

      return(out)
    }
  }

  # custom function to get unique count of injuries in Patient Registry
  # a true estimation of the number of incidents

  injury_incident_count <- function(df, ..., descriptive_stats = F) {
    # set up a temp file so dplyr::distinct() does not have to be ran more than once
    # this function will get the distinct rows based on unique combination of
    # Unique Patient ID and Incident Dates, which gets down to # of unique injury
    # events.
    # in all cases the function will return # injury events that led to a trauma
    # center visit per group, depending on the grouping variable provided by the user

    temp <- df |>
      dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE)

    if (descriptive_stats == F) {
      out <- temp |>
        dplyr::count(...)

      cli::cli_alert_success(
        "Returning the count(s) of total unique injury events leading to a trauma center visit."
      )

      return(out)
    } else if (descriptive_stats == T) {
      # provide descriptive statistics along with injury event counts

      stat <- temp |>
        dplyr::filter(!is.na(Unique_Patient_ID)) |>
        dplyr::count(Year, Unique_Patient_ID) |>
        dplyr::filter(n > 1) |>
        dplyr::summarize(
          Min_Reinjury = min(n, na.rm = T),
          Max_Reinjury = max(n, na.rm = T),
          Avg_Injuries = mean(n, na.rm = T),
          .by = Year
        )

      out <- temp |>
        dplyr::count(Year) |>
        dplyr::mutate(
          change = n - dplyr::lag(n),
          prop_change = (n - dplyr::lag(n)) / dplyr::lag(n),
          prop_label = traumar::pretty_percent(
            prop_change,
            n_decimal = 2
          )
        ) |>
        dplyr::left_join(stat, by = "Year")

      cli::cli_alert_success(
        "Returning the count(s) of total unique injury events leading to a trauma center visit and descriptive statistics."
      )

      return(out)
    }
  }

  # custom function to get unique count of patients in Patient Registry
  # a true estimation of the number of patients
  # given that patient unique identifiers are the same across all years (for the most part)
  # this function runs by grouping first using group_by() and then running the dplyr::distinct() function
  # running the count grouped by the column supplied, usually Year

  injury_patient_count <- function(df, ..., descriptive_stats = F) {
    # set up a temp file so dplyr::distinct() does not have to be ran more than once
    # this function will get the distinct rows based on Unique Patient ID,
    # which gets down to # of unique patients who were served at a trauma center
    # in all cases the function will return # patients that had a trauma
    # center visit per group, depending on the grouping variable provided by the user
    # important that using this method that the group_by(Year) is ran before dplyr::distinct(),
    # or else you will get the distinct patients in the whole table

    temp <- df |>
      dplyr::filter(!is.na(Unique_Patient_ID)) |>
      dplyr::group_by(Year) |>
      dplyr::distinct(Unique_Patient_ID, .keep_all = TRUE) |>
      ungroup()

    if (descriptive_stats == F) {
      out <- temp |>
        dplyr::count(...)

      cli::cli_alert_success(
        "Returning the count(s) of total unique patients who had a trauma center visit and have a non-missing unique patient ID."
      )

      return(out)
    } else if (descriptive_stats == T) {
      # get descriptive statistics related to # change and % change in patient counts
      # in addition to unique patient counts per year

      out <- temp |>
        dplyr::count(...) |>
        dplyr::mutate(
          change = n - dplyr::lag(n),
          prop_change = (n - dplyr::lag(n)) / dplyr::lag(n),
          prop_label = traumar::pretty_percent(
            prop_change,
            n_decimal = 2
          )
        )

      cli::cli_alert_success(
        "Returning the count(s) of total unique patients who had a trauma center visit and have a non-missing unique patient ID and descriptive statistics."
      )

      return(out)
    }
  }

  # custom function to get unique count of reinjured patients in Patient Registry
  # reinjured is the same as n_injury > 1 per year per patient
  # a true estimation of the number of incidents

  reinjury_patient_count <- function(df, descriptive_stats = F) {
    # get unique count of patients that had more than 1 injury in any given year
    # table is made distinct by Unique Patient ID and Incident date to get unique
    # injury events and see patients that had more than 1 injury in the year
    # then, count of unique patients each year is taken.  1 unique patient will only
    # be counted once in a year, but can be counted again in a different year if
    # they were injured again
    # Null unique patient IDs are removed as they could represent dozens of
    # patients that were missing some data related to creating the unique ID

    temp <- df |>
      dplyr::filter(!is.na(Unique_Patient_ID)) |> # remove null patient IDs to get accurate stats
      dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE) |>
      dplyr::count(Year, Unique_Patient_ID) |>
      dplyr::mutate(reinjury = n > 1)

    # calculate the descriptive statistics just once

    summary <- temp |>
      dplyr::filter(n > 1) |>
      dplyr::summarize(
        Min_Reinjury = min(n, na.rm = T),
        Max_Reinjury = max(n, na.rm = T),
        Avg_Injuries = mean(n, na.rm = T),
        .by = Year
      )

    if (descriptive_stats == F) {
      # count cases where reinjury == TRUE which gives count of unique patients
      # this works because the table at this point is unique patients each year
      # and their corresponding # of injury events in that year

      out <- temp |>
        dplyr::summarize(Reinjury = sum(reinjury == T, na.rm = T), .by = Year)

      cli::cli_alert_success(
        "Returning count(s) of injured patients that had more than 1 injury event in a given year, and have a non-null unique identifier."
      )

      return(out)
    } else if (descriptive_stats == T) {
      stat <- temp |>
        dplyr::summarize(Reinjury = sum(reinjury == T, na.rm = T), .by = Year)

      counts <- df |>
        injury_patient_dplyr::count(Year) # get total patients for the years, including patients that were or were not reinjured

      out <- stat |>
        dplyr::left_join(counts, by = "Year") |> # join overall patient counts to the table of reinjured counts
        dplyr::left_join(summary, by = "Year") |> # join descriptive statistics
        dplyr::mutate(
          change = (Reinjury - dplyr::lag(Reinjury)) / dplyr::lag(Reinjury),
          change_label = traumar::pretty_percent(
            change,
            n_decimal = 2
          )
        ) |>
        dplyr::mutate(
          prop = Reinjury / n,
          prop_label = traumar::pretty_percent(
            prop,
            n_decimal = 2
          ),
          .by = Year
        )

      cli::cli_alert_success(
        "Returning count(s) of injured patients (with non-null unique identifier) that had more than 1 injury event in a given year and the proportion(s) of reinjured patients."
      )

      return(out)
    }
  }

  reinjury_case_count <- function(df, descriptive_stats = F) {
    ###
    # these first actions are done so that they do not have to be done
    # more than once before the for() loop
    ###

    # filter down to distinct reinjured patients
    init <- df |>
      dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE) |>
      dplyr::count(Year, Unique_Patient_ID) |>
      dplyr::mutate(reinjury = n > 1)

    # dataframe with unique patients identified, no other fields, for join
    reinjured_patients <- init |>
      dplyr::select(Year, Unique_Patient_ID, reinjury) |>
      dplyr::distinct(Year, Unique_Patient_ID, reinjury)

    # join reinjury data to larger table
    temp <- df |>
      dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
      dplyr::left_join(
        reinjured_patients,
        by = c("Year", "Unique_Patient_ID")
      ) |>
      dplyr::distinct(Unique_Incident_ID, .keep_all = T)

    if (descriptive_stats == F) {
      # count of reinjured patient cases per year
      out <- temp |>
        dplyr::filter(reinjury == T) |>
        dplyr::count(Year)

      cli::cli_alert_success(
        "Returning count(s) of injury cases of patients that had more than 1 injury event in a given year."
      )

      return(out)
    } else if (descriptive_stats == T) {
      counts <- temp |>
        dplyr::filter(
          reinjury == T, # only records involving a reinjured patient
          !is.na(Unique_Patient_ID) # remove records missing unique patient identifier to get accurate descriptive stats
        ) |>
        dplyr::count(Year, Unique_Patient_ID) |> # get unique case counts among reinjured patients
        dplyr::filter(n > 1) |>
        dplyr::summarize(
          Avg_cases = mean(n, na.rm = T), # get descriptive statistics here for the reinjured patient cases
          Max_cases = max(n, na.rm = T), # the counts are not maintained in this object
          Min_cases = min(n, na.rm = T),
          .by = Year
        )

      stat <- temp |> # get count of recases, total cases, proportions, and then join descriptive statistics
        dplyr::summarize(
          Reinjury_cases = sum(reinjury == T, na.rm = T),
          Total_cases = n(),
          prop = Reinjury_cases / Total_cases,
          prop_label = traumar::pretty_percent(
            prop,
            n_decimal = 2
          ),
          .by = Year
        ) |>
        dplyr::mutate(
          change = (Reinjury_cases - dplyr::lag(Reinjury_cases)) /
            dplyr::lag(Reinjury_cases),
          change_label = traumar::pretty_percent(
            change,
            n_decimal = 2
          )
        ) |>
        dplyr::left_join(counts, by = "Year")

      cli::cli_alert_success(
        "Returning count(s) and descriptive statistics of injury cases of patients that had more than 1 injury event in a given year."
      )

      return(stat)
    }
  }

  reinjury_injury_count <- function(df, descriptive_stats = F) {
    # before the distinct operation first so it is only done once
    # make the table distinct based on Unique Patient ID and Incident Date,
    # which will give the unique number of times each patient was injured in a
    # given year, not # of times admitted

    init <- df |>
      dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE)

    if (descriptive_stats == F) {
      # get the counts of the total injury events that resulted in a trauma center visit
      # among patients that had more than 1 injury event in a given year

      out <- init |>
        dplyr::count(Year, Unique_Patient_ID) |>
        dplyr::mutate(reinjury = n > 1) |>
        dplyr::filter(reinjury == T) |>
        dplyr::summarize(Reinjury = sum(n, na.rm = T), .by = Year)

      cli::cli_alert_success(
        "Returning the count(s) of unique injury events related to reinjury."
      )

      return(out)
    } else if (descriptive_stats == T) {
      # get the descriptive statistics for the unique count of injury events
      # among patients that were injured more than once in a given year

      counts <- init |>
        dplyr::filter(!is.na(Unique_Patient_ID)) |>
        dplyr::count(Year, Unique_Patient_ID) |>
        dplyr::mutate(reinjury = n > 1) |>
        dplyr::filter(reinjury == T) |>
        dplyr::summarize(
          Avg_Injuries = mean(n, na.rm = T),
          Min_Reinjury = min(n, na.rm = T),
          Max_Reinjury = max(n, na.rm = T),
          .by = Year
        )

      # get total injury events for all patients for each year

      stat <- df |>
        injury_incident_dplyr::count(Year)

      # get the reinjury event counts among patients that were injured more than
      # once in a given year and join descriptive statistics, calculate others

      out <- init |>
        dplyr::count(Year, Unique_Patient_ID) |>
        dplyr::mutate(reinjury = n > 1) |>
        dplyr::filter(reinjury == T) |>
        dplyr::summarize(Reinjury = sum(n, na.rm = T), .by = Year) |>
        dplyr::left_join(stat, by = "Year") |>
        dplyr::mutate(
          prop = Reinjury / n,
          prop_label = traumar::pretty_percent(
            prop,
            n_decimal = 2
          ),
          change = (Reinjury - dplyr::lag(Reinjury)) / dplyr::lag(Reinjury),
          change_label = traumar::pretty_percent(
            change,
            n_decimal = 2
          )
        ) |>
        dplyr::left_join(counts, by = "Year")

      cli::cli_alert_success(
        "Returning counts and statistics of unique injury events."
      )

      return(out)
    }
  }

  # function to do the work of the final dplyr::mutate() for
  # the cases using counts of special cases

  case_mutate <- function(df) {
    out <- df |>
      dplyr::mutate(
        percent = round(cases / sum(cases), 3),
        label = traumar::pretty_percent(
          cases,
          n_decimal = 2 / sum(cases),
        ),
        .by = Year
      )

    return(out)
  }

  injury_mutate <- function(df) {
    out <- df |>
      dplyr::mutate(
        percent = round(Injury_Events / sum(Injury_Events), 3),
        label = pretty_percent(
          Injury_Events / sum(Injury_Events),
        ),
        .by = Year
      )
  }
}

###_____________________________________________________________________________
# Function to calculate age-adjusted EMS run rates by county and BH district
# This function computes directly standardized (age-adjusted) rates using
# pre-aggregated data that includes event counts, local population estimates,
# and standard population weights.
#
# Assumptions:
#   • The input data has already been grouped (e.g., by County and Age Group).
#   • {{ count }}, {{ local_population }}, and {{ standard_population_weight }}
#     are scalar fields within these grouped rows.
#   • The user provides any necessary grouping variables via ... to summarize
#     results at the desired geographic or demographic resolution.
#
# This function returns both crude and age-adjusted rates per specified
# multiplier (default is per 100,000 population).
#
# Inputs:
#   • data — a grouped or ungrouped data.frame or tibble with input variables
#   • count — unquoted column name representing the event count
#   • local_population — unquoted column name for the local population (e.g., county-age)
#   • standard_population_weight — unquoted column name for the standard weight (proportional)
#   • ... — grouping variables to aggregate final rates (e.g., County, District)
#   • rate — numeric value for scaling rates (default: 100,000)
#
# Output:
#   • A tibble with Count, Crude_Rate, and Age_Adjusted_Rate per grouping
###_____________________________________________________________________________

calc_age_adjusted_rate <- function(
  data, # input tibble or data.frame with grouped or stratified rows
  count, # unquoted column name for observed event count (e.g., EMS runs)
  local_population, # unquoted column name for stratum-specific local population
  standard_population_weight, # unquoted column name for proportional weight from standard population
  .by = NULL, # grouping variables for aggregating final rates (e.g., County, District)
  rate = 100000 # rate multiplier (e.g., per 1000, 10000, or 100000)
) {
  # Step 1: Calculate age-specific crude rate and weighted contribution
  rate_data <- data |>
    dplyr::mutate(
      crude_rate = ({{ count }} / {{ local_population }}) * rate, # rate within each age group
      weighted_rate = crude_rate * {{ standard_population_weight }} # contribution to adjusted rate
    )

  # Step 2: Aggregate to final grouping level
  rate_summary <- rate_data |>
    dplyr::summarize(
      Count = sum({{ count }}, na.rm = TRUE), # total count of events
      Crude_Rate = sum({{ count }}, na.rm = TRUE) /
        sum({{ local_population }}, na.rm = TRUE) *
        rate, # overall crude rate
      Age_Adjusted_Rate = sum(weighted_rate, na.rm = TRUE), # final adjusted rate
      .by = tidyselect::all_of({{ .by }}) # group by user-supplied variables (e.g., County)
    )

  return(rate_summary)
}
