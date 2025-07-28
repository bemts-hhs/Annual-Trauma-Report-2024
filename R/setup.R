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
#   'naniar'
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
        distinct(Patient_Linking, Date_of_Service, .keep_all = TRUE) |>
        count(...)

      message(
        "Returning the count(s) of unique injury events that led to inpatient hospitalization."
      )
    } else if (which == "Outpatient") {
      # get oupatient counts
      # filter down to Inpatient and then unique injury counts by only looking at unique
      # combinations of patient and date of service

      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "Outpatient") |>
        distinct(Patient_Linking, Date_of_Service, .keep_all = TRUE) |>
        count(...)

      message(
        "Returning the count(s) of unique injury events that led to outpatient visits."
      )
    } else if (which == "IPOP") {
      # get oupatient and inpatient counts
      # filter down to Inpatient and then unique injury counts by only looking at unique
      # combinations of patient and date of service

      temp <- df |>
        distinct(Patient_Linking, Date_of_Service, .keep_all = TRUE) |>
        count(...)

      message(
        "Returning the count(s) of unique injury events that led to inpatient hospitalization and outpatient visits."
      )
    }

    if (descriptive_stats == FALSE) {
      return(temp)
    } else {
      out <- temp |>
        mutate(
          change = (n - lag(n)) / lag(n),
          change_label = pretty_percent(change)
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
        distinct(
          Patient_Linking,
          Hospital_Number,
          Date_of_Service,
          .keep_all = TRUE
        ) |>
        count(...)

      message("Returning the count(s) of total unique inpatient injury cases.")
    } else if (which == "Outpatient") {
      # get outpatient case record counts
      # filter down to Inpatient and then unique case counts by only looking at unique
      # combinations of Patient_Linking, Record_ID, Hospital_Number, Date_of_Service

      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "Outpatient") |>
        distinct(
          Patient_Linking,
          Hospital_Number,
          Date_of_Service,
          .keep_all = TRUE
        ) |>
        count(...)

      message(
        "Returning the count(s) of total unique injury outpatient visits."
      )
    } else if (which == "IPOP") {
      # get both outpatient and inpatient case record counts
      # filter down to Inpatient and then unique case counts by only looking at unique
      # combinations of Patient_Linking, Record_ID, Hospital_Number, Date_of_Service

      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "IPOP") |>
        distinct(
          Patient_Linking,
          Hospital_Number,
          Date_of_Service,
          .keep_all = TRUE
        ) |>
        count(...)

      message(
        "Returning the count(s) of total unique injury inpatient cases and injury outpatient visits."
      )
    }

    if (descriptive_stats == FALSE) {
      return(temp)
    } else {
      out <- temp |>
        mutate(
          change = (n - lag(n)) / lag(n),
          change_label = pretty_percent(change)
        )

      return(out)
    }
  }

  # custom function to get unique count of cases in IPOP
  # this function runs by grouping first using group_by() and then running the distinct() function
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
        group_by(Year) |>
        distinct(Patient_Linking, .keep_all = TRUE) |>
        ungroup() |>
        count(...)

      message(
        "Returning the count(s) of total unique injured patients who had an inpatient case event."
      )
    } else if (which == "Outpatient") {
      # get outpatient counts
      # filter down to Inpatient and then unique patient counts by only looking at unique
      # instances of Patient_Linking

      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "Outpatient") |>
        group_by(Year) |>
        distinct(Patient_Linking, .keep_all = TRUE) |>
        ungroup() |>
        count(...)

      message(
        "Returning the count(s) of total unique injured patients who had an outpatient visit."
      )
    } else if (which == "IPOP") {
      # get both outpatient and inpatient counts
      # filter down to Inpatient and then unique patient counts by only looking at unique
      # instances of Patient_Linking

      temp <- df |>
        dplyr::filter(Inpatient_or_Outpatient == "IPOP") |>
        group_by(Year) |>
        distinct(Patient_Linking, .keep_all = TRUE) |>
        ungroup() |>
        count(...)

      message(
        "Returning the count(s) of total unique injured patients who had an inpatient case event or outpatient visit."
      )
    }

    if (descriptive_stats == FALSE) {
      return(temp)
    } else {
      out <- temp |>
        mutate(
          change = (n - lag(n)) / lag(n),
          change_label = pretty_percent(change)
        )

      return(out)
    }
  }

  # custom function to get unique count of cases from Patient Registry
  # synonymous with "incident count" from the past

  injury_case_count <- function(df, ..., descriptive_stats = F) {
    # set up a temp file so distinct() does not have to be ran more than once
    # this function will get the distinct rows based on Unique Incident ID,
    # which is equal to # of inpatient visits overall
    # in all cases the function will return # cases per group, depending
    # on the grouping variable provided by the user

    temp <- df |>
      distinct(Unique_Incident_ID, .keep_all = TRUE)

    if (descriptive_stats == F) {
      # provide only the counts, user can define grouping based on dynamic dots

      out <- temp |>
        count(...)

      message("Returning the count(s) of total unique inpatient injury cases.")

      return(out)
    } else if (descriptive_stats == T) {
      # provide descriptive statistics about % change and # change
      # count based on user input, user can define grouping based on dynamic dots

      out <- temp |>
        count(...) |>
        mutate(
          change = n - lag(n),
          prop_change = (n - lag(n)) / lag(n),
          prop_label = pretty_percent(prop_change, n_decimal = 0.1)
        )

      message(
        "Returning the count(s) of total unique inpatient injury cases and descriptive statistics."
      )

      return(out)
    }
  }

  # custom function to get unique count of injuries in Patient Registry
  # a true estimation of the number of incidents

  injury_incident_count <- function(df, ..., descriptive_stats = F) {
    # set up a temp file so distinct() does not have to be ran more than once
    # this function will get the distinct rows based on unique combination of
    # Unique Patient ID and Incident Dates, which gets down to # of unique injury
    # events.
    # in all cases the function will return # injury events that led to a trauma
    # center visit per group, depending on the grouping variable provided by the user

    temp <- df |>
      distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE)

    if (descriptive_stats == F) {
      out <- temp |>
        count(...)

      message(
        "Returning the count(s) of total unique injury events leading to a trauma center visit."
      )

      return(out)
    } else if (descriptive_stats == T) {
      # provide descriptive statistics along with injury event counts

      stat <- temp |>
        dplyr::filter(!is.na(Unique_Patient_ID)) |>
        count(Year, Unique_Patient_ID) |>
        dplyr::filter(n > 1) |>
        summarize(
          Min_Injuries = min(n, na.rm = T),
          Max_Injuries = max(n, na.rm = T),
          Avg_Injuries = mean(n, na.rm = T),
          .by = Year
        )

      out <- temp |>
        count(Year) |>
        mutate(
          change = n - lag(n),
          prop_change = (n - lag(n)) / lag(n),
          prop_label = pretty_percent(prop_change, n_decimal = 0.1)
        ) |>
        left_join(stat, by = "Year")

      message(
        "Returning the count(s) of total unique injury events leading to a trauma center visit and descriptive statistics."
      )

      return(out)
    }
  }

  # custom function to get unique count of patients in Patient Registry
  # a true estimation of the number of patients
  # given that patient unique identifiers are the same across all years (for the most part)
  # this function runs by grouping first using group_by() and then running the distinct() function
  # running the count grouped by the column supplied, usually Year

  injury_patient_count <- function(df, ..., descriptive_stats = F) {
    # set up a temp file so distinct() does not have to be ran more than once
    # this function will get the distinct rows based on Unique Patient ID,
    # which gets down to # of unique patients who were served at a trauma center
    # in all cases the function will return # patients that had a trauma
    # center visit per group, depending on the grouping variable provided by the user
    # important that using this method that the group_by(Year) is ran before distinct(),
    # or else you will get the distinct patients in the whole table

    temp <- df |>
      dplyr::filter(!is.na(Unique_Patient_ID)) |>
      group_by(Year) |>
      distinct(Unique_Patient_ID, .keep_all = TRUE) |>
      ungroup()

    if (descriptive_stats == F) {
      out <- temp |>
        count(...)

      message(
        "Returning the count(s) of total unique patients who had a trauma center visit and have a non-missing unique patient ID."
      )

      return(out)
    } else if (descriptive_stats == T) {
      # get descriptive statistics related to # change and % change in patient counts
      # in addition to unique patient counts per year

      out <- temp |>
        count(...) |>
        mutate(
          change = n - lag(n),
          prop_change = (n - lag(n)) / lag(n),
          prop_label = pretty_percent(prop_change, n_decimal = 0.1)
        )

      message(
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
      distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE) |>
      count(Year, Unique_Patient_ID) |>
      mutate(reinjury = n > 1)

    # calculate the descriptive statistics just once

    summary <- temp |>
      dplyr::filter(n > 1) |>
      summarize(
        Min_Injuries = min(n, na.rm = T),
        Max_Injuries = max(n, na.rm = T),
        Avg_Injuries = mean(n, na.rm = T),
        .by = Year
      )

    if (descriptive_stats == F) {
      # count cases where reinjury == TRUE which gives count of unique patients
      # this works because the table at this point is unique patients each year
      # and their corresponding # of injury events in that year

      out <- temp |>
        summarize(Reinjury = sum(reinjury == T, na.rm = T), .by = Year)

      message(
        "Returning count(s) of injured patients that had more than 1 injury event in a given year, and have a non-null unique identifier."
      )

      return(out)
    } else if (descriptive_stats == T) {
      stat <- temp |>
        summarize(Reinjury = sum(reinjury == T, na.rm = T), .by = Year)

      counts <- df |>
        injury_patient_count(Year) # get total patients for the years, including patients that were or were not reinjured

      out <- stat |>
        left_join(counts, by = "Year") |> # join overall patient counts to the table of reinjured counts
        left_join(summary, by = "Year") |> # join descriptive statistics
        mutate(
          change = (Reinjury - lag(Reinjury)) / lag(Reinjury),
          change_label = pretty_percent(change, n_decimal = 0.1)
        ) |>
        mutate(
          prop = Reinjury / n,
          prop_label = pretty_percent(prop, n_decimal = 0.1),
          .by = Year
        )

      message(
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
      distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE) |>
      count(Year, Unique_Patient_ID) |>
      mutate(reinjury = n > 1)

    # dataframe with unique patients identified, no other fields, for join
    reinjured_patients <- init |>
      dplyr::select(Year, Unique_Patient_ID, reinjury) |>
      distinct(Year, Unique_Patient_ID, reinjury)

    # join reinjury data to larger table
    temp <- df |>
      distinct(Unique_Incident_ID, .keep_all = TRUE) |>
      left_join(reinjured_patients, by = c("Year", "Unique_Patient_ID")) |>
      distinct(Unique_Incident_ID, .keep_all = T)

    if (descriptive_stats == F) {
      # count of reinjured patient cases per year
      out <- temp |>
        dplyr::filter(reinjury == T) |>
        count(Year)

      message(
        "Returning count(s) of injury cases of patients that had more than 1 injury event in a given year."
      )

      return(out)
    } else if (descriptive_stats == T) {
      counts <- temp |>
        dplyr::filter(
          reinjury == T, # only records involving a reinjured patient
          !is.na(Unique_Patient_ID) # remove records missing unique patient identifier to get accurate descriptive stats
        ) |>
        count(Year, Unique_Patient_ID) |> # get unique case counts among reinjured patients
        dplyr::filter(n > 1) |>
        summarize(
          Avg_cases = mean(n, na.rm = T), # get descriptive statistics here for the reinjured patient cases
          Max_cases = max(n, na.rm = T), # the counts are not maintained in this object
          Min_cases = min(n, na.rm = T),
          .by = Year
        )

      stat <- temp |> # get count of recases, total cases, proportions, and then join descriptive statistics
        summarize(
          Reinjury_cases = sum(reinjury == T, na.rm = T),
          Total_cases = n(),
          prop = Reinjury_cases / Total_cases,
          prop_label = pretty_percent(prop, n_decimal = 0.1),
          .by = Year
        ) |>
        mutate(
          change = (Reinjury_cases - lag(Reinjury_cases)) / lag(Reinjury_cases),
          change_label = pretty_percent(change, n_decimal = 0.1)
        ) |>
        left_join(counts, by = "Year")

      message(
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
      distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE)

    if (descriptive_stats == F) {
      # get the counts of the total injury events that resulted in a trauma center visit
      # among patients that had more than 1 injury event in a given year

      out <- init |>
        count(Year, Unique_Patient_ID) |>
        mutate(reinjury = n > 1) |>
        dplyr::filter(reinjury == T) |>
        summarize(Reinjury = sum(n, na.rm = T), .by = Year)

      message(
        "Returning the count(s) of unique injury events related to reinjury."
      )

      return(out)
    } else if (descriptive_stats == T) {
      # get the descriptive statistics for the unique count of injury events
      # among patients that were injured more than once in a given year

      counts <- init |>
        dplyr::filter(!is.na(Unique_Patient_ID)) |>
        count(Year, Unique_Patient_ID) |>
        mutate(reinjury = n > 1) |>
        dplyr::filter(reinjury == T) |>
        summarize(
          Avg_Injuries = mean(n, na.rm = T),
          Min_Injuries = min(n, na.rm = T),
          Max_Injuries = max(n, na.rm = T),
          .by = Year
        )

      # get total injury events for all patients for each year

      stat <- df |>
        injury_incident_count(Year)

      # get the reinjury event counts among patients that were injured more than
      # once in a given year and join descriptive statistics, calculate others

      out <- init |>
        count(Year, Unique_Patient_ID) |>
        mutate(reinjury = n > 1) |>
        dplyr::filter(reinjury == T) |>
        summarize(Reinjury = sum(n, na.rm = T), .by = Year) |>
        left_join(stat, by = "Year") |>
        mutate(
          prop = Reinjury / n,
          prop_label = pretty_percent(prop, n_decimal = 0.1),
          change = (Reinjury - lag(Reinjury)) / lag(Reinjury),
          change_label = pretty_percent(change, n_decimal = 0.1)
        ) |>
        left_join(counts, by = "Year")

      message("Returning counts and statistics of unique injury events.")

      return(out)
    }
  }

  # function to do the work of the final mutate() for
  # the cases using counts of special cases

  case_mutate <- function(df) {
    out <- df |>
      mutate(
        percent = round(cases / sum(cases), 3),
        label = pretty_percent(cases / sum(cases), n_decimal = 0.1),
        .by = Year
      )

    return(out)
  }

  injury_mutate <- function(df) {
    out <- df |>
      mutate(
        percent = round(Injury_Events / sum(Injury_Events), 3),
        label = pretty_percent(
          Injury_Events / sum(Injury_Events),
          n_decimal = 0.1
        ),
        .by = Year
      )
  }
}
