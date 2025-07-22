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
# created from the custom_functions script to complete the needed operations.
# It is not necessary to copy those scripts into this one, they can be run separately,
# simply signal that you are doing the SEQIC or OOH portion of the analysis
# at the appropriate time
###_____________________________________________________________________________

###_____________________________________________________________________________
# Get palettes ----
###_____________________________________________________________________________

# easy to access list of qualitative palettes

good_palettes <- paletteer::palettes_d_names |>
  dplyr::filter(grepl(pattern = "blind", x = package, ignore.case = T)) |>
  dplyr::arrange(package, desc(length))

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

###_____________________________________________________________________________
# Load the files used to categorize mechanism and nature of injury based
# on the ICD-10 injury code
# NOTICE THAT IN ORDER TO GET THE SAME COUNTS AS IN TABLEAU WITH REGARD TO THE
# CAUSE OF INJURY / NATURE OF INJURY / body region (lvl1 and lvl2) you must run
# RUN distinct(Unique_Incident_ID, [coi_ar, cc2, body region], .keep_all = TRUE)
# and then your count() function or else you will not get the same counts in R.
# Tableau does a better job of automating the grouping via AI, and in R you have
# to do that manually.
###_____________________________________________________________________________

# mechanism of injury mapping

mechanism_injury_mapping <- read_csv(
  "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Desktop/Analytics/Analytics Builds/GitHub/Reference-Files/icd10cm_injury_poisoning_nonpoisoning_2023.csv"
)

mechanism_injury_mapping <- mechanism_injury_mapping |>
  dplyr::select(
    UPPER_CODE,
    INTENTIONALITY,
    CUSTOM_CATEGORY2,
    CAUSE_OF_INJURY_AR
  )

# nature of injury mapping

nature_injury_mapping <- read_excel(
  "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Desktop/Analytics/Analytics Builds/GitHub/Reference-Files/icd10_diagnosis_matrix_custom.xlsx"
)

nature_injury_mapping <- nature_injury_mapping |>
  dplyr::select(
    ICD_10_CODE_TRIM,
    NATURE_OF_INJURY_DESCRIPTOR,
    BODY_REGION_CATEGORY_LEVEL_1,
    BODY_REGION_CATEGORY_LEVEL_2
  )

# classify counties in the data

location_data <- read_csv(
  "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Desktop/Analytics/Analytics Builds/GitHub/Reference-Files/IA Counties, Regions.csv"
)

location_data <- location_data |>
  dplyr::select(County, Designation, Urbanicity)

###_____________________________________________________________________________
# census bureau standard pops 2010-2023 census ----
# documentation here:
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/
###_____________________________________________________________________________

# get years for each county population

{
  county_pops_all <- read_csv(
    "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/asrh/CC-EST2020-ALLDATA-19.csv"
  ) # 2010-2020 Census Bureau County population data

  # 2018
  county_pops_2018 <- county_pops_all |>
    dplyr::filter(YEAR == 11, AGEGRP == 0) |> #  7/1/2018 population estimate
    dplyr::select(Year = YEAR, County = CTYNAME, Population = TOT_POP) |>
    mutate(
      Year = 2018,
      County = str_squish(str_remove_all(County, pattern = "\\sCounty")),
      County = str_to_title(County),
      County = if_else(
        grepl(pattern = "o[']brien", x = County, ignore.case = TRUE),
        "O'Brien",
        County
      )
    )

  # 2019
  county_pops_2019 <- county_pops_all |>
    dplyr::filter(YEAR == 12, AGEGRP == 0) |> #  7/1/2019 population estimate
    dplyr::select(Year = YEAR, County = CTYNAME, Population = TOT_POP) |>
    mutate(
      Year = 2019,
      County = str_squish(str_remove_all(County, pattern = "\\sCounty")),
      County = str_to_title(County),
      County = if_else(
        grepl(pattern = "o[']brien", x = County, ignore.case = TRUE),
        "O'Brien",
        County
      )
    )

  # 2020
  county_pops_2020 <- county_pops_all |>
    dplyr::filter(YEAR == 13, AGEGRP == 0) |> #  7/1/2018 population estimate
    dplyr::select(Year = YEAR, County = CTYNAME, Population = TOT_POP) |>
    mutate(
      Year = 2020,
      County = str_squish(str_remove_all(County, pattern = "\\sCounty")),
      County = str_to_title(County),
      County = if_else(
        grepl(pattern = "o[']brien", x = County, ignore.case = TRUE),
        "O'Brien",
        County
      )
    )

  # 2021-2023 county pops - no age groups for 2023 so need to use 2022 for age adjustment

  county_pops_all2 <- read_csv(
    "https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/totals/co-est2023-alldata.csv"
  )

  # 2021
  county_pops_2021 <- county_pops_all2 |>
    dplyr::filter(COUNTY != "000", STNAME == "Iowa") |> #  7/1/2021 population estimate
    dplyr::select(County = CTYNAME, Population = POPESTIMATE2021) |>
    mutate(Year = 2021, .before = 1) |>
    mutate(
      County = str_squish(str_remove_all(County, pattern = "\\sCounty")),
      County = str_to_title(County),
      County = if_else(
        grepl(pattern = "o[']brien", x = County, ignore.case = TRUE),
        "O'Brien",
        County
      )
    )

  # 2022
  county_pops_2022 <- county_pops_all2 |>
    dplyr::filter(COUNTY != "000", STNAME == "Iowa") |> #  7/1/2022 population estimate
    dplyr::select(County = CTYNAME, Population = POPESTIMATE2022) |>
    mutate(Year = 2022, .before = 1) |>
    mutate(
      County = str_squish(str_remove_all(County, pattern = "\\sCounty")),
      County = str_to_title(County),
      County = if_else(
        grepl(pattern = "o[']brien", x = County, ignore.case = TRUE),
        "O'Brien",
        County
      )
    )

  # 2023
  county_pops_2023 <- county_pops_all2 |>
    dplyr::filter(COUNTY != "000", STNAME == "Iowa") |> #  7/1/2023 population estimate
    dplyr::select(County = CTYNAME, Population = POPESTIMATE2023) |>
    mutate(Year = 2023, .before = 1) |>
    mutate(
      County = str_squish(str_remove_all(County, pattern = "\\sCounty")),
      County = str_to_title(County),
      County = if_else(
        grepl(pattern = "o[']brien", x = County, ignore.case = TRUE),
        "O'Brien",
        County
      )
    )

  # union all populations

  county_pops <- bind_rows(
    county_pops_2018,
    county_pops_2019,
    county_pops_2020,
    county_pops_2021,
    county_pops_2022,
    county_pops_2023
  )
}

# standard county pops by age group
# will use the 2018-2022 population estimates for stability's sake, 2023 not available for age groups yet

county_age_group_pops1 <- read_csv(
  "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-19.csv"
)

county_age_group_pops2 <- read_csv(
  "https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/counties/asrh/cc-est2022-agesex-19.csv"
)

# Get variables of interest from the larger data.frame to allow for the 18 age bands

{
  # 2018
  age_group_pops_2018 <- county_age_group_pops1 |>
    dplyr::select(
      CTYNAME,
      YEAR,
      matches(
        "age(04|59|1014|1519|2024|2529|3034|3539|4044|4549|5054|5559|6064|6569|7074|7579|8084|85plus)_tot",
        ignore.case = TRUE
      ),
      POPESTIMATE
    ) |>
    dplyr::filter(YEAR == 11) |> # as of 7/1/2018 population estimate
    dplyr::select(-YEAR) |>
    pivot_longer(
      cols = AGE04_TOT:AGE85PLUS_TOT,
      names_to = "Age_Group",
      values_to = "Population"
    ) |>
    mutate(Year = 2018, .before = 1) |>
    mutate(
      Age_Group = str_extract(Age_Group, pattern = "\\d+"),
      Age_Group = if_else(
        Age_Group == "85",
        "85+",
        if_else(
          nchar(Age_Group) == 2,
          paste0(str_sub(Age_Group, 1, 1), "-", str_sub(Age_Group, 2, 2)),
          if_else(
            nchar(Age_Group) == 4,
            paste0(str_sub(Age_Group, 1, 2), "-", str_sub(Age_Group, 3, 4)),
            "Missing"
          )
        )
      ),
      CTYNAME = str_remove_all(CTYNAME, pattern = "(?:\\sCounty)*")
    ) |>
    rename(County = CTYNAME, County_Population = POPESTIMATE) |>
    relocate(County_Population, .after = Population) |>
    mutate(County_Weight = Population / County_Population)

  # 2019
  age_group_pops_2019 <- county_age_group_pops1 |>
    dplyr::select(
      CTYNAME,
      YEAR,
      matches(
        "age(04|59|1014|1519|2024|2529|3034|3539|4044|4549|5054|5559|6064|6569|7074|7579|8084|85plus)_tot",
        ignore.case = TRUE
      ),
      POPESTIMATE
    ) |>
    dplyr::filter(YEAR == 12) |> # as of 7/1/2018 population estimate
    dplyr::select(-YEAR) |>
    pivot_longer(
      cols = AGE04_TOT:AGE85PLUS_TOT,
      names_to = "Age_Group",
      values_to = "Population"
    ) |>
    mutate(Year = 2019, .before = 1) |>
    mutate(
      Age_Group = str_extract(Age_Group, pattern = "\\d+"),
      Age_Group = if_else(
        Age_Group == "85",
        "85+",
        if_else(
          nchar(Age_Group) == 2,
          paste0(str_sub(Age_Group, 1, 1), "-", str_sub(Age_Group, 2, 2)),
          if_else(
            nchar(Age_Group) == 4,
            paste0(str_sub(Age_Group, 1, 2), "-", str_sub(Age_Group, 3, 4)),
            "Missing"
          )
        )
      ),
      CTYNAME = str_remove_all(CTYNAME, pattern = "(?:\\sCounty)*")
    ) |>
    rename(County = CTYNAME, County_Population = POPESTIMATE) |>
    relocate(County_Population, .after = Population) |>
    mutate(County_Weight = Population / County_Population)

  # 2020
  age_group_pops_2020 <- county_age_group_pops2 |>
    dplyr::select(
      CTYNAME,
      YEAR,
      matches(
        "age(04|59|1014|1519|2024|2529|3034|3539|4044|4549|5054|5559|6064|6569|7074|7579|8084|85plus)_tot",
        ignore.case = TRUE
      ),
      POPESTIMATE
    ) |>
    dplyr::filter(YEAR == 2) |> # as of 7/1/2020 population estimate
    dplyr::select(-YEAR) |>
    pivot_longer(
      cols = AGE04_TOT:AGE85PLUS_TOT,
      names_to = "Age_Group",
      values_to = "Population"
    ) |>
    mutate(Year = 2020, .before = 1) |>
    mutate(
      Age_Group = str_extract(Age_Group, pattern = "\\d+"),
      Age_Group = if_else(
        Age_Group == "85",
        "85+",
        if_else(
          nchar(Age_Group) == 2,
          paste0(str_sub(Age_Group, 1, 1), "-", str_sub(Age_Group, 2, 2)),
          if_else(
            nchar(Age_Group) == 4,
            paste0(str_sub(Age_Group, 1, 2), "-", str_sub(Age_Group, 3, 4)),
            "Missing"
          )
        )
      ),
      CTYNAME = str_remove_all(CTYNAME, pattern = "(?:\\sCounty)*")
    ) |>
    rename(County = CTYNAME, County_Population = POPESTIMATE) |>
    relocate(County_Population, .after = Population) |>
    mutate(County_Weight = Population / County_Population)

  # 2021
  age_group_pops_2021 <- county_age_group_pops2 |>
    dplyr::select(
      CTYNAME,
      YEAR,
      matches(
        "age(04|59|1014|1519|2024|2529|3034|3539|4044|4549|5054|5559|6064|6569|7074|7579|8084|85plus)_tot",
        ignore.case = TRUE
      ),
      POPESTIMATE
    ) |>
    dplyr::filter(YEAR == 3) |> # as of 7/1/2021 population estimate
    dplyr::select(-YEAR) |>
    pivot_longer(
      cols = AGE04_TOT:AGE85PLUS_TOT,
      names_to = "Age_Group",
      values_to = "Population"
    ) |>
    mutate(Year = 2021, .before = 1) |>
    mutate(
      Age_Group = str_extract(Age_Group, pattern = "\\d+"),
      Age_Group = if_else(
        Age_Group == "85",
        "85+",
        if_else(
          nchar(Age_Group) == 2,
          paste0(str_sub(Age_Group, 1, 1), "-", str_sub(Age_Group, 2, 2)),
          if_else(
            nchar(Age_Group) == 4,
            paste0(str_sub(Age_Group, 1, 2), "-", str_sub(Age_Group, 3, 4)),
            "Missing"
          )
        )
      ),
      CTYNAME = str_remove_all(CTYNAME, pattern = "(?:\\sCounty)*")
    ) |>
    rename(County = CTYNAME, County_Population = POPESTIMATE) |>
    relocate(County_Population, .after = Population) |>
    mutate(County_Weight = Population / County_Population)

  # 2022
  age_group_pops_2022 <- county_age_group_pops2 |>
    dplyr::select(
      CTYNAME,
      YEAR,
      matches(
        "age(04|59|1014|1519|2024|2529|3034|3539|4044|4549|5054|5559|6064|6569|7074|7579|8084|85plus)_tot",
        ignore.case = TRUE
      ),
      POPESTIMATE
    ) |>
    dplyr::filter(YEAR == 4) |> # as of 7/1/2022 population estimate
    dplyr::select(-YEAR) |>
    pivot_longer(
      cols = AGE04_TOT:AGE85PLUS_TOT,
      names_to = "Age_Group",
      values_to = "Population"
    ) |>
    mutate(Year = 2022, .before = 1) |>
    mutate(
      Age_Group = str_extract(Age_Group, pattern = "\\d+"),
      Age_Group = if_else(
        Age_Group == "85",
        "85+",
        if_else(
          nchar(Age_Group) == 2,
          paste0(str_sub(Age_Group, 1, 1), "-", str_sub(Age_Group, 2, 2)),
          if_else(
            nchar(Age_Group) == 4,
            paste0(str_sub(Age_Group, 1, 2), "-", str_sub(Age_Group, 3, 4)),
            "Missing"
          )
        )
      ),
      CTYNAME = str_remove_all(CTYNAME, pattern = "(?:\\sCounty)*")
    ) |>
    rename(County = CTYNAME, County_Population = POPESTIMATE) |>
    relocate(County_Population, .after = Population) |>
    mutate(County_Weight = Population / County_Population)

  # 2023
  age_group_pops_2023 <- county_age_group_pops2 |>
    dplyr::select(
      CTYNAME,
      YEAR,
      matches(
        "age(04|59|1014|1519|2024|2529|3034|3539|4044|4549|5054|5559|6064|6569|7074|7579|8084|85plus)_tot",
        ignore.case = TRUE
      ),
      POPESTIMATE
    ) |>
    dplyr::filter(YEAR == 4) |> # as of 7/1/2022 population estimate, using 2022 for 2023 as 2023 is not available
    dplyr::select(-YEAR) |>
    pivot_longer(
      cols = AGE04_TOT:AGE85PLUS_TOT,
      names_to = "Age_Group",
      values_to = "Population"
    ) |>
    mutate(Year = 2023, .before = 1) |>
    mutate(
      Age_Group = str_extract(Age_Group, pattern = "\\d+"),
      Age_Group = if_else(
        Age_Group == "85",
        "85+",
        if_else(
          nchar(Age_Group) == 2,
          paste0(str_sub(Age_Group, 1, 1), "-", str_sub(Age_Group, 2, 2)),
          if_else(
            nchar(Age_Group) == 4,
            paste0(str_sub(Age_Group, 1, 2), "-", str_sub(Age_Group, 3, 4)),
            "Missing"
          )
        )
      ),
      CTYNAME = str_remove_all(CTYNAME, pattern = "(?:\\sCounty)*")
    ) |>
    rename(County = CTYNAME, County_Population = POPESTIMATE) |>
    relocate(County_Population, .after = Population) |>
    mutate(County_Weight = Population / County_Population)

  # union the age group populations

  age_group_pops <- bind_rows(
    age_group_pops_2018,
    age_group_pops_2019,
    age_group_pops_2020,
    age_group_pops_2021,
    age_group_pops_2022,
    age_group_pops_2023
  )
}

###_____________________________________________________________________________
# standard age group populations for Iowa ----
# from https://wonder.cdc.gov/single-race-population.html
# utilize this section: Single-Race Population Estimates: State-level data with single year and broader age groups
# selected year 2022, state of Iowa, all gender, all ethnicity, all race, 5 yr age groups
# choose to export the file as a download, which comes down as a .txt file named:
###
# Single-Race Population Estimates 2020-2022 by State and Single-Year Age.txt
###
# rename as Iowa state level age groups.txt
###_____________________________________________________________________________

# Iowa age groups at the state level for 2018-2022
# clean Iowa age group populations

{
  # 2018
  state_age_group_pops_2018 <- read_tsv(
    "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/Iowa state level age groups 2018.txt",
    n_max = 19
  ) |>
    mutate(Year = 2018, .before = 1)

  # clean
  state_age_group_pops_2018_clean <- state_age_group_pops_2018 |>
    dplyr::select(Year, `Five-Year Age Groups Code`, Population) |>
    mutate(
      Age_Group = if_else(
        `Five-Year Age Groups Code` %in% c("1", "1-4"),
        "0-4",
        `Five-Year Age Groups Code`
      )
    ) |>
    summarize(Population = sum(Population), .by = c(Year, Age_Group)) |>
    mutate(
      Age_Group = factor(
        Age_Group,
        levels = c(
          "0-4",
          "5-9",
          "10-14",
          "15-19",
          "20-24",
          "25-29",
          "30-34",
          "35-39",
          "40-44",
          "45-49",
          "50-54",
          "55-59",
          "60-64",
          "65-69",
          "70-74",
          "75-79",
          "80-84",
          "85+"
        )
      )
    )

  # 2019
  state_age_group_pops_2019 <- read_tsv(
    "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/Iowa state level age groups 2019.txt",
    n_max = 19
  ) |>
    mutate(Year = 2019, .before = 1)

  # clean
  state_age_group_pops_2019_clean <- state_age_group_pops_2019 |>
    dplyr::select(Year, `Five-Year Age Groups Code`, Population) |>
    mutate(
      Age_Group = if_else(
        `Five-Year Age Groups Code` %in% c("1", "1-4"),
        "0-4",
        `Five-Year Age Groups Code`
      )
    ) |>
    summarize(Population = sum(Population), .by = c(Year, Age_Group)) |>
    mutate(
      Age_Group = factor(
        Age_Group,
        levels = c(
          "0-4",
          "5-9",
          "10-14",
          "15-19",
          "20-24",
          "25-29",
          "30-34",
          "35-39",
          "40-44",
          "45-49",
          "50-54",
          "55-59",
          "60-64",
          "65-69",
          "70-74",
          "75-79",
          "80-84",
          "85+"
        )
      )
    )

  # 2020
  state_age_group_pops_2020 <- read_tsv(
    "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/Iowa state level age groups 2020.txt",
    n_max = 19
  ) |>
    mutate(Year = 2020, .before = 1)

  # clean
  state_age_group_pops_2020_clean <- state_age_group_pops_2020 |>
    dplyr::select(Year, `Five-Year Age Groups Code`, Population) |>
    mutate(
      Age_Group = if_else(
        `Five-Year Age Groups Code` %in% c("1", "1-4"),
        "0-4",
        `Five-Year Age Groups Code`
      )
    ) |>
    summarize(Population = sum(Population), .by = c(Year, Age_Group)) |>
    mutate(
      Age_Group = factor(
        Age_Group,
        levels = c(
          "0-4",
          "5-9",
          "10-14",
          "15-19",
          "20-24",
          "25-29",
          "30-34",
          "35-39",
          "40-44",
          "45-49",
          "50-54",
          "55-59",
          "60-64",
          "65-69",
          "70-74",
          "75-79",
          "80-84",
          "85+"
        )
      )
    )

  # 2021
  state_age_group_pops_2021 <- read_tsv(
    "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/Iowa state level age groups 2021.txt",
    n_max = 19
  ) |>
    mutate(Year = 2021, .before = 1)

  # clean
  state_age_group_pops_2021_clean <- state_age_group_pops_2021 |>
    dplyr::select(Year, `Five-Year Age Groups Code`, Population) |>
    mutate(
      Age_Group = if_else(
        `Five-Year Age Groups Code` %in% c("1", "1-4"),
        "0-4",
        `Five-Year Age Groups Code`
      )
    ) |>
    summarize(Population = sum(Population), .by = c(Year, Age_Group)) |>
    mutate(
      Age_Group = factor(
        Age_Group,
        levels = c(
          "0-4",
          "5-9",
          "10-14",
          "15-19",
          "20-24",
          "25-29",
          "30-34",
          "35-39",
          "40-44",
          "45-49",
          "50-54",
          "55-59",
          "60-64",
          "65-69",
          "70-74",
          "75-79",
          "80-84",
          "85+"
        )
      )
    )

  # 2022
  state_age_group_pops_2022 <- read_tsv(
    "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/Iowa state level age groups 2022-2023.txt",
    n_max = 19
  ) |>
    mutate(Year = 2022, .before = 1)

  # clean
  state_age_group_pops_2022_clean <- state_age_group_pops_2022 |>
    dplyr::select(Year, `Five-Year Age Groups Code`, Population) |>
    mutate(
      Age_Group = if_else(
        `Five-Year Age Groups Code` %in% c("1", "1-4"),
        "0-4",
        `Five-Year Age Groups Code`
      )
    ) |>
    summarize(Population = sum(Population), .by = c(Year, Age_Group)) |>
    mutate(
      Age_Group = factor(
        Age_Group,
        levels = c(
          "0-4",
          "5-9",
          "10-14",
          "15-19",
          "20-24",
          "25-29",
          "30-34",
          "35-39",
          "40-44",
          "45-49",
          "50-54",
          "55-59",
          "60-64",
          "65-69",
          "70-74",
          "75-79",
          "80-84",
          "85+"
        )
      )
    )

  # 2023 - will just use the 2022 estimations given that the age-sex populations are not out yet for 2023

  state_age_group_pops_2023 <- read_tsv(
    "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/Iowa state level age groups 2022-2023.txt",
    n_max = 19
  ) |>
    mutate(Year = 2023, .before = 1)

  # clean
  state_age_group_pops_2023_clean <- state_age_group_pops_2023 |>
    dplyr::select(Year, `Five-Year Age Groups Code`, Population) |>
    mutate(
      Age_Group = if_else(
        `Five-Year Age Groups Code` %in% c("1", "1-4"),
        "0-4",
        `Five-Year Age Groups Code`
      )
    ) |>
    summarize(Population = sum(Population), .by = c(Year, Age_Group)) |>
    mutate(
      Age_Group = factor(
        Age_Group,
        levels = c(
          "0-4",
          "5-9",
          "10-14",
          "15-19",
          "20-24",
          "25-29",
          "30-34",
          "35-39",
          "40-44",
          "45-49",
          "50-54",
          "55-59",
          "60-64",
          "65-69",
          "70-74",
          "75-79",
          "80-84",
          "85+"
        )
      )
    )

  # union the years

  state_age_group_pops <- bind_rows(
    state_age_group_pops_2018_clean,
    state_age_group_pops_2019_clean,
    state_age_group_pops_2020_clean,
    state_age_group_pops_2021_clean,
    state_age_group_pops_2022_clean,
    state_age_group_pops_2023_clean
  )
}

# standard US populations

us_age_pops <- read_tsv(
  file = "https://seer.cancer.gov/stdpopulations/stdpop.18ages.txt",
  col_names = FALSE
)

{
  us_age_pops_clean <- us_age_pops |>
    dplyr::filter(grepl(pattern = "^204", x = X1)) |>
    mutate(
      Age_Group = str_sub(X1, start = 4, end = 6),
      Population = str_sub(X1, start = 7, end = 14),
      Population = as.numeric(str_remove(Population, pattern = "^0")),
      Weight = round(Population / sum(Population), digits = 6)
    ) |>
    dplyr::select(-X1) |>
    mutate(
      Age_Group = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80-84",
        "85+"
      ),
      Age_Group = factor(
        Age_Group,
        levels = c(
          "0-4",
          "5-9",
          "10-14",
          "15-19",
          "20-24",
          "25-29",
          "30-34",
          "35-39",
          "40-44",
          "45-49",
          "50-54",
          "55-59",
          "60-64",
          "65-69",
          "70-74",
          "75-79",
          "80-84",
          "85+"
        )
      )
    )
}

###_____________________________________________________________________________
# Pull in files ----
###_____________________________________________________________________________

# trauma data

trauma_data <- read_csv(
  "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/trauma_all.csv"
)

# deal with missing values in cause of injury categories

trauma_data_clean <- {
  trauma_data |>
    mutate(
      Age_Group = case_when(
        Patient_Age_Years < 5 ~ "0-4",
        Patient_Age_Years >= 5 & Patient_Age_Years < 10 ~ "5-9",
        Patient_Age_Years >= 10 & Patient_Age_Years < 15 ~ "10-14",
        Patient_Age_Years >= 15 & Patient_Age_Years < 20 ~ "15-19",
        Patient_Age_Years >= 20 & Patient_Age_Years < 25 ~ "20-24",
        Patient_Age_Years >= 25 & Patient_Age_Years < 30 ~ "25-29",
        Patient_Age_Years >= 30 & Patient_Age_Years < 35 ~ "30-34",
        Patient_Age_Years >= 35 & Patient_Age_Years < 40 ~ "35-39",
        Patient_Age_Years >= 40 & Patient_Age_Years < 45 ~ "40-44",
        Patient_Age_Years >= 45 & Patient_Age_Years < 50 ~ "45-49",
        Patient_Age_Years >= 50 & Patient_Age_Years < 55 ~ "50-54",
        Patient_Age_Years >= 55 & Patient_Age_Years < 60 ~ "55-59",
        Patient_Age_Years >= 60 & Patient_Age_Years < 65 ~ "60-64",
        Patient_Age_Years >= 65 & Patient_Age_Years < 70 ~ "65-69",
        Patient_Age_Years >= 70 & Patient_Age_Years < 75 ~ "70-74",
        Patient_Age_Years >= 75 & Patient_Age_Years < 80 ~ "75-79",
        Patient_Age_Years >= 80 & Patient_Age_Years < 85 ~ "80-84",
        Patient_Age_Years >= 85 ~ "85+",
        TRUE ~ "Missing",
        .default = "Missing"
      ),
      Age_Group = factor(
        Age_Group,
        levels = c(
          "0-4",
          "5-9",
          "10-14",
          "15-19",
          "20-24",
          "25-29",
          "30-34",
          "35-39",
          "40-44",
          "45-49",
          "50-54",
          "55-59",
          "60-64",
          "65-69",
          "70-74",
          "75-79",
          "80-84",
          "85+",
          "Missing"
        )
      ),
      .after = Age_Range
    ) |>
    mutate(
      Injury_County = str_to_title(Injury_County),
      Injury_County = if_else(
        grepl(pattern = "o'b", x = Injury_County, ignore.case = T),
        "O'Brien",
        Injury_County
      ),
      NATURE_OF_INJURY_DESCRIPTOR_1 = if_else(
        is.na(NATURE_OF_INJURY_DESCRIPTOR_1),
        NATURE_OF_INJURY_DESCRIPTOR_2,
        NATURE_OF_INJURY_DESCRIPTOR_1
      ),
      BODY_REGION_CATEGORY_LEVEL_1_1 = if_else(
        is.na(BODY_REGION_CATEGORY_LEVEL_1_1),
        BODY_REGION_CATEGORY_LEVEL_1_2,
        BODY_REGION_CATEGORY_LEVEL_1_1
      ),
      BODY_REGION_CATEGORY_LEVEL_2_1 = if_else(
        is.na(BODY_REGION_CATEGORY_LEVEL_2_1),
        BODY_REGION_CATEGORY_LEVEL_2_2,
        BODY_REGION_CATEGORY_LEVEL_2_1
      ),
      BODY_REGION_CATEGORY_LEVEL_3_1 = if_else(
        is.na(BODY_REGION_CATEGORY_LEVEL_3_1),
        BODY_REGION_CATEGORY_LEVEL_3_2,
        BODY_REGION_CATEGORY_LEVEL_3_1
      ),
      INTENTIONALITY_1 = if_else(
        is.na(INTENTIONALITY_1),
        INTENTIONALITY_2,
        INTENTIONALITY_1
      ),
      MECHANISM_1 = if_else(is.na(MECHANISM_1), MECHANISM_2, MECHANISM_1),
      LEVEL_FALL1_1 = if_else(
        is.na(LEVEL_FALL1_1),
        LEVEL_FALL1_2,
        LEVEL_FALL1_1
      ),
      CAUSE_OF_INJURY_AR_1 = if_else(
        is.na(CAUSE_OF_INJURY_AR_1),
        CAUSE_OF_INJURY_AR_2,
        CAUSE_OF_INJURY_AR_1
      )
    ) |>
    left_join(location_data, by = c("Patient_County" = "County")) |>
    rename(
      Designation_Patient = Designation,
      Urbanicity_Patient = Urbanicity
    ) |>
    relocate(
      all_of(c("Designation_Patient", "Urbanicity_Patient")),
      .after = Patient_County
    ) |>
    left_join(location_data, by = c("Injury_County" = "County")) |>
    rename(Designation_Injury = Designation, Urbanicity_Injury = Urbanicity) |>
    relocate(
      all_of(c("Designation_Injury", "Urbanicity_Injury")),
      .after = Injury_County
    ) |>
    left_join(location_data, by = "County") |>
    relocate(all_of(c("Designation", "Urbanicity")), .after = County)
}

# filter for the year of interest
trauma_2023 <- trauma_data_clean |>
  dplyr::filter(Year == 2023)

# ems data

ems_data <- read_csv(
  "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/ems_data.csv"
)

# deal with missing injury categories
ems_data_clean <- ems_data |>
  mutate(Injury_1 = if_else(is.na(Injury_1), Injury_2, Injury_1))

# filter down to the year of interest
ems_2023 <- ems_data |>
  dplyr::filter(Year == 2023)

# ipop data import

ipop_data <- read_csv(
  "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/ipop_check.csv"
)

# clean ipop data

{
  ipop_data_clean <- ipop_data |>
    mutate(
      Census_Age_Group = factor(
        Census_Age_Group,
        levels = c(
          "0 to 4",
          "5 to 9",
          "10 to 14",
          "15 to 19",
          "20 to 24",
          "25 to 29",
          "30 to 34",
          "35 to 39",
          "40 to 44",
          "45 to 49",
          "50 to 54",
          "55 to 59",
          "60 to 64",
          "65 to 69",
          "70 to 74",
          "75 to 79",
          "80 to 84",
          "85 And Over"
        ),
        labels = c(
          "0-4",
          "5-9",
          "10-14",
          "15-19",
          "20-24",
          "25-29",
          "30-34",
          "35-39",
          "40-44",
          "45-49",
          "50-54",
          "55-59",
          "60-64",
          "65-69",
          "70-74",
          "75-79",
          "80-84",
          "85+"
        )
      )
    ) |>
    mutate(
      ICD_10_CODE_TRIM = if_else(
        str_detect(Diagnosis_ICD10_Raw, pattern = "[:alpha:]\\d{2}\\d{1}"),
        str_extract(Diagnosis_ICD10_Raw, pattern = "[:alpha:]\\d{2}\\d{1}"),
        str_extract(
          Diagnosis_ICD10_Raw,
          pattern = "[:alpha:]\\d{2}[:alpha:]{1}[\\d{1}]?"
        )
      ),
      ICD_10_CODE_TRIM = str_squish(ICD_10_CODE_TRIM),
      .before = Diagnosis_ICD10_Raw
    ) |>
    left_join(nature_injury_mapping, by = "ICD_10_CODE_TRIM")
}

###_____________________________________________________________________________
# Approximate age adjusted rates of injury prevalence resulting in inpatient hospitalization ----
# based on the trauma registry
###_____________________________________________________________________________

# total trauma cases

trauma_cases_years <- trauma_data_clean |>
  dplyr::filter(Year < 2024) |>
  injury_case_count(Year)

# check overall trauma case counts with injury location of Iowa

trauma_years_iowa <- trauma_data_clean |>
  dplyr::filter(
    grepl(pattern = "^ia$|iowa", x = Injury_State, ignore.case = T),
    !is.na(Injury_County),
    Injury_County %not_in%
      c(
        "Not Applicable",
        "Not Known",
        "Not Known / Not Recorded",
        "Not Known/Not Recorded",
        "Rock Island"
      )
  ) |>
  dplyr::filter(Year < 2024) |>
  injury_case_count(Year) # close to the annual trauma report, but is a different file if not filtering out non-Iowa incidents

# get injury counts and case counts by year, county, and age group

{
  # injuries
  injury_counts <- trauma_data_clean |>
    dplyr::filter(
      Year < 2024,
      grepl(pattern = "^ia$|iowa", x = Injury_State, ignore.case = T),
      !is.na(Injury_County),
      Injury_County %not_in%
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
    complete(Year, Injury_County, Age_Group, fill = list(n = 0L)) |>
    arrange(Year, Injury_County, Age_Group) |> # returns 99 counties, and more accurate counts
    left_join(
      age_group_pops,
      by = c("Injury_County" = "County", "Age_Group", "Year")
    ) |>
    rename(Count = n, Stratified_Population = Population) |>
    dplyr::filter(Age_Group != "Missing") |>
    mutate(
      Crude_Rate = round(Count / Stratified_Population, digits = 6),
      .after = County_Population
    ) |>
    left_join(us_age_pops_clean, by = "Age_Group") |>
    mutate(Adjusted_Rate_Component = round(Crude_Rate * Weight, digits = 6)) |>
    mutate(Rate_Type = "Injury_Count")

  # cases
  # often mentioned as 'incidents'

  case_counts <- trauma_data_clean |>
    dplyr::filter(Year < 2024) |>
    injury_case_count(Year, County, Age_Group) |>
    complete(Year, County, Age_Group, fill = list(n = 0L)) |>
    arrange(Year, County, Age_Group) |> # returns 99 counties, and more accurate counts
    left_join(age_group_pops, by = c("County", "Age_Group", "Year")) |>
    rename(Count = n, Stratified_Population = Population) |>
    dplyr::filter(Age_Group != "Missing") |>
    mutate(
      Crude_Rate = round(Count / Stratified_Population, digits = 6),
      .after = County_Population
    ) |>
    left_join(us_age_pops_clean, by = "Age_Group") |>
    mutate(Adjusted_Rate_Component = round(Crude_Rate * Weight, digits = 6)) |>
    mutate(Rate_Type = "case_Count")
}

# get injury counts and case counts by year and age group

{
  # injury counts by age group
  iowa_injury_counts_age <- trauma_data_clean |>
    dplyr::filter(
      Year < 2024,
      grepl(pattern = "^ia$|iowa", x = Injury_State, ignore.case = T),
      !is.na(Injury_County),
      Injury_County %not_in%
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
    complete(Year, Age_Group, fill = list(n = 0L)) |>
    arrange(Year, Age_Group) |> # returns 99 counties, and more accurate counts
    dplyr::filter(Age_Group != "Missing") |>
    left_join(state_age_group_pops, by = c("Age_Group", "Year")) |>
    left_join(
      us_age_pops_clean,
      by = "Age_Group",
      suffix = c("_state", "_standard")
    ) |>
    rename(Count = n) |>
    mutate(
      Crude_Rate = round(Count / Population_state, digits = 6),
      .after = Population_state
    ) |>
    mutate(Adjusted_Rate_Component = round(Crude_Rate * Weight, digits = 6))

  # get case counts

  iowa_case_counts_age <- trauma_data_clean |>
    dplyr::filter(Year < 2024) |>
    injury_case_count(Year, Age_Group) |>
    complete(Year, Age_Group, fill = list(n = 0L)) |>
    arrange(Year, Age_Group) |> # returns 99 counties, and more accurate counts
    dplyr::filter(Age_Group != "Missing") |>
    left_join(state_age_group_pops, by = c("Age_Group", "Year")) |>
    left_join(
      us_age_pops_clean,
      by = "Age_Group",
      suffix = c("_state", "_standard")
    ) |>
    rename(Count = n) |>
    mutate(
      Crude_Rate = round(Count / Population_state, digits = 6),
      .after = Population_state
    ) |>
    mutate(Adjusted_Rate_Component = round(Crude_Rate * Weight, digits = 6))
}

# check for missingness
# if this check produces many missing age_groups
# go back to the trauma_data_final file and
# review the DOB / Incident dates to see what is driving
# the missingness

trauma_counts_na <- injury_counts |>
  dplyr::filter(if_any(everything(), ~ is.na(.)))

# check the trauma_data_final file for missing DOB / Incident_Date

trauma_data_na <- trauma_data_clean |>
  dplyr::filter(if_any(c(Patient_DOB, Incident_Date), ~ is.na(.)))

# run this to get a big picture of where the NAs are at and if it seems to be truly unusual before

trauma_data_clean |>
  distinct(Unique_Incident_ID, .keep_all = T) |>
  count(Year, Age_Group) |>
  dplyr::filter(Age_Group == "Missing") |>
  print(n = Inf)

###_____________________________________________________________________________
# Trauma data age adjustments ----
###_____________________________________________________________________________

# rates summarized by year and county

{
  # injuries
  injury_rates <- injury_counts |>
    summarize(
      Count = sum(Count, na.rm = T),
      Rate_Category = "Injury_Count",
      Crude_Rate = round(
        (sum(Count, na.rm = T) / sum(Stratified_Population, na.rm = T)) *
          100000,
        digits = 1
      ),
      Age_Adj_Injury_Rate = round(
        sum(Adjusted_Rate_Component, na.rm = TRUE) * 100000,
        digits = 1
      ),
      County_Population = min(County_Population, na.rm = T),
      .by = c(Year, Injury_County)
    ) |>
    left_join(
      location_data |> dplyr::select(County, Designation, Urbanicity),
      by = c("Injury_County" = "County")
    ) |>
    mutate(
      pretty_label = if_else(
        Year %in% c(2018, 2022, 2023),
        pretty_number(x = Age_Adj_Injury_Rate, n_decimal = 1),
        ""
      ),
      .by = Injury_County
    )

  # cases / incidents
  case_rates <- case_counts |>
    summarize(
      Count = sum(Count, na.rm = T),
      Rate_Category = "case_Count",
      Crude_Rate = round(
        (sum(Count, na.rm = T) / sum(Stratified_Population, na.rm = T)) *
          100000,
        digits = 1
      ),
      Age_Adj_Trauma_case_Rate = round(
        sum(Adjusted_Rate_Component, na.rm = TRUE) * 100000,
        digits = 1
      ),
      County_Population = min(County_Population, na.rm = T),
      .by = c(Year, County)
    ) |>
    left_join(
      location_data |> dplyr::select(County, Designation, Urbanicity),
      by = "County"
    ) |>
    mutate(
      pretty_label = if_else(
        Year %in% c(2018, 2022, 2023),
        pretty_number(x = Age_Adj_Trauma_case_Rate, n_decimal = 1),
        ""
      ),
      .by = County
    )

  # union the by year and county age adjusted rates

  injury_case_rates <- bind_rows(
    injury_rates |> rename(Age_Adj_Rate = Age_Adj_Injury_Rate),
    case_rates |> rename(Age_Adj_Rate = Age_Adj_Trauma_case_Rate)
  )
}

###_____________________________________________________________________________
# Plot the by county, by year rates ----
###_____________________________________________________________________________

# injuries by county and year - urban

{
  injury_rates_plot_urban <- injury_rates |>
    dplyr::filter(Designation == "Urban") |>
    ggplot(aes(
      x = Year,
      y = Age_Adj_Injury_Rate,
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
      title = "Urban Age Adjusted Injury* Rates by County and Year",
      subtitle = "Bureau of Emergency Medical and Trauma Services | Division of Public Health | Iowa HHS",
      caption = "- Data: Iowa Trauma Registry via ImageTrend\n- All rates are age adjusted per 100,000 population using US 2000 standard pops\n- *Injury incidence rate refers to the incidence of injury events resulting in evaluation/treatment at a verified trauma center based on the injury county"
    ) +
    guides(color = "none") +
    scale_x_continuous(labels = 2018:2023) +
    scale_color_viridis_d(option = "viridis")

  plot_save_params(
    filename = "injury_rates_plot_urban.png",
    plot = injury_rates_plot_urban,
    path = plot_path
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

###_____________________________________________________________________________
# Get the unique responses for the ICD-10 causes of injury to make categories ----
###_____________________________________________________________________________

cause_of_injury_trauma <- trauma_2023 |>
  injury_case_count(CAUSE_OF_INJURY_AR_1, sort = T) |>
  mutate(percent = percent(n / sum(n)))

# check missingness in the cause of injury annual report field

cause_of_injury_missing <- trauma_2023 |>
  distinct(Unique_Incident_ID, .keep_all = T) |>
  dplyr::filter(is.na(CAUSE_OF_INJURY_AR_1))

# find unique injury codes that are missing

injury_codes_missing <- cause_of_injury_missing |>
  distinct(Unique_Incident_ID, .keep_all = T) |>
  dplyr::select(Mechanism_Injury_1_code) |>
  count(Mechanism_Injury_1_code) |>
  arrange(Mechanism_Injury_1_code)

###_____________________________________________________________________________
# Get counts ----
###_____________________________________________________________________________

# patients

{
  patient_count <- trauma_2023 |>
    injury_patient_count() |>
    pull(n)

  patient_count_years <- trauma_data_clean |>
    injury_patient_count(Year) |>
    mutate(
      increase = round((n - lag(n)) / lag(n), digits = 3),
      label = pretty_percent((n - lag(n)) / lag(n), n_decimal = 0.1)
    )
}

# injuries

{
  incident_count <- trauma_2023 |>
    injury_incident_count() |>
    pull(n)

  incident_count_years <- trauma_data_clean |>
    injury_incident_count(Year) |>
    mutate(
      increase = round((n - lag(n)) / lag(n), digits = 3),
      label = pretty_percent((n - lag(n)) / lag(n), n_decimal = 0.1)
    )
}

# cases

{
  case_count <- trauma_2023 |>
    injury_case_count() |>
    pull(n)

  case_count_years <- trauma_data_clean |>
    injury_case_count(Year) |>
    mutate(
      increase = round((n - lag(n)) / lag(n), digits = 3),
      label = pretty_percent((n - lag(n)) / lag(n), n_decimal = 0.1)
    )
}

###_____________________________________________________________________________
# Transfers and transfer delay ----
###_____________________________________________________________________________

{
  transfer_delays_2023 <- trauma_data_clean |>
    dplyr::filter(Year %in% 2022:2023, Transfer_Out == "Yes", ) |>
    distinct(Unique_Incident_ID, .keep_all = T) |>
    mutate(
      Final_LOS = impute(
        Final_LOS,
        focus = "skew",
        method = "iqr",
        direction = "upper"
      ),
      Final_LOS = impute(
        Final_LOS,
        focus = "missing",
        method = "median",
        direction = "upper"
      ),
      .by = Year
    ) |>
    summarize(
      Delayed_2hr = sum(Final_LOS > 120, na.rm = T),
      Delayed_3hr = sum(Final_LOS > 180, na.rm = T),
      Timely_2hr = sum(Final_LOS < 120, na.rm = T),
      Timely_3hr = sum(Final_LOS < 180, na.rm = T),
      Total = n(),
      Percent_Delay_2hr = Delayed_2hr / Total,
      Percent_Delay_3hr = Delayed_3hr / Total,
      Percent_Timely_2hr = Timely_2hr / Total,
      Percent_Timely_3hr = Timely_3hr / Total,
      .by = Year
    ) |>
    left_join(
      trauma_data_clean |>
        dplyr::filter(Year %in% 2022:2023) |>
        injury_case_count(Year, name = "Total cases"),
      by = "Year"
    )
}

###_____________________________________________________________________________
# Gender ----
###_____________________________________________________________________________

# get gender data

gender_group <- trauma_data_clean |>
  dplyr::filter(Year %in% 2022:2023) |>
  injury_incident_count(Year, Patient_Gender) |>
  replace_na(list(Patient_Gender = "Missing")) |>
  mutate(Proportion = (n / sum(n)) * 100, .by = Year) |>
  mutate(
    Patient_Gender = if_else(
      grepl(pattern = "non-binary", x = Patient_Gender, ignore.case = T),
      "Non-Binary",
      Patient_Gender
    )
  )

# create a table visualization using gt()

gender_group_tbl <- {
  gender_group |>
    mutate(
      n = small_count_label(
        var = n,
        cutoff = 6,
        replacement = NA_integer_
      )
    ) |>
    gt(groupname_col = "Year", rowname_col = "Patient_Gender") |>
    sub_missing(columns = n) |>
    fmt_number(columns = n, drop_trailing_zeros = T) |>
    tab_header(
      title = "Summary: Injury Events by Gender",
      subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2022-2023"
    ) |>
    cols_label(n = "# Injury Events") |>
    gt_plt_bar_pct(
      column = Proportion,
      scaled = T,
      labels = T,
      decimals = 2,
      height = 20,
      width = 125
    ) |>
    row_group_order(groups = c("2022", "2023")) |>
    tab_footnote(
      footnote = "All counts and other measures are related to reinjured patients, only.",
      locations = cells_row_groups()
    ) |>
    tab_footnote(
      footnote = "Injury event refers to the number of unique injury incidents that led to evaluation/treatment at a verified trauma center.  Each injury event could involve multiple cases, and each patient may have one or more injury events in a specified timespan.",
      locations = cells_column_labels(columns = n)
    ) |>
    tab_footnote(
      footnote = "Refers to the proportion of injuries attributed to reinjured patients in a given year",
      locations = cells_column_labels(columns = Proportion)
    ) |>
    tab_footnote(
      footnote = "Small counts < 6 are masked to protect confidentiality",
      locations = cells_body(columns = n, rows = 3)
    ) |>
    opt_footnote_marks(marks = "extended") |>
    tab_style_hhs(border_cols = n:Proportion)
}

###_____________________________________________________________________________
# Age ----
###_____________________________________________________________________________

# source df

{
  age_group <- trauma_data_clean |>
    mutate(
      Age_Range = replace_na(Age_Range, "Missing"),
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
    mutate(
      percent = n / sum(n),
      percent_label = pretty_percent(n / sum(n), n_decimal = 0.1),
      .by = Year
    ) |>
    arrange(Age_Range) |>
    mutate(
      change = (n - lag(n)) / lag(n),
      change_label = pretty_percent(change, n_decimal = 0.1),
      .by = Age_Range
    ) |>
    arrange(Year, Age_Range)
}

# df for printing
age_group_filtered <- age_group |>
  dplyr::filter(Year %in% 2022:2023)

# df for the plot
age_group_plot_df <- age_group |>
  dplyr::filter(Year %in% 2021:2023, Age_Range != "Missing") |>
  mutate(
    category = if_else(
      change > 0,
      "Increase",
      if_else(change < 0, "Decrease", "Neutral")
    )
  )

# produce the plot

{
  age_group_plot <- age_group_plot_df |>
    ggplot(aes(
      factor(Year),
      change,
      group = Age_Range,
      fill = category,
      label = change_label
    )) +
    geom_col(color = "black", width = 0.8) +
    geom_text_repel(
      size = 6,
      direction = "y",
      family = "Work Sans SemiBold",
      nudge_y = if_else(age_group_plot_df$change < 0, -0.035, 0.025),
      segment.color = NA
    ) +
    labs(
      title = "Percent Change in Injury Events by Age Group",
      subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2021-2023",
      caption = "Injury event refers to the number of unique injury incidents that led to evaluation/treatment at a verified trauma center.\nEach injury event could involve multiple cases, and each patient may have one or more injury events in a specified timespan.",
      x = "",
      y = "% Change",
      fill = "Change Type"
    ) +
    facet_wrap(~Age_Range) +
    guides(color = "none") +
    scale_y_continuous(labels = function(x) pretty_percent(x)) +
    scale_fill_viridis_d(option = "inferno") +
    theme_cleaner_facet(
      base_size = 15,
      vjust_title = 2,
      vjust_subtitle = 1.25,
      facet_text_size = 16,
      title_text_size = 20,
      subtitle_text_size = 18,
      draw_panel_border = T
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
    ggplot(aes(
      factor(Year),
      n,
      group = Age_Range,
      color = Age_Range,
      label = pretty_number(x = n, n_decimal = 2)
    )) +
    geom_line(linewidth = 2, lineend = "round", linejoin = "round") +
    geom_point(size = 3, color = "black") +
    geom_text_repel(
      size = 6,
      segment.color = NA,
      direction = "y",
      color = "black",
      family = "Work Sans SemiBold"
    ) +
    labs(
      title = "Count of Unique Injury Events by Age Group",
      subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2021-2023",
      caption = "Injury event refers to the number of unique injury incidents that led to evaluation/treatment at a verified trauma center.\nEach injury event could involve multiple cases, and each patient may have one or more injury events in a specified timespan.",
      x = "",
      y = "Injury Event Count",
      fill = "Change Type"
    ) +
    facet_wrap(~Age_Range) +
    guides(color = "none", fill = "none") +
    scale_y_continuous(
      breaks = waiver(),
      labels = function(x) pretty_number(x, n_decimal = 2)
    ) +
    scale_color_paletteer_d("colorBlindness::Blue2Orange12Steps") +
    theme_cleaner_facet(
      base_size = 15,
      vjust_title = 2,
      vjust_subtitle = 1.25,
      facet_text_size = 16,
      title_text_size = 20,
      subtitle_text_size = 18,
      draw_panel_border = T
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
    distinct(Unique_Incident_ID, .keep_all = TRUE) |>
    mutate(
      Patient_Race = if_else(
        grepl(pattern = "not|select", x = Patient_Race, ignore.case = T),
        "Not Known/Not Recorded",
        if_else(
          grepl(pattern = "american indian", x = Patient_Race, ignore.case = T),
          "AIAN",
          if_else(
            grepl(
              pattern = "native hawaiian",
              x = Patient_Race,
              ignore.case = T
            ),
            "NHOPI",
            Patient_Race
          )
        )
      ),
      Patient_Race = replace_na(Patient_Race, "Not Known/Not Recorded")
    ) |>
    injury_incident_count(Year, Patient_Race, name = "Injury_Events") |>
    complete(Year, Patient_Race, fill = list(Injury_Events = 0)) |>
    injury_mutate() |>
    arrange(Patient_Race) |>
    mutate(
      change = (Injury_Events - lag(Injury_Events)) / lag(Injury_Events),
      change_label = pretty_percent(change, n_decimal = 0.01),
      .by = Patient_Race
    ) |>
    mutate(
      change = if_else(change == Inf, 0, change),
      change_label = if_else(change_label == Inf, "0%", change_label)
    ) |>
    replace_na(list(change = 0, change_label = "0%"))
}

# df for plotting

race_group_plot_df <- race_group |>
  dplyr::filter(Year %in% 2021:2023) |>
  mutate(normal_events = normalize(Injury_Events), .by = Patient_Race)

# df for printing

race_group_filtered <- race_group |>
  dplyr::filter(Year %in% 2022:2023)

# build the race group column plot

{
  race_group_plot <- race_group_plot_df |>
    dplyr::filter(Patient_Race != "Not Known/Not Recorded") |>
    mutate(category = if_else(change > 0, "Increase", "Decrease")) |>
    ggplot(aes(
      factor(Year),
      change,
      group = Patient_Race,
      fill = category,
      label = change_label
    )) +
    geom_col(color = "black", width = 0.8) +
    geom_text_repel(
      size = 6,
      segment.color = NA,
      nudge_y = if_else(race_group_plot_df$change < 0, -0.1, 0.1),
      direction = "y",
      family = "Work Sans SemiBold",
      max.iter = 30000
    ) +
    labs(
      title = "Percent Change in Injury Events by Patient Race",
      subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2022-2023",
      caption = "Injury event refers to the number of unique injury incidents that led to evaluation/treatment at a verified trauma center.\nEach injury event could involve multiple cases, and each patient may have one or more injury events in a specified timespan.",
      x = "",
      y = "% Change",
      fill = "Change Type"
    ) +
    facet_wrap(~Patient_Race) +
    guides(color = "none") +
    scale_y_continuous(labels = function(x) pretty_percent(x)) +
    scale_fill_viridis_d(option = "inferno") +
    theme_cleaner_facet(
      base_size = 15,
      vjust_title = 2,
      vjust_subtitle = 1.25,
      facet_text_size = 16,
      title_text_size = 20,
      subtitle_text_size = 18,
      draw_panel_border = T
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
    ggplot(aes(
      factor(Year),
      Injury_Events,
      group = Patient_Race,
      color = Patient_Race,
      label = pretty_number(x = Injury_Events, n_decimal = 2)
    )) +
    geom_line(
      linewidth = 2,
      lineend = "round",
      linejoin = "round",
      alpha = 0.9
    ) +
    geom_point(size = 4, color = "black") +
    geom_text_repel(
      size = 6,
      nudge_y = if_else(race_group_plot_df$Patient_Race == "Asian", 3, 8),
      max.iter = 30000,
      direction = "x",
      segment.color = NA,
      color = "black",
      family = "Work Sans SemiBold"
    ) +
    labs(
      title = "Count of Unique Injury Events by Patient Race",
      subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2021-2023",
      caption = "Note: Scale does not reach zero and each subplot may have a different scale.\nInjury event refers to the number of unique injury incidents that led to evaluation/treatment at a verified trauma center.\nEach injury event could involve multiple cases, and each patient may have one or more injury events in a specified timespan.",
      x = "",
      y = "Injury Event Count",
    ) +
    facet_wrap(~Patient_Race, scales = "free_y") +
    guides(color = "none", fill = "none") +
    scale_y_continuous(
      breaks = waiver(),
      labels = function(x) pretty_number(x, n_decimal = 2)
    ) +
    scale_color_paletteer_d("colorBlindness::Blue2DarkOrange12Steps") +
    theme_cleaner_facet(
      vjust_title = 2,
      vjust_subtitle = 1.25,
      facet_text_size = 16,
      title_text_size = 20,
      subtitle_text_size = 18,
      draw_panel_border = T
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
  group_by(Year) |>
  arrange(desc(n), .by_group = T) |>
  ungroup() |>
  na.omit() |>
  mutate(percent = n / sum(n, na.rm = T), .by = Year) |>
  mutate(
    label_num = small_count_label(var = n, cutoff = 6, replacement = "*"),
    percent_label = if_else(
      label_num == "*",
      "*",
      if_else(
        n == 6,
        pretty_percent(percent, n_decimal = 0.01),
        pretty_percent(percent, n_decimal = 0.1)
      )
    )
  ) |>
  dplyr::filter(Year > 2020, Year < 2024)

# leading causes of injury events
leading_causes_years <- trauma_data_clean |>
  injury_incident_count(Year, CAUSE_OF_INJURY_AR_1) |>
  group_by(Year) |>
  arrange(desc(n), .by_group = T) |>
  ungroup() |>
  na.omit() |>
  mutate(percent = n / sum(n, na.rm = T), .by = Year) |>
  mutate(
    label_num = small_count_label(var = n, cutoff = 6, replacement = "*"),
    percent_label = if_else(
      label_num == "*",
      "*",
      if_else(
        n == 6,
        pretty_percent(percent, n_decimal = 0.01),
        pretty_percent(percent, n_decimal = 0.1)
      )
    )
  )

# for printing

leading_causes_recent <- leading_causes_years |>
  dplyr::filter(Year %in% 2022:2023)

# get color order

legend_order = leading_causes_years |>
  dplyr::filter(Year > 2020, Year < 2024) |>
  distinct(CAUSE_OF_INJURY_AR_1) |>
  pull()

# plot of leading causes by year

{
  leading_causes_plot <- leading_causes_years |>
    dplyr::filter(Year > 2020, Year < 2024) |>
    ggplot(aes(
      reorder(CAUSE_OF_INJURY_AR_1, -percent),
      percent,
      fill = CAUSE_OF_INJURY_AR_1,
      label = percent_label
    )) +
    geom_col(alpha = 0.75, position = "dodge", width = 0.5, color = "black") +
    geom_text_repel(
      size = 5.5,
      direction = "y",
      segment.color = NA,
      color = hhs_palette_1$primary_1,
      fontface = "bold",
      nudge_y = 0.01
    ) +
    labs(
      fill = "Cause of Injury",
      title = "Comparing Leading Causes of Injury by Year",
      subtitle = "Proportion of Total Injury Events is Column Height and Label | Years: 2019-2023",
      caption = "\nAll facets are scaled the same, starting at 0.\n'*' indicates small counts that are masked to protect confidentiality\nData: Iowa Trauma Registry | Bureau of Emergency Medical and Trauma Services | Division of Public Health | Iowa HHS",
      x = "",
      y = ""
    ) +
    guides(fill = guide_legend(nrow = 1), color = "none") +
    scale_y_continuous(labels = function(x) pretty_percent(x)) +
    scale_fill_paletteer_d(
      palette = "colorBlindness::PairedColor12Steps",
      breaks = legend_order
    ) +
    theme_cleaner_facet(
      axis.text.x = element_blank(),
      legend_position = "bottom",
      vjust_title = 2,
      vjust_subtitle = 1,
      title_text_size = 20,
      subtitle_text_size = 18,
      base_size = 15,
      facet_text_size = 15,
      draw_panel_border = T
    ) +
    facet_wrap(~Year)

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
    distinct(Unique_Incident_ID, .keep_all = T) |>
    summarize(
      Falls = sum(
        grepl(pattern = "fall", x = CAUSE_OF_INJURY_AR_1, ignore.case = T),
        na.rm = T
      ),
      cases = n(),
      percent = round(Falls / cases, digits = 3),
      label = pretty_percent(Falls / cases, n_decimal = 0.1),
      .by = Year
    ) |>
    mutate(
      increase = round((Falls - lag(Falls)) / lag(Falls), digits = 3),
      increase_label = pretty_percent(
        (Falls - lag(Falls)) / lag(Falls),
        n_decimal = 0.01
      )
    )
}

fall_related_injuries <- {
  trauma_data_clean |>
    distinct(Unique_Patient_ID, Incident_Date, .keep_all = T) |>
    summarize(
      Falls = sum(
        grepl(pattern = "fall", x = CAUSE_OF_INJURY_AR_1, ignore.case = T),
        na.rm = T
      ),
      Injuries = n(),
      percent = round(Falls / Injuries, digits = 3),
      label = pretty_percent(Falls / Injuries, n_decimal = 0.1),
      .by = Year
    ) |>
    mutate(
      increase = round((Falls - lag(Falls)) / lag(Falls), digits = 3),
      increase_label = pretty_percent(
        (Falls - lag(Falls)) / lag(Falls),
        n_decimal = 0.01
      )
    )
}

###_____________________________________________________________________________
# Motor vehicle, boating, and air incidents ----
###_____________________________________________________________________________

motor_vehicle_related_cases <- {
  trauma_data_clean |>
    distinct(Unique_Incident_ID, .keep_all = T) |>
    summarize(
      MVC = sum(
        grepl(
          pattern = "mvc/transport",
          x = CAUSE_OF_INJURY_AR_1,
          ignore.case = T
        ),
        na.rm = T
      ),
      cases = n(),
      percent = round(MVC / cases, digits = 4),
      .by = Year
    ) |>
    mutate(
      increase = round((MVC - lag(MVC)) / lag(MVC), digits = 3),
      increase_label = pretty_percent(
        (MVC - lag(MVC)) / lag(MVC),
        n_decimal = 0.01
      )
    )
}

motor_vehicle_related_injuries <- {
  trauma_data_clean |>
    distinct(Unique_Patient_ID, Incident_Date, .keep_all = T) |>
    summarize(
      MVC = sum(
        grepl(
          pattern = "mvc/transport",
          x = CAUSE_OF_INJURY_AR_1,
          ignore.case = T
        ),
        na.rm = T
      ),
      Injury_Events = n(),
      percent = round(MVC / Injury_Events, digits = 4),
      .by = Year
    ) |>
    mutate(
      increase = round((MVC - lag(MVC)) / lag(MVC), digits = 3),
      increase_label = pretty_percent(
        (MVC - lag(MVC)) / lag(MVC),
        n_decimal = 0.01
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
    set_names(nm = c("Category", 2018:2023)) |>
    mutate(
      Category = c(
        "MVC Injury Event Count",
        "Total Injury Events",
        "Proportion of Injuries",
        "% Change in MVC Injuries"
      )
    ) |>
    mutate(
      `2018-2023 Trend` = list(c(
        `2018`,
        `2019`,
        `2020`,
        `2021`,
        `2022`,
        `2023`
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
      column = `2018-2023 Trend`,
      type = "shaded",
      same_limit = F,
      label = F
    ) |>
    fmt_percent(rows = 3:4) |>
    fmt_number(rows = 1:2, drop_trailing_zeros = T) |>
    tab_header(
      title = "Summary: Trend of Motor Vehicle Injury Events ",
      subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2018-2023"
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
  reinjury_patient_count(descriptive_stats = T)

# cases

reinjuries_stats_cases <- trauma_data_clean |>
  reinjury_case_count(descriptive_stats = T)

# injury events

reinjury_stats_injuries <- trauma_data_clean |>
  reinjury_injury_count(descriptive_stats = T)

# create a gt() table to be explored in the report

reinjury_stat_tbl_object <- reinjury_stats_injuries |>
  dplyr::select(-matches("_label")) |>
  dplyr::filter(Year < 2024) |>
  replace_na(list(change = 0)) |>
  t() |>
  as.data.frame() |>
  set_names(nm = c(2018:2023)) |>
  rownames_to_column(var = "Category") |>
  dplyr::filter(Category != "Year") |>
  rowwise() |>
  mutate(
    `2018-2023 Trend` = list(c(`2018`, `2019`, `2020`, `2021`, `2022`, `2023`))
  ) |>
  ungroup() |>
  dplyr::select(-c(`2018`, `2019`, `2020`))

# generate the gt() table

reinjury_stat_tbl <- {
  reinjury_stat_tbl_object |>
    mutate(
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
      column = `2018-2023 Trend`,
      same_limit = F,
      type = "shaded",
      label = F
    ) |>
    fmt_number(
      columns = everything(),
      rows = c(1:2, 5:7),
      drop_trailing_zeros = T
    ) |>
    fmt_percent(columns = everything(), rows = 3:4, drop_trailing_zeros = T) |>
    tab_header(
      title = "Summary: Trend of Reinjury in Iowa",
      subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2018-2023"
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
    mutate(
      Financial_Work_Related = if_else(
        Financial_Work_Related != "Yes" | is.na(Financial_Work_Related),
        "No",
        Financial_Work_Related
      )
    ) |>
    injury_case_count(Year, Financial_Work_Related, name = "cases") |>
    case_mutate() |>
    dplyr::filter(Financial_Work_Related == "Yes") |>
    mutate(
      increase = round((cases - lag(cases)) / lag(cases), digits = 3),
      increase_label = pretty_percent(
        (cases - lag(cases)) / lag(cases),
        n_decimal = 0.01
      )
    )
}

work_related_injuries <- {
  trauma_data_clean |>
    mutate(
      Financial_Work_Related = if_else(
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
    mutate(
      increase = round(
        (Injury_Events - lag(Injury_Events)) / lag(Injury_Events),
        digits = 3
      ),
      increase_label = pretty_percent(
        (Injury_Events - lag(Injury_Events)) / lag(Injury_Events),
        n_decimal = 0.01
      )
    )
}

###_____________________________________________________________________________
# farm related ----
###_____________________________________________________________________________

farm_related_cases <- {
  trauma_data_clean |>
    mutate(
      Farm_Ag_Related = if_else(
        Farm_Ag_Related != "Yes" | is.na(Farm_Ag_Related),
        "No",
        Farm_Ag_Related
      )
    ) |>
    injury_case_count(Year, Farm_Ag_Related, name = "cases") |>
    case_mutate() |>
    dplyr::filter(Farm_Ag_Related == "Yes") |>
    mutate(
      increase = round((cases - lag(cases)) / lag(cases), digits = 3),
      increase_label = pretty_percent(
        (cases - lag(cases)) / lag(cases),
        n_decimal = 0.01
      )
    )
}

farm_related_injuries <- {
  trauma_data_clean |>
    mutate(
      Farm_Ag_Related = if_else(
        Farm_Ag_Related != "Yes" | is.na(Farm_Ag_Related),
        "No",
        Farm_Ag_Related
      )
    ) |>
    injury_incident_count(Year, Farm_Ag_Related, name = "Injury_Events") |>
    injury_mutate() |>
    dplyr::filter(Farm_Ag_Related == "Yes") |>
    mutate(
      increase = round(
        (Injury_Events - lag(Injury_Events)) / lag(Injury_Events),
        digits = 3
      ),
      increase_label = pretty_percent(
        (Injury_Events - lag(Injury_Events)) / lag(Injury_Events),
        n_decimal = 0.01
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
    replace_na(list(INTENTIONALITY_1 = "Categorization Not Possible")) |>
    distinct(Incident_Date, Unique_Patient_ID, .keep_all = T) |>
    summarize(
      `Intentional Injury Events` = sum(
        !grepl(
          pattern = "categorization|unintentional",
          x = INTENTIONALITY_1,
          ignore.case = T
        ),
        na.rm = T
      ),
      `Unintentional Injury Events` = sum(
        grepl(pattern = "unintentional", x = INTENTIONALITY_1, ignore.case = T),
        na.rm = T
      ),
      `Categorization Not Possible` = sum(
        grepl(
          pattern = "categorization",
          x = INTENTIONALITY_1,
          ignore.case = T
        ),
        na.rm = T
      ),
      `Total Categorized Injury Events` = `Intentional Injury Events` +
        `Unintentional Injury Events`,
      `Total Injury Events` = n(),
      `% Intentional Injury Events` = `Intentional Injury Events` /
        `Total Categorized Injury Events`,
      `% Unintentional Injury Events` = `Unintentional Injury Events` /
        `Total Categorized Injury Events`,
      .by = Year
    ) |>
    mutate(
      `% Change Intentional Injury Events` = (`Intentional Injury Events` -
        lag(`Intentional Injury Events`)) /
        lag(`Intentional Injury Events`),
      `% Change Unintentional Injury Events` = (`Unintentional Injury Events` -
        lag(`Unintentional Injury Events`)) /
        lag(`Unintentional Injury Events`)
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
  replace_na(list(`2018` = 0)) |>
  mutate(
    `2018-2023 Trend` = list(c(`2018`, `2019`, `2020`, `2021`, `2022`, `2023`)),
    .by = Category
  ) |>
  dplyr::select(-c(`2018`:`2020`))

# develop the gt() table for intentionality of injury

intentionality_of_injury_tbl <- {
  intentionality_of_injury_pivot |>
    gt() |>
    cols_label(Category = "") |>
    gt_plt_sparkline(
      column = `2018-2023 Trend`,
      type = "shaded",
      same_limit = F,
      label = F
    ) |>
    fmt_number(columns = everything(), rows = 1:5, drop_trailing_zeros = T) |>
    fmt_percent(columns = everything(), rows = 6:9, drop_trailing_zeros = T) |>
    tab_header(
      title = "Summary: Trend of Intentional/Unintentional Injury Events in Iowa",
      subtitle = "Patients Seen at a Trauma Center | Data: Iowa Trauma Registry 2018-2023"
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
    mutate(
      Trauma_Team_Activation_Level = if_else(
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
      Trauma_Team_Activation_Level = if_else(
        Trauma_Team_Activation_Level == "Level 3",
        "Level 2",
        Trauma_Team_Activation_Level
      )
    ) |>
    injury_case_count(Year, Trauma_Team_Activation_Level) |>
    complete(Year, Trauma_Team_Activation_Level, fill = list(n = 0)) |>
    mutate(
      percent = n / sum(n, na.rm = T),
      percent_label = pretty_percent(percent, n_decimal = 0.1),
      .by = Year
    ) |>
    arrange(Trauma_Team_Activation_Level) |>
    mutate(
      change = (n - lag(n)) / lag(n),
      change_label = pretty_percent(change),
      .by = Trauma_Team_Activation_Level
    ) |>
    mutate(
      change = if_else(
        Year > 2018 &
          Year <= 2021 &
          Trauma_Team_Activation_Level == "Non-Trauma",
        0,
        change
      ),
      change_label = if_else(
        Year > 2018 & Trauma_Team_Activation_Level == "Non-Trauma",
        "0%",
        change_label
      )
    )
}

# df for printing

trauma_activation_cases_recent <- trauma_activation_cases |>
  dplyr::filter(Year %in% 2022:2023)

# overall activations

{
  trauma_activation_cases_binary <- trauma_data_clean |>
    dplyr::filter(Year < 2024) |>
    mutate(
      Trauma_Team_Activation_Level = if_else(
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
    distinct(Unique_Incident_ID, .keep_all = T) |>
    summarize(
      Activations = sum(
        Trauma_Team_Activation_Level %in% c("Level 1", "Level 2", "Level 3"),
        na.rm = T
      ),
      `Not Activated` = sum(
        Trauma_Team_Activation_Level %in%
          c("Consultation", "Not Activated", "Non-Trauma"),
        na.rm = T
      ),
      Missing = sum(Trauma_Team_Activation_Level == "Missing", na.rm = T),
      `Records Not Missing Activation Data` = Activations + `Not Activated`,
      cases = n(),
      .by = Year
    ) |>
    mutate(
      percent_activations = Activations / `Records Not Missing Activation Data`,
      percent_label = pretty_percent(percent_activations),
      .by = Year
    ) |>
    mutate(
      change_activations = (Activations - lag(Activations)) / lag(Activations),
      change_activations_label = pretty_percent(change_activations),
      change_non_activations = (`Not Activated` - lag(`Not Activated`)) /
        lag(`Not Activated`),
      change_non_activations_label = pretty_percent(change_non_activations)
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
    mutate(
      `2018-2023 Trend` = list(c(
        `2018`,
        `2019`,
        `2020`,
        `2021`,
        `2022`,
        `2023`
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
    mutate(
      `2018-2023 Trend` = list(c(
        change_2018,
        change_2019,
        change_2020,
        change_2021,
        change_2022,
        change_2023
      )),
      .by = Trauma_Team_Activation_Level
    ) |>
    dplyr::select(-matches("_20(18|19|20)"))
}

# gt() table for overall trauma activation stats

trauma_activation_cases_overall_tbl <- {
  trauma_activation_cases_binary_pivot |>
    mutate(
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
      column = `2018-2023 Trend`,
      type = "shaded",
      same_limit = F,
      label = F
    ) |>
    fmt_percent(rows = 6:8) |>
    fmt_number(rows = 1:5, drop_trailing_zeros = T) |>
    tab_header(
      title = "Summary: Trend of Overall Trauma Team Activation Statistics",
      subtitle = "Trauma Center case Data | Data: Iowa Trauma Registry 2018-2023"
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
      n_2022 = "Count",
      n_2023 = "Count",
      percent_2021 = "% Cases",
      percent_2022 = "% Cases",
      percent_2023 = "% Cases",
      change_2021 = "% Change",
      change_2022 = "% Change",
      change_2023 = "% Change"
    ) |>
    gt_plt_sparkline(
      column = `2018-2023 Trend`,
      type = "shaded",
      same_limit = F,
      label = F
    ) |>
    tab_spanner(label = "2021", columns = matches("_2021")) |>
    tab_spanner(label = "2022", columns = matches("_2022")) |>
    tab_spanner(label = "2023", columns = matches("_2023")) |>
    fmt_percent(columns = matches("percent|change")) |>
    fmt_number(columns = matches("n_"), drop_trailing_zeros = T) |>
    tab_header(
      title = "Summary: Trend of Trauma Team Activation Level Statistics",
      subtitle = "Trauma Center Case Data | Data: Iowa Trauma Registry 2018-2023"
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
      footnote = "Reflects trend in % change of case count for each activation level category from 2018-2023.",
      locations = cells_column_labels(columns = `2018-2023 Trend`)
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
    distinct(Unique_Run_ID, .keep_all = T) |>
    count(Year, name = "Incidents") |>
    mutate(
      change_incident = (Incidents - lag(Incidents)) / lag(Incidents),
      change_incident_label = pretty_percent(change_incident)
    )
}

# get a unique count of total resources used

{
  ems_runs <- ems_data_clean |>
    dplyr::filter(Year < 2024) |>
    distinct(Unique_Run_ID, .keep_all = T) |>
    count(Year, name = "Runs") |>
    mutate(
      change_runs = (Runs - lag(Runs)) / lag(Runs),
      change_runs_label = pretty_percent(change_runs)
    )
}

# bind columns of the incidents and runs table

ems_incidents_runs <- ems_incidents |>
  left_join(ems_runs, by = "Year")

# ems incidents for printing

ems_incidents_runs_recent <- ems_incidents_runs |>
  dplyr::filter(Year %in% 2022:2023)

# Get All Transports count by filtering Disposition Incident Patient Disposition by only values in disposition_incident_patient_disposition

# transport incidents

{
  transport_incidents <- ems_data_clean |>
    dplyr::filter(Scene_First_EMS_Unit_On_Scene == "Yes", Year < 2024) |>
    count(Year, Patient_Transported) |>
    dplyr::filter(Patient_Transported == T) |>
    left_join(
      ems_incidents |>
        dplyr::filter(Year < 2024) |>
        dplyr::select(Year, Incidents),
      by = "Year"
    ) |>
    mutate(
      percent_incidents = n / Incidents,
      change_incidents = (n - lag(n)) / lag(n)
    )
}

# transport runs

{
  transport_runs <- ems_data_clean |>
    dplyr::filter(Year < 2024) |>
    distinct(Unique_Run_ID, .keep_all = T) |>
    count(Year, Patient_Transported) |>
    dplyr::filter(Patient_Transported == T) |>
    left_join(
      ems_runs |> dplyr::filter(Year < 2024) |> dplyr::select(Year, Runs),
      by = "Year"
    ) |>
    mutate(percent_runs = n / Runs, change_runs = (n - lag(n)) / lag(n))
}

# join the transport runs and incidents table

transport_incidents_runs <- transport_incidents |>
  rename(Transport_Incidents = n) |>
  left_join(
    transport_runs |> rename(Transport_Runs = n),
    by = c("Patient_Transported", "Year")
  ) |>
  dplyr::select(-Patient_Transported)

# transports for printing

transport_incidents_runs_recent <- transport_incidents_runs |>
  dplyr::filter(Year %in% 2022:2023)

# Get trauma related ems responses

{
  ems_trauma_incidents <- ems_data_clean |>
    dplyr::filter(
      Trauma_Flag == "Yes",
      Scene_First_EMS_Unit_On_Scene == "Yes",
      Year < 2024
    ) |>
    distinct(Unique_Run_ID, .keep_all = T) |>
    count(Year, name = "Incidents") |>
    mutate(
      change_incident = (Incidents - lag(Incidents)) / lag(Incidents),
      change_incident_label = pretty_percent(change_incident)
    )
}

# Get n trauma runs

{
  ems_trauma_runs <- ems_data_clean |>
    dplyr::filter(Trauma_Flag == "Yes", Year < 2024) |>
    distinct(Unique_Run_ID, .keep_all = T) |>
    count(Year, name = "Runs") |>
    mutate(
      change_runs = (Runs - lag(Runs)) / lag(Runs),
      change_runs_label = pretty_percent(change_runs)
    )
}

# ems trauma join incidents and runs

ems_trauma_incidents_runs <- ems_trauma_incidents |>
  left_join(ems_trauma_runs, by = "Year")

# ems trauma stats for printing

ems_trauma_incidents_runs_recent <- ems_trauma_incidents_runs |>
  dplyr::filter(Year %in% 2022:2023)

# Get trauma related ems transport runs

{
  ems_trauma_transport_incidents <- ems_data_clean |>
    dplyr::filter(
      Trauma_Flag == "Yes",
      Scene_First_EMS_Unit_On_Scene == "Yes",
      Year < 2024
    ) |>
    distinct(Unique_Run_ID, .keep_all = T) |>
    count(Year, Patient_Transported) |>
    dplyr::filter(Patient_Transported == T) |>
    left_join(
      ems_trauma_incidents |> dplyr::select(Year, Incidents),
      by = "Year"
    ) |>
    mutate(
      percent_trauma_transport_incidents = n / Incidents,
      change_trauma_transport_incidents = (Incidents - lag(Incidents)) /
        lag(Incidents)
    )
}

# Get trauma related ems transport incidents

{
  ems_trauma_transport_runs <- ems_data_clean |>
    dplyr::filter(Trauma_Flag == "Yes", Year < 2024) |>
    distinct(Unique_Run_ID, .keep_all = T) |>
    count(Year, Patient_Transported) |>
    dplyr::filter(Patient_Transported == T) |>
    left_join(ems_trauma_runs |> dplyr::select(Year, Runs), by = "Year") |>
    mutate(
      percent_trauma_transport_runs = n / Runs,
      change_trauma_transport_runs = (Runs - lag(Runs)) / lag(Runs)
    )
}

# join the ems trauma transport incident and runs tables

ems_trauma_transport_incidents_runs <- ems_trauma_transport_incidents |>
  rename(Trauma_Transport_Incidents = n) |>
  left_join(
    ems_trauma_transport_runs |> rename(Trauma_Transport_Runs = n),
    by = c("Patient_Transported", "Year")
  ) |>
  dplyr::select(-Patient_Transported)

# trauma transports for printing

ems_trauma_transport_incidents_runs_recent <- ems_trauma_transport_incidents_runs |>
  dplyr::filter(Year %in% 2022:2023)

####
# Main report section ----
####

# Trauma facility count by trauma level df

trauma_facility_count_by_level <- trauma_2023 |>
  distinct(Level, Facility_Name) |>
  count(Level, name = "Count")

# Trauma facility count by trauma level plot

{
  trauma_facility_count_by_level_plot <- trauma_facility_count_by_level |>
    ggplot(aes(Level, Count, fill = Level, label = Count)) +
    geom_col(color = "black") +
    geom_text_repel(
      direction = "y",
      color = "black",
      nudge_y = if_else(trauma_facility_count_by_level$Count < 10, 1, 0),
      size = 8,
      family = "Work Sans",
      segment.color = NA,
      max.iter = 30000
    ) +
    guides(fill = "none") +
    labs(
      title = "Trauma Facility Count by Trauma Facility Verification Level",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023",
      x = "",
      y = "Count of Facilities"
    ) +
    theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 2,
      vjust_subtitle = 1.25
    ) +
    scale_fill_colorblind()

  # save the plot

  plot_save_params(
    filename = "trauma_facility_count_by_level_plot.png",
    plot = trauma_facility_count_by_level_plot,
    path = plot_path
  )
}

# Count of cases by trauma facility level

{
  trauma_cases_by_facility_level <- trauma_2023 |>
    dplyr::filter(Level %in% c("I", "II", "III", "IV")) |>
    injury_case_count(Level) |>
    mutate(
      percent = n / sum(n),
      percent_label = pretty_percent(percent, n_decimal = 0.1)
    )

  # plot Count of cases by trauma facility level

  trauma_cases_by_facility_level_plot <- trauma_cases_by_facility_level |>
    ggplot(aes(
      x = Level,
      y = n,
      fill = Level,
      label = prettyNum(n, big.mark = ",")
    )) +
    geom_col(color = "black") +
    geom_text_repel(
      direction = "y",
      nudge_y = 1,
      color = "black",
      size = 8,
      family = "Work Sans",
      segment.color = NA,
      max.iter = 30000
    ) +
    geom_text(
      aes(y = 300, label = percent_label),
      color = "white",
      family = "Work Sans",
      size = 8
    ) +
    guides(fill = "none") +
    labs(
      title = "Case Count by Trauma Facility Verification Level",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023",
      caption = " These data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a patient enters\n an ED or hospital for treatment of an injury.",
      x = "",
      y = "Count of Cases"
    ) +
    theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 2,
      vjust_subtitle = 1.25
    ) +
    scale_y_continuous(labels = function(x) pretty_number(x, n_decimal = 1)) +
    scale_fill_colorblind()

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
    arrange(Level) |>
    mutate(change = (n - lag(n)) / lag(n), .by = Level)
}

# count of incidents by definitive care facility level

trauma_cases_by_def_care_level <- trauma_2023 |>
  dplyr::filter(Level %in% c("I", "II", "III", "IV"), Receiving == "Yes") |>
  injury_case_count(Level) |>
  mutate(
    n_label = prettyNum(n, big.mark = ","),
    percent = n / sum(n),
    percent_label = pretty_percent(percent, n_decimal = 0.1)
  )

# plot the count of incidents by definitive care facility level

{
  trauma_cases_by_def_care_level_plot <- trauma_cases_by_def_care_level |>
    ggplot(aes(
      x = reorder(Level, n),
      y = n,
      color = Level,
      label = paste0(n_label, " (", percent_label, ")")
    )) +
    geom_col(
      color = "white",
      fill = "darkslategray",
      width = 0.03,
      alpha = 0.75
    ) +
    geom_point(size = 8) +
    geom_text(
      aes(y = if_else(Level == "I", n - 225, n + 275)),
      nudge_x = if_else(trauma_cases_by_def_care_level$Level == "I", -0.175, 0),
      color = "black",
      family = "Work Sans",
      size = 8
    ) +
    labs(
      x = "",
      y = "",
      title = "Count of Receiving Facility Cases by Trauma Facility Verification Level",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023",
      caption = "These data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a patient enters\nan ED or hospital for treatment of an injury."
    ) +
    coord_flip() +
    guides(color = "none") +
    theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.5,
      vjust_subtitle = 1
    ) +
    scale_y_continuous(labels = function(x) pretty_number(x, n_decimal = 2)) +
    scale_color_colorblind()

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
  transport_mode_to_facility <- trauma_2023 |>
    mutate(
      Transport_To_Your_Facility_By = if_else(
        grepl(
          pattern = "not known|not applicable",
          x = Transport_To_Your_Facility_By,
          ignore.case = T
        ),
        "Not Known/Not Recorded",
        if_else(
          grepl(
            pattern = "ALS|BLS",
            x = Transport_To_Your_Facility_By,
            ignore.case = F
          ),
          "Ground Ambulance",
          Transport_To_Your_Facility_By
        )
      )
    ) |>
    injury_case_count(Transport_To_Your_Facility_By) |>
    arrange(desc(n)) |>
    mutate(
      Transport_To_Your_Facility_By = str_wrap(
        Transport_To_Your_Facility_By,
        whitespace_only = F,
        width = 20
      ),
      number_label = pretty_number(
        small_count_label(var = n, cutoff = 6, replacement = NA_integer_),
        n_decimal = 2
      ),
      percent = n / sum(n),
      percent_label = pretty_percent(percent)
    )
}

# transport mode to facility plot

{
  transport_mode_to_facility_plot <- transport_mode_to_facility |>
    ggplot(aes(
      x = reorder(Transport_To_Your_Facility_By, n),
      y = n,
      label = paste0(number_label, " (", percent_label, ")")
    )) +
    geom_col(color = "black", alpha = 0.5, fill = "dodgerblue1") +
    coord_flip() +
    geom_text(
      aes(y = if_else(n > 5000, 0, n)),
      family = "Work Sans",
      size = 8,
      nudge_y = if_else(
        transport_mode_to_facility$n > 5000,
        2100,
        if_else(
          transport_mode_to_facility$n < 5000 &
            transport_mode_to_facility$n > 1000,
          transport_mode_to_facility$n + (transport_mode_to_facility$n * 0.35),
          1500
        )
      )
    ) +
    guides(fill = "none") +
    labs(
      x = "",
      y = "\n",
      title = "Count of Cases by Transport Mode to Facility",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023",
      caption = "These data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a\npatient enters an ED or hospital for treatment of an injury."
    ) +
    theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 2,
      vjust_subtitle = 1.25
    ) +
    scale_y_continuous(labels = function(x) pretty_number(x, n_decimal = 1))

  # save the transport mode plot

  plot_save_params(
    filename = "transport_mode_to_facility_plot.png",
    plot = transport_mode_to_facility_plot,
    path = plot_path
  )
}

# transport mode to facility among receiving facilities df

{
  transport_mode_to_facility_receiving <- trauma_2023 |>
    dplyr::filter(Receiving == "Yes") |>
    mutate(
      Transport_To_Your_Facility_By = if_else(
        grepl(
          pattern = "not known|not applicable",
          x = Transport_To_Your_Facility_By,
          ignore.case = T
        ),
        "Not Known/Not Recorded",
        if_else(
          grepl(
            pattern = "ALS|BLS",
            x = Transport_To_Your_Facility_By,
            ignore.case = F
          ),
          "Ground Ambulance",
          Transport_To_Your_Facility_By
        )
      )
    ) |>
    injury_case_count(Transport_To_Your_Facility_By) |>
    arrange(desc(n)) |>
    mutate(
      Transport_To_Your_Facility_By = str_wrap(
        Transport_To_Your_Facility_By,
        whitespace_only = F,
        width = 20
      ),
      number_label = if_else(
        n >= 6,
        prettyNum(
          small_count_label(var = n, cutoff = 6, replacement = NA_integer_),
          big.mark = ","
        ),
        "*"
      ),
      percent = n / sum(n),
      percent_label = if_else(number_label == "*", "*", pretty_percent(percent))
    )
}

# transport mode to facility among receiving facilities plot

{
  transport_mode_to_facility_receiving_plot <- transport_mode_to_facility_receiving |>
    ggplot(aes(
      x = reorder(Transport_To_Your_Facility_By, n),
      y = n,
      label = paste0(number_label, " (", percent_label, ")")
    )) +
    geom_col(color = "black", alpha = 0.5, fill = "coral") +
    coord_flip() +
    geom_text(
      aes(y = if_else(n > 700, 0, n)),
      family = "Work Sans",
      size = 8,
      nudge_y = if_else(
        transport_mode_to_facility_receiving$n > 1000,
        450,
        if_else(
          transport_mode_to_facility_receiving$n < 1000 &
            transport_mode_to_facility_receiving$n > 700,
          350,
          if_else(
            transport_mode_to_facility_receiving$n < 700 &
              transport_mode_to_facility_receiving$n > 10,
            325,
            200
          )
        )
      )
    ) +
    guides(fill = "none") +
    labs(
      x = "",
      y = "\n",
      title = "Count of Cases by Transport Mode to Among Receiving Facilities",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023",
      caption = "These data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a\npatient enters an ED or hospital for treatment of an injury.\n'*' is used to mask counts < 6 to protect confidentiality."
    ) +
    theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 2,
      vjust_subtitle = 1.25
    ) +
    scale_y_continuous(labels = function(x) pretty_number(x, n_decimal = 1))

  # save the transport mode plot

  plot_save_params(
    filename = "transport_mode_to_facility_receiving_plot.png",
    plot = transport_mode_to_facility_receiving_plot,
    path = plot_path
  )
}

# case count by ISS range

{
  case_count_iss_range_df <- trauma_2023 |>
    dplyr::filter(!is.na(ISS_Range)) |>
    mutate(
      ISS_Range = factor(
        ISS_Range,
        levels = c("1 - 8", "9 - 15", "16+"),
        labels = c("1-8", "9-15", "16+")
      ),
      Level = factor(Level, levels = c("I", "II", "III", "IV"))
    ) |>
    injury_case_count(ISS_Range, Level) |>
    arrange(ISS_Range, Level) |>
    mutate(
      number_label = prettyNum(n, big.mark = ","),
      percent = n / sum(n),
      percent_label = pretty_percent(percent),
      full_label = paste0(number_label, " (", percent_label, ")"),
      .by = ISS_Range
    )
}

# case count by ISS range plot

{
  case_count_iss_range_plot <- case_count_iss_range_df |>
    ggplot(aes(x = fct_rev(Level), y = n, fill = Level, label = full_label)) +
    geom_col(color = "black", alpha = 0.75) +
    geom_text(
      aes(y = if_else(n > 1400, 0, n)),
      nudge_y = if_else(case_count_iss_range_df$n > 1400, 525, 500),
      family = "Work Sans",
      size = 7,
      color = if_else(
        case_count_iss_range_df$n > 1400,
        "white",
        "black"
      )
    ) +
    coord_flip() +
    facet_grid(rows = vars(ISS_Range), switch = "y") +
    labs(
      x = "",
      y = "\n",
      title = "Count of Cases by ISS Range and Trauma Facility Verification Level",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023",
      caption = "These data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a patient enters an ED or\nhospital for treatment of an injury.\nProportions in each ISS Range row sum to 100%."
    ) +
    guides(fill = "none") +
    theme_cleaner_facet(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 2,
      vjust_subtitle = 1.25,
      facet_text_size = 18,
      strip.placement = "outside",
      draw_panel_border = T
    ) +
    scale_fill_colorblind() +
    scale_y_continuous(labels = function(x) pretty_number(x, n_decimal = 1))

  plot_save_params(
    filename = "case_count_iss_range_plot.png",
    plot = case_count_iss_range_plot,
    path = plot_path
  )
}

# case count by ISS range and facility level at receiving facilities

{
  case_count_iss_range_receiving_df <- trauma_2023 |>
    dplyr::filter(!is.na(ISS_Range), Receiving == "Yes") |>
    mutate(
      ISS_Range = factor(
        ISS_Range,
        levels = c("1 - 8", "9 - 15", "16+"),
        labels = c("1-8", "9-15", "16+")
      ),
      Level = factor(Level, levels = c("I", "II", "III", "IV"))
    ) |>
    injury_case_count(ISS_Range, Level) |>
    arrange(ISS_Range, Level) |>
    mutate(
      number_label = prettyNum(n, big.mark = ","),
      percent = n / sum(n),
      percent_label = pretty_percent(percent),
      full_label = paste0(number_label, " (", percent_label, ")"),
      .by = ISS_Range
    )
}


# case count by ISS range and facility level at receiving facilities plot

{
  case_count_iss_range_receiving_plot <- case_count_iss_range_receiving_df |>
    ggplot(aes(x = fct_rev(Level), y = n, fill = Level, label = full_label)) +
    geom_col(color = "black", alpha = 0.75) +
    geom_text(
      aes(y = if_else(n > 200, 0, n)),
      nudge_y = if_else(case_count_iss_range_receiving_df$n > 200, 100, 90),
      family = "Work Sans",
      size = 7,
      color = if_else(
        case_count_iss_range_receiving_df$n > 200,
        "white",
        "black"
      )
    ) +
    coord_flip() +
    facet_grid(rows = vars(ISS_Range), switch = "y") +
    labs(
      x = "",
      y = "\n",
      title = "Count of Cases by ISS Range and Receiving Trauma Facility Verification Level",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023",
      caption = "These data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a patient enters an ED or\nhospital for treatment of an injury.\nProportions in each ISS Range row sum to 100%."
    ) +
    guides(fill = "none") +
    theme_cleaner_facet(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 2,
      vjust_subtitle = 1.25,
      facet_text_size = 18,
      strip.placement = "outside",
      draw_panel_border = T
    ) +
    scale_fill_colorblind() +
    scale_y_continuous(labels = function(x) pretty_number(x, n_decimal = 1))

  # save the plot

  plot_save_params(
    filename = "case_count_iss_range_receiving_plot.png",
    plot = case_count_iss_range_receiving_plot,
    path = plot_path
  )
}

# cause of injury frequency collapsed categories

{
  cause_of_injury_freq <- trauma_2023 |>
    mutate(Level = factor(Level, levels = c("I", "II", "III", "IV"))) |>
    dplyr::filter(Level %in% c("I", "II", "III", "IV")) |>
    injury_case_count(Level, CAUSE_OF_INJURY_AR_1) |>
    arrange(Level, desc(n)) |>
    mutate(number_label = prettyNum(n, big.mark = ",")) |>
    drop_na()
}

# cause of injury frequency collapsed categories plot

{
  cause_of_injury_freq_plot <- cause_of_injury_freq |>
    ggplot(aes(
      x = reorder(Level, n),
      y = n,
      fill = CAUSE_OF_INJURY_AR_1,
      label = if_else(n < 200, "", number_label)
    )) +
    geom_col(color = "black", alpha = 0.75, position = "stack") +
    geom_text(
      position = position_stack(vjust = 0.5),
      size = 8,
      color = "black",
      family = "Work Sans",
      fontface = "bold",
      angle = if_else(cause_of_injury_freq$n < 500, 90, 0)
    ) +
    labs(
      x = "",
      y = "Case Count\n",
      title = "Cause of Injury Frequency by Trauma Facility Verification Level",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023",
      caption = "- These data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a patient enters an ED or\n   hospital for treatment of an injury.\n- As the colors descend in the legend from top to bottom, so are the colors ordered in the bars from right to left."
    ) +
    coord_flip() +
    theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 2,
      vjust_subtitle = 1,
      legend_position = "inside",
      legend.position.inside = c(.75, .25)
    ) +
    theme(legend.title = element_blank()) +
    scale_fill_paletteer_d(palette = "colorblindr::OkabeIto_black") +
    scale_y_continuous(labels = function(x) pretty_number(x, n_decimal = 1))

  plot_save_params(
    filename = "cause_of_injury_freq_plot.png",
    plot = cause_of_injury_freq_plot,
    path = plot_path
  )
}

# additional cause of injury frequency df

{
  injuries_not_needed_pattern <- c("fall|motor|mvc|firearm|struck")

  cause_of_injury_additional_freq <- trauma_2023 |>
    dplyr::filter(
      !is.na(LEVEL_FALL1_1),
      !grepl(
        pattern = injuries_not_needed_pattern,
        x = LEVEL_FALL1_1,
        ignore.case = T
      )
    ) |>
    injury_case_count(LEVEL_FALL1_1, sort = T) |>
    mutate(
      LEVEL_FALL1_1 = case_when(
        LEVEL_FALL1_1 == "Other Specified, Unintentional" ~
          "Other-Unintentional",
        LEVEL_FALL1_1 == "Other Specified, Assault" ~ "Other-Assault",
        LEVEL_FALL1_1 == "Poisoning, Non-Drug" ~ "Poisoning Non-Drug",
        TRUE ~ LEVEL_FALL1_1
      ),
      number_label = small_count_label(var = n, cutoff = 6, replacement = "*"),
      full_label = if_else(
        n < 50,
        paste0(LEVEL_FALL1_1, " (", number_label, ")"),
        paste0(LEVEL_FALL1_1, "\n(", number_label, ")")
      )
    )
}

# additional cause of injury frequency treemap using treemapify package

{
  cause_of_injury_additional_freq_plots <- cause_of_injury_additional_freq |>
    ggplot(aes(area = n, label = full_label, fill = n)) +
    geom_treemap(color = "white", layout = "squarified", start = "bottomleft") +
    geom_treemap_text(
      family = "Work Sans",
      size = 18,
      color = "white",
      fontface = "bold"
    ) +
    guides(fill = "none") +
    labs(
      title = "Cause of Injury Frequency with Expanded Categories",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023",
      caption = "Read the order of factors by changing color and box area, signaling decreasing count, from the bottom left to top right.\nThese data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a patient enters an ED or hospital\nfor treatment of an injury."
    ) +
    theme_cleaner(
      title_text_size = 20,
      subtitle_text_size = 18,
      base_size = 15,
      vjust_title = 2,
      vjust_subtitle = 1
    ) +
    scale_fill_viridis(option = "cividis", direction = -1)

  # save the plot

  plot_save_params(
    filename = "cause_of_injury_additional_freq_plots.png",
    plot = cause_of_injury_additional_freq_plots,
    path = plot_path
  )
}

# transfers out by trauma level df

{
  transfers_out_by_trauma_lvl <- trauma_2023 |>
    dplyr::filter(Transfer_Out == "Yes") |>
    injury_case_count(Level) |>
    mutate(
      number_label = prettyNum(n, big.mark = ","),
      full_label = paste0(Level, " (", number_label, ")"),
      size_mod = log(n)
    )
}

# transfers out by trauma level plot

{
  transfers_out_by_trauma_lvl_plot <- transfers_out_by_trauma_lvl |>
    ggplot(aes(Level, n, fill = Level, label = full_label)) +
    geom_point(
      shape = 21,
      color = "black",
      size = 6 * transfers_out_by_trauma_lvl$size_mod
    ) +
    geom_text(
      family = "Work Sans",
      size = 7,
      fontface = "bold",
      color = "white"
    ) +
    coord_flip() +
    guides(color = "none", size = "none", fill = "none") +
    labs(
      title = "Cases Transferring Out by Trauma Verification Level",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023",
      x = "",
      y = "Case Count",
      caption = "These data reflect cases, and so include transfers.  Cases are defined as each distinct episode when a patient enters an ED or hospital\nfor treatment of an injury."
    ) +
    theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      axis.text.y = element_blank()
    ) +
    scale_y_continuous(
      labels = function(x) pretty_number(x, n_decimal = 1),
      limits = c(-100, 5000)
    ) +
    scale_fill_colorblind()

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

  transfer_delays_transfer_out <- trauma_2023 |>
    dplyr::filter(Transfer_Out == "Yes") |>
    replace_na(list(Transfer_Delay_Reason = "Not Applicable")) |>
    mutate(
      Transfer_Delay_Reason = if_else(
        grepl(
          pattern = "select|not\\sknown|not\\sapplicable",
          x = Transfer_Delay_Reason,
          ignore.case = T
        ),
        "Missing",
        Transfer_Delay_Reason
      )
    ) |>
    injury_case_count(Transfer_Delay_Reason, sort = T) |>
    mutate(number_label = prettyNum(n, big.mark = ","), size_mod = log(n))

  # df for plotting

  transfer_delays_transfer_out_main <- transfer_delays_transfer_out |>
    dplyr::filter(Transfer_Delay_Reason %not_in% c("Missing", "Other"))
}

# get the 'missing' and 'other' values for the transfer delay reasons

{
  missing_transfer_delays <- transfer_delays_transfer_out |>
    dplyr::filter(Transfer_Delay_Reason == "Missing") |>
    pull(n)

  other_transfer_delays <- transfer_delays_transfer_out |>
    dplyr::filter(Transfer_Delay_Reason == "Other") |>
    pull(n)
}

# transfer delays among patients being transferred out plot

{
  transfer_delays_transfer_out_plot <- transfer_delays_transfer_out_main |>
    mutate(
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
    ggplot(aes(
      reorder(Transfer_Delay_Reason, n),
      n,
      fill = n,
      label = small_count_label(var = n, cutoff = 6, replacement = "*")
    )) +
    geom_col(color = "black", width = 0.5) +
    geom_text(
      nudge_y = if_else(
        transfer_delays_transfer_out_main$n > 100,
        transfer_delays_transfer_out_main$size_mod * 2.5,
        if_else(
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
    coord_flip() +
    guides(fill = "none") +
    labs(
      title = "Transfer Delay Reasons Among Patients Being Transferred Out",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023",
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
    scale_fill_viridis(option = "magma", direction = -1) +
    scale_y_continuous(labels = function(x) pretty_number(x, n_decimal = 1)) +
    theme_cleaner(
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
  avg_ed_stay_transfers_iss <- trauma_2023 |>
    dplyr::filter(Transfer_Out == "Yes") |>
    distinct(Unique_Incident_ID, .keep_all = T) |>
    mutate(
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
      Length_of_Stay = impute(
        Length_of_Stay,
        method = "winsorize",
        percentile = 0.90,
        direction = "upper"
      ),
      Length_of_Stay = impute(
        Length_of_Stay,
        focus = "missing",
        method = "mean"
      ),
      .by = ISS_Range
    ) |>
    summarize(
      median_los = median(Length_of_Stay, na.rm = T),
      avg_los = mean(Length_of_Stay, na.rm = T),
      .by = c(Trauma_Team_Activated, ISS_Range)
    ) |>
    mutate(mod = log(avg_los)) |>
    arrange(Trauma_Team_Activated, ISS_Range)

  # get differences

  avg_ed_stay_transfers_iss_diff <- avg_ed_stay_transfers_iss |>
    dplyr::select(-c(median_los, mod)) |>
    pivot_wider(
      id_cols = ISS_Range,
      names_from = Trauma_Team_Activated,
      values_from = avg_los
    ) |>
    mutate(diff = abs(Activated - `Not Activated`))

  # get overall avg diff

  avg_diff_ed_los <- avg_ed_stay_transfers_iss_diff |>
    summarize(mean = round(mean(diff), digits = 1)) |>
    pull(mean)
}

# average ED stay prior to transfer by ISS range plot

{
  avg_ed_stay_transfers_iss_plot <- avg_ed_stay_transfers_iss |>
    ggplot(aes(
      x = fct_relevel(ISS_Range, rev(levels(ISS_Range))),
      y = avg_los,
      fill = ISS_Range,
      label = pretty_number(avg_los, n_decimal = 1)
    )) +
    geom_col(width = 0.5, color = "black") +
    geom_text(
      nudge_y = avg_ed_stay_transfers_iss$avg_los /
        (avg_ed_stay_transfers_iss$mod * 2.5),
      family = "Work Sans",
      size = 8,
      color = "black"
    ) +
    facet_grid(rows = vars(Trauma_Team_Activated), switch = "y") +
    coord_flip() +
    labs(
      title = "Average ED Length of Stay in Minutes Prior to Transfer by ISS Range",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023",
      caption = "- ED LOS calculated from datetime of patient arrival to datetime of physical discharge.\n- These data reflect cases, which include transfers.  Cases are defined as each distinct episode when a patient enters an ED\n   or hospital for treatment of an injury.\n- Imputation methods: Winsorization at 10th / 90th percentiles, then mean imputation on missing values.",
      x = "",
      y = "Case Count\n",
      fill = "ISS Range"
    ) +
    scale_fill_paletteer_d(palette = "colorBlindness::PairedColor12Steps") +
    theme_cleaner_facet(
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
      draw_panel_border = T
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
  avg_ED_LOS_transfer_iss_level <- trauma_2023 |>
    dplyr::filter(Transfer_Out == "Yes") |>
    distinct(Unique_Incident_ID, .keep_all = T) |>
    mutate(
      ISS_Range = factor(
        ISS_Range,
        levels = c("1 - 8", "9 - 15", "16+"),
        labels = c("1-8", "9-15", "16+")
      ),
      Length_of_Stay = impute(
        Length_of_Stay,
        method = "winsorize",
        percentile = 0.90,
        direction = "upper"
      ),
      Length_of_Stay = impute(
        Length_of_Stay,
        focus = "missing",
        method = "mean"
      ),
      .by = c(ISS_Range, Level)
    ) |>
    summarize(
      avg_los = mean(Length_of_Stay),
      median_los = median(Length_of_Stay),
      min_los = min(Length_of_Stay),
      max_los = max(Length_of_Stay),
      N = n(),
      N_label = if_else(N < 6, "*", prettyNum(N, big.mark = ",")),
      mod = log(avg_los),
      .by = c(Trauma_Team_Activated, ISS_Range, Level)
    ) |>
    arrange(Trauma_Team_Activated, ISS_Range, Level)
}

# average ED stay in minutes prior to transfer by ISS range and trauma level plot

{
  avg_ED_LOS_transfer_iss_level_plot <- avg_ED_LOS_transfer_iss_level |>
    ggplot(aes(x = ISS_Range, y = avg_los, fill = ISS_Range)) +
    geom_col(color = "black") +
    geom_text_repel(
      aes(label = round(avg_los, digits = 1)),
      nudge_y = if_else(avg_ED_LOS_transfer_iss_level$avg_los > 500, -1, 1),
      direction = "y",
      segment.color = NA,
      size = 8,
      color = if_else(
        avg_ED_LOS_transfer_iss_level$avg_los > 500,
        "white",
        "black"
      ),
      family = "Work Sans"
    ) +
    geom_text(
      aes(y = 65, label = paste0("n = ", N_label)),
      size = 8,
      color = "white",
      family = "Work Sans"
    ) +
    facet_grid(
      rows = vars(Level),
      cols = vars(Trauma_Team_Activated),
      switch = "y"
    ) +
    labs(
      title = "Average ED Length of Stay in Minutes Prior to Transfer by ISS Range and Trauma Level",
      subtitle = "Source: Iowa ImageTrend patient Registry | 2023",
      caption = "- Top value = average ED LOS, bottom value = # cases\n- ED LOS calculated from datetime of patient arrival to datetime of physical discharge.\n- Imputation methods: Winsorization at 10th / 90th percentiles, then mean imputation on missing values.\n- These data reflect cases, which include transfers.  Cases are defined as each distinct episode when a patient enters an ED or\n   hospital for treatment of an injury.",
      x = "",
      y = "",
      fill = "ISS Range"
    ) +
    scale_fill_paletteer_d(
      palette = "colorBlindness::ModifiedSpectralScheme11Steps",
      direction = 1
    ) +
    theme_cleaner_facet(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      facet_text_size = 18,
      strip.placement = "outside",
      axis.text.y = element_blank(),
      draw_panel_border = T
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
    dplyr::filter(Transfer_Out == "Yes", Year %in% 2019:2023) |>
    group_by(Year) |>
    distinct(Unique_Incident_ID, .keep_all = T) |>
    ungroup() |>
    mutate(
      Length_of_Stay = impute(
        Length_of_Stay,
        method = "winsorize",
        percentile = 0.90,
        direction = "upper"
      ),
      Length_of_Stay = impute(
        Length_of_Stay,
        focus = "missing",
        method = "mean"
      ),
      .by = c(Year, Trauma_Team_Activated)
    ) |>
    summarize(
      avg_los = round(mean(Length_of_Stay), digits = 1),
      .by = c(Year, Trauma_Team_Activated)
    ) |>
    arrange(Year, Trauma_Team_Activated) |>
    pivot_wider(
      id_cols = Year,
      names_from = Trauma_Team_Activated,
      values_from = avg_los
    ) |>
    set_names(nm = c("Year", "Activated", "Not Activated")) |>
    mutate(
      label_1 = if_else(
        Year == min(Year) | Year == max(Year),
        Activated,
        NA_real_
      ),
      label_2 = if_else(
        Year == min(Year) | Year == max(Year),
        `Not Activated`,
        NA_real_
      )
    )
}

# year with no activation status strata

{
  longitudinal_avg_ed_los_year <- trauma_data_clean |>
    dplyr::filter(Transfer_Out == "Yes", Year %in% 2019:2023) |>
    group_by(Year) |>
    distinct(Unique_Incident_ID, .keep_all = T) |>
    ungroup() |>
    mutate(
      Length_of_Stay = impute(
        Length_of_Stay,
        method = "winsorize",
        percentile = 0.90,
        direction = "upper"
      ),
      Length_of_Stay = impute(
        Length_of_Stay,
        focus = "missing",
        method = "mean"
      ),
      .by = Year
    ) |>
    summarize(
      avg_los = round(mean(Length_of_Stay), digits = 1),
      median_los = median(Length_of_Stay),
      min_los = min(Length_of_Stay),
      max_los = max(Length_of_Stay),
      N = n(),
      N_label = if_else(N < 6, "*", prettyNum(N, big.mark = ",")),
      mod = log(avg_los),
      .by = Year
    ) |>
    arrange(Year)
}

# longitudinal average ED stay prior to transfer plot

{
  longitudinal_avg_ed_los_plot <- longitudinal_avg_ed_los_year |>
    ggplot(aes(factor(Year), avg_los, fill = factor(Year))) +
    geom_col(alpha = 0.1) +
    geom_text(
      aes(y = 20, label = paste0(avg_los, "\n", "n = ", N_label)),
      family = "Work Sans",
      size = 8,
      color = "black"
    ) +
    geom_line(
      data = longitudinal_avg_ed_los,
      aes(x = factor(Year), y = Activated, color = "dodgerblue", group = 1),
      linewidth = 2.25,
      lineend = "round",
      linejoin = "round"
    ) +
    geom_text_repel(
      data = longitudinal_avg_ed_los,
      aes(x = factor(Year), y = Activated, label = label_1),
      family = "Work Sans",
      size = 8,
      color = "black",
      direction = "y",
      nudge_y = 1,
      max.iter = 30000,
      segment.color = NA
    ) +
    geom_line(
      data = longitudinal_avg_ed_los,
      aes(
        x = factor(Year),
        y = `Not Activated`,
        color = "orangered",
        group = 2
      ),
      linewidth = 2.25,
      lineend = "round",
      linejoin = "round"
    ) +
    geom_text_repel(
      data = longitudinal_avg_ed_los,
      aes(x = factor(Year), y = `Not Activated`, label = label_2),
      family = "Work Sans",
      size = 8,
      color = "black",
      direction = "y",
      nudge_y = 4,
      max.iter = 30000,
      segment.color = NA
    ) +
    guides(fill = "none") +
    labs(
      title = "Longitudinal Average ED Length of Stay Prior to Transfer",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2019-2023",
      caption = "- Top value = average ED LOS, bottom value = # cases\n- ED LOS calculated from datetime of patient arrival to datetime of physical discharge.\n- Imputation methods: Winsorization at 10th / 90th percentiles, then mean imputation on missing values.\n- These data reflect cases.  Cases are defined as each distinct episode when a patient enters an ED or\n   hospital for treatment of an injury.",
      x = "",
      y = ""
    ) +
    scale_fill_viridis_d(option = "cividis", direction = -1) +
    scale_color_manual(
      name = "Activation Status",
      values = c("dodgerblue", "orangered"),
      labels = c("Activated", "Not Activated")
    ) +
    theme_cleaner(
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
  reinjured_patients <- trauma_2023 |>
    distinct(Incident_Date, Unique_Patient_ID, .keep_all = T) |>
    mutate(reinjury = if_else(n() > 1, TRUE, FALSE), .by = Unique_Patient_ID) |>
    dplyr::filter(reinjury == T) |>
    pull(Unique_Patient_ID)
}

# df giving patients and their injury category

{
  injury_category_patients <- trauma_2023 |>
    distinct(Incident_Date, Unique_Patient_ID, .keep_all = T) |>
    mutate(
      reinjured = if_else(
        Unique_Patient_ID %in% reinjured_patients,
        TRUE,
        FALSE
      )
    ) |>
    mutate(
      injury_category = if_else(
        n() < 2,
        paste0(n(), " injury event"),
        paste0(n(), " injury events")
      ),
      .by = Unique_Patient_ID
    ) |>
    dplyr::filter(!is.na(Unique_Patient_ID)) |>
    distinct(Unique_Patient_ID, injury_category)
}

# complete df including reinjury category

{
  reinjured_trauma_2023 <- trauma_2023 |>
    mutate(
      reinjured = if_else(
        Unique_Patient_ID %in% reinjured_patients,
        TRUE,
        FALSE
      )
    ) |>
    distinct(Incident_Date, Unique_Patient_ID, .keep_all = T) |>
    mutate(n_injuries = n(), .by = Unique_Patient_ID) |>
    mutate(
      n_injury_cat = if_else(
        n_injuries < 2,
        paste0(n_injuries, " injury event"),
        paste0(n_injuries, " injury events")
      ),
      n_injury_cat = factor(n_injury_cat)
    )
}

# plot the reinjured patients

{
  reinjured_trauma_2023_plot <- reinjured_trauma_2023 |>
    dplyr::filter(!is.na(Unique_Patient_ID)) |>
    ggplot(aes(
      Unique_Patient_ID,
      n_injuries,
      size = n_injuries,
      fill = n_injuries
    )) +
    geom_point(
      shape = 21,
      color = "black",
      position = position_jitter(),
      alpha = 0.5
    ) +
    labs(
      x = "Reinjured Patients in Registry per Injury Date\n",
      y = paste0("# Injury Events in ", max(reinjured_trauma_2023$Year)),
      fill = "# Injury Events",
      title = "Iowa Trauma Patient Reinjury - All Patients",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023",
      caption = "Reinjured patients are identified by more than one injury date for any unique patient identifier.\nIncreasing color intensity and point size indicate higher injury counts"
    ) +
    guides(size = "none") +
    scale_fill_viridis(option = "turbo", direction = 1) +
    scale_y_continuous(breaks = 1:8, labels = 1:8) +
    theme_cleaner(
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
    filename = "reinjured_trauma_2023_plot.png",
    plot = reinjured_trauma_2023_plot,
    path = plot_path
  )
}

# table of reinjured patients and trend statistics

reinjured_patients_tbl <- trauma_data_clean |>
  reinjury_patient_count(descriptive_stats = T)

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
    mutate(
      `2018-2023 Trend` = list(c(
        `2018`,
        `2019`,
        `2020`,
        `2021`,
        `2022`,
        `2023`
      )),
      .by = Category
    ) |>
    dplyr::select(Category, `2021`:`2018-2023 Trend`) |>
    gt() |>
    gt_plt_sparkline(
      column = `2018-2023 Trend`,
      type = "shaded",
      same_limit = F,
      label = F
    ) |>
    fmt_number(rows = 1:3, drop_trailing_zeros = T) |>
    fmt_percent(rows = 4:5, drop_trailing_zeros = T) |>
    tab_row_group(label = "Counts", rows = 1:2) |>
    tab_row_group(label = "Proportion and Change", rows = 3:5) |>
    row_group_order(groups = c("Counts", "Proportion and Change")) |>
    tab_header(
      title = "Summary: Trend of Reinjured Patients in Iowa",
      subtitle = "Patients Seen at a Trauma Center | Data: iowa Trauma Registry 2018-2023"
    ) |>
    tab_style_hhs(border_cols = `2021`:`2018-2023 Trend`)
}

# gender reinjuries

{
  gender_reinjury_events_tbl <- reinjured_trauma_2023 |>
    dplyr::filter(!is.na(Unique_Patient_ID)) |>
    distinct(Unique_Patient_ID, Patient_Gender, n_injury_cat) |>
    count(Patient_Gender, n_injury_cat) |>
    mutate(
      number_label = prettyNum(
        x = small_count_label(var = n, cutoff = 6, replacement = NA_integer_),
        big.mark = ","
      ),
      mod = log(n)
    ) |>
    replace_with_na(list(number_label = "NA")) |>
    replace_na(list(Patient_Gender = "Missing", number_label = "*"))

  # special gender category cases to be mentioned in the caption of the plot

  non_binary_reinjury <- gender_reinjury_events_tbl |>
    dplyr::filter(grepl(
      pattern = "binary",
      x = Patient_Gender,
      ignore.case = T
    )) |>
    pull(n)

  missing_gender_reinjury <- gender_reinjury_events_tbl |>
    dplyr::filter(grepl(
      pattern = "missing",
      x = Patient_Gender,
      ignore.case = T
    )) |>
    pull(n)
}

# plot gender reinjury patients

{
  gender_reinjury_events_plot <- gender_reinjury_events_tbl |>
    dplyr::filter(Patient_Gender %in% c("Male", "Female")) |>
    ggplot(aes(n_injury_cat, mod, fill = n_injury_cat, label = number_label)) +
    geom_col(width = 0.5, color = "black") +
    geom_text_repel(
      direction = "y",
      nudge_y = 0.25,
      max.iter = 30000,
      segment.color = NA,
      size = 8,
      family = "Work Sans",
      color = "black"
    ) +
    facet_wrap(Patient_Gender ~ .) +
    labs(
      x = "",
      y = "# Patients (log)",
      title = "Iowa Trauma Patient Reinjury by Gender",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023",
      caption = paste0(
        "- '*' indicates counts < 6 masked to protect confidentiality\n- Reinjured patients are identified by more than one injury date for any unique patient identifier.\n- Log scale used for y axis due to the '1 injury event' group outlier, labels are actual reinjured patient counts.\n",
        "- Non-binary/indeterminate gender pts. = ",
        non_binary_reinjury,
        " | Missing gender pts. = ",
        missing_gender_reinjury,
        ". All these patients had 1 injury event in 2023."
      ),
      fill = "Injury Count Category"
    ) +
    scale_fill_viridis_d(option = "turbo", direction = 1) +
    theme_cleaner_facet(
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
      draw_panel_border = T
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

  race_reinjury_events_tbl <- reinjured_trauma_2023 |>
    dplyr::filter(!is.na(Unique_Patient_ID)) |>
    distinct(Unique_Patient_ID, .keep_all = T) |>
    mutate(
      Patient_Race = case_when(
        grepl(pattern = "indian", x = Patient_Race, ignore.case = T) ~ "AIAN",
        grepl(pattern = "black", x = Patient_Race, ignore.case = T) ~ "Black",
        grepl(pattern = "hawaiian", x = Patient_Race, ignore.case = T) ~
          "NHOPI",
        grepl(
          pattern = "select|not\\s(known|applicable)",
          x = Patient_Race,
          ignore.case = T
        ) ~
          "Missing",
        is.na(Patient_Race) ~ "Missing",
        TRUE ~ Patient_Race
      )
    ) |>
    count(Patient_Race, n_injury_cat) |>
    mutate(
      number_label = if_else(n < 6, "*", pretty_number(n, n_decimal = 2)),
      mod = log(n)
    ) |>
    replace_with_na(list(number_label = "NA")) |>
    replace_na(list(number_label = "*"))

  # totals by race of reinjury

  race_reinjury_events_tbl_totals <- reinjured_trauma_2023 |>
    dplyr::filter(!is.na(Unique_Patient_ID)) |>
    distinct(Unique_Patient_ID, .keep_all = T) |>
    mutate(
      Patient_Race = case_when(
        grepl(pattern = "indian", x = Patient_Race, ignore.case = T) ~ "AIAN",
        grepl(pattern = "black", x = Patient_Race, ignore.case = T) ~ "Black",
        grepl(pattern = "hawaiian", x = Patient_Race, ignore.case = T) ~
          "NHOPI",
        grepl(
          pattern = "select|not\\s(known|applicable)",
          x = Patient_Race,
          ignore.case = T
        ) ~
          "Missing",
        is.na(Patient_Race) ~ "Missing",
        TRUE ~ Patient_Race
      )
    ) |>
    dplyr::filter(n_injury_cat != "1 injury event") |>
    count(Patient_Race) |>
    mutate(
      percent = n / sum(n),
      percent_label = pretty_percent(percent, n_decimal = 0.1),
      number_label = if_else(n < 6, "*", pretty_number(n, n_decimal = 2)),
      mod = log(n)
    ) |>
    replace_with_na(list(number_label = "NA")) |>
    replace_na(list(number_label = "*"))

  # save this file to use for reporting

  write_csv(
    x = race_reinjury_events_tbl_totals,
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/reference/race_reinjury_events_tbl_totals.csv"
  )

  # overall patient population race statistics

  trauma_reg_race_pop_stats <- trauma_2023 |>
    mutate(
      Patient_Race = case_when(
        grepl(pattern = "indian", x = Patient_Race, ignore.case = T) ~ "AIAN",
        grepl(pattern = "black", x = Patient_Race, ignore.case = T) ~ "Black",
        grepl(pattern = "hawaiian", x = Patient_Race, ignore.case = T) ~
          "NHOPI",
        grepl(
          pattern = "select|not\\s(known|applicable)",
          x = Patient_Race,
          ignore.case = T
        ) ~
          "Missing",
        is.na(Patient_Race) ~ "Missing",
        TRUE ~ Patient_Race
      )
    ) |>
    injury_patient_count(Patient_Race) |>
    mutate(percent = n / sum(n), percent_label = pretty_percent(percent))

  # save this file to use for reporting

  write_csv(
    x = trauma_reg_race_pop_stats,
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/reference/trauma_reg_race_pop_stats.csv"
  )
}

# create the reinjured pt count by race plot

{
  race_reinjury_events_plot <- race_reinjury_events_tbl |>
    dplyr::filter(Patient_Race != "Missing") |>
    ggplot(aes(
      x = n_injury_cat,
      y = mod,
      fill = n_injury_cat,
      label = number_label,
    )) +
    geom_col(color = "black", position = "dodge") +
    geom_text(
      size = 7,
      family = "Work Sans",
      nudge_y = 0.75
    ) +
    facet_wrap(Patient_Race ~ .) +
    guides(color = "none") +
    labs(
      x = "",
      y = "# Patients (log)",
      fill = "Injury Count Category",
      title = "Count of Reinjured Patients by Race",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023",
      caption = paste0(
        "- '*' indicates counts < 6 masked to protect confidentiality.\n- Reinjured patients are identified by more than one injury date for any unique patient identifier.\n- Log scale used for y axis due to the '1 injury event' group outlier, labels are actual reinjured patient counts."
      )
    ) +
    scale_fill_viridis_d(option = "turbo", direction = 1) +
    theme_cleaner_facet(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      draw_panel_border = T,
      facet_text_size = 18,
      legend_position = "inside",
      legend.position.inside = c(0.65, 0.05),
      legend.direction = "horizontal"
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

  age_reinjury_events_tbl <- reinjured_trauma_2023 |>
    dplyr::filter(!is.na(Unique_Patient_ID)) |>
    distinct(Unique_Patient_ID, .keep_all = T) |>
    mutate(
      Age_Range = replace_na(Age_Range, "Missing"),
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
    count(Age_Range, n_injury_cat) |>
    mutate(
      number_label = if_else(n < 6, "*", pretty_number(n, n_decimal = 1)),
      mod = log(n)
    ) |>
    replace_with_na(list(number_label = "NA")) |>
    replace_na(list(number_label = "*"))

  # totals by age and reinjury

  age_reinjury_events_tbl_totals <- reinjured_trauma_2023 |>
    dplyr::filter(!is.na(Unique_Patient_ID)) |>
    distinct(Unique_Patient_ID, .keep_all = T) |>
    mutate(
      Age_Range = replace_na(Age_Range, "Missing"),
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
    count(Age_Range) |>
    mutate(
      percent = n / sum(n),
      percent_label = pretty_percent(percent, n_decimal = 0.1),
      cum_percent = cumsum(n / sum(n)),
      cum_percent_label = pretty_percent(cum_percent),
      number_label = if_else(n < 6, "*", pretty_number(n, n_decimal = 2)),
      mod = log(n)
    ) |>
    replace_with_na(list(number_label = "NA")) |>
    replace_na(list(number_label = "*"))

  # save this file to use for reporting

  write_csv(
    x = age_reinjury_events_tbl_totals,
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/reference/age_reinjury_events_tbl_totals.csv"
  )

  # overall patient population age statistics

  trauma_reg_age_pop_stats <- trauma_2023 |>
    mutate(
      Age_Range = replace_na(Age_Range, "Missing"),
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
    mutate(percent = n / sum(n), percent_label = pretty_percent(percent))

  # save this file to use for reporting

  write_csv(
    x = trauma_reg_age_pop_stats,
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/reference/trauma_reg_age_pop_stats.csv"
  )
}

# create the reinjured pt count by age plot

{
  age_reinjury_events_plot <- age_reinjury_events_tbl |>
    dplyr::filter(Age_Range != "Missing") |>
    ggplot(aes(
      x = n_injury_cat,
      y = mod,
      fill = n_injury_cat,
      label = number_label
    )) +
    geom_col(color = "black", position = "dodge") +
    geom_text(
      size = 7,
      family = "Work Sans",
      nudge_y = 0.75
    ) +
    facet_wrap(Age_Range ~ .) +
    guides(color = "none") +
    labs(
      x = "",
      y = "# Patients (log)",
      fill = "Injury Count Category",
      title = "Count of Reinjured Patients by Age Group",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023",
      caption = paste0(
        "- '*' indicates counts < 6 masked to protect confidentiality.\n- Reinjured patients are identified by more than one injury date for any unique patient identifier.\n- Log scale used for y axis due to the '1 injury event' group outlier, labels are actual reinjured patient counts."
      )
    ) +
    scale_fill_viridis_d(option = "turbo", direction = 1) +
    theme_cleaner_facet(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      draw_panel_border = T,
      facet_text_size = 18
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
  cause_of_injury_reinjury <- reinjured_trauma_2023 |>
    dplyr::filter(reinjured == T) |>
    injury_incident_count(MECHANISM_1, sort = T) |>
    replace_na(list(MECHANISM_1 = "Missing")) |>
    mutate(
      percent = n / sum(n),
      percent_label = pretty_percent(percent),
      mod = log(n),
      color = if_else(MECHANISM_1 == "Fall", "white", "black")
    )

  # df for plotting

  cause_of_injury_reinjury_complete <- cause_of_injury_reinjury |>
    dplyr::filter(MECHANISM_1 != "Missing") |>
    slice_max(n, n = 15)

  # cause of injury among reinjured patients plot

  cause_of_injury_reinjury_plot <- cause_of_injury_reinjury_complete |>
    ggplot(aes(
      x = reorder(MECHANISM_1, mod),
      y = n,
      fill = n,
      label = if_else(n < 6, "*", prettyNum(n, big.mark = ","))
    )) +
    geom_col(color = "black", width = 0.75) +
    geom_text(
      nudge_y = if_else(
        cause_of_injury_reinjury$n > 1000,
        -cause_of_injury_reinjury$n + 75,
        30
      ),
      size = 8,
      family = "Work Sans",
      color = if_else(
        cause_of_injury_reinjury_complete$MECHANISM_1 == "Fall",
        "white",
        "black"
      )
    ) +
    coord_flip() +
    labs(
      x = "",
      y = "\n# Injury Events among Reinjured Pts.\n",
      title = "Cause of Injury Among Reinjured Patients",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023",
      caption = "Top mechanisms of injury shown and their injury event counts",
      fill = "Higher count gives\ndarker color"
    ) +
    scale_fill_viridis(option = "viridis", direction = -1) +
    theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      axis.text.x = element_blank(),
      axis_lines = T,
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

urbanicity_reinjury <- reinjured_trauma_2023 |>
  replace_na(list(Designation_Patient = "Missing")) |>
  dplyr::filter(Designation_Patient != "Missing") |>
  distinct(Unique_Patient_ID, .keep_all = T) |>
  count(Designation_Patient, reinjured) |>
  #dplyr::filter(reinjured == T) |>
  mutate(percent = n / sum(n))

# plot the proportions of reinjured patients among rural / urban areas

{
  urbanicity_reinjury_plot <- reinjured_trauma_2023 |>
    replace_na(list(Designation_Patient = "Missing")) |>
    dplyr::filter(Designation_Patient != "Missing") |>
    distinct(Unique_Patient_ID, .keep_all = T) |>
    count(Designation_Patient, reinjured) |>
    mutate(percent = n / sum(n), .by = Designation_Patient) |>
    ggplot(aes(
      x = Designation_Patient,
      y = percent,
      fill = reinjured,
      label = pretty_percent(percent)
    )) +
    geom_col(position = "fill", color = "dodgerblue") +
    geom_text(
      color = "white",
      family = "Work Sans",
      size = 8,
      nudge_y = -0.02
    ) +
    labs(
      x = "",
      y = "% Reinjured Patients\n",
      fill = "Patient Reinjured",
      title = "Proportions of Reinjured Patients by Urban/Rural Patient County",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023"
    ) +
    scale_y_continuous(labels = label_percent()) +
    scale_fill_paletteer_d(
      palette = "colorblindr::OkabeIto_black",
      direction = 1
    ) +
    theme_cleaner(
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

urbanicity_reinjury_phat <- reinjured_trauma_2023 |>
  replace_na(list(Designation_Patient = "Missing")) |>
  dplyr::filter(Designation_Patient != "Missing") |>
  distinct(Unique_Patient_ID, .keep_all = T) |>
  summarize(p_hat = mean(reinjured == T, na.rm = T), .by = Designation_Patient)

# get the difference between the probabilities

{
  urbanicity_reinjury_phat_diff <- reinjured_trauma_2023 |>
    mutate(
      reinjured_fct = factor(
        reinjured,
        levels = c("TRUE", "FALSE"),
        labels = c("yes", "no")
      )
    ) |>
    replace_na(list(Designation_Patient = "Missing")) |>
    dplyr::filter(Designation_Patient != "Missing") |>
    mutate(
      Designation_Patient = factor(
        Designation_Patient,
        levels = c("Urban", "Rural")
      )
    ) |>
    distinct(Unique_Patient_ID, .keep_all = T) |>
    specify(reinjured_fct ~ Designation_Patient, success = "yes") |>
    calculate(stat = "diff in props", order = c("Urban", "Rural")) |>
    pull() # urban - rural
}

# complete the test of equal proportions of reinjured patients between rural and urban Iowa locations

{
  urbanicity_reinjury_props <- reinjured_trauma_2023 |>
    replace_na(list(Designation_Patient = "Missing")) |>
    dplyr::filter(Designation_Patient != "Missing") |>
    mutate(
      reinjured = if_else(reinjured == T, "yes", "no"),
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
  urbanicity_reinjury_model <- reinjured_trauma_2023 |>
    replace_na(list(Designation_Patient = "Missing")) |>
    dplyr::filter(Designation_Patient != "Missing") |>
    mutate(
      reinjured = if_else(reinjured == T, "yes", "no"),
      reinjured = factor(reinjured, levels = c("yes", "no"))
    ) |>
    distinct(Unique_Patient_ID, .keep_all = T) |>
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
  mutate(area = if_else(stat <= lower_crit | stat >= upper_crit, TRUE, FALSE))

# Calculate density

density_data <- density(urbanicity_reinjury_model$stat)
density_df <- data.frame(x = density_data$x, y = density_data$y)

# plot the distribution of differences between proportions

{
  urbanicity_reinjury_diff_plot <- urbanicity_reinjury_model_mod |>
    ggplot(aes(
      x = stat,
      label = pretty_number(abs(urbanicity_reinjury_phat_diff), n_decimal = 5)
    )) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 15,
      fill = "lightgray",
      color = "black",
      alpha = 0.5
    ) +
    geom_line(
      data = density_df,
      aes(x = x, y = y),
      color = "blue",
      linewidth = 1.25,
      alpha = 0.5
    ) +
    geom_area(
      data = density_df |> dplyr::filter(x <= lower_crit),
      aes(x = x, y = y),
      fill = "coral1",
      alpha = 0.5
    ) +
    geom_area(
      data = density_df |> dplyr::filter(x >= upper_crit),
      aes(x = x, y = y),
      fill = "coral1",
      alpha = 0.5
    ) +
    geom_area(
      data = density_df |> dplyr::filter(x <= upper_crit, x >= lower_crit),
      aes(x = x, y = y),
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
        pretty_number(urbanicity_reinjury_phat_diff, n_decimal = 5)
      ),
      size = 8,
      family = "Work Sans",
      angle = 90
    ) +
    labs(
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
    theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      axis_lines = T
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

dead_patients <- trauma_2023 |>
  dplyr::filter(Death == T) |>
  distinct(Unique_Patient_ID) |>
  pull()

# df for this analysis
reinjury_mortality_df <- trauma_2023 |>
  mutate(
    reinjured = if_else(Unique_Patient_ID %in% reinjured_patients, TRUE, FALSE),
    dead = if_else(Unique_Patient_ID %in% dead_patients, TRUE, FALSE) # this tells us if the pt. ever died
  ) |>
  distinct(Unique_Patient_ID, .keep_all = T) |>
  left_join(injury_category_patients, by = "Unique_Patient_ID") |>
  mutate(
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
    reinjured_binary = if_else(reinjured == "yes", 1, 0),
    injury_category = factor(injury_category)
  ) |>
  dplyr::filter(!is.na(Unique_Patient_ID))

# observed probability

reinjury_mortality <- reinjury_mortality_df |>
  summarize(N = n(), mortality_rate = mean(dead == "yes"), .by = reinjured) |>
  dplyr::filter(reinjured == T) |>
  mutate(injury_category = NA_character_, .before = N) |>
  arrange(reinjured)

###_____________________________________________________________________________
# Relationship between reinjury, risk definition, and death ----
###_____________________________________________________________________________

# describe the relationship between reinjury, risk definition, and death

reinjury_risk_death <- reinjury_mortality_df |>
  summarize(
    N = n(),
    mortality_rate = mean(dead == "yes"),
    .by = c(reinjured, injury_category)
  ) |>
  arrange(reinjured, injury_category)

# create a gt() table to illustrate differences in mortality between reinjured / singularly injured groups
# and the different risk definitions groups

{
  reinjury_risk_death_tbl <- bind_rows(reinjury_risk_death, reinjury_mortality)

  reinjury_risk_death_gt <- reinjury_risk_death_tbl |>
    dplyr::select(-reinjured) |>
    mutate(
      injury_category = if_else(
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
    fmt_number(columns = N, drop_trailing_zeros = T) |>
    fmt_percent(columns = mortality_rate, drop_trailing_zeros = T) |>
    tab_row_group(rows = 1, label = "Singularly Injured Pts.") |>
    tab_row_group(rows = c(2:7), label = "Reinjured Pts.") |>
    row_group_order(groups = c("Singularly Injured Pts.", "Reinjured Pts.")) |>
    sub_missing(columns = injury_category) |>
    sub_missing(columns = N) |>
    tab_header(
      title = "Differences in Mortality Rate Among Reinjured / Singularly Injured Pts.",
      subtitle = "Source: Iowa ImageTrend Patient Registry | 2023"
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
  pull()

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
    ggplot(aes(
      x = stat,
      label = pretty_number(abs(reinjury_risk_death_diff), n_decimal = 5)
    )) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 15,
      fill = "lightgray",
      color = "black",
      alpha = 0.5
    ) +
    geom_line(
      data = reinjury_risk_death_density_df,
      aes(x = x, y = y),
      color = "blue",
      linewidth = 1.25,
      alpha = 0.5
    ) +
    geom_area(
      data = reinjury_risk_death_density_df |>
        dplyr::filter(x >= reinjury_risk_death_upper_crit),
      aes(x = x, y = y),
      fill = "coral1",
      alpha = 0.5
    ) +
    geom_area(
      data = reinjury_risk_death_density_df |>
        dplyr::filter(x <= reinjury_risk_death_upper_crit),
      aes(x = x, y = y),
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
        pretty_number(reinjury_risk_death_diff, n_decimal = 5)
      ),
      size = 8,
      family = "Work Sans",
      angle = 90
    ) +
    labs(
      x = "Differences in Proportions",
      y = "Count / Kernel Density",
      title = "Differences in Proportions of Deceased Pts. Between Reinjured and Singularly Injured Samples",
      subtitle = "Simulation-Based Null Distribution from 1,000 Permutated Samples",
      caption = paste0(
        "- p = ",
        pretty_number(
          reinjury_risk_death_prop_test_greater$p_value,
          n_decimal = 4
        ),
        ", alt. hypothesis: mortality rate is greater among reinjured patients compared to singularly injured patients",
        "\n- Shaded area under curve = critical values where observed statistic needs to fall to indicate a significant difference",
        "\n- Red line is the observed difference in proportions of reinjured patients in rural and urban areas in Iowa in the population\n- These data suggest there no statistically significant difference in mortality rate between singularly / reinjured patients."
      )
    ) +
    theme_cleaner(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      axis_lines = T
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
    ipop_case_count(Year, which = "Inpatient", descriptive_stats = T) |>
    dplyr::filter(Year >= 2018, Year < 2024) |>
    dplyr::select(-change_label) |>
    rename(`Total Cases` = n, `% Change in Cases` = change) |>
    pivot_longer(
      cols = `Total Cases`:`% Change in Cases`,
      names_to = "Category",
      values_to = "Value"
    ) |>
    pivot_wider(id_cols = Category, names_from = Year, values_from = Value) |>
    mutate(
      `2018-2023 Trend` = list(c(
        `2018`,
        `2019`,
        `2020`,
        `2021`,
        `2022`,
        `2023`
      )),
      .by = Category
    ) |>
    dplyr::select(-c(`2018`, `2019`, `2020`))
}

# longitudinal patients

{
  ipop_longitudinal_patients <- ipop_data_clean |>
    ipop_patient_count(Year, which = "Inpatient", descriptive_stats = T) |>
    dplyr::filter(Year >= 2018, Year < 2024) |>
    dplyr::select(-change_label) |>
    rename(`Total Pts.` = n, `% Change in Pts.` = change) |>
    pivot_longer(
      cols = `Total Pts.`:`% Change in Pts.`,
      names_to = "Category",
      values_to = "Value"
    ) |>
    pivot_wider(id_cols = Category, names_from = Year, values_from = Value) |>
    mutate(
      `2018-2023 Trend` = list(c(
        `2018`,
        `2019`,
        `2020`,
        `2021`,
        `2022`,
        `2023`
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
      column = `2018-2023 Trend`,
      type = "shaded",
      same_limit = F,
      label = T
    ) |>
    tab_header(
      title = "Count and Rate of Change in IPOP Inpatient Injury Hospitalizations",
      subtitle = "Source: Iowa Inpatient Outpatient Database 2018-2023"
    ) |>
    fmt_percent(
      columns = `2021`:`2023`,
      rows = c(2, 4),
      drop_trailing_zeros = T
    ) |>
    fmt_number(
      columns = `2021`:`2023`,
      rows = c(1, 3),
      drop_trailing_zeros = T
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
    tab_style_hhs(border_cols = `2021`:`2018-2023 Trend`)
}

# age distribution of hospital admissions

ipop_age_dist <- ipop_data_clean |>
  dplyr::filter(Year == 2023) |>
  ipop_case_count(Census_Age_Group, which = "Inpatient") |>
  mutate(
    full_label = paste0(
      Census_Age_Group,
      "\n(",
      pretty_number(n, n_decimal = 2),
      ")"
    ),
    font_color = if_else(
      Census_Age_Group %not_in% c("0-4", "5-9", "10-14"),
      "white",
      hhs_palette_2$primary_2
    )
  )

# plot the age distribution within the IPOP database

{
  ipop_age_dist_bar <- ipop_age_dist |>
    ggplot(aes(area = n, label = full_label, fill = n)) +
    geom_treemap(color = "white", layout = "squarified", start = "bottomleft") +
    geom_treemap_text(
      family = "Work Sans",
      size = 18,
      color = ipop_age_dist$font_color,
      fontface = "bold"
    ) +
    guides(fill = "none") +
    labs(
      title = "Age Distribution of Cases in the IPOP Database",
      subtitle = "Source: Iowa Inpatient Outpatient Database | 2023",
      caption = "Read the order of factors by changing color and box area, signaling decreasing count, from the bottom left to top right.\nThese data reflect inpatient cases from the IPOP database, only."
    ) +
    theme_cleaner(
      title_text_size = 20,
      subtitle_text_size = 18,
      base_size = 15,
      vjust_title = 1.75,
      vjust_subtitle = 1
    ) +
    scale_fill_viridis(option = "rocket", direction = -1)

  # save the treemap

  plot_save_params(
    filename = "ipop_age_dist_bar.png",
    plot = ipop_age_dist_bar,
    path = plot_path
  )
}

# IPOP nature of injury frequency

ipop_nature_injury_freq <- ipop_data_clean |>
  dplyr::filter(Year == 2023) |>
  mutate(
    NATURE_OF_INJURY_DESCRIPTOR = if_else(
      NATURE_OF_INJURY_DESCRIPTOR %in% c("Unspecified", "Other specified"),
      "Other",
      NATURE_OF_INJURY_DESCRIPTOR
    )
  ) |>
  ipop_case_count(NATURE_OF_INJURY_DESCRIPTOR, sort = T, which = "Inpatient") |>
  replace_na(list(NATURE_OF_INJURY_DESCRIPTOR = "Missing")) |>
  mutate(
    mod = sqrt(n),
    angle = 2 * pi * rank(mod) / n(),
    angle_mod = cos(angle)
  )

# plot nature of injury frequency via area chart
# if this plot saves dark, you can load in Paint, edit size down, and it will turn to white background

{
  ipop_nature_injury_freq_plot <- ipop_nature_injury_freq |>
    ggplot(aes(
      x = reorder(str_wrap(NATURE_OF_INJURY_DESCRIPTOR, width = 5), -mod),
      y = mod,
      fill = mod,
      label = pretty_number(n, n_decimal = 2)
    )) +
    geom_col(position = "dodge2", show.legend = T, alpha = 0.9) +
    # First segment: from the top of the bar to just before the text
    geom_segment(
      aes(
        x = reorder(str_wrap(NATURE_OF_INJURY_DESCRIPTOR, 5), -mod),
        xend = reorder(str_wrap(NATURE_OF_INJURY_DESCRIPTOR, 5), -mod),
        y = mod, # Start at the top of the bar
        yend = if_else(
          NATURE_OF_INJURY_DESCRIPTOR == "Fracture",
          mod,
          max(mod) - 2
        ) # End just before the text
      ),
      linetype = "dashed",
      color = hhs_palette_1$accent_2
    ) +
    geom_label(
      fill = if_else(
        ipop_nature_injury_freq$NATURE_OF_INJURY_DESCRIPTOR == "Fracture",
        "transparent",
        "white"
      ),
      nudge_y = if_else(
        ipop_nature_injury_freq$NATURE_OF_INJURY_DESCRIPTOR == "Fracture",
        -10,
        20
      ),
      color = if_else(
        ipop_nature_injury_freq$NATURE_OF_INJURY_DESCRIPTOR == "Fracture",
        "transparent",
        "white"
      )
    ) +
    geom_text(
      nudge_y = if_else(
        ipop_nature_injury_freq$NATURE_OF_INJURY_DESCRIPTOR == "Fracture",
        -12,
        20
      ),
      family = "Work Sans",
      fontface = "bold",
      color = if_else(
        ipop_nature_injury_freq$NATURE_OF_INJURY_DESCRIPTOR == "Fracture",
        "white",
        "black"
      ),
      size = 8
    ) +
    coord_radial(clip = "off", inner.radius = 0.15) +
    labs(
      x = "Nature of Injury Description\n",
      y = "",
      fill = str_wrap("Orange to blue High to low count", width = 15),
      title = "Nature of Injury Frequency Among IPOP Trauma Cases",
      subtitle = "Source: Iowa Inpatient Outpatient Database | 2023",
      caption = "Note: Square-root transformation applied to the y-axis due to the 'Fracture' category outlier,\nthe labels are true case counts."
    ) +
    scale_fill_paletteer_c(
      palette = "ggthemes::Orange-Blue Diverging",
      direction = -1
    ) +
    theme_cleaner(
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
  dplyr::filter(Year == 2023) |>
  replace_na(list(
    BODY_REGION_CATEGORY_LEVEL_1 = "Unclassifiable by body region"
  )) |>
  ipop_case_count(
    BODY_REGION_CATEGORY_LEVEL_1,
    sort = T,
    which = "Inpatient"
  ) |>
  mutate(
    BODY_REGION_CATEGORY_LEVEL_1 = replace_symbol(BODY_REGION_CATEGORY_LEVEL_1),
    BODY_REGION_CATEGORY_LEVEL_1 = if_else(
      grepl(
        pattern = "unclass",
        x = BODY_REGION_CATEGORY_LEVEL_1,
        ignore.case = T
      ),
      "Unclassifiable",
      if_else(
        grepl(
          pattern = "head and neck",
          x = BODY_REGION_CATEGORY_LEVEL_1,
          ignore.case = T
        ),
        "Head and neck",
        BODY_REGION_CATEGORY_LEVEL_1
      )
    ),
    mod = sqrt(n),
    angle = 2 * pi * rank(mod) / n(),
    angle_mod = cos(angle)
  )

# make the circular bar plot

{
  ipop_body_region_plot <- ipop_body_region |>
    ggplot(aes(
      x = reorder(BODY_REGION_CATEGORY_LEVEL_1, -mod),
      y = mod,
      fill = mod,
      label = pretty_number(n, n_decimal = 2)
    )) +
    geom_col(position = "dodge2", width = 0.5) +
    geom_text(
      family = "Work Sans",
      size = 8,
      color = c(rep("white", 2), rep("black", 4)),
      fontface = "bold",
      nudge_y = c(-10, -15, 8, 8, 10, 8)
    ) +
    #ylim(-1, 9.1) +
    labs(
      x = "",
      y = "",
      title = "Body Region of Injury Frequency Among Trauma Cases",
      subtitle = "Source: Iowa Inpatient Outpatient Database | 2023",
      caption = "Note: Square-root transformation applied to the y-axis due to the 'Extremities' category outlier,\nthe labels are true case counts.",
      fill = str_wrap("Dark to light red High to low count", width = 19)
    ) +
    scale_fill_paletteer_c(palette = "grDevices::Reds", direction = -1) +
    coord_radial(start = 0, clip = "off") +
    theme_cleaner(
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
  # compile 2022 data

  death_iowa_state_2022 <- data.frame(
    Year = 2022,
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
    mutate(Percent = Deaths / sum(Deaths))

  # Create a tibble with the provided CDC data for 2023
  death_iowa_state_2023 <- tibble(
    Year = 2023,
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
    mutate(Percent = Deaths / sum(Deaths))

  # union 2022 and 2023

  death_iowa_state <- bind_rows(death_iowa_state_2022, death_iowa_state_2023)

  # export the file

  write_csv(
    death_iowa_state,
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/iowa_deaths.csv"
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
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/Top 15 Causes of Death in the United States All Ages 2018.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    mutate(Year = 2018)

  # 2019

  death_cdc_wonder_nation_all_2019 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/Top 15 Causes of Death in the United States All Ages 2019.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    mutate(Year = 2019)

  # 2020

  death_cdc_wonder_nation_all_2020 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/Top 15 Causes of Death in the United States All Ages 2020.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    mutate(Year = 2020)

  # 2021

  death_cdc_wonder_nation_all_2021 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/Top 15 Causes of Death in the United States All Ages 2021.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    mutate(Year = 2021)

  # 2022

  death_cdc_wonder_nation_all_2022 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/Top 15 Causes of Death in the United States All Ages 2022.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    mutate(Year = 2022)

  # 2023

  death_cdc_wonder_nation_all_2023 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/Top 15 Causes of Death in the United States All Ages 2023.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    mutate(Year = 2023)

  # 2018-2023

  death_cdc_wonder_nation_all_2018_2023_aggregate <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/Top 15 Causes of Death in the United States All Ages 2018-2023.txt",
    n_max = 10
  ) |>
    dplyr::select(-Notes) |>
    mutate(
      `UCD - 15 Leading Causes of Death` = str_replace_all(
        `UCD - 15 Leading Causes of Death`,
        pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
        ""
      )
    )

  # All US deaths 2018 - 2023

  death_cdc_wonder_nation_all_2018_2023_detail <- bind_rows(
    death_cdc_wonder_nation_all_2018,
    death_cdc_wonder_nation_all_2019,
    death_cdc_wonder_nation_all_2020,
    death_cdc_wonder_nation_all_2021,
    death_cdc_wonder_nation_all_2022,
    death_cdc_wonder_nation_all_2023
  ) |>
    mutate(
      `UCD - 15 Leading Causes of Death` = str_replace_all(
        `UCD - 15 Leading Causes of Death`,
        pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
        ""
      )
    )

  # Download top 10 deaths yearly detail file to .csv for future reference

  write_csv(
    death_cdc_wonder_nation_all_2018_2023_detail,
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/death_cdc_wonder_nation_all_2018_2023_detail.csv"
  )

  # Download top 10 deaths aggregate file to .csv for future reference

  write_csv(
    death_cdc_wonder_nation_all_2018_2023_aggregate,
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/death_cdc_wonder_nation_all_2018_2023_aggregate.csv"
  )
}

# CDC WONDER ages 1-44

{
  # 2018

  death_cdc_wonder_nation_1_44_2018 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/Top 15 Causes of Death in the United States Ages 1-44 2018.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    mutate(Year = 2018)

  # 2019

  death_cdc_wonder_nation_1_44_2019 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/Top 15 Causes of Death in the United States Ages 1-44 2019.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    mutate(Year = 2019)

  # 2020

  death_cdc_wonder_nation_1_44_2020 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/Top 15 Causes of Death in the United States Ages 1-44 2020.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    mutate(Year = 2020)

  # 2021

  death_cdc_wonder_nation_1_44_2021 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/Top 15 Causes of Death in the United States Ages 1-44 2021.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    mutate(Year = 2021)

  # 2022

  death_cdc_wonder_nation_1_44_2022 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/Top 15 Causes of Death in the United States Ages 1-44 2022.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    mutate(Year = 2022)

  # 2023

  death_cdc_wonder_nation_1_44_2023 <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/Top 15 Causes of Death in the United States Ages 1-44 2023.txt",
    n_max = 10
  ) |>
    rename(Year = Notes) |>
    mutate(Year = 2023)

  # 2018-2023

  death_cdc_wonder_nation_1_44_2018_2023_aggregate <- read_tsv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/Top 15 Causes of Death in the United States Ages 1-44 2018-2023.txt",
    n_max = 10
  ) |>
    dplyr::select(-Notes) |>
    mutate(
      `UCD - 15 Leading Causes of Death` = str_replace_all(
        `UCD - 15 Leading Causes of Death`,
        pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
        ""
      )
    )

  # All US deaths 2018 - 2023

  death_cdc_wonder_nation_1_44_2018_2023_detail <- bind_rows(
    death_cdc_wonder_nation_1_44_2018,
    death_cdc_wonder_nation_1_44_2019,
    death_cdc_wonder_nation_1_44_2020,
    death_cdc_wonder_nation_1_44_2021,
    death_cdc_wonder_nation_1_44_2022,
    death_cdc_wonder_nation_1_44_2023
  ) |>
    mutate(
      `UCD - 15 Leading Causes of Death` = str_replace_all(
        `UCD - 15 Leading Causes of Death`,
        pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
        ""
      )
    )

  # Download top 10 deaths yearly detail file to .csv for future reference

  write_csv(
    death_cdc_wonder_nation_1_44_2018_2023_detail,
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/death_cdc_wonder_nation_1_44_2018_2023_detail.csv"
  )

  # Download top 10 deaths aggregate file to .csv for future reference

  write_csv(
    death_cdc_wonder_nation_1_44_2018_2023_aggregate,
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/death_cdc_wonder_nation_1_44_2018_2023_aggregate.csv"
  )
}

###
# CDC WISQARS Data
# Show how injury remains the #1 cause of death among individuals ages 1-44 per CDC WISQARS
###

{
  # 2018 - 2022

  death_cdc_wisqars_all <- tibble(
    Year = rep(2018:2022, each = 10),
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
  # Create a tibble with the provided CDC WISQARS data for 2018:2022
  ###

  unintentional_injury_data <- tibble(
    Year = rep(2018:2022, each = 5),
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
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/P15_data.csv"
  ) |>
    rename(
      Intentionality = `_TR P15`,
      Year = `The Year`,
      Deaths = `# of Deaths`
    )

  # for the Iowa unintentional trauma deaths by cause plot

  iowa_deaths_cause <- read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/P16-1_data.csv"
  ) |>
    rename(Cause = `_TR P16-1`, Year = `The Year`, Deaths = `# of Deaths`)

  # for the Iowa trauma suicides by cause plot

  iowa_suicides_cause <- read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/P16-2_data.csv"
  ) |>
    rename(Cause = `_TR P16-2`, Year = `The Year`, Deaths = `# of Deaths`)

  # for the trends in causes of death table

  iowa_death_trends_cause <- read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/P17_data.csv"
  ) |>
    rename(Cause = `_TR P17`, Year = `The Year`, Deaths = `# of Deaths`)

  # for Iowa unintentional falls trends plot

  iowa_death_unintentional_falls <- read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/P30_data.csv"
  ) |>
    rename(Cause = `_TR P30`, Year = `The Year`, Deaths = `# of Deaths`)

  # for Iowa poisoning death trends plot

  # unintentional poisoning
  iowa_death_unintentional_poisoning <- read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/P33-1_data.csv"
  ) |>
    rename(Cause = `_TR P33-1`, Year = `The Year`, Deaths = `# of Deaths`)

  # suicide poisoning
  iowa_death_suicide_poisoning <- read_csv(
    file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/death/P33-2_data.csv"
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
  top_10_causes_us_plot <- death_cdc_wonder_nation_all_2018_2023_aggregate |>
    ggplot(aes(
      x = reorder(
        str_wrap(`UCD - 15 Leading Causes of Death`, width = 20),
        Deaths
      ),
      y = Deaths,
      fill = Deaths,
      label = pretty_number(Deaths, n_decimal = 1)
    )) +
    geom_col(position = position_dodge2(width = 0.5)) +
    geom_text(
      aes(y = if_else(Deaths < 500000, Deaths + 200000, 250000)),
      color = if_else(
        death_cdc_wonder_nation_all_2018_2023_aggregate$Deaths < 500000,
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
    coord_flip() +
    labs(
      x = "",
      y = "",
      title = "Top 10 Causes of Death Among All Age Groups in the U.S.",
      subtitle = "Source: CDC WONDER | 2018-2023",
      caption = "Note: 2023 data used in this report via CDC WONDER are provisional.",
      fill = "# Deaths"
    ) +
    guides(color = "none") +
    scale_y_continuous(labels = function(x) pretty_number(x, n_decimal = 2)) +
    scale_fill_paletteer_c(
      palette = "ggthemes::Orange-Gold",
      direction = 1,
      labels = function(x) pretty_number(x, n_decimal = 1)
    ) +
    theme_cleaner(
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
  death_cdc_wisqars_all_1_44_tbl <- death_cdc_wonder_nation_1_44_2018_2023_aggregate |>
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
      subtitle = "Source: CDC WONDER | 2018-2023"
    ) |>
    tab_source_note(
      source_note = md(paste0(
        fontawesome::fa("magnifying-glass"),
        " 2023 Data included here are provisional."
      ))
    ) |>
    tab_source_note(
      source_note = md(paste0(
        fontawesome::fa("sticky-note"),
        " Injuries remain in the leading causes of death for the years 2018-2023 in the U.S."
      ))
    ) |>
    tab_footnote(
      footnote = "Rate per 100,000 population.",
      locations = cells_column_labels(columns = `Age Adjusted Rate`)
    ) |>
    opt_footnote_marks(marks = "standard") |>
    fmt_number(columns = Deaths, drop_trailing_zeros = T) |>
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
    dplyr::filter(Year == 2023) |>
    ggplot(aes(
      x = reorder(str_wrap(Cause_of_Death, width = 10), -Deaths),
      y = Deaths,
      fill = Deaths,
      label = pretty_number(Deaths)
    )) +
    geom_col(width = 0.75, position = position_dodge(width = 1)) +
    geom_text(
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
    labs(
      x = "",
      y = "",
      title = "Top 10 Causes of Death in Iowa Among All Age Groups",
      subtitle = "Source: Iowa Death Certificate Data | 2023"
    ) +
    scale_fill_paletteer_c(
      palette = "ggthemes::Orange-Gold",
      direction = 1,
      labels = function(x) pretty_number(x)
    ) +
    theme_cleaner_facet(
      base_size = 15,
      title_text_size = 20,
      subtitle_text_size = 18,
      vjust_title = 1.75,
      vjust_subtitle = 1,
      axis.text.y = element_blank(),
      legend_position = "inside",
      legend.position.inside = c(0.75, 0.75)
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
    mutate(
      Intentionality = str_remove_all(Intentionality, pattern = "^\\d{2}-"),
      labels = if_else(
        Year %in% c(2018, 2023) & Deaths >= 6,
        pretty_number(Deaths),
        if_else(
          Year %in% c(2018, 2023) & Deaths < 6,
          small_count_label(var = Deaths, cutoff = 6, replacement = "*"),
          NA_character_
        )
      )
    ) |>
    ggplot(aes(
      x = factor(Year),
      y = Deaths,
      color = reorder(Intentionality, -Deaths),
      label = labels,
      group = Intentionality
    )) +
    geom_line(
      alpha = 0.5,
      linewidth = 3,
      lineend = "round",
      linejoin = "round"
    ) +
    geom_text_repel(
      segment.color = NA,
      family = "Work Sans",
      size = 8,
      color = "black",
      direction = "both"
    ) +
    labs(
      x = "",
      y = "",
      title = "Iowa Trauma Deaths by Intentionality",
      subtitle = "Source: Iowa Death Certificate Data | 2018-2023",
      color = "Intentionality",
      caption = "Note: Order of color legend follows descending order of lines.\n'*' indicates a masked value < 6 to protect confidentiality."
    ) +
    scale_y_continuous(labels = function(x) pretty_number(x)) +
    scale_color_paletteer_d(palette = "colorblindr::OkabeIto", direction = 1) +
    theme_cleaner(
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
    mutate(
      Cause = str_remove_all(Cause, pattern = "^\\d{2}-"),
      labels = if_else(
        Year %in% c(2018, 2023) & Deaths >= 6,
        pretty_number(Deaths),
        if_else(
          Year %in% c(2018, 2023) & Deaths < 6,
          small_count_label(var = Deaths, cutoff = 6, replacement = "*"),
          NA_character_
        )
      )
    ) |>
    ggplot(aes(
      x = factor(Year),
      y = Deaths,
      color = reorder(Cause, -Deaths),
      label = labels,
      group = Cause
    )) +
    geom_line(
      alpha = 0.5,
      linewidth = 3,
      lineend = "round",
      linejoin = "round"
    ) +
    geom_text_repel(
      segment.color = NA,
      family = "Work Sans",
      size = 8,
      color = "black",
      direction = "both"
    ) +
    labs(
      x = "",
      y = "",
      title = "Iowa Unintentional Trauma Deaths by Cause",
      subtitle = "Source: Iowa Death Certificate Data | 2018-2023",
      color = "Cause of Death",
      caption = "Note: Order of color legend follows descending order of lines."
    ) +
    scale_y_continuous(labels = function(x) pretty_number(x)) +
    scale_color_paletteer_d(palette = "colorblindr::OkabeIto", direction = 1) +
    theme_cleaner(
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
    mutate(
      Cause = str_remove_all(Cause, pattern = "^\\d{2}-"),
      labels = if_else(
        Year %in% c(2018, 2023) & Deaths >= 6,
        pretty_number(Deaths),
        if_else(
          Year %in% c(2018, 2023) & Deaths < 6,
          small_count_label(var = Deaths, cutoff = 6, replacement = "*"),
          NA_character_
        )
      )
    ) |>
    ggplot(aes(
      x = factor(Year),
      y = Deaths,
      color = reorder(Cause, -Deaths),
      label = labels,
      group = Cause
    )) +
    geom_line(
      alpha = 0.5,
      linewidth = 3,
      lineend = "round",
      linejoin = "round"
    ) +
    geom_text_repel(
      segment.color = NA,
      family = "Work Sans",
      size = 8,
      color = "black",
      direction = "x"
    ) +
    labs(
      x = "",
      y = "",
      title = "Iowa Trauma Suicide Deaths by Cause",
      subtitle = "Source: Iowa Death Certificate Data | 2018-2023",
      color = "Cause",
      caption = "Note: Order of color legend follows descending order of lines."
    ) +
    scale_y_continuous(labels = function(x) pretty_number(x)) +
    scale_color_paletteer_d(palette = "colorblindr::OkabeIto", direction = 1) +
    theme_cleaner(
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
    arrange(Year, desc(Deaths)) |>
    mutate(Cause = str_remove_all(Cause, pattern = "^\\d{2}-")) |>
    pivot_wider(
      id_cols = Cause,
      names_from = Year,
      values_from = Deaths,
      values_fill = NA_integer_
    ) |>
    mutate(
      `Five Year Avg.` = mean(`2018`:`2023`, na.rm = T),
      `% Diff. from Five Year Avg.` = (`2023` - `Five Year Avg.`) /
        `Five Year Avg.`,
      `2018-2023 Trend` = list(c(
        `2018`,
        `2019`,
        `2020`,
        `2021`,
        `2022`,
        `2023`
      )),
      .by = Cause
    ) |>
    arrange(desc(`Five Year Avg.`)) |>
    dplyr::select(Cause, `2023`:last_col()) |>
    gt() |>
    tab_header(
      title = "Iowa Trends in Causes of Traumatic Death",
      subtitle = "Source: Iowa Death Certificate Data | 2018-2023"
    ) |>
    cols_label(`2023` = "# Deaths 2023") |>
    fmt_number(
      columns = c(`2023`, `Five Year Avg.`),
      drop_trailing_zeros = T
    ) |>
    fmt_percent(
      columns = `% Diff. from Five Year Avg.`,
      drop_trailing_zeros = T
    ) |>
    tab_footnote(
      footnote = "Bars under cause of death track with count of deaths in 2023.",
      locations = cells_column_labels(columns = Cause)
    ) |>
    tab_footnote(
      footnote = "Five year avg. calculated from counts of each cause of death from 2018 through 2023.",
      locations = cells_column_labels(columns = `Five Year Avg.`)
    ) |>
    opt_footnote_marks(marks = "standard") |>
    gt_plt_dot(
      column = `2023`,
      category_column = Cause,
      palette = "colorblindr::OkabeIto"
    ) |>
    gt_plt_sparkline(
      column = `2018-2023 Trend`,
      type = "shaded",
      same_limit = F,
      label = T,
      fig_dim = c(8, 30)
    ) |>
    tab_style_hhs(border_cols = `2023`:`2018-2023 Trend`)
}

# trends in unintentional and suicide poisonings

{
  iowa_death_poisoning_plot <- iowa_death_poisoning |>
    arrange(Cause, Year) |>
    mutate(
      labels = if_else(
        Year %in% c(2018, 2023),
        pretty_number(x = Deaths),
        NA_character_
      )
    ) |>
    ggplot(aes(
      x = Year,
      y = Deaths,
      color = Cause,
      label = labels,
      group = Cause
    )) +
    ggplot2::annotate(
      geom = "segment",
      x = 2018,
      xend = 2023,
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
    geom_line(
      linewidth = 2,
      lineend = "round",
      linejoin = "round",
      alpha = 0.9
    ) +
    geom_text_repel(
      direction = "y",
      segment.color = NA,
      color = "black",
      family = "Work Sans",
      size = 8,
      fontface = "bold"
    ) +
    labs(
      title = "Trends in Iowa Poisoning Deaths",
      subtitle = "Source: Iowa Death Certificate Data | 2018-2023",
      x = "",
      y = "# Deaths\n"
    ) +
    scale_color_paletteer_d(palette = "colorblindr::OkabeIto") +
    theme_cleaner(
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
level_indicators <- trauma_2023 |>
  seqic_level(
    toggle = T,
    pivot = T,
    bind_goals = T,
    include_state = T
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
region_indicators <- trauma_2023 |>
  seqic_region(
    toggle = T,
    pivot = T,
    bind_goals = T,
    include_state = T,
    region_randomize = T
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
