###_____________________________________________________________________________
# Get the unique responses for the ICD-10 causes of injury to make categories ----
###_____________________________________________________________________________

####
# You must first run data_load.R and setup.R before running this script
####

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
