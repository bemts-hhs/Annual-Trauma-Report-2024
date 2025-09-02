###_____________________________________________________________________________
# Patient Demographics ----
###_____________________________________________________________________________

# get overall counts of gender
gender_counts <- trauma_data_clean |>
    injury_patient_count(Year, Sex = Patient_Gender) |>
    dplyr::mutate(
        Sex = ifelse(
            grepl(pattern = "not", x = Sex, ignore.case = TRUE),
            "Missing",
            Sex
        )
    ) |>
    dplyr::arrange(Sex) |>
    dplyr::mutate(
        change = n - dplyr::lag(n),
        prop_change = change / dplyr::lag(n),
        .by = Sex
    )

# get proportions of patients by gender over the years
gender_proportions <- trauma_data_clean |>
    injury_patient_count(Year, Sex = Patient_Gender) |>
    dplyr::mutate(
        Sex = ifelse(
            grepl(pattern = "not", x = Sex, ignore.case = TRUE),
            "Missing",
            Sex
        ),
        prop = n / sum(n),
        .by = Year
    )

# Get a table of counts by patient gender and survival rates
gender_counts_survival <- trauma_data_clean |>
    dplyr::mutate(
        Mortality = max(Death, na.rm = TRUE),
        .by = c(Year, Unique_Patient_ID)
    ) |>
    injury_patient_count(Year, Sex = Patient_Gender, Mortality) |>
    dplyr::mutate(
        Mortality = dplyr::if_else(Mortality == 1, "Died", "Lived"),
        Sex = ifelse(
            grepl(pattern = "not", x = Sex, ignore.case = TRUE),
            "Missing",
            Sex
        )
    ) |>
    tidyr::pivot_wider(
        id_cols = Year:Sex,
        names_from = Mortality,
        values_from = n,
        values_fill = 0
    ) |>
    dplyr::mutate(prop_survived = Lived / (Lived + Died))

# Get comparison overall survival rates among trauma patients
survival_stats <- trauma_data_2024 |>
    dplyr::mutate(
        Mortality = max(Death, na.rm = TRUE),
        Mortality = ifelse(Mortality == 1, TRUE, FALSE),
        .by = c(Year, Unique_Patient_ID)
    ) |>
    traumar::seqic_indicator_8(
        level = Level,
        unique_incident_id = Unique_Patient_ID,
        mortality_indicator = Mortality,
        risk_group = Risk_Definition,
        groups = "Patient_Gender"
    ) |>
    purrr::pluck(1)
