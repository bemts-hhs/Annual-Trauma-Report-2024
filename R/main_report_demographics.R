###_____________________________________________________________________________
# Patient Demographics ----
###_____________________________________________________________________________

###
# In order to utilize this script, you must first run setup.R and then
# data_load.R in order to have the necessary custom functions and data.
###

###
# Sex ----
###

# get overall counts of gender ----
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

# get proportions of patients by gender over the years ----
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

# create gender plot to show patient counts over the years ----
gender_proportions_plot <- gender_proportions |>
    dplyr::filter(Sex %in% c("Male", "Female")) |>
    ggplot2::ggplot(ggplot2::aes(x = Year, y = n, fill = Sex)) +
    ggplot2::geom_col(
        width = 0.5,
        position = ggplot2::position_dodge(width = 0.7)
    ) +
    ggplot2::geom_text(
        ggplot2::aes(y = 2250, label = traumar::pretty_number(x = n)),
        color = "white",
        angle = 90,
        position = ggplot2::position_dodge(width = 0.7),
        size = 12,
        fontface = "bold",
        family = "Work Sans"
    ) +
    ggthemes::scale_fill_colorblind() +
    ggplot2::scale_y_continuous(n.breaks = 5, labels = function(x) {
        traumar::pretty_number(x = x)
    }) +
    ggplot2::labs(x = "", y = "") +
    traumar::theme_cleaner(
        base_size = 30,
        axis.text.y = ggplot2::element_blank()
    )

# save the gender plot ----
ggplot2::ggsave(
    filename = "gender_proportions_plot.png",
    plot = gender_proportions_plot,
    path = plot_folder,
    height = 6.67,
    width = 6.67 * 1.78
)

# Get a table of counts by patient gender and survival rates ----
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

# Get comparison overall survival rates among trauma patients ----
survival_stats <- trauma_data_2024 |>
    dplyr::mutate(
        Mortality = max(Death, na.rm = TRUE),
        Mortality = ifelse(Mortality == 1, TRUE, FALSE),
        .by = c(Year, Unique_Patient_ID)
    ) |>
    traumar::seqic_indicator_8(
        level = Level,
        included_levels = c("I", "II", "III", "IV", "FSED"),
        unique_incident_id = Unique_Patient_ID,
        mortality_indicator = Mortality,
        risk_group = Risk_Definition,
        groups = "Patient_Gender"
    ) |>
    purrr::pluck(1)

###_____________________________________________________________________________
### Patient race ----
###_____________________________________________________________________________

# race label manipulations to reduce categories / label length ----
patient_race_fix <- trauma_data_clean |>
    dplyr::mutate(
        Patient_Race = ifelse(
            is.na(Patient_Race) |
                grepl(
                    pattern = "^not\\s|select",
                    x = Patient_Race,
                    ignore.case = TRUE
                ),
            "Missing",
            ifelse(
                grepl(pattern = "black", x = Patient_Race, ignore.case = TRUE),
                "Black",
                ifelse(
                    grepl(
                        pattern = "american indian",
                        x = Patient_Race,
                        ignore.case = TRUE
                    ),
                    "AIAN",
                    ifelse(
                        grepl(
                            pattern = "native hawaiian",
                            x = Patient_Race,
                            ignore.case = TRUE
                        ),
                        "NHOPI",
                        Patient_Race
                    )
                )
            )
        )
    )

# patient counts by race ----
# useful for a summary
patient_race_counts <- patient_race_fix |>
    dplyr::filter(Patient_Race != "Missing") |>
    injury_patient_count(Year, Race = Patient_Race) |>
    dplyr::mutate(percent = n / sum(n), .by = Year)

# patient counts by race 2023 and 2024 ----
patient_race_counts_select <- patient_race_counts |>
    dplyr::filter(Year >= 2023)

# create a gt table for race
patient_race_gt <- patient_race_counts_select |>
    tidyr::pivot_wider(
        id_cols = Race,
        names_from = Year,
        values_from = c(n, percent),
        values_fill = 0
    ) |>
    dplyr::mutate(
        Total = rowSums(x = dplyr::across(n_2023:n_2024)),
        .after = n_2024
    ) |>
    dplyr::mutate(percent_total = Total / sum(Total), .after = Total) |>
    dplyr::arrange(desc(Total)) |>
    gt::gt() |>
    gt::fmt_number(
        columns = c(tidyselect::matches("n_\\d+"), Total),
        drop_trailing_zeros = TRUE
    ) |>
    gt::fmt_percent(
        columns = c(tidyselect::matches("percent_\\d+"), percent_total)
    ) |>
    gt::cols_merge_n_pct(col_n = n_2023, col_pct = percent_2023) |>
    gt::cols_merge_n_pct(col_n = n_2024, col_pct = percent_2024) |>
    gt::cols_label(
        percent_total ~ "% Total"
    ) |>
    gt::cols_label_with(
        columns = tidyselect::matches("n_\\d+"),
        fn = ~ stringr::str_remove(string = ., pattern = "^n_")
    ) |>
    gt::tab_footnote(
        footnote = gt::md(paste0(
            fontawesome::fa("user-slash"),
            " Excluded: patients with missing race."
        )),
        locations = gt::cells_column_labels(columns = percent_total)
    ) |>
    gt::opt_footnote_marks(marks = "standard") |>
    tab_style_hhs(
        border_cols = 2:tidyselect::last_col(),
        column_labels = 18,
        body = 16,
        footnote = 16
    )

# save the gt race table ----
gt::gtsave(
    data = patient_race_gt,
    filename = "patient_race_gt.png",
    path = plot_folder
)

# patient counts by age groups ----

# get unique patient age ranges ----
age_ranges <- unique(trauma_data_clean$Age_Range) |> sort()

# get age group counts ----
age_range_counts <- trauma_data_clean |>
    dplyr::filter(!is.na(Age_Range)) |>
    dplyr::mutate(
        Age_Range = factor(Age_Range),
        Age_Range = forcats::fct_relevel(Age_Range, "100+", after = Inf)
    ) |>
    injury_patient_count(Year, Age = Age_Range) |>
    dplyr::mutate(percent = n / sum(n), .by = Year)

# get an abbreviated table for the visualization ----
age_range_counts_select <- age_range_counts |>
    dplyr::filter(Year >= 2023) |>
    tidyr::pivot_wider(
        id_cols = Age,
        names_from = Year,
        values_from = c(n, percent)
    ) |>
    dplyr::mutate(
        Total = rowSums(x = dplyr::across(n_2023:n_2024)),
        percent_total = Total / sum(Total)
    )

# create a gt table for the age range data ----
age_range_gt <- age_range_counts_select |>
    gt::gt() |>
    gt::fmt_number(
        columns = tidyselect::matches("n_\\d+|^total$"),
        drop_trailing_zeros = TRUE
    ) |>
    gt::fmt_percent(columns = tidyselect::matches("percent_(\\d+|total)")) |>
    gt::cols_merge_n_pct(col_n = n_2023, col_pct = percent_2023) |>
    gt::cols_merge_n_pct(col_n = n_2024, col_pct = percent_2024) |>
    gt::cols_label(
        percent_total ~ "% Total"
    ) |>
    gt::cols_label_with(
        columns = tidyselect::matches("n_\\d+"),
        fn = ~ stringr::str_remove(string = ., pattern = "^n_")
    ) |>
    gt::tab_footnote(
        footnote = gt::md(paste0(
            fontawesome::fa("user-slash"),
            " Excluded: patients with missing age."
        )),
        locations = gt::cells_column_labels(columns = percent_total)
    ) |>
    gt::opt_footnote_marks(marks = "standard") |>
    tab_style_hhs(
        border_cols = 2:tidyselect::last_col(),
        column_labels = 18,
        body = 16,
        footnote = 16
    )

# save the race count gt table ----
gt::gtsave(
    data = age_range_gt,
    filename = "age_range_gt.png",
    path = plot_folder
)
