###_____________________________________________________________________________
### Mechanism of Injury ----
###_____________________________________________________________________________

###
# In order to utilize this script, you must first run setup.R and then
# data_load.R in order to have the necessary custom functions and data.
###

# get counts and proportions of injury events by mechanism of injury
mechanism_of_injury_counts <- trauma_data_clean |>
  dplyr::filter(!is.na(CAUSE_OF_INJURY_AR_1)) |>
  injury_incident_count(Year, CAUSE_OF_INJURY_AR_1) |>
  dplyr::mutate(percent = n / sum(n), .by = Year)

# get a subset of the counts for the plot
mechanism_of_injury_counts_select <- mechanism_of_injury_counts |>
  dplyr::filter(Year == 2024)

# plot the age distribution within the IPOP database
mechanism_of_injury_cols <- mechanism_of_injury_counts_select |>
  ggplot2::ggplot(ggplot2::aes(
    x = reorder(x = CAUSE_OF_INJURY_AR_1, n),
    y = n,
    label = prettyNum(x = n, big.mark = ","),
    fill = n
  )) +
  ggplot2::geom_col(
    width = 0.75,
    position = ggplot2::position_dodge(width = 0.25)
  ) +
  ggrepel::geom_text_repel(
    ggplot2::aes(y = ifelse(CAUSE_OF_INJURY_AR_1 == "Poisoning", 100, n)),
    family = "Work Sans",
    size = 18,
    fontface = "bold",
    direction = "x",
    color = ifelse(
      !mechanism_of_injury_counts_select$CAUSE_OF_INJURY_AR_1 %in%
        c("Poisoning", "Firearm", "Struck by/against"),
      "white",
      "black"
    ),
    nudge_y = ifelse(
      !mechanism_of_injury_counts_select$CAUSE_OF_INJURY_AR_1 %in%
        c("Poisoning", "Firearm", "Struck by/against"),
      -1,
      1
    ),
    segment.color = "transparent",
    seed = 12
  ) +
  ggplot2::guides(fill = "none") +
  traumar::theme_cleaner(
    base_size = 20,
    axis.text.x = ggplot2::element_blank()
  ) +
  ggplot2::labs(x = "", y = "") +
  ggplot2::coord_flip()

# save the treemap
ggplot2::ggsave(
  filename = "mechanism_of_injury_cols.png",
  plot = mechanism_of_injury_cols,
  path = plot_folder,
  height = 6.67,
  width = 6.67 * 1.78
)

# get statistics on special cases of injuries ----

# work related and ag ---
work_related_agricultural <- trauma_data_clean |>
  dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE) |>
  dplyr::summarize(
    work_related = sum(Financial_Work_Related == "Yes", na.rm = TRUE),
    ag_related = sum(Farm_Ag_Related == "Yes", na.rm = TRUE),
    n = dplyr::n(),
    percent_work = work_related / n,
    percent_ag = ag_related / n,
    .by = Year
  )

# reinjury ----

# patients that were reinjured
reinjury_patient_statistics <- trauma_data_clean |>
  reinjury_patient_count(Year, descriptive_stats = TRUE) |>
  dplyr::select(-c(9:14))

# injuries that occurred as reinjuries
reinjury_injury_statistics <- trauma_data_clean |>
  reinjury_injury_count(Year, descriptive_stats = TRUE) |>
  dplyr::select(-c(8:13))

# prepare data for a gt table
reinjury_data_prep <- reinjury_patient_statistics |>
  dplyr::select(Year, reinjured_patients, n_patients, prop_reinjured) |>
  dplyr::left_join(
    reinjury_injury_statistics |>
      dplyr::select(Year, Reinjury, injury_events = n, prop),
    by = dplyr::join_by(Year)
  )

# reinjury gt table
reinjury_gt <- reinjury_data_prep |>
  gt::gt() |>
  gt::cols_label(
    reinjured_patients ~ "Reinjured Pts",
    n_patients ~ "Total Pts",
    Reinjury ~ "Reinjury Events",
    injury_events ~ "Injury Events",
  ) |>
  gt::fmt_number(
    columns = c(reinjured_patients:n_patients, Reinjury:injury_events),
    drop_trailing_zeros = TRUE
  ) |>
  gt::fmt_percent(columns = tidyselect::matches("prop")) |>
  gt::cols_merge_n_pct(col_n = reinjured_patients, col_pct = prop_reinjured) |>
  gt::cols_merge_n_pct(col_n = Reinjury, col_pct = prop) |>
  tab_style_hhs(
    border_cols = 2:tidyselect::last_col(),
    column_labels = 18,
    body = 16
  )

# save the gt reinjury table
gt::gtsave(data = reinjury_gt, filename = "reinjury_gt.png", path = plot_folder)

# get counts of the nature of injury and body regions ----
