###_____________________________________________________________________________
### Load data for the 2024 Annual Trauma report project ----
### For any analyses, these data must be loaded for the applicable section
### You must run setup.R first before utilizing this script
###_____________________________________________________________________________

### Get environment variables ----

# clinical data
trauma_data_path <- Sys.getenv("trauma_data_2024")
ems_data_path <- Sys.getenv("ems_data_2024")
ipop_data_path <- Sys.getenv("ipop_data_2024")

# files for classification
mech_injury_path <- Sys.getenv("mech_injury_map")
injury_matrix_path <- Sys.getenv("injury_matrix")
iowa_counties_districts_path <- Sys.getenv("iowa_counties_districts")

# population files
iowa_county_pops_path <- Sys.getenv("IOWA_COUNTY_POPS")
iowa_county_age_pops_path <- Sys.getenv("IOWA_COUNTY_AGE_POPS")
us_standard_pops_path <- Sys.getenv("US_STANDARD_POPS")
iowa_state_age_pops_path <- Sys.getenv("IOWA_STATE_POPS")

### trauma data ----
trauma_data <- readr::read_csv(file = trauma_data_path)

# check the trauma data
dplyr::glimpse(trauma_data)

### ems data ----
ems_data <- readr::read_csv(file = ems_data_path)

# check the ems data
dplyr::glimpse(ems_data)

### ipop data ----
ipop_data <- readxl::read_excel(path = ipop_data_path, sheet = 1)

# check the ipop data
dplyr::glimpse(ipop_data)

###_____________________________________________________________________________
# Load the files used to categorize mechanism and nature of injury ----
# based on the ICD-10 injury code
# NOTICE THAT IN ORDER TO GET THE SAME COUNTS AS IN TABLEAU WITH REGARD TO THE
# CAUSE OF INJURY / NATURE OF INJURY / body region (lvl1 and lvl2) you must run
# RUN distinct(Unique_Incident_ID, [coi_ar, cc2, body region], .keep_all = TRUE)
# and then your count() function or else you will not get the same counts in R.
# Tableau does a better job of automating the grouping via AI, and in R you have
# to do that manually.
###_____________________________________________________________________________

# mechanism of injury mapping ----
mechanism_injury_mapping <- readr::read_csv(
  file = mech_injury_path
)

# select variables of interest for mappings
mechanism_injury_mapping <- mechanism_injury_mapping |>
  dplyr::select(
    UPPER_CODE,
    INTENTIONALITY,
    CUSTOM_CATEGORY2,
    CAUSE_OF_INJURY_AR
  )

# nature of injury mapping ----
nature_injury_mapping <- readxl::read_excel(path = injury_matrix_path)

# select variables of interest for mappings
nature_injury_mapping <- nature_injury_mapping |>
  dplyr::select(
    ICD_10_CODE_TRIM,
    NATURE_OF_INJURY_DESCRIPTOR,
    BODY_REGION_CATEGORY_LEVEL_1,
    BODY_REGION_CATEGORY_LEVEL_2
  )

# classify counties in the data ----
location_data <- readxl::read_excel(path = iowa_counties_districts_path)

# select variables of interest for Iowa county classification
location_data <- location_data |>
  dplyr::select(County, Designation, Urbanicity)

###_____________________________________________________________________________
# census bureau standard pops 2020-2024 census ----
# documentation here:
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/
###_____________________________________________________________________________

# 2020-2024 Census Bureau County population data ----
# get years for each county population
county_pops_all <- readr::read_csv(
  file = iowa_county_pops_path
)

# get columns of interest for the county-level population data ----
county_pops_select <- county_pops_all |>
  dplyr::filter(STATE == "19", COUNTY != "000") |>
  dplyr::select(County = CTYNAME, tidyselect::matches("popestimate\\d{4}$")) |>
  tidyr::pivot_longer(
    cols = -County,
    names_to = "Year",
    values_to = "Population"
  ) |>
  dplyr::mutate(
    Year = stringr::str_remove(string = Year, pattern = "POPESTIMATE"),
    Year = forcats::as_factor(as.numeric(Year)),
    County = stringr::str_squish(stringr::str_remove_all(
      County,
      pattern = "\\sCounty"
    )),
    County = stringr::str_to_title(County),
    County = dplyr::if_else(
      grepl(pattern = "o[']brien", x = County, ignore.case = TRUE),
      "O'Brien",
      County
    )
  )

# Iowa county pops by age group ----

# ingest data
age_group_pops <- readr::read_csv(iowa_county_age_pops_path)

# 2020-2024 data Iowa county population data by age group
age_group_pops_final <- age_group_pops |>
  dplyr::select(
    CTYNAME,
    YEAR,
    tidyselect::matches(
      "age(04|59|1014|1519|2024|2529|3034|3539|4044|4549|5054|5559|6064|6569|7074|7579|8084|85plus)_tot",
      ignore.case = TRUE
    ),
    POPESTIMATE
  ) |>

  # Year #1 here is the base year at 4/1/2020
  dplyr::filter(YEAR != 1) |> # 7/1/2020 - 7/1/2024 pop estimates

  # Add 2018 to each year so that 1 == 2020, 2 == 2021, 3 == 2022, 4 == 2023,
  # and 5 == 2024, which is the intended meaning
  dplyr::mutate(YEAR = 2018 + YEAR) |>
  dplyr::rename(Year = YEAR) |>
  tidyr::pivot_longer(
    cols = AGE04_TOT:AGE85PLUS_TOT,
    names_to = "Age_Group",
    values_to = "Population"
  ) |>
  dplyr::mutate(
    Age_Group = stringr::str_extract(Age_Group, pattern = "\\d+"),
    Age_Group = dplyr::if_else(
      Age_Group == "85",
      "85+",
      dplyr::if_else(
        nchar(Age_Group) == 2,
        paste0(
          stringr::str_sub(Age_Group, 1, 1),
          "-",
          stringr::str_sub(Age_Group, 2, 2)
        ),
        dplyr::if_else(
          nchar(Age_Group) == 4,
          paste0(
            stringr::str_sub(Age_Group, 1, 2),
            "-",
            stringr::str_sub(Age_Group, 3, 4)
          ),
          "Missing"
        )
      )
    ),
    CTYNAME = stringr::str_remove_all(CTYNAME, pattern = "(?:\\sCounty)*")
  ) |>
  dplyr::rename(County = CTYNAME, County_Population = POPESTIMATE) |>
  dplyr::relocate(County_Population, .after = Population) |>
  dplyr::mutate(County_Weight = Population / County_Population)

###_____________________________________________________________________________
# age group populations for Iowa at the state (not county) level ----
# work with the sc-est[year]-agesex-civ.csv files for this via
# https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/state/asrh/
###_____________________________________________________________________________

# Iowa age groups at the state level for 2018-2022
# clean Iowa age group populations
# these are NOT standard populations
us_state_age_pops <- readr::read_csv(file = iowa_state_age_pops_path)

# get Iowa state age populations
iowa_state_age_pops <- us_state_age_pops |>

  # Iowa == State #19, and SEX == 0 is for totals not sex-specific
  dplyr::filter(STATE == 19, SEX == 0) |>
  dplyr::select(
    -c(SUMLEV, REGION, DIVISION, STATE, NAME, SEX, ESTBASE2020_CIV)
  ) |>
  tidyr::pivot_longer(
    cols = -AGE,
    names_to = "Year",
    values_to = "Population"
  ) |>
  dplyr::mutate(
    Year = as.numeric(stringr::str_extract(string = Year, pattern = "\\d+")),
    Age_Group = dplyr::if_else(
      AGE < 5,
      "0-4",
      dplyr::if_else(
        AGE >= 5 & AGE < 10,
        "5-9",
        dplyr::if_else(
          AGE >= 10 & AGE < 15,
          "10-14",
          dplyr::if_else(
            AGE >= 15 & AGE < 20,
            "15-19",
            dplyr::if_else(AGE >= 20 & AGE < 25, "20-24")
          )
        )
      )
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

{
  # 2018
  state_age_group_pops_2018 <- readr::read_tsv(
    "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/Annual Trauma Report/2023/data/Iowa state level age groups 2018.txt",
    n_max = 19
  ) |>
    dplyr::mutate(Year = 2018, .before = 1)

  # clean
  state_age_group_pops_2018_clean <- state_age_group_pops_2018 |>
    dplyr::select(Year, `Five-Year Age Groups Code`, Population) |>
    dplyr::mutate(
      Age_Group = dplyr::if_else(
        `Five-Year Age Groups Code` %in% c("1", "1-4"),
        "0-4",
        `Five-Year Age Groups Code`
      )
    ) |>
    dplyr::summarize(
      Population = sum(Population),
      .by = c(Year, Age_Group)
    ) |>
    dplyr::mutate(
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
