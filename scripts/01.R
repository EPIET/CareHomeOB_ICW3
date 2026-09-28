## Check wether pacman is installed and activate it or install and activate otherwise
if(!require("pacman")){install.packages(pacman)}

## Activate libraries with pacman
pacman::p_load(
  here,          # to create file paths
  rio,           # to import data
  janitor,       # tools for data cleaning and exploration
  gtsummary,     # for summary tables
  skimr,         # to explore dataframes  [NEW ONE]
  tidyverse      # for data manipulation (always the last library!!!)
  
)

## Import clean data using rio and here
raw <- import(here("data", "raw", "carehome_familyday_raw.csv"))

# Fix the age variable whit case_when()
clean <- raw %>% 
  mutate(age = case_when(
    # negative age value to positive
    age == -35  ~ 35, 
    # if 190 years and family member, assume 19 years typo
    age == 190 & group == "family"  ~ 19, 
    # if 190 years and resident, assume 90 years typo
    age == 190 & group == "resident"  ~ 90,
    # everyone else stays the same
    .default = age
  ))

# Update the clean object from before
clean <- clean %>% 
  mutate(
    dayonset = dmy(dayonset),
    starthour = hours(starthour)
  ) 


# Or add a new pipe to your cleaning chain
clean <- raw %>% 
  mutate(age = case_when(
    # negative age value to positive
    age == -35  ~ 35, 
    # if 190 years and family member, assume 19 years typo
    age == 190 & group == "family"  ~ 19, 
    # if 190 years and resident, assume 90 years typo
    age == 190 & group == "resident"  ~ 90,
    # everyone else stays the same
    .default = age
  )) %>% 
  
  # Fix dates format
  mutate(
    dayonset = dmy(dayonset),
    starthour = hours(starthour)
  ) 

binary_cols <- c("diarrhoea", "vomiting", "abdo", "fever", "nausea",
                 "buffet")

clean <- clean %>% 
  mutate(across(
    # Transform all variables in the binary_cols vector
    .cols = all_of(binary_cols),
    # By transforming then into 0-1 factors with "No"-"Yes" labels
    # Base R version of transforming into factor
    .fns = ~ factor(., 
                    levels = c(0, 1),
                    labels = c("No", "Yes"))
  ))

clean <- clean %>% 
  mutate(across(
    .cols = ends_with("D", ignore.case = FALSE),
    # Tidyverse version to convert into factor
    .fns = ~ as_factor(.)  
  ))

export(clean, here("data", "clean", "carehome_clean1.rds"))
