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
clean <- import(here("data", "backup", "carehome_clean1.rds"), 
                trust = T)

# Filter away those who did not eat from the buffet
clean <- clean %>% 
  filter(buffet == "Yes")

# Filter away those who did not eat from the buffet
clean <- clean %>%
  mutate(gastro_symptoms = case_when(
    # Use OR criteria to capture any of the symptoms as possitive condition
    diarrhoea == "Yes" | vomiting == "Yes" | 
      abdo == "Yes" | fever == "Yes" | 
      nausea == "Yes"    ~ 1,
    
    # If none of the above conditions are met, then
    .default = 0
  )
  )


# Create a date-time variable
clean <- clean %>% 
  mutate(onset_datetime = dayonset + starthour)


# Calculate the time span between two dates (Diffdate)
clean <- clean %>% 
  mutate(buffet_datetime = ymd_h("2026-09-13 14h"),
         incubation = as.numeric(onset_datetime - buffet_datetime))


clean <- clean %>% 
  mutate(case = case_when(
    # They experienced gastrointestinal symptoms
    gastro_symptoms == 1 & 
      
      # Symptom onset between 12 and 72h after eating at the buffet   
      incubation >= 12 & incubation <= 72   ~ 1,
    
    # Everything else
    .default = 0
  ))

# Count and Percentage of cases vs no cases
tabyl(clean$case)

export(clean, here("data", "clean", "carehome_clean2.rds"))
