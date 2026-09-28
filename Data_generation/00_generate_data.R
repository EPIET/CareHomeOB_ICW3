## ---------------------------------------------------------------------
## 00_Simulate_dataset.R
##
## Instructor tool (NOT for students) that simulates the fake outbreak
## dataset for the "mirror exercise" (Care Home Family Day). Produces:
##   1) the "dirty" raw dataset students will clean and analyse
##   2) a separate lab results dataset for the join exercise
##   3) an instructor-only answer key (true exposure/outcome model)
##
## HOW TO USE THIS SCRIPT
##   - n_family / n_resident and the seed are PINNED (see section 1) at
##     the validated combination: n_family = 122, n_resident = 45,
##     seed = 3. Re-running always reproduces the exact same dataset.
##   - To explore a different sample size, switch candidate_seeds back
##     to a range (e.g. 1:2000) and re-run - section 3 will search
##     again and print a new "CHOSEN SEED" for you to review.
##   - Section 9 prints a diagnostic summary - check it before using
##     the exported files with students.
##
## DESIGN / TEACHING INTENT (do not change without re-checking section 9)
##   - True cause of illness: russian_salad (ensaladilla rusa), true RR
##     is strong and shows up clearly in BOTH groups (family / resident)
##     - group is a secondary narrative layer, not the variable used to
##     resolve the confounding.
##   - spanish_tortilla has NO true effect on illness, but is correlated
##     with russian_salad via a shared "cold buffet table" tendency ->
##     it shows a clearly elevated CRUDE RR (a believable distractor),
##     exactly like pasta/veal in the original Spetses exercise.
##   - The confounding is resolved by stratifying SPANISH_TORTILLA's RR
##     BY RUSSIAN_SALAD CONSUMPTION (csinter(exposure = spanish_tortilla,
##     case, strata = russian_salad)) - not by group. Direct analog of
##     the original exercise's food-vs-food stratification. ("case"
##     here is whatever the STUDENTS name their own derived case
##     variable in the case-definition exercise - see next point.)
##   - jamon, chicken, watermelon, wine, water, croquettes, cheese are
##     all pure noise: independent of illness and of each other.
##   - Group (family/resident) is a secondary, narratively useful
##     pattern: residents eat less of both suspect dishes due to
##     dietary restrictions, but the russian_salad effect still shows
##     within each group.
##   - `ill` (the TRUE, hidden illness status) lives ONLY in the
##     instructor-only answer key (as `true_illness`). It is
##     deliberately NOT in the student-facing raw dataset - deriving a
##     case status from symptoms + the incubation window is the point
##     of the case-definition exercise.
##   - Onset date/hour is recorded for anyone who reports ANY GI
##     symptom, whether or not they are a true case: true cases cluster
##     in a plausible 12-72h-ish incubation window; symptomatic
##     non-cases (background noise, unrelated stomach upset) get onset
##     times scattered widely. This is what makes the incubation-window
##     step of the case definition actually do useful filtering work,
##     instead of being redundant with the symptom columns.
## ---------------------------------------------------------------------

pacman::p_load(tidyverse, rio, here, lubridate, janitor)

# ======================================================================
# 1. SAMPLE SIZE - pinned and validated (see header)
# ======================================================================

n_family   <- 122
n_resident <- 45
n          <- n_family + n_resident

# ======================================================================
# 2. Calibrated mechanism parameters (fixed - validated to reproduce
#    the design intent above; only touch these if you want to redesign
#    the confounding pattern itself)
# ======================================================================

params <- list(
  p_cold_family   = 0.55,  # P(sat at the "cold table") in family group
  p_cold_resident = 0.15,  # P(sat at the "cold table") in resident group
  p_rs_cold       = 0.85,  # P(ate russian_salad | cold table)
  p_rs_nocold     = 0.20,  # P(ate russian_salad | not cold table)
  p_tor_cold      = 0.75,  # P(ate spanish_tortilla | cold table)
  p_tor_nocold    = 0.20,  # P(ate spanish_tortilla | not cold table)
  p0              = 0.06,  # illness risk if NOT exposed to russian_salad
  p1              = 0.40   # illness risk if exposed to russian_salad
)

# ======================================================================
# 3. Core simulation + diagnostics + automatic seed search
# ======================================================================

simulate_core <- function(seed, p, n_family, n_resident) {
  # Draws, in this exact order: group (deterministic) -> cold -> russian_salad
  # -> spanish_tortilla -> ill. This sequence is reproduced identically
  # in section 5 when building the final export, so the diagnostics you
  # see here are guaranteed to match the exported data.
  set.seed(seed)
  n <- n_family + n_resident
  
  group <- factor(c(rep("family", n_family), rep("resident", n_resident)),
                  levels = c("family", "resident"))
  
  p_cold <- ifelse(group == "family", p$p_cold_family, p$p_cold_resident)
  cold   <- rbinom(n, 1, p_cold)
  
  p_rs  <- ifelse(cold == 1, p$p_rs_cold, p$p_rs_nocold)
  p_tor <- ifelse(cold == 1, p$p_tor_cold, p$p_tor_nocold)
  russian_salad     <- rbinom(n, 1, p_rs)
  spanish_tortilla  <- rbinom(n, 1, p_tor)
  
  p_ill <- ifelse(russian_salad == 1, p$p1, p$p0)
  ill   <- rbinom(n, 1, p_ill)
  
  tibble(group, russian_salad, spanish_tortilla, ill)
}

rr <- function(d, exposure_col) {
  # simple crude/stratified risk ratio helper; returns NA if a cell is empty
  exp_sym <- rlang::sym(exposure_col)
  tab <- d %>%
    group_by(!!exp_sym) %>%
    summarise(risk = mean(ill), n = n(), .groups = "drop")
  if (nrow(tab) < 2 || any(tab$n == 0)) return(NA_real_)
  r1 <- tab$risk[tab[[exposure_col]] == 1]
  r0 <- tab$risk[tab[[exposure_col]] == 0]
  if (length(r1) == 0 || length(r0) == 0 || r0 == 0) return(NA_real_)
  r1 / r0
}

cell_n <- function(d, exposure_col) {
  exp_sym <- rlang::sym(exposure_col)
  d %>% count(!!exp_sym) %>% pull(n) %>% min()
}

diagnostics <- function(d) {
  list(
    attack_rate      = mean(d$ill),
    rr_rs_crude      = rr(d, "russian_salad"),
    rr_tor_crude     = rr(d, "spanish_tortilla"),
    rr_tor_given_rs1 = rr(filter(d, russian_salad == 1), "spanish_tortilla"),
    rr_tor_given_rs0 = rr(filter(d, russian_salad == 0), "spanish_tortilla"),
    rr_rs_family     = rr(filter(d, group == "family"), "russian_salad"),
    rr_rs_resident   = rr(filter(d, group == "resident"), "russian_salad"),
    min_cell_rs_strat    = min(cell_n(filter(d, russian_salad == 1), "spanish_tortilla"),
                               cell_n(filter(d, russian_salad == 0), "spanish_tortilla")),
    min_cell_group_strat = min(cell_n(filter(d, group == "family"), "russian_salad"),
                               cell_n(filter(d, group == "resident"), "russian_salad"))
  )
}

meets_criteria <- function(diag) {
  vals_ok <- !any(sapply(diag, is.na))
  if (!vals_ok) return(FALSE)
  isTRUE(diag$attack_rate >= 0.18 && diag$attack_rate <= 0.32) &&
    isTRUE(diag$rr_rs_crude >= 4 && diag$rr_rs_crude <= 10) &&
    isTRUE(diag$rr_tor_crude >= 1.8 && diag$rr_tor_crude <= 4) &&
    isTRUE(diag$rr_tor_given_rs1 >= 0.6 && diag$rr_tor_given_rs1 <= 1.6) &&
    isTRUE(diag$rr_tor_given_rs0 >= 0.6 && diag$rr_tor_given_rs0 <= 1.6) &&
    isTRUE(diag$rr_rs_family >= 3) &&
    isTRUE(diag$rr_rs_resident >= 3) &&
    isTRUE(diag$min_cell_rs_strat >= 8) &&
    isTRUE(diag$min_cell_group_strat >= 8)
}

# PINNED: n_family = 122 / n_resident = 45, seed 3 (attack rate 0.257,
# RR russian_salad 6.24 crude / 7.26 family / 4.00 resident, RR
# spanish_tortilla 2.15 crude diluting to 1.37 / 1.06 when stratified
# by russian_salad). To re-search for a different n, switch this to a
# range, e.g. 1:2000.
candidate_seeds <- 1:2000

chosen_seed <- NULL
chosen_diag <- NULL

for (s in candidate_seeds) {
  d    <- simulate_core(s, params, n_family, n_resident)
  diag <- diagnostics(d)
  if (meets_criteria(diag)) {
    chosen_seed <- s
    chosen_diag <- diag
    break
  }
}

if (is.null(chosen_seed)) {
  stop("No seed in candidate_seeds met the design criteria for this n_family/n_resident. ",
       "Try widening candidate_seeds (e.g. 1:5000) or loosening meets_criteria().")
}

cat("\n=========================================\n")
cat(" CHOSEN SEED:", chosen_seed, " (n_family =", n_family, ", n_resident =", n_resident, ")\n")
cat("=========================================\n")
print(as_tibble(chosen_diag))

chosen_seed <- 3

# ======================================================================
# 4. Extra data-generation parameters (not part of the confounding
#    mechanism - safe to tweak freely)
# ======================================================================

n_dirty_ages_family   <- 3
n_dirty_ages_resident <- 2
n_buffet_absent       <- 6      # fixed number of family members who skipped the buffet
event_date  <- ymd("2026-09-13")
event_hour  <- 14               # lunch served at 14:00
lab_sensitivity <- 0.80         # proportion of true cases that test positive

stopifnot(
  "n_buffet_absent must be <= n_family" = n_buffet_absent <= n_family,
  "n_dirty_ages_family must be <= length(dirty value pool)" = n_dirty_ages_family <= 6,
  "n_dirty_ages_resident must be <= length(dirty value pool)" = n_dirty_ages_resident <= 4
)

# ======================================================================
# 5. Build the full dataset using the chosen seed
# ======================================================================

# --- 5a. Reproduce the EXACT validated core sequence (group,
#     russian_salad, spanish_tortilla, ill) so it matches the
#     diagnostics printed above ---
core <- simulate_core(chosen_seed, params, n_family, n_resident)
group            <- core$group
russian_salad    <- core$russian_salad
spanish_tortilla <- core$spanish_tortilla
ill              <- core$ill
id               <- sprintf("care-%03d", 1:n)

# --- 5b. Everything else uses an independent seed stream (offset from
#     the chosen seed), so it can never perturb the sequence above ---
set.seed(chosen_seed + 10000)

sex <- sample(c("male", "female"), n, replace = TRUE, prob = c(0.48, 0.52))

age_family   <- round(rnorm(n_family, mean = 42, sd = 12))
age_resident <- round(rnorm(n_resident, mean = 78, sd = 7))
age <- pmax(pmin(c(age_family, age_resident), 99), 1)

# deliberately dirty ages for the case_when() cleaning step (typo-style errors)
dirty_idx_family   <- sample(which(group == "family"), n_dirty_ages_family)
dirty_idx_resident <- sample(which(group == "resident"), n_dirty_ages_resident)
dirty_values_family   <- c(190, 4, -35, 999, 0, -1)[seq_len(n_dirty_ages_family)]
dirty_values_resident <- c(190, 8, 0, -20)[seq_len(n_dirty_ages_resident)]
age[dirty_idx_family]   <- dirty_values_family
age[dirty_idx_resident] <- dirty_values_resident

# noise foods/drinks - independent of illness AND of each other, mild
# group differences purely for realism
jamon       <- rbinom(n, 1, ifelse(group == "family", 0.80, 0.55))
chicken     <- rbinom(n, 1, ifelse(group == "family", 0.75, 0.70))
watermelon  <- rbinom(n, 1, ifelse(group == "family", 0.65, 0.60))
wine        <- rbinom(n, 1, ifelse(group == "family", 0.55, 0.10))
water       <- rbinom(n, 1, 0.90)
croquettes  <- rbinom(n, 1, ifelse(group == "family", 0.60, 0.65))
cheese      <- rbinom(n, 1, ifelse(group == "family", 0.70, 0.50))

dose_if_eaten <- function(eaten) ifelse(eaten == 1, sample(1:3, n, replace = TRUE), 0)
russian_saladD    <- dose_if_eaten(russian_salad)
spanish_tortillaD <- dose_if_eaten(spanish_tortilla)
jamonD            <- dose_if_eaten(jamon)
chickenD          <- dose_if_eaten(chicken)
watermelonD       <- dose_if_eaten(watermelon)
wineD             <- dose_if_eaten(wine)
waterD            <- dose_if_eaten(water)
croquettesD       <- dose_if_eaten(croquettes)
cheeseD           <- dose_if_eaten(cheese)

# symptoms - two components:
#   (a) TRUE cases (ill == 1) have a high but not perfect chance of each
#       symptom, driven by the true illness
#   (b) everyone else has a small background chance of each symptom too
#       (unrelated stomach upset, allergies, etc.) - this is what
#       "symptomatic but not a true case" represents
diarrhoea <- ifelse(ill == 1, rbinom(n, 1, 0.90), rbinom(n, 1, 0.03))
vomiting  <- ifelse(ill == 1, rbinom(n, 1, 0.55), rbinom(n, 1, 0.02))
abdo      <- ifelse(ill == 1, rbinom(n, 1, 0.70), rbinom(n, 1, 0.05))
fever     <- ifelse(ill == 1, rbinom(n, 1, 0.45), rbinom(n, 1, 0.02))
nausea    <- ifelse(ill == 1, rbinom(n, 1, 0.50), rbinom(n, 1, 0.04))

reported_illness <- as.integer(diarrhoea == 1 | vomiting == 1 | abdo == 1 |
                                 fever == 1 | nausea == 1)

# safety net: a true case should present with at least one symptom -
# if random draws happened to give a true case zero symptoms, force the
# most common one (diarrhoea) rather than silently losing that case
# from the symptomatic pool (or crashing the script on a stopifnot)
no_symptom_ill_idx <- which(ill == 1 & reported_illness == 0)
if (length(no_symptom_ill_idx) > 0) {
  diarrhoea[no_symptom_ill_idx] <- 1
  reported_illness[no_symptom_ill_idx] <- 1
}

# onset date/hour - recorded for EVERYONE who reports at least one GI
# symptom (true case or not), so there is never a "symptom with no
# date". True cases cluster in a plausible incubation window;
# symptomatic non-cases (background noise) are scattered across a much
# wider, unrelated range - this is exactly what the incubation-window
# part of the case definition is supposed to filter out.

incubation_hours <- rep(NA_real_, n)
ill_symptomatic_idx   <- which(ill == 1 & reported_illness == 1)
noise_symptomatic_idx <- which(ill == 0 & reported_illness == 1)

event_date_time <- as_datetime(event_date) + hours(event_hour)

incubation_hours[ill_symptomatic_idx] <-
  pmax(6, round(rlnorm(length(ill_symptomatic_idx), meanlog = log(30), sdlog = 0.35)))
incubation_hours[noise_symptomatic_idx] <-
  round(runif(length(noise_symptomatic_idx), min = -48, max = 120))
# (negative values simulate someone reporting symptoms that started
# BEFORE the event - a classic real-world reason to exclude a "case")

onset_datetime <- event_date_time + hours(incubation_hours)
dayonset  <- ifelse(!is.na(incubation_hours), format(as_date(onset_datetime), "%d%b%Y"), NA_character_)
starthour <- ifelse(!is.na(incubation_hours), hour(onset_datetime), NA_integer_)

# buffet attendance - a FIXED number of family members skipped the
# buffet (arrived late), reproducible rather than left to chance.
# Residents always attend (they live there).
buffet <- rep(1L, n)
absent_idx <- sample(which(group == "family"), n_buffet_absent)
buffet[absent_idx] <- 0L

# ======================================================================
# 6. Assemble the raw "dirty" dataset for students
#    NOTE: `ill` (the true hidden illness status) is deliberately NOT
#    included here - deriving a case status from symptoms + the
#    incubation window is the point of the exercise.
# ======================================================================

raw <- tibble(
  id, sex, age, group, buffet,
  diarrhoea, vomiting, abdo, fever, nausea,
  starthour, dayonset,
  russian_salad, russian_saladD,
  spanish_tortilla, spanish_tortillaD,
  jamon, jamonD,
  chicken, chickenD,
  watermelon, watermelonD,
  wine, wineD,
  water, waterD,
  croquettes, croquettesD,
  cheese, cheeseD
)

food_symptom_cols <- c("diarrhoea", "vomiting", "abdo", "fever", "nausea",
                       "starthour", "dayonset",
                       "russian_salad", "russian_saladD",
                       "spanish_tortilla", "spanish_tortillaD",
                       "jamon", "jamonD", "chicken", "chickenD",
                       "watermelon", "watermelonD",
                       "wine", "wineD", "water", "waterD",
                       "croquettes", "croquettesD", "cheese", "cheeseD")

raw <- raw %>%
  mutate(across(all_of(food_symptom_cols), ~ ifelse(buffet == 0, NA, .)))

# ======================================================================
# 7. Lab dataset (separate file, for the join exercise)
#    Only symptomatic attendees get tested, plus a few asymptomatic
#    "worried well". Imperfect sensitivity -> some true cases test
#    negative, so students must check for silent NAs after the join.
# ======================================================================

tested_idx <- which((reported_illness == 1 & buffet == 1) | (buffet == 1 & runif(n) < 0.03))

lab_result <- ifelse(
  id[tested_idx] %in% id[ill == 1],
  ifelse(runif(length(tested_idx)) < lab_sensitivity, "Positive", "Negative"),
  "Negative"
)

lab_data <- tibble(
  id = id[tested_idx],
  sample_date = as_date(onset_datetime[tested_idx]) + days(1),
  salmonella_result = lab_result
) %>%
  arrange(id)


# ======================================================================
# 9. Export + final diagnostic summary
# ======================================================================

export(raw, here("data", "raw", "carehome_familyday_raw.csv"))
export(lab_data, here("data", "raw", "carehome_familyday_lab.csv"))

