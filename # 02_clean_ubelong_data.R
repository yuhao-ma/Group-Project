# ============================================================
# 02_clean_ubelong_data.R
# Data cleaning for UBelong longitudinal dataset
# ============================================================

library(tidyverse)
library(janitor)
library(lubridate)

# ------------------------------------------------------------
# 1. File paths
# ------------------------------------------------------------

raw_path <- "data/raw/ubelong_longitudinalData_FINAL201225_forReShare.csv"

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 2. Import raw data
# ------------------------------------------------------------
dat_raw <- ubelong_longitudinalData_FINAL201225_forReShare|>
	clean_names()

# Basic checks
glimpse(dat_raw)

dat_raw |>
	summarise(
		n_rows = n(),
		n_participants = n_distinct(participant_id)
	)

dat_raw |>
	count(timepoint)

dat_raw |>
	count(completion_level)

dat_raw |>
	count(longitudinal)

# ------------------------------------------------------------
# 3. Basic variable cleaning
# ------------------------------------------------------------

dat_clean <- dat_raw |>
	mutate(
		# Make timepoint ordered
		timepoint = factor(
			timepoint,
			levels = c("T1", "T2"),
			ordered = TRUE
		),
		
		# Numeric time variable for modelling if needed
		time_num = case_when(
			timepoint == "T1" ~ 0,
			timepoint == "T2" ~ 1,
			TRUE ~ NA_real_
		),
		
		# Convert survey year if needed
		survey_year = suppressWarnings(as.numeric(survey_year)),
		
		# date_completed is month-year, e.g. Sep-23
		date_completed_month = suppressWarnings(my(date_completed)),
		
		# Make longitudinal indicator easier to read
		longitudinal_binary = case_when(
			longitudinal == 1 ~ TRUE,
			longitudinal == 0 ~ FALSE,
			TRUE ~ NA
		)
	)

# ------------------------------------------------------------
# 4. Identify actual response rows
# ------------------------------------------------------------

key_response_vars <- c(
	"gad_full_score",
	"phq_full_score",
	"ucla_full_score",
	"djgls_full_score",
	"scs_full_score",
	"mspss_full_score",
	"ons_full_score",
	"gpc_full_score"
)

dat_clean <- dat_clean |>
	mutate(
		has_key_response = if_any(
			all_of(key_response_vars),
			~ !is.na(.x)
		)
	)

# Check structural missingness
response_check <- dat_clean |>
	count(timepoint, completion_level, has_key_response)

response_check

write_csv(
	response_check,
	"outputs/tables/ubelong_response_check.csv"
)

# ------------------------------------------------------------
# 5. Keep actual response rows only
# ------------------------------------------------------------

dat_responses <- dat_clean |>
	filter(has_key_response)

dat_responses |>
	summarise(
		n_rows = n(),
		n_participants = n_distinct(participant_id)
	)

dat_responses |>
	count(timepoint)

actual_wave_check <- dat_responses |>
	distinct(participant_id, timepoint) |>
	count(participant_id, name = "n_actual_waves") |>
	count(n_actual_waves)

actual_wave_check

write_csv(
	actual_wave_check,
	"outputs/tables/ubelong_actual_wave_check.csv"
)
# ------------------------------------------------------------
# 6. Create longitudinal subset: participants with both T1 and T2
# ------------------------------------------------------------

longitudinal_ids <- dat_responses |>
	distinct(participant_id, timepoint) |>
	count(participant_id, name = "n_actual_waves") |>
	filter(n_actual_waves == 2) |>
	pull(participant_id)

dat_longitudinal <- dat_responses |>
	filter(participant_id %in% longitudinal_ids)

longitudinal_summary <- dat_longitudinal |>
	summarise(
		n_rows = n(),
		n_participants = n_distinct(participant_id)
	)

longitudinal_summary

dat_longitudinal |>
	count(timepoint)

write_csv(
	longitudinal_summary,
	"outputs/tables/ubelong_longitudinal_summary.csv"
)

# ------------------------------------------------------------
# 7. Create core analysis dataset
# ------------------------------------------------------------

core_vars <- c(
	"participant_id",
	"timepoint",
	"time_num",
	"survey_year",
	"date_completed",
	"date_completed_month",
	"completion_level",
	"longitudinal",
	"longitudinal_binary",
	
	# demographics
	"age",
	"gender",
	"ethnicity",
	"ethnicity_level1",
	"sexual_ori",
	"disability_yn",
	"international",
	"year_of_study",
	"hei",
	"hei_cluster",
	"hei_type",
	"ses",
	
	# key mental health and wellbeing outcomes
	"gad_full_score",
	"phq_full_score",
	"ucla_full_score",
	"djgls_full_score",
	"scs_full_score",
	"mspss_full_score",
	"ons_full_score",
	"gpc_full_score",
	"bes_full_score",
	"sias_full_score",
	"dir_lon_full_score"
)

dat_longitudinal_core <- dat_longitudinal |>
	select(any_of(core_vars))

glimpse(dat_longitudinal_core)
# ------------------------------------------------------------
# 8. Missingness summary for core analysis variables
# ------------------------------------------------------------

core_missing_summary <- dat_longitudinal_core |>
	summarise(
		across(
			everything(),
			~ sum(is.na(.x))
		)
	) |>
	pivot_longer(
		cols = everything(),
		names_to = "variable",
		values_to = "n_missing"
	) |>
	mutate(
		percent_missing = round(n_missing / nrow(dat_longitudinal_core) * 100, 2)
	) |>
	arrange(desc(percent_missing))

core_missing_summary

write_csv(
	core_missing_summary,
	"outputs/tables/ubelong_core_missing_summary.csv"
)
# ------------------------------------------------------------
# 9. Save cleaned datasets
# ------------------------------------------------------------

write_csv(
	dat_clean,
	"data/processed/ubelong_clean_full_with_flags.csv"
)

write_csv(
	dat_responses,
	"data/processed/ubelong_actual_responses.csv"
)

write_csv(
	dat_longitudinal,
	"data/processed/ubelong_longitudinal_only.csv"
)

write_csv(
	dat_longitudinal_core,
	"data/processed/ubelong_longitudinal_core.csv"
)

saveRDS(
	dat_longitudinal_core,
	"data/processed/ubelong_longitudinal_core.rds"
)
