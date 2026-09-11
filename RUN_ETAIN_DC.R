# S2DM representative-user dose model
# Input sources:
# - DDM_person_scenario.csv: representative-user behavior and durations
# - Exposure indicator_CALL_DATA_FF__06072026_V1.xlsx: original source workbook
# - dose_model_parameters.xlsx: fixed parameters extracted from the source workbook
# - representative_user_signal_inputs.csv: optional per-user signal strength inputs
#
# Signal logic:
# - If a representative user has signal values in representative_user_signal_inputs.csv,
#   those values are used.
# - If signal values are missing, P25/P50/P75 values from dose_model_parameters.xlsx
#   are used as fallback scenarios.

library(dplyr)
library(readr)
library(readxl)
library(tidyr)

base_dir <- "C:/Users/jaliha/Documents/Codexin/ETAIN_dose_model"
input_dir <- file.path(base_dir, "input")
output_dir <- file.path(base_dir, "output", "RUN_ETAIN_DC")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

ddm_path <- file.path(input_dir, "DDM_person_scenario.csv")
parameter_path <- file.path(input_dir, "dose_model_parameters.xlsx")
source_workbook_path <- file.path(
  input_dir,
  "Exposure indicator_CALL_DATA_FF__06072026_V1.xlsx"
)
user_signal_path <- file.path(input_dir, "representative_user_signal_inputs.csv")

# =========================
# Helpers
# =========================

as_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

read_parameter_table <- function(sheet) {
  read_excel(parameter_path, sheet = sheet)
}

read_named_parameters <- function(sheet) {
  tbl <- read_parameter_table(sheet)
  stats::setNames(as_num(tbl$value), tbl$parameter)
}

get_scenario_prop <- function(tbl, group_name, scenario_name, component_name) {
  value <- tbl %>%
    filter(
      .data$group == group_name,
      .data$scenario == scenario_name,
      .data$network_component == component_name
    ) %>%
    pull(.data$proportion)

  if (length(value) != 1) {
    stop(
      "Missing or duplicated scenario proportion for ",
      group_name, " / ", scenario_name, " / ", component_name
    )
  }

  as_num(value)
}

safe_divide <- function(numerator, denominator) {
  ifelse(is.na(denominator) | denominator == 0, 0, numerator / denominator)
}

clean_sample_id <- function(x) {
  dplyr::recode(
    x,
    "Non-WPD user" = "Non- WPD user",
    "non-WPD user" = "Non- WPD user",
    "non- wireless personal device user" = "Non- WPD user",
    .default = x
  )
}

has_user_signal <- function(signal_row) {
  has_lte <- !is.na(signal_row$lte_rsrp_dbm) && !is.na(signal_row$lte_rsrq_db)
  has_nr <- !isTRUE(signal_row$use_5g_logical) || !is.na(signal_row$nr_ssrsrp_dbm)

  has_lte && has_nr
}

build_user_signal_rows <- function(user_row, user_signal_inputs, signal_percentiles) {
  signal_row <- user_signal_inputs %>%
    filter(.data$sample_id == user_row$sample_id)

  if (nrow(signal_row) > 1) {
    stop("More than one signal-input row found for: ", user_row$sample_id)
  }

  if (nrow(signal_row) == 1 && has_user_signal(signal_row[1, ])) {
    return(
      signal_row %>%
        transmute(
          signal_level = "user_signal",
          signal_source = "representative_user_signal_inputs",
          lte_rsrp_dbm = .data$lte_rsrp_dbm,
          lte_rsrq_db = .data$lte_rsrq_db,
          nr_ssrsrp_dbm = .data$nr_ssrsrp_dbm
        )
    )
  }

  signal_percentiles %>%
    mutate(signal_source = "fallback_percentile")
}

# =========================
# Frequency conversion
# =========================

lte_downlink_frequency_mhz <- function(earfcn) {
  case_when(
    earfcn <= 599 ~ 2110 + 0.1 * (earfcn - 0),
    earfcn <= 1199 ~ 1930 + 0.1 * (earfcn - 600),
    earfcn <= 1949 ~ 1805 + 0.1 * (earfcn - 1200),
    earfcn <= 3449 ~ 2620 + 0.1 * (earfcn - 2750),
    earfcn <= 3799 ~ 925 + 0.1 * (earfcn - 3450),
    earfcn <= 6449 ~ 791 + 0.1 * (earfcn - 6150),
    earfcn <= 7399 ~ 3510 + 0.1 * (earfcn - 6600),
    earfcn <= 9659 ~ 758 + 0.1 * (earfcn - 9210),
    earfcn <= 10359 ~ 1452 + 0.1 * (earfcn - 9920),
    earfcn <= 38249 ~ 2570 + 0.1 * (earfcn - 37750),
    TRUE ~ NA_real_
  )
}

lte_uplink_frequency_mhz <- function(earfcn) {
  case_when(
    earfcn <= 599 ~ 1920 + 0.1 * (earfcn - 0),
    earfcn <= 1199 ~ 1850 + 0.1 * (earfcn - 600),
    earfcn <= 1949 ~ 1710 + 0.1 * (earfcn - 1200),
    earfcn <= 3449 ~ 2500 + 0.1 * (earfcn - 2750),
    earfcn <= 3799 ~ 880 + 0.1 * (earfcn - 3450),
    earfcn <= 6449 ~ 832 + 0.1 * (earfcn - 6150),
    earfcn <= 7399 ~ 3410 + 0.1 * (earfcn - 6600),
    earfcn <= 9659 ~ 703 + 0.1 * (earfcn - 9210),
    earfcn <= 38249 ~ 2570 + 0.1 * (earfcn - 37750),
    TRUE ~ NA_real_
  )
}

nr_frequency_mhz <- function(nrarfcn) {
  case_when(
    nrarfcn <= 599999 ~ 0.005 * nrarfcn,
    nrarfcn <= 2016666 ~ 3000 + 0.015 * (nrarfcn - 600000),
    TRUE ~ 24250.08 + 0.06 * (nrarfcn - 2016667)
  )
}

normalized_signal_db <- function(signal_dbm, frequency_mhz) {
  signal_dbm + 20 * log10(frequency_mhz / 1800)
}

# =========================
# Signal to exposure formulas
# =========================

lte_rsrp_exposure_vm <- function(normalized_lte_dl_db, lte_rsrq_db) {
  ifelse(
    is.na(normalized_lte_dl_db) | is.na(lte_rsrq_db),
    0,
    10^((151.82281687 + 0.73934626 * normalized_lte_dl_db -
           1.33658123 * lte_rsrq_db) / 20) / 1000000
  )
}

lte_rssi_exposure_vm <- function(normalized_lte_dl_db, lte_rsrq_db) {
  ifelse(
    is.na(normalized_lte_dl_db) | is.na(lte_rsrq_db),
    0,
    10^((151.02485845 + 0.72082081 * normalized_lte_dl_db -
           0.56521145 * lte_rsrq_db) / 20) / 1000000
  )
}

nr_ssrsrp_exposure_vm <- function(normalized_nr_dl_db) {
  ifelse(
    is.na(normalized_nr_dl_db),
    0,
    10^((145.29697777 + 0.49580818 * normalized_nr_dl_db) / 20) / 1000000
  )
}

phone_call_power_4g_native <- function(normalized_lte_ul_db) {
  ifelse(
    is.na(normalized_lte_ul_db),
    0,
    10^((9.16782 - 0.10642 * normalized_lte_ul_db -
           0.47428 * pmax(0, normalized_lte_ul_db + 115)) / 10)
  )
}

phone_call_power_4g_data <- function(normalized_lte_ul_db) {
  ifelse(
    is.na(normalized_lte_ul_db),
    0,
    10^((6.795432 - 0.131499 * normalized_lte_ul_db -
           0.902453 * pmax(0, normalized_lte_ul_db + 92.384)) / 10)
  )
}

phone_call_power_5g_data <- function(normalized_nr_ul_db) {
  ifelse(
    is.na(normalized_nr_ul_db),
    0,
    10^((13.0413 - 0.06774 * normalized_nr_ul_db -
           0.55511 * pmax(0, normalized_nr_ul_db + 100)) / 10)
  )
}

mobile_data_power_4g <- function(normalized_lte_ul_db) {
  ifelse(
    is.na(normalized_lte_ul_db),
    0,
    10^((11.971024 - 0.081254 * normalized_lte_ul_db -
           0.264309 * pmax(0, normalized_lte_ul_db + 97.113)) / 10)
  )
}

mobile_data_power_5g <- function(normalized_nr_ul_db) {
  ifelse(
    is.na(normalized_nr_ul_db),
    0,
    10^((11.39817 - 0.09704 * normalized_nr_ul_db -
           0.18295 * pmax(0, normalized_nr_ul_db + 96.354)) / 10)
  )
}

# =========================
# Load inputs
# =========================

if (!file.exists(source_workbook_path)) {
  warning(
    "Original source workbook not found: ",
    source_workbook_path,
    ". Calculations will continue using dose_model_parameters.xlsx."
  )
}

signal_percentiles <- read_parameter_table("signal_percentiles") %>%
  mutate(
    signal_source = "fallback_percentile",
    lte_rsrp_dbm = as_num(.data$lte_rsrp_dbm),
    lte_rsrq_db = as_num(.data$lte_rsrq_db),
    nr_ssrsrp_dbm = as_num(.data$nr_ssrsrp_dbm)
  )

lte_earfcn <- read_parameter_table("lte_earfcn") %>%
  mutate(earfcn = as_num(.data$earfcn))

nr_parameters <- read_named_parameters("nr_parameters")
coefficients <- read_named_parameters("coefficients")

scenario_proportions <- read_parameter_table("scenario_proportions") %>%
  mutate(proportion = as_num(.data$proportion))

far_field_contribution <- read_parameter_table("far_field_contribution") %>%
  mutate(
    ff_4g = as_num(.data$ff_4g),
    ff_5g = as_num(.data$ff_5g),
    rssi_proxy_used = as.logical(.data$rssi_proxy_used)
  ) %>%
  filter(!.data$rssi_proxy_used)

duty_cycles <- read_named_parameters("duty_cycles")

sar_table <- read_parameter_table("sar_table") %>%
  mutate(across(everything(), as_num))

ddm <- read_csv(ddm_path, show_col_types = FALSE) %>%
  mutate(
    across(
      -c(sample_id, use_5g, country, urbanicity),
      ~ as_num(.x)
    ),
    sample_id = clean_sample_id(.data$sample_id),
    mobile_data_duration_s =
      mpd_dur_low +
      mpd_dur_lowtomed +
      mpd_dur_medtohigh +
      mpd_dur_high,
    browsing_prop = safe_divide(mpd_dur_low, mobile_data_duration_s),
    voice_prop = safe_divide(mpd_dur_lowtomed, mobile_data_duration_s),
    video_prop = safe_divide(mpd_dur_medtohigh, mobile_data_duration_s),
    upload_prop = safe_divide(mpd_dur_high, mobile_data_duration_s),
    use_5g_logical = use_5g %in% c(TRUE, "TRUE", "True", "true", 1, "1")
  )

if (file.exists(user_signal_path)) {
  user_signal_inputs <- read_csv(user_signal_path, show_col_types = FALSE) %>%
    mutate(
      sample_id = clean_sample_id(.data$sample_id),
      use_5g_logical = use_5g %in% c(TRUE, "TRUE", "True", "true", 1, "1"),
      lte_rsrp_dbm = as_num(.data$lte_rsrp_dbm),
      lte_rsrq_db = as_num(.data$lte_rsrq_db),
      nr_ssrsrp_dbm = as_num(.data$nr_ssrsrp_dbm)
    )
} else {
  user_signal_inputs <- ddm %>%
    transmute(
      sample_id,
      use_5g,
      use_5g_logical,
      lte_rsrp_dbm = NA_real_,
      lte_rsrq_db = NA_real_,
      nr_ssrsrp_dbm = NA_real_
    )
}

# =========================
# Derived fixed model values
# =========================

sar_4g <- sar_table %>%
  summarise(
    brain_phone_head = sum(brain_sar_phone_head * freq_weight_4g),
    skin_phone_head = sum(skin_sar_phone_head * freq_weight_4g),
    wb_phone_head = sum(wb_sar_phone_head * freq_weight_4g),
    brain_phone_eyes_30cm = sum(brain_sar_phone_eyes_30cm * freq_weight_4g),
    skin_phone_eyes_30cm = sum(skin_sar_phone_eyes_30cm * freq_weight_4g),
    wb_phone_eyes_30cm = sum(wb_sar_phone_eyes_30cm * freq_weight_4g),
    brain_ff = sum(brain_sar_ff * freq_weight_4g),
    skin_ff = sum(skin_sar_ff * freq_weight_4g),
    wb_ff = sum(wb_sar_ff * freq_weight_4g),
    .groups = "drop"
  )

sar_5g <- sar_table %>%
  filter(frequency_mhz == 3500) %>%
  transmute(
    brain_phone_head = brain_sar_phone_head,
    skin_phone_head = skin_sar_phone_head,
    wb_phone_head = wb_sar_phone_head,
    brain_phone_eyes_30cm = brain_sar_phone_eyes_30cm,
    skin_phone_eyes_30cm = skin_sar_phone_eyes_30cm,
    wb_phone_eyes_30cm = wb_sar_phone_eyes_30cm,
    brain_ff = brain_sar_ff,
    skin_ff = skin_sar_ff,
    wb_ff = wb_sar_ff
  )

lte_freq <- lte_earfcn %>%
  mutate(
    lte_dl_freq_mhz = lte_downlink_frequency_mhz(.data$earfcn),
    lte_ul_freq_mhz = lte_uplink_frequency_mhz(.data$earfcn)
  )

nr_freq_mhz <- nr_frequency_mhz(nr_parameters[["nr_0_nrarfcn"]])

# =========================
# Calculation functions
# =========================

build_signal_context <- function(signal_row) {
  lte_signals <- lte_freq %>%
    mutate(
      normalized_lte_dl_db =
        normalized_signal_db(signal_row$lte_rsrp_dbm, .data$lte_dl_freq_mhz),
      normalized_lte_ul_db =
        normalized_signal_db(signal_row$lte_rsrp_dbm, .data$lte_ul_freq_mhz),
      lte_rsrp_exposure_v_m =
        lte_rsrp_exposure_vm(.data$normalized_lte_dl_db, signal_row$lte_rsrq_db),
      # LTE-RSRP is used as a proxy for LTE-RSSI for Cases 3 and 4.
      lte_rssi_proxy_exposure_v_m =
        lte_rssi_exposure_vm(.data$normalized_lte_dl_db, signal_row$lte_rsrq_db)
    )

  nr_dl_db <- normalized_signal_db(signal_row$nr_ssrsrp_dbm, nr_freq_mhz)
  nr_ul_db <- normalized_signal_db(signal_row$nr_ssrsrp_dbm, nr_freq_mhz)
  nr_exposure <- nr_ssrsrp_exposure_vm(nr_dl_db)

  list(
    lte = lte_signals,
    normalized_lte_ul_db = lte_signals$normalized_lte_ul_db[1],
    normalized_nr_ul_db = nr_ul_db,
    lte_rsrp_total_exposure_v_m = sqrt(sum(lte_signals$lte_rsrp_exposure_v_m^2)),
    lte_rssi_proxy_total_exposure_v_m =
      sqrt(sum(lte_signals$lte_rssi_proxy_exposure_v_m^2)),
    nr_exposure_v_m = nr_exposure
  )
}

calculate_downlink <- function(user_row, signal_row, signal_context) {
  far_field_contribution %>%
    rowwise() %>%
    mutate(
      valid_for_user = TRUE,
      lte_exposure_component =
        ifelse(
          rssi_proxy_used,
          signal_context$lte_rssi_proxy_total_exposure_v_m,
          signal_context$lte_rsrp_total_exposure_v_m
        ),
      exposure_v_m = sqrt(
        ff_4g * lte_exposure_component^2 +
          ff_5g * signal_context$nr_exposure_v_m^2
      ),
      brain_sar_w_kg =
        (ff_4g * lte_exposure_component^2 *
           coefficients[["e2_to_w_m2"]] * sar_4g$brain_ff) +
        (ff_5g * signal_context$nr_exposure_v_m^2 *
           coefficients[["e2_to_w_m2"]] * sar_5g$brain_ff),
      brain_dose_mj_kg_day =
        coefficients[["environmental_duration_s"]] *
        (
          (ff_4g * lte_exposure_component^2 *
             coefficients[["e2_to_mw_m2"]] * sar_4g$brain_ff) +
          (ff_5g * signal_context$nr_exposure_v_m^2 *
             coefficients[["e2_to_mw_m2"]] * sar_5g$brain_ff)
        ),
      skin_dose_mj_kg_day =
        coefficients[["environmental_duration_s"]] *
        (
          (ff_4g * lte_exposure_component^2 *
             coefficients[["e2_to_mw_m2"]] * sar_4g$skin_ff) +
          (ff_5g * signal_context$nr_exposure_v_m^2 *
             coefficients[["e2_to_mw_m2"]] * sar_5g$skin_ff)
        ),
      whole_body_dose_mj_kg_day =
        coefficients[["environmental_duration_s"]] *
        (
          (ff_4g * lte_exposure_component^2 *
             coefficients[["e2_to_mw_m2"]] * sar_4g$wb_ff) +
          (ff_5g * signal_context$nr_exposure_v_m^2 *
             coefficients[["e2_to_mw_m2"]] * sar_5g$wb_ff)
        ),
      user_profile = user_row$sample_id,
      signal_level = signal_row$signal_level,
      signal_source = signal_row$signal_source,
      module = "Downlink",
      case_or_scenario = .data$case,
      normalized_lte_ul_db = signal_context$normalized_lte_ul_db,
      normalized_nr_ul_db = signal_context$normalized_nr_ul_db,
      max_brain_sar_w_kg = NA_real_
    ) %>%
    mutate(
      across(
        c(
          exposure_v_m,
          brain_sar_w_kg,
          brain_dose_mj_kg_day,
          skin_dose_mj_kg_day,
          whole_body_dose_mj_kg_day
        ),
        ~ ifelse(valid_for_user, .x, NA_real_)
      )
    ) %>%
    ungroup() %>%
    select(
      user_profile, signal_level, signal_source, module, case_or_scenario,
      normalized_lte_ul_db, normalized_nr_ul_db, exposure_v_m,
      brain_sar_w_kg, max_brain_sar_w_kg,
      brain_dose_mj_kg_day, skin_dose_mj_kg_day, whole_body_dose_mj_kg_day,
      rssi_proxy_used
    )
}

calculate_call <- function(user_row, signal_row, signal_context) {
  scenarios <- c(
    "Scenario 1: 4G and 5G call",
    "Scenario 2: 4G call",
    "Scenario 3: 5G call"
  )

  p4g_native <- phone_call_power_4g_native(signal_context$normalized_lte_ul_db)
  p4g_data <- phone_call_power_4g_data(signal_context$normalized_lte_ul_db)
  p5g_data <- phone_call_power_5g_data(signal_context$normalized_nr_ul_db)

  bind_rows(lapply(scenarios, function(scenario_name) {
    prop_4g_native <- get_scenario_prop(
      scenario_proportions, "call", scenario_name, "4G_native_call"
    )
    prop_4g_data <- get_scenario_prop(
      scenario_proportions, "call", scenario_name, "4G_data_call"
    )
    prop_5g_data <- get_scenario_prop(
      scenario_proportions, "call", scenario_name, "5G_data_call"
    )
    valid_for_user <- isTRUE(user_row$use_5g_logical) || prop_5g_data == 0

    brain_sar <- (
      sar_4g$brain_phone_head * coefficients[["mw_to_w"]] *
        prop_4g_native * duty_cycles[["4G_native_call"]] * p4g_native
    ) + (
      sar_4g$brain_phone_head * coefficients[["mw_to_w"]] *
        prop_4g_data * duty_cycles[["4G_data_call"]] * p4g_data
    ) + (
      sar_5g$brain_phone_head * coefficients[["mw_to_w"]] *
        prop_5g_data * duty_cycles[["5G_data_call"]] * p5g_data
    )

    max_factor <- case_when(
      scenario_name == "Scenario 1: 4G and 5G call" ~
        coefficients[["scenario1_10g_to_whole_brain"]],
      scenario_name == "Scenario 2: 4G call" ~
        coefficients[["scenario2_10g_to_whole_brain"]],
      scenario_name == "Scenario 3: 5G call" ~
        coefficients[["scenario3_10g_to_whole_brain"]],
      TRUE ~ NA_real_
    )

    tibble(
      user_profile = user_row$sample_id,
      signal_level = signal_row$signal_level,
      signal_source = signal_row$signal_source,
      module = "Mobile phone call",
      case_or_scenario = scenario_name,
      normalized_lte_ul_db = signal_context$normalized_lte_ul_db,
      normalized_nr_ul_db = signal_context$normalized_nr_ul_db,
      exposure_v_m = NA_real_,
      brain_sar_w_kg = ifelse(valid_for_user, brain_sar, NA_real_),
      max_brain_sar_w_kg = ifelse(valid_for_user, brain_sar * max_factor, NA_real_),
      brain_dose_mj_kg_day =
        ifelse(
          valid_for_user,
          user_row$mpc_duration *
            (
              sar_4g$brain_phone_head * prop_4g_native *
                duty_cycles[["4G_native_call"]] * p4g_native +
              sar_4g$brain_phone_head * prop_4g_data *
                duty_cycles[["4G_data_call"]] * p4g_data +
              sar_5g$brain_phone_head * prop_5g_data *
                duty_cycles[["5G_data_call"]] * p5g_data
            ),
          NA_real_
        ),
      skin_dose_mj_kg_day =
        ifelse(
          valid_for_user,
          user_row$mpc_duration *
            (
              sar_4g$skin_phone_head * prop_4g_native *
                duty_cycles[["4G_native_call"]] * p4g_native +
              sar_4g$skin_phone_head * prop_4g_data *
                duty_cycles[["4G_data_call"]] * p4g_data +
              sar_5g$skin_phone_head * prop_5g_data *
                duty_cycles[["5G_data_call"]] * p5g_data
            ),
          NA_real_
        ),
      whole_body_dose_mj_kg_day =
        ifelse(
          valid_for_user,
          user_row$mpc_duration *
            (
              sar_4g$wb_phone_head * prop_4g_native *
                duty_cycles[["4G_native_call"]] * p4g_native +
              sar_4g$wb_phone_head * prop_4g_data *
                duty_cycles[["4G_data_call"]] * p4g_data +
              sar_5g$wb_phone_head * prop_5g_data *
                duty_cycles[["5G_data_call"]] * p5g_data
            ),
          NA_real_
        ),
      rssi_proxy_used = FALSE
    )
  }))
}

calculate_mobile_data <- function(user_row, signal_row, signal_context) {
  scenarios <- c(
    "Scenario 1: 4G and 5G data",
    "Scenario 2: 4G data",
    "Scenario 3: 5G data"
  )

  p4g <- mobile_data_power_4g(signal_context$normalized_lte_ul_db)
  p5g <- mobile_data_power_5g(signal_context$normalized_nr_ul_db)

  activity_4g <- (
    user_row$browsing_prop * duty_cycles[["4G_browsing"]] * p4g +
      user_row$voice_prop * duty_cycles[["4G_voice"]] * p4g +
      user_row$video_prop * duty_cycles[["4G_video"]] * p4g +
      user_row$upload_prop * duty_cycles[["4G_upload"]] * p4g
  )

  activity_5g <- (
    user_row$browsing_prop * duty_cycles[["5G_browsing"]] * p5g +
      user_row$voice_prop * duty_cycles[["5G_voice"]] * p5g +
      user_row$video_prop * duty_cycles[["5G_video"]] * p5g +
      user_row$upload_prop * duty_cycles[["5G_upload"]] * p5g
  )

  bind_rows(lapply(scenarios, function(scenario_name) {
    prop_4g_data <- get_scenario_prop(
      scenario_proportions, "data", scenario_name, "4G_data"
    )
    prop_5g_data <- get_scenario_prop(
      scenario_proportions, "data", scenario_name, "5G_data"
    )
    valid_for_user <- isTRUE(user_row$use_5g_logical) || prop_5g_data == 0

    tibble(
      user_profile = user_row$sample_id,
      signal_level = signal_row$signal_level,
      signal_source = signal_row$signal_source,
      module = "Mobile phone data",
      case_or_scenario = scenario_name,
      normalized_lte_ul_db = signal_context$normalized_lte_ul_db,
      normalized_nr_ul_db = signal_context$normalized_nr_ul_db,
      exposure_v_m = NA_real_,
      brain_sar_w_kg = NA_real_,
      max_brain_sar_w_kg = NA_real_,
      brain_dose_mj_kg_day =
        ifelse(
          valid_for_user,
          user_row$mobile_data_duration_s *
            (
              sar_4g$brain_phone_eyes_30cm * prop_4g_data * activity_4g +
              sar_5g$brain_phone_eyes_30cm * prop_5g_data * activity_5g
            ),
          NA_real_
        ),
      skin_dose_mj_kg_day =
        ifelse(
          valid_for_user,
          user_row$mobile_data_duration_s *
            (
              sar_4g$skin_phone_eyes_30cm * prop_4g_data * activity_4g +
              sar_5g$skin_phone_eyes_30cm * prop_5g_data * activity_5g
            ),
          NA_real_
        ),
      whole_body_dose_mj_kg_day =
        ifelse(
          valid_for_user,
          user_row$mobile_data_duration_s *
            (
              sar_4g$wb_phone_eyes_30cm * prop_4g_data * activity_4g +
              sar_5g$wb_phone_eyes_30cm * prop_5g_data * activity_5g
            ),
          NA_real_
        ),
      rssi_proxy_used = FALSE
    )
  }))
}

# =========================
# Run model
# =========================

user_order <- c(
  "Default",
  "Influencer",
  "University student",
  "Office worker",
  "Remote worker",
  "Gamer (portable-VR)",
  "Commuter",
  "Farmer",
  "Feature phone user",
  "Non- WPD user"
)

module_order <- c(
  "Mobile phone call",
  "Mobile phone data",
  "Downlink"
)

results <- bind_rows(lapply(seq_len(nrow(ddm)), function(i) {
  user_row <- ddm[i, ]
  user_signal_rows <- build_user_signal_rows(
    user_row,
    user_signal_inputs,
    signal_percentiles
  )

  bind_rows(lapply(seq_len(nrow(user_signal_rows)), function(j) {
    downlink_signal_row <- user_signal_rows[j, ]
    downlink_signal_context <- build_signal_context(downlink_signal_row)

    signal_row <- user_signal_rows[j, ] %>%
      mutate(
        nr_ssrsrp_dbm = ifelse(
          isTRUE(user_row$use_5g_logical),
          .data$nr_ssrsrp_dbm,
          NA_real_
        )
      )
    signal_context <- build_signal_context(signal_row)

    bind_rows(
      calculate_downlink(user_row, downlink_signal_row, downlink_signal_context),
      calculate_call(user_row, signal_row, signal_context),
      calculate_mobile_data(user_row, signal_row, signal_context)
    )
  }))
})) %>%
  mutate(
    user_profile = factor(user_profile, levels = user_order),
    module = factor(module, levels = module_order)
  ) %>%
  arrange(user_profile, signal_level, signal_source, module, case_or_scenario) %>%
  mutate(
    user_profile = as.character(user_profile),
    module = as.character(module)
  )

output_csv <- file.path(output_dir, "representative_user_dose_outputs.csv")
write_csv(results, output_csv)

output_xlsx <- file.path(output_dir, "representative_user_dose_outputs.xlsx")
if (requireNamespace("openxlsx", quietly = TRUE)) {
  openxlsx::write.xlsx(
    list(
      dose_outputs = results,
      signal_percentiles = signal_percentiles,
      user_signal_inputs = user_signal_inputs,
      input_sources = tibble(
        input_source = c(
          "DDM_person_scenario.csv",
          "Exposure indicator_CALL_DATA_FF__06072026_V1.xlsx",
          "dose_model_parameters.xlsx",
          "representative_user_signal_inputs.csv"
        ),
        role = c(
          "Representative-user behavior and durations",
          "Original calculation workbook used as source reference",
          "Fixed parameters extracted from the original calculation workbook",
          "Optional per-user LTE-RSRP, LTE-RSRQ, and NR-ssRSRP values"
        )
      ),
      notes = tibble(
        note = c(
          "Mobile data activity proportions are calculated from DDM_person_scenario.csv durations for each representative user.",
          "If representative_user_signal_inputs.csv has complete signal values for a user, those values are used.",
          "If representative_user_signal_inputs.csv is missing signal values for a user, P25/P50/P75 signal values from dose_model_parameters.xlsx are used.",
          "Far-field RSSI cases are excluded because LTE-RSSI percentiles were not provided.",
          "For users without 5G, scenarios/cases requiring 5G are returned as NA.",
          "Downlink max_brain_sar_w_kg is NA because the source workbook calculates brain SAR and dose, but not a separate peak brain SAR for downlink."
        )
      )
    ),
    output_xlsx,
    overwrite = TRUE
  )
}

message("Wrote: ", output_csv)
if (file.exists(output_xlsx)) {
  message("Wrote: ", output_xlsx)
} else {
  message("Skipped XLSX output because the R package 'openxlsx' is not installed.")
}
