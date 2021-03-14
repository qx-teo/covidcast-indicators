## Functions handling renaming, reformatting, or recoding response columns.

#' Rename question codes to informative descriptions.
#'
#' Column names beginning with "b_" are binary (T/F/NA); with "t_" are user-
#' entered text; with "n_" are user-entered numeric; with "mc_" are multiple
#' choice (where only a single response can be selected); and with "ms_" are
#' so-called multi-select, where multiple responses can be selected.
#'
#' Only binary responses with a third "I don't know" option are mapped from
#' response codes to interpretable values. Multiple choice, multi-select, and
#' pure binary (yes/no) questions use the original numeric response codes.
#'
#' @param input_data    Data frame of individual response data
#'
#' @return Data frame with descriptive column names
#'
#' @importFrom dplyr rename
#'
#' @export
make_human_readable <- function(input_data) {
  input_data <- remap_responses(input_data)
  input_data <- rename_responses(input_data)
  input_data$t_zipcode <- input_data$zip5 # Keep existing parsed zipcode column
  input_data <- create_derivative_columns(input_data)

  return(input_data)
}

#' Rename all columns to make more interpretable.
#'
#' @param df Data frame of individual response data.
#'
#' @return data frame of individual response data with newly mapped columns
rename_responses <- function(df) {
  # Named vector of new response names and the response codes they are replacing.
  # These columns are not available for aggregation:
  #   "t_zipcode" = "A3", -> Please use `zip5` instead
  #   "t_symptoms_other" = "B2_14_TEXT",
  #   "t_unusual_symptoms_other" = "B2c_14_TEXT",
  #   "t_gender_other" = "D1_4_TEXT",
  map_new_old_names <- c(
    ## free response
    # Either number ("n"; can be averaged although may need processing) or text ("t")
    "n_hh_num_sick" = "hh_number_sick", # A2
    "n_hh_num_children" = "A5_1",
    "n_hh_num_adults" = "A5_2",
    "n_hh_num_seniors" = "A5_3",
    "n_cmnty_num_sick" = "A4",
    "n_days_unusual_symptoms" = "B2b",
    "n_contact_num_work" = "C10_1_1",
    "n_contact_num_shopping" = "C10_2_1",
    "n_contact_num_social" = "C10_3_1",
    "n_contact_num_other" = "C10_4_1",
    "n_hh_num_total" = "hh_number_total", # A2b from Waves <4 and summed A5 from Wave 4
    "n_highest_temp_f" = "Q40",
    "n_hh_num_children_old" = "D3", # Wave 1, etc versions of A5
    "n_hh_num_adults_not_self" = "D4",
    "n_hh_num_seniors_not_self" = "D5",

    ## binary response (b)
    ## generally, False (no) is mapped to 2 and True (yes/agreement) is mapped to 1
    "b_consent" = "S1",
    "b_hh_fever" = "hh_fever", # A1_1
    "b_hh_sore_throat" = "hh_sore_throat", # A1_2
    "b_hh_cough" = "hh_cough", # A1_3
    "b_hh_shortness_of_breath" = "hh_short_breath", # A1_4
    "b_hh_difficulty_breathing" = "hh_diff_breath", # A1_5
    "b_tested_ever" = "B8",
    "b_tested_14d" = "t_tested_14d", # B10; "No" coded as 3, but dealt with in conversion to "t_tested_14d"
    "b_wanted_test_14d" = "t_wanted_test_14d", # B12
    "b_state_travel" = "C6", # c_travel_state
    "b_contact_tested_pos" = "C11",
    "b_contact_tested_pos_hh" = "C12",
    "b_hispanic" = "D6",
    "b_worked_4w" = "D9",
    "b_worked_outside_home_4w" = "D10",
    "b_took_temp" = "B3",
    "b_flu_shot_12m" = "C2",
    "b_worked_outside_home_5d" = "c_work_outside_5d", # C3
    "b_worked_healthcare_5d" = "C4",
    "b_worked_nursing_home_5d" = "C5",
    "b_anxious" = "mh_anxious", # Binary version of C8_1
    "b_depressed" = "mh_depressed", # Binary version of C8_2
    "b_isolated" = "mh_isolated", # Binary version of C8_3
    "b_worried_family_ill" = "mh_worried_ill", # Binary version of C9
    "b_public_mask_often" = "c_mask_often", # Binary version of C14
    "b_tested_pos_14d" = "t_tested_positive_14d", # B10a; binary with an "I don't know" (3) option
    "b_tested_pos_ever" = "B11", # binary with an "I don't know" (3) option
    "b_have_cli" = "is_cli", # Based on symptoms in A1
    "b_have_ili" = "is_ili", # Based on symptoms in A1
    "b_cmnty_have_cli" = "community_yes",
    "b_hh_or_cmnty_have_cli" = "hh_community_yes",
    # Wave 5 additions
    "b_flu_shot_jun2020" = "C17", # binary with "I don't know" option
    "b_children_grade_prek_k" = "E1_1", # binary with "I don't know" option
    "b_children_grade_1_5" = "E1_2", # binary with "I don't know" option
    "b_children_grade_6_8" = "E1_3", # binary with "I don't know" option
    "b_children_grade_9_12" = "E1_4", # binary with "I don't know" option
    "b_children_fulltime_school" = "E2_1", # binary with "I don't know" option
    "b_children_parttime_school" = "E2_2", # binary with "I don't know" option
    # Wave 6 additions
    "b_had_cov_vaccine" = "V1",
    "b_accept_cov_vaccine" = "v_accept_covid_vaccine", # Binary version of V3; "definitely" and "probably" vaccinate map to TRUE
    "b_vaccine_likely_friends" = "v_vaccine_likely_friends", # Binary version of V4_1
    "b_vaccine_likely_local_health" = "v_vaccine_likely_local_health", # Binary version of V4_2
    "b_vaccine_likely_who" = "v_vaccine_likely_who", # Binary version of V4_3
    "b_vaccine_likely_govt_health" = "v_vaccine_likely_govt_health", # Binary version of V4_4
    "b_vaccine_likely_politicians" = "v_vaccine_likely_politicians", # Binary version of V4_5
    # Wave 7 additions
    "b_received_2_vaccine_doses" = "v_received_2_vaccine_doses", # Binary version of V2
    # Wave 10 additions
    "b_vaccine_likely_doctors" = "v_vaccine_likely_doctors", # Binary version of V4_2
    
    ## multiple choice (mc)
    ## Can only select one of n > 2 choices
    "mc_state" = "A3b",
    "mc_mask_often" = "C14",
    "mc_anxiety" = "C8_1",
    "mc_depression" = "C8_2",
    "mc_isolation" = "C8_3",
    "mc_worried_family_ill" = "C9",
    "mc_financial_worry" = "C15",
    "mc_gender" = "D1",
    "mc_age" = "D2",
    "mc_race" = "D7",
    "mc_education" = "D8",
    "mc_occupational_group" = "Q64",
    "mc_job_type_cmnty_social" = "Q65",
    "mc_job_type_education" = "Q66",
    "mc_job_type_arts_media" = "Q67",
    "mc_job_type_healthcare" = "Q68",
    "mc_job_type_healthcare_support" = "Q69",
    "mc_job_type_protective" = "Q70",
    "mc_job_type_food" = "Q71",
    "mc_job_type_maintenance" = "Q72",
    "mc_job_type_personal_care" = "Q73",
    "mc_job_type_sales" = "Q74",
    "mc_job_type_office_admin" = "Q75",
    "mc_job_type_construction" = "Q76",
    "mc_job_type_repair" = "Q77",
    "mc_job_type_production" = "Q78",
    "mc_job_type_transport" = "Q79",
    "mc_occupational_group_other" = "Q80",
    "mc_cough_mucus" = "B4",
    "mc_tested_current_illness" = "B5",
    "mc_hospital" = "B6",
    "mc_social_avoidance" = "C7",
    "mc_financial_threat" = "Q36",
    "mc_pregnant" = "D1b", # Somewhat of a binary response (yes, no, prefer not to answer, and not applicable)
    # Wave 5 additions
    "mc_cmnty_mask_prevalence" = "C16",
    # Wave 6 additions
    "mc_accept_cov_vaccine" = "V3",
    "mc_num_cov_vaccine_doses" = "V2",
    "mc_vaccine_likely_friends" = "V4_1",
    "mc_vaccine_likely_local_health" = "V4_2",
    "mc_vaccine_likely_who" = "V4_3",
    "mc_vaccine_likely_govt_health" = "V4_4",
    "mc_vaccine_likely_politicians" = "V4_5",
    # Wave 7 additions
    "mc_concerned_sideeffects" = "V9",

    ## multiselect (ms)
    ## Can select more than one choice; saved as comma-separated list of choice codes
    "ms_symptoms" = "B2",
    "ms_unusual_symptoms" = "B2c",
    "ms_medical_care" = "B7",
    "ms_reasons_tested_14d" = "B10b",
    "ms_reasons_not_tested_14d" = "B12a",
    "ms_trips_outside_home" = "C13",
    "ms_mask_outside_home" = "C13a",
    "ms_comorbidities" = "C1",
    # Wave 5 additions
    "ms_school_safety_measures" = "E3",

    ## other (created in previous data-cleaning steps)
    "n_num_symptoms" = "cnt_symptoms", # Based on symptoms in A1
    "n_hh_prop_cli" = "hh_p_cli", # Based on symptoms in A1, and hh sick and total counts
    "n_hh_prop_ili" = "hh_p_ili" # Based on symptoms in A1, and hh sick and total counts
  )

  map_new_old_names <- map_new_old_names[!(names(map_new_old_names) %in% names(df))]
  df <- rename(df, map_new_old_names[map_new_old_names %in% names(df)])

  return(df)
}

#' Remap binary columns, race, and others to make more interpretable.
#'
#' @param df Data frame of individual response data.
#'
#' @return data frame of individual response data with newly mapped columns
remap_responses <- function(df) {
  msg_plain(paste0("Mapping response codes to descriptive values..."))
  # Map responses with multiple races selected into a single category.
  if ("D7" %in% names(df)) {
    df[grepl(",", df$D7), "D7"] <- "multiracial"
  }

  # Map "I don't know" to NA in otherwise binary columns.
  df <- remap_response(df, "B11", c("1"=1, "2"=0, "3"=NA)) %>%
    remap_response("C17", c("1"=1, "4"=0, "2"=NA)) %>%

    remap_response("E1_1", c("1"=1, "2"=0, "5"=NA)) %>%
    remap_response("E1_2", c("1"=1, "2"=0, "5"=NA)) %>%
    remap_response("E1_3", c("1"=1, "2"=0, "5"=NA)) %>%
    remap_response("E1_4", c("1"=1, "2"=0, "5"=NA)) %>%

    remap_response("E2_1", c("2"=1, "3"=0, "4"=NA)) %>%
    remap_response("E2_2", c("2"=1, "3"=0, "4"=NA))

  ## Specifies human-readable values that response codes correspond to for each
  ## question. `default` is the value that all non-specified response codes map
  ## to. Please avoid including commas or other punctuation in replacement
  ## strings for ease of down-stream usage.
  map_old_new_responses <- list(
    D2=list(
      "map"=c(
        "1"="18-24", 
        "2"="25-44", 
        "3"="25-44", 
        "4"="45-64", 
        "5"="45-64", 
        "6"="65-74", 
        "7"="75+"),
      "default"=NULL,
      "type"="mc"
    ),
    D7=list(
      "map"=c(
        "1"="American Indian or Alaska Native", 
        "2"="Asian", 
        "3"="Black or African American", 
        "4"="Native Hawaiian or Pacific Islander", 
        "5"="White", 
        "6"="Other", 
        "multiracial"="Other"),
      "default"=NULL,
      "type"="mc"
    ),
    V3=list(
      "map"=c(
        "1"="def vaccinate",
        "2"="prob vaccinate",
        "3"="prob not vaccinate",
        "4"="def not vaccinate"),
      "default"=NULL,
      "type"="mc"
    ),
    D1=list(
      "map"=c(
        "1"="Male", 
        "2"="Female", 
        "3"="Other", 
        "4"="Other", 
        "5"=NA),
      "default"=NULL,
      "type"="mc"
    ),
    D8=list(
      "map"=c(
        "1"="Less than high school",
        "2"="High school graduate or equivalent",
        "3"="Some college",
        "4"="2 year degree",
        "5"="4 year degree",
        "6"="Master's degree",
        "7"="Professional degree",
        "8"="Doctorate"),
      "default"=NULL,
      "type"="mc"
    ),
    C1=list(
      "map"=c(
        "1"="Diabetes", # Waves 1-3; later separated into types 1 and 2
        "2"="Cancer",
        "3"="Heart disease",
        "4"="High blood pressure",
        "5"="Asthma",
        "6"="Chronic lung disease",
        "7"="Kidney disease",
        "8"="Autoimmune disorder",
        "9"="None listed",
        "10"="Type 2 diabetes",
        "11"="Compromised immune system",
        "12"="Type 1 diabetes"),
      "default"=NULL,
      "type"="ms"
    ),
    Q64=list(
      "map"=c(
        "1"="Community and social",
        "2"="Education",
        "3"="Arts and media",
        "4"="Healthcare practitioner",
        "5"="Healthcare support",
        "6"="Protective",
        "7"="Food",
        "8"="Building upkeep",
        "9"="Personal care",
        "10"="Sales",
        "11"="Administrative",
        "12"="Construction and extraction",
        "13"="Maintenance and repair",
        "14"="Production",
        "15"="Transportation and delivery",
        "16"="Other"),
      "default"=NULL,
      "type"="mc"
    )
  )


  for (col_var in names(map_old_new_responses)) {
    df <- remap_response(df, col_var,
                         map_old_new_responses[[col_var]][["map"]],
                         map_old_new_responses[[col_var]][["default"]],
                         map_old_new_responses[[col_var]][["type"]]
    )
  }

  msg_plain(paste0("Finished remapping response codes"))
  return(df)
}


#' Create new columns, based on existing ones, for use in aggregates.
#'
#' @param df Data frame of individual response data.
#'
#' @return data frame of individual response data with newly derived columns
#' 
#' @importFrom dplyr case_when coalesce
create_derivative_columns <- function(df) {
  df$mc_agecondensed <- case_when(
    df$mc_age == "18-24" ~ "18-24", 
    df$mc_age == "25-44" ~ "25-44", 
    df$mc_age == "45-64" ~ "45-64",
    df$mc_age == "65-74" ~ "65+", 
    df$mc_age == "75+" ~ "65+",
    TRUE ~ NA_character_
  )
  
  df$b_65plus <- (	
    df$mc_age == "65-74" | df$mc_age == "75+"	
  )
  
  # Make derivative columns.	
  if ("D11" %in% names(df)) {
    df$b_smoke <- case_when(
      df$D11 == 1 ~ 1,
      df$D11 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  }
  
  if ("mc_pregnant" %in% names(df)) {
    df$b_pregnant <- case_when(
      df$mc_pregnant == 1 ~ 1,
      df$mc_pregnant == 2 ~ 0,
      df$mc_pregnant == 4 ~ 0,
      df$mc_gender == "Male" ~ 0,
      TRUE ~ NA_real_
    )
  }

  df$b_any_comorbidity <- as.numeric(
    df$b_heart_disease == 1 | 
      df$b_cancer == 1 | df$b_chronic_kidney_disease == 1 | 
      df$b_chronic_lung_disease == 1 | df$b_diabetes == 1 | 
      df$b_immunocompromised == 1  
  )
  
  if ( "D11" %in% names(df) && "mc_pregnant" %in% names(df) ) {
    df$b_any_comorbidity2 <- as.numeric(
      df$b_heart_disease == 1 | 
        df$b_cancer == 1 | df$b_chronic_kidney_disease == 1 | 
        df$b_chronic_lung_disease == 1 | df$b_diabetes == 1 | 
        df$b_immunocompromised == 1  | df$b_pregnant == 1 |
        df$b_smoke == 1 | df$b_obese == 1
    )
  } else {
    df$b_any_comorbidity_preg_smoke_obese <- NA_real_
  }
  
  if ("mc_occupational_group" %in% names(df)) {	
    df$b_work_in_healthcare <- as.numeric(	
      df$mc_occupational_group == "Healthcare support" | df$mc_occupational_group == "Healthcare practitioner"	
    )	
  } else {	
    df$b_work_in_healthcare <- NA_real_	
  }	

  if ("mc_accept_cov_vaccine" %in% names(df)) {	
    df$b_hesitant_cov_vaccine <- as.numeric(	
      df$mc_accept_cov_vaccine == "prob not vaccinate" | df$mc_accept_cov_vaccine == "def not vaccinate"	
    )	
 
    df$b_accepting_yes_definitely <- df$mc_accept_cov_vaccine == "def vaccinate"
    df$b_accepting_yes_probably <- df$mc_accept_cov_vaccine == "prob vaccinate"
    df$b_accepting_no_probably <- df$mc_accept_cov_vaccine == "prob not vaccinate"
    df$b_accepting_no_definitely <- df$mc_accept_cov_vaccine == "def not vaccinate"
    
  } else {	
    df$b_hesitant_cov_vaccine <- NA_real_
    
    df$b_accepting_yes_definitely <- NA
    df$b_accepting_yes_probably <- NA
    df$b_accepting_no_probably <- NA
    df$b_accepting_no_definitely <- NA
  }	
  
  if ("V5a" %in% names(df) && "V5b" %in% names(df) && "V5c" %in% names(df)) {
    # introduced in Wave 8
    hesitancy_reasons_a <- split_options(df$V5a)
    hesitancy_reasons_b <- split_options(df$V5b)
    hesitancy_reasons_c <- split_options(df$V5c)
    
    df$v_hesitancy_reason_sideeffects_a <- is_selected(hesitancy_reasons_a, "1")
    df$v_hesitancy_reason_allergic_a <- is_selected(hesitancy_reasons_a, "2")
    df$v_hesitancy_reason_ineffective_a <- is_selected(hesitancy_reasons_a, "3")
    df$v_hesitancy_reason_unnecessary_a <- is_selected(hesitancy_reasons_a, "4")
    df$v_hesitancy_reason_dislike_vaccines_a <- is_selected(hesitancy_reasons_a, "5")
    df$v_hesitancy_reason_not_recommended_a <- is_selected(hesitancy_reasons_a, "6")
    df$v_hesitancy_reason_wait_safety_a <- is_selected(hesitancy_reasons_a, "7")
    df$v_hesitancy_reason_low_priority_a <- is_selected(hesitancy_reasons_a, "8")
    df$v_hesitancy_reason_cost_a <- is_selected(hesitancy_reasons_a, "9")
    df$v_hesitancy_reason_distrust_vaccines_a <- is_selected(hesitancy_reasons_a, "10")
    df$v_hesitancy_reason_distrust_gov_a <- is_selected(hesitancy_reasons_a, "11")
    df$v_hesitancy_reason_health_condition_a <- is_selected(hesitancy_reasons_a, "12")
    df$v_hesitancy_reason_other_a <- is_selected(hesitancy_reasons_a, "13")
    df$v_hesitancy_reason_pregnant_a <- is_selected(hesitancy_reasons_a, "14")
    df$v_hesitancy_reason_religious_a <- is_selected(hesitancy_reasons_a, "15")
    
    df$v_hesitancy_reason_sideeffects_b <- is_selected(hesitancy_reasons_b, "1")
    df$v_hesitancy_reason_allergic_b <- is_selected(hesitancy_reasons_b, "2")
    df$v_hesitancy_reason_ineffective_b <- is_selected(hesitancy_reasons_b, "3")
    df$v_hesitancy_reason_unnecessary_b <- is_selected(hesitancy_reasons_b, "4")
    df$v_hesitancy_reason_dislike_vaccines_b <- is_selected(hesitancy_reasons_b, "5")
    df$v_hesitancy_reason_not_recommended_b <- is_selected(hesitancy_reasons_b, "6")
    df$v_hesitancy_reason_wait_safety_b <- is_selected(hesitancy_reasons_b, "7")
    df$v_hesitancy_reason_low_priority_b <- is_selected(hesitancy_reasons_b, "8")
    df$v_hesitancy_reason_cost_b <- is_selected(hesitancy_reasons_b, "9")
    df$v_hesitancy_reason_distrust_vaccines_b <- is_selected(hesitancy_reasons_b, "10")
    df$v_hesitancy_reason_distrust_gov_b <- is_selected(hesitancy_reasons_b, "11")
    df$v_hesitancy_reason_health_condition_b <- is_selected(hesitancy_reasons_b, "12")
    df$v_hesitancy_reason_other_b <- is_selected(hesitancy_reasons_b, "13")
    df$v_hesitancy_reason_pregnant_b <- is_selected(hesitancy_reasons_b, "14")
    df$v_hesitancy_reason_religious_b <- is_selected(hesitancy_reasons_b, "15")
    
    df$v_hesitancy_reason_sideeffects_c <- is_selected(hesitancy_reasons_c, "1")
    df$v_hesitancy_reason_allergic_c <- is_selected(hesitancy_reasons_c, "2")
    df$v_hesitancy_reason_ineffective_c <- is_selected(hesitancy_reasons_c, "3")
    df$v_hesitancy_reason_unnecessary_c <- is_selected(hesitancy_reasons_c, "4")
    df$v_hesitancy_reason_dislike_vaccines_c <- is_selected(hesitancy_reasons_c, "5")
    df$v_hesitancy_reason_not_recommended_c <- is_selected(hesitancy_reasons_c, "6")
    df$v_hesitancy_reason_wait_safety_c <- is_selected(hesitancy_reasons_c, "7")
    df$v_hesitancy_reason_low_priority_c <- is_selected(hesitancy_reasons_c, "8")
    df$v_hesitancy_reason_cost_c <- is_selected(hesitancy_reasons_c, "9")
    df$v_hesitancy_reason_distrust_vaccines_c <- is_selected(hesitancy_reasons_c, "10")
    df$v_hesitancy_reason_distrust_gov_c <- is_selected(hesitancy_reasons_c, "11")
    df$v_hesitancy_reason_health_condition_c <- is_selected(hesitancy_reasons_c, "12")
    df$v_hesitancy_reason_other_c <- is_selected(hesitancy_reasons_c, "13")
    df$v_hesitancy_reason_pregnant_c <- is_selected(hesitancy_reasons_c, "14")
    df$v_hesitancy_reason_religious_c <- is_selected(hesitancy_reasons_c, "15")
    
  } else {
    df$v_hesitancy_reason_sideeffects_a <- NA_real_
    df$v_hesitancy_reason_allergic_a <- NA_real_
    df$v_hesitancy_reason_ineffective_a <- NA_real_
    df$v_hesitancy_reason_unnecessary_a <- NA_real_
    df$v_hesitancy_reason_dislike_vaccines_a <- NA_real_
    df$v_hesitancy_reason_not_recommended_a <- NA_real_
    df$v_hesitancy_reason_wait_safety_a <- NA_real_
    df$v_hesitancy_reason_low_priority_a <- NA_real_
    df$v_hesitancy_reason_cost_a <- NA_real_
    df$v_hesitancy_reason_distrust_vaccines_a <- NA_real_
    df$v_hesitancy_reason_distrust_gov_a <- NA_real_
    df$v_hesitancy_reason_health_condition_a <- NA_real_
    df$v_hesitancy_reason_other_a <- NA_real_
    df$v_hesitancy_reason_pregnant_a <- NA_real_
    df$v_hesitancy_reason_religious_a <- NA_real_
    
    df$v_hesitancy_reason_sideeffects_b <- NA_real_
    df$v_hesitancy_reason_allergic_b <- NA_real_
    df$v_hesitancy_reason_ineffective_b <- NA_real_
    df$v_hesitancy_reason_unnecessary_b <- NA_real_
    df$v_hesitancy_reason_dislike_vaccines_b <- NA_real_
    df$v_hesitancy_reason_not_recommended_b <- NA_real_
    df$v_hesitancy_reason_wait_safety_b <- NA_real_
    df$v_hesitancy_reason_low_priority_b <- NA_real_
    df$v_hesitancy_reason_cost_b <- NA_real_
    df$v_hesitancy_reason_distrust_vaccines_b <- NA_real_
    df$v_hesitancy_reason_distrust_gov_b <- NA_real_
    df$v_hesitancy_reason_health_condition_b <- NA_real_
    df$v_hesitancy_reason_other_b <- NA_real_
    df$v_hesitancy_reason_pregnant_b <- NA_real_
    df$v_hesitancy_reason_religious_b <- NA_real_
    
    df$v_hesitancy_reason_sideeffects_c <- NA_real_
    df$v_hesitancy_reason_allergic_c <- NA_real_
    df$v_hesitancy_reason_ineffective_c <- NA_real_
    df$v_hesitancy_reason_unnecessary_c <- NA_real_
    df$v_hesitancy_reason_dislike_vaccines_c <- NA_real_
    df$v_hesitancy_reason_not_recommended_c <- NA_real_
    df$v_hesitancy_reason_wait_safety_c <- NA_real_
    df$v_hesitancy_reason_low_priority_c <- NA_real_
    df$v_hesitancy_reason_cost_c <- NA_real_
    df$v_hesitancy_reason_distrust_vaccines_c <- NA_real_
    df$v_hesitancy_reason_distrust_gov_c <- NA_real_
    df$v_hesitancy_reason_health_condition_c <- NA_real_
    df$v_hesitancy_reason_other_c <- NA_real_
    df$v_hesitancy_reason_pregnant_c <- NA_real_
    df$v_hesitancy_reason_religious_c <- NA_real_
    
  }
  
  df$v_hesitancy_reason_sideeffects_5bc <- coalesce(df$v_hesitancy_reason_sideeffects_b, df$v_hesitancy_reason_sideeffects_c)
  df$v_hesitancy_reason_allergic_5bc <- coalesce(df$v_hesitancy_reason_allergic_b, df$v_hesitancy_reason_allergic_c)
  df$v_hesitancy_reason_ineffective_5bc <- coalesce(df$v_hesitancy_reason_ineffective_b, df$v_hesitancy_reason_ineffective_c)
  df$v_hesitancy_reason_unnecessary_5bc <- coalesce(df$v_hesitancy_reason_unnecessary_b, df$v_hesitancy_reason_unnecessary_c)
  df$v_hesitancy_reason_dislike_vaccines_5bc <- coalesce(df$v_hesitancy_reason_dislike_vaccines_b, df$v_hesitancy_reason_dislike_vaccines_c)
  df$v_hesitancy_reason_not_recommended_5bc <- coalesce(df$v_hesitancy_reason_not_recommended_b, df$v_hesitancy_reason_not_recommended_c)
  df$v_hesitancy_reason_wait_safety_5bc <- coalesce(df$v_hesitancy_reason_wait_safety_b, df$v_hesitancy_reason_wait_safety_c)
  df$v_hesitancy_reason_low_priority_5bc <- coalesce(df$v_hesitancy_reason_low_priority_b, df$v_hesitancy_reason_low_priority_c)
  df$v_hesitancy_reason_cost_5bc <- coalesce(df$v_hesitancy_reason_cost_b, df$v_hesitancy_reason_cost_c)
  df$v_hesitancy_reason_distrust_vaccines_5bc <- coalesce(df$v_hesitancy_reason_distrust_vaccines_b, df$v_hesitancy_reason_distrust_vaccines_c)
  df$v_hesitancy_reason_distrust_gov_5bc <- coalesce(df$v_hesitancy_reason_distrust_gov_b, df$v_hesitancy_reason_distrust_gov_c)
  df$v_hesitancy_reason_health_condition_5bc <- coalesce(df$v_hesitancy_reason_health_condition_b, df$v_hesitancy_reason_health_condition_c)
  df$v_hesitancy_reason_other_5bc <- coalesce(df$v_hesitancy_reason_other_b, df$v_hesitancy_reason_other_c)
  df$v_hesitancy_reason_pregnant_5bc <- coalesce(df$v_hesitancy_reason_pregnant_b, df$v_hesitancy_reason_pregnant_c)
  df$v_hesitancy_reason_religious_5bc <- coalesce(df$v_hesitancy_reason_religious_b, df$v_hesitancy_reason_religious_c)
  
  
  if ("V6" %in% names(df)) {
    dontneed_reasons <- split_options(df$V6)
    
    df$v_dontneed_reason_alreadyhad <- is_selected(dontneed_reasons, "1")
    df$v_dontneed_reason_dontspendtime <- is_selected(dontneed_reasons, "2")
    df$v_dontneed_reason_nothighrisk <- is_selected(dontneed_reasons, "3")
    df$v_dontneed_reason_precautions <- is_selected(dontneed_reasons, "4")
    df$v_dontneed_reason_notserious <- is_selected(dontneed_reasons, "5")
    df$v_dontneed_reason_notbeneficial <- is_selected(dontneed_reasons, "7")
    df$v_dontneed_reason_other <- is_selected(dontneed_reasons, "8")
    
  } else {
    df$v_dontneed_reason_alreadyhad <- NA
    df$v_dontneed_reason_dontspendtime <- NA
    df$v_dontneed_reason_nothighrisk <- NA
    df$v_dontneed_reason_precautions <- NA
    df$v_dontneed_reason_notserious <- NA
    df$v_dontneed_reason_notbeneficial <- NA
    df$v_dontneed_reason_other <- NA
  }
  
  df$v_dontneed_reason_alreadyhad[is.na(df$v_hesitancy_reason_unnecessary_5bc)] <- NA
  df$v_dontneed_reason_dontspendtime[is.na(df$v_hesitancy_reason_unnecessary_5bc)] <- NA
  df$v_dontneed_reason_nothighrisk[is.na(df$v_hesitancy_reason_unnecessary_5bc)] <- NA
  df$v_dontneed_reason_precautions[is.na(df$v_hesitancy_reason_unnecessary_5bc)] <- NA
  df$v_dontneed_reason_notserious[is.na(df$v_hesitancy_reason_unnecessary_5bc)] <- NA
  df$v_dontneed_reason_notbeneficial[is.na(df$v_hesitancy_reason_unnecessary_5bc)] <- NA
  df$v_dontneed_reason_other[is.na(df$v_hesitancy_reason_unnecessary_5bc)] <- NA
  
  
  df$mc_vaccinated_acceptance <- case_when(
    df$v_covid_vaccinated == 1 ~ "vaccinated",
    df$v_covid_vaccinated == 0 & df$b_accept_cov_vaccine == 1 ~ "not vaccinated and accepting",
    df$v_covid_vaccinated == 0 & df$b_accept_cov_vaccine == 0 ~ "not vaccinated and hesitant",
    df$v_covid_vaccinated == 0 & is.na(df$b_accept_cov_vaccine) == TRUE ~ "not vaccinated and did not indicate hesitance level"
  )
  
  df$b_vaccinated <- df$mc_vaccinated_acceptance == "vaccinated"
  df$b_not_vaccinated_and_accepting <- df$mc_vaccinated_acceptance == "not vaccinated and accepting"
  df$b_not_vaccinated_and_hesitant <- df$mc_vaccinated_acceptance == "not vaccinated and hesitant"
  df$b_not_vaccinated_and_did_not_indicate_hesitance_level <- df$mc_vaccinated_acceptance == "not vaccinated and did not indicate hesitance level"
  
  
  if ("ms_symptoms" %in% names(df)) {
    df$individ_fever <- (df$ms_symptoms == 1L)
    df$individ_sore_throat <- (df$ms_symptoms == 9L)
    df$individ_cough <- (df$ms_symptoms == 2L)
    df$individ_short_breath <- (df$ms_symptoms == 3L)
    df$individ_diff_breath <- (df$ms_symptoms == 4L)
    df$individ_anosmia <- (df$ms_symptoms == 13L)
    
    df$individ_cli <- df$individ_fever & (
      df$individ_cough | df$individ_short_breath | df$individ_diff_breath
    )
    df$individ_ili <- df$individ_fever & (df$individ_sore_throat | df$individ_cough)
    
  } else {
    df$individ_cli <- NA
    df$individ_ili <- NA
  }
  
  
  if ("mc_concerned_sideeffects" %in% names(df)) {	
    df$b_concerned_sideeffects <- as.numeric(	
      df$mc_concerned_sideeffects == 1 | df$mc_concerned_sideeffects == 2	
    )	
  } else {	
    df$b_concerned_sideeffects <- NA_real_	
  }

  df$b_hesitant_sideeffects <- case_when(
    df$b_hesitant_cov_vaccine == 1 & df$b_concerned_sideeffects == 1 ~ 1,
    df$b_hesitant_cov_vaccine == 1 & df$b_concerned_sideeffects == 0 ~ 0,
    TRUE ~ NA_real_
  )
  
  if ( "b_vaccine_likely_friends" %in% names(df) &
       "b_vaccine_likely_local_health" %in% names(df) &
       "b_vaccine_likely_who" %in% names(df) &
       "b_vaccine_likely_govt_health" %in% names(df) &
       "b_vaccine_likely_politicians" %in% names(df) &
       "b_vaccine_likely_doctors" %in% names(df) ) {
    df$b_hesitant_trust_fam <- case_when(
      df$b_hesitant_cov_vaccine == 1 & df$b_vaccine_likely_friends == 1 ~ 1,
      df$b_hesitant_cov_vaccine == 1 & df$b_vaccine_likely_friends == 0 ~ 0,
      TRUE ~ NA_real_
    )
    df$b_hesitant_trust_healthcare <- case_when(
      df$b_hesitant_cov_vaccine == 1 & df$b_vaccine_likely_local_health == 1 ~ 1,
      df$b_hesitant_cov_vaccine == 1 & df$b_vaccine_likely_local_health == 0 ~ 0,
      TRUE ~ NA_real_
    )
    df$b_hesitant_trust_who <- case_when(
      df$b_hesitant_cov_vaccine == 1 & df$b_vaccine_likely_who == 1 ~ 1,
      df$b_hesitant_cov_vaccine == 1 & df$b_vaccine_likely_who == 0 ~ 0,
      TRUE ~ NA_real_
    )
    df$b_hesitant_trust_govt <- case_when(
      df$b_hesitant_cov_vaccine == 1 & df$b_vaccine_likely_govt_health == 1 ~ 1,
      df$b_hesitant_cov_vaccine == 1 & df$b_vaccine_likely_govt_health == 0 ~ 0,
      TRUE ~ NA_real_
    )
    df$b_hesitant_trust_politicians <- case_when(
      df$b_hesitant_cov_vaccine == 1 & df$b_vaccine_likely_politicians == 1 ~ 1,
      df$b_hesitant_cov_vaccine == 1 & df$b_vaccine_likely_politicians == 0 ~ 0,
      TRUE ~ NA_real_
    )
    df$b_hesitant_trust_doctors <- case_when(
      df$b_hesitant_cov_vaccine == 1 & df$b_vaccine_likely_doctors == 1 ~ 1,
      df$b_hesitant_cov_vaccine == 1 & df$b_vaccine_likely_doctors == 0 ~ 0,
      TRUE ~ NA_real_
    )
    
  } else {
    df$b_hesitant_trust_fam <- NA_real_
    df$b_hesitant_trust_healthcare <- NA_real_
    df$b_hesitant_trust_who <- NA_real_
    df$b_hesitant_trust_govt <- NA_real_
    df$b_hesitant_trust_politicians <- NA_real_
  }

  df$mc_race_ethnicity <- case_when(
    df$b_hispanic == 1 ~ "Hispanic",
    df$b_hispanic == 2 & df$mc_race == "American Indian or Alaska Native" ~ "Non-Hispanic American Indian or Alaska Native",
    df$b_hispanic == 2 & df$mc_race == "Asian" ~ "Non-Hispanic Asian",
    df$b_hispanic == 2 & df$mc_race == "Black or African American" ~ "Non-Hispanic Black or African American",
    df$b_hispanic == 2 & df$mc_race == "Native Hawaiian or Pacific Islander" ~ "Non-Hispanic Native Hawaiian or Pacific Islander",
    df$b_hispanic == 2 & df$mc_race == "White" ~ "Non-Hispanic White",
    df$b_hispanic == 2 & df$mc_race == "Other" ~ "Non-Hispanic other or multiracial",
    TRUE ~ NA_character_
  )
  
  return(df)
}


#' Convert numeric response codes in a single survey item to values specified in
#' map. Returns as-is for numeric columns.
#'
#' Maps for recoding are set manually in `remap_responses`.
#'
#' @param df Data frame of individual response data.
#' @param col_var Name of response var to recode
#' @param map_old_new Named vector of new values we want to use; names are the
#'     original response codes
#' @param default Default to use if value is not explicitly remapped in
#'     `map_old_new`; often `NA`, `NA_character_`, etc. See `recode`
#'     [documentation](https://rdrr.io/cran/dplyr/man/recode.html) for more info
#' @param response_type Str indicating if response is binary, multiple choice, or
#'     multi-select.
#'
#' @importFrom dplyr recode
#' @importFrom parallel mcmapply
#' @importFrom readr write_csv
#'
#' @return list of data frame of individual response data with newly mapped column
remap_response <- function(df, col_var, map_old_new, default=NULL, response_type="b") {
  msg_plain(paste0("Mapping codes for ", col_var))
  if (  is.null(df[[col_var]]) | (response_type == "b" & FALSE %in% df[[col_var]]) | inherits(df[[col_var]], "logical") ) {
    # Column is missing/not in this wave or already in boolean format
    return(df)
  }

  if (response_type %in% c("b", "mc")) {
    df[[col_var]] <- recode(df[[col_var]], !!!map_old_new, .default=default)
  } else if (response_type == "ms") {
    msg_plain(paste0("Mapping response codes for ", col_var, " to meaningful strings..."))
    
    split_col <- split_options(df[[col_var]])
    if (col_var == "C1") {
      msg_plain("Creating binary columns for subset of comorbidities")
      df$b_heart_disease <- as.numeric(is_selected(split_col, "3"))
      df$b_cancer <- as.numeric(is_selected(split_col, "2"))
      df$b_chronic_kidney_disease <- as.numeric(is_selected(split_col, "7"))
      df$b_chronic_lung_disease <- as.numeric(is_selected(split_col, "6"))
      df$b_diabetes <- as.numeric(is_selected(split_col, "12") | is_selected(split_col, "10"))
      df$b_immunocompromised <- as.numeric(is_selected(split_col, "11"))
      df$b_obese <- as.numeric(is_selected(split_col, "13"))
    }
    
    map_fn <- ifelse( is.null(getOption("mc.cores")) , mapply, mcmapply)
    df[[col_var]] <- map_fn(split_col, FUN=function(row) {
      if ( length(row) == 1 && all(is.na(row)) ) {
        NA
      } else {
        paste(recode(row, !!!map_old_new, .default=default), collapse=",")
      }
    })
  }
  
  return(df)
}


#' Wrapper for `remap_response` that returns `aggregations` also
#'
#' Assumes binary response variable and is coded with 1 = TRUE (agree), 2 = FALSE,
#' 3 = "I don't know"
#'
#' @param df Data frame of individual response data.
#' @param aggregations Data frame with columns `name`, `var_weight`, `metric`,
#'   `group_by`, `compute_fn`, `post_fn`. Each row represents one aggregate
#'   to report. `name` is the aggregate's base column name; `var_weight` is the
#'   column to use for its weights; `metric` is the column of `df` containing the
#'   response value. `group_by` is a list of variables used to perform the
#'   aggregations over. `compute_fn` is the function that computes
#'   the aggregate response given many rows of data. `post_fn` is applied to the
#'   aggregate data after megacounty aggregation, and can perform any final
#'   calculations necessary.
#' @param col_var Name of response var
#'
#' @return list of data frame of individual response data and user-set data
#' frame of desired aggregations
#'
#' @export
code_binary <- function(df, aggregations, col_var) {
  df <- remap_response(df, col_var, c("1"=1, "2"=0, "3"=NA))
  return(list(df, aggregations))
}

#' Convert a single multi-select response column to a set of boolean columns.
#'
#' Update aggregations table to use new set of columns where `col_var` had
#' previously been used as the metric to aggregate. Does not change columns
#' referenced in `groupby`
#'
#' @param df Data frame of individual response data.
#' @param aggregations Data frame with columns `name`, `var_weight`, `metric`,
#'   `group_by`, `compute_fn`, `post_fn`. Each row represents one aggregate
#'   to report. `name` is the aggregate's base column name; `var_weight` is the
#'   column to use for its weights; `metric` is the column of `df` containing the
#'   response value. `group_by` is a list of variables used to perform the
#'   aggregations over. `compute_fn` is the function that computes
#'   the aggregate response given many rows of data. `post_fn` is applied to the
#'   aggregate data after megacounty aggregation, and can perform any final
#'   calculations necessary.
#' @param col_var Name of response var
#'
#' @return list of data frame of individual response data and user-set data
#' frame of desired aggregations
#'
#' @importFrom stats na.omit
#' @importFrom tibble add_row
#' @importFrom stringi stri_replace_all
#'
#' @export
code_multiselect <- function(df, aggregations, col_var) {
  # Get unique response codes. Sort alphabetically.
  response_codes <- sort( na.omit(
    unique(do.call(c, strsplit(unique(df[[col_var]]), ",")))))

  # Turn each response code into a new binary col
  new_binary_cols <- as.character(lapply(
    response_codes,
    function(code) {
      paste(col_var,
            stri_replace_all(code, "_", fixed=" "),
            sep="_")
      }
    ))
  
  #### TODO: eval(parse()) here is not the best approach, but I can't find another
  # way to get col_var (a string) to be used as a var that references a column
  # rather than as an actual string. This approach causes a shallow copy to be
  # made (warning is raised).
  df[!is.na(df[[col_var]]), c(new_binary_cols) :=
       lapply(response_codes, function(code) {
         as.numeric( grepl(sprintf("^%s$", code), eval(parse(text=col_var))) |
                       grepl(sprintf("^%s,", code), eval(parse(text=col_var))) |
                       grepl(sprintf(",%s$", code), eval(parse(text=col_var))) |
                       grepl(sprintf(",%s,", code), eval(parse(text=col_var))) )
       })]

  # Update aggregations table
  old_rows <- aggregations[aggregations$metric == col_var, ]
  for (row_ind in seq_along(old_rows$id)) {
    old_row <- old_rows[row_ind, ]

    for (col_ind in seq_along(new_binary_cols)) {
      new_row <- old_row
      response_code <- response_codes[col_ind]

      new_row$name <- paste(old_row$name,
                            stri_replace_all(response_code, "_", fixed=" "),
                            sep="_")
      new_row$id <- paste(old_row$id, response_code, sep="_")
      new_row$metric <- new_binary_cols[col_ind]
      aggregations <- add_row(aggregations, new_row)
    }
  }

  return(list(df, aggregations[aggregations$metric != col_var, ]))
}

#' Convert a single free response column to numeric.
#'
#' @param df Data frame of individual response data.
#' @param aggregations Data frame with columns `name`, `var_weight`, `metric`,
#'   `group_by`, `compute_fn`, `post_fn`. Each row represents one aggregate
#'   to report. `name` is the aggregate's base column name; `var_weight` is the
#'   column to use for its weights; `metric` is the column of `df` containing the
#'   response value. `group_by` is a list of variables used to perform the
#'   aggregations over. `compute_fn` is the function that computes
#'   the aggregate response given many rows of data. `post_fn` is applied to the
#'   aggregate data after megacounty aggregation, and can perform any final
#'   calculations necessary.
#' @param col_var Name of response var
#'
#' @return list of data frame of individual response data and user-set data
#' frame of desired aggregations
#'
#' @export
code_numeric_freeresponse <- function(df, aggregations, col_var) {
  df[[col_var]] <- as.numeric(df[[col_var]])
  return(list(df, aggregations))
}
