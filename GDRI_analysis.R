options(scipen = 999)             # Modify global options in R

# Install Packages, Load Libraries ----
packages_required <- c("tidyverse", 
                       "readxl", 
                       "Microsoft365R",
                       "glue", 
                       "ggtext", 
                       "devtools",
                       "treemap",
                       "treemapify",
                       "knitr",
                       "zoo",
                       "lubridate",
                       "directlabels",
                       "wordcloud",
                       "RColorBrewer",
                       "wordcloud2",
                       "tm",
                       "SnowballC",
                       "countrycode",
                       "rnaturalearth",
                       "rnaturalearthdata",
                       "sf",
                       "writexl",
                       "openxlsx",
                       "eeptools",
                       "stringr") ## List of required packages


new_packages <- packages_required[!(packages_required %in% installed.packages()[,"Package"])] ## List of packages to be installed

if(length(new_packages)) install.packages(new_packages) ## Install packages if necessary

lapply(packages_required, library, character.only = TRUE) ## Load libraries if necessary

## Set Directory, Clear Console and Environment ----
rm(list = ls()) # Clear environment

cat("\014") # Clear console

setwd("C:/Users/genaf/OneDrive/Documents/Viz for Social Good/Global Deaf Research Institute")

##load raw data ----

data <- read_excel("Deaf Community Research Nigeria Data for Viz for Social Good V.2.xlsx")


## clean raw data ----

# rename columns

data_clean <- data %>%
  rename(response_ID = "Response ID",
         languages = "Languages you use comfortably",
         DOB = "What is your date of birth?",
         best_communication_contact_method = "Best contact method for communication",
         best_emergency_contact_method = "Best contact method for emergencies",
         tribal_affiliation = "Which tribal affiliation do you identify with?",
         religion = "Which religion do you follow?",
         gender_identity = "Sex",
         is_LGBTQ = "Do you identify as queer, gay, bisexual, lesbian, transgender, or asexual?",
         relationship_status = "Describe your relationship status",
         is_partner_deaf = "Is your partner deaf?",
         employment_status = "Current occupation status",
         job_description = "What kind job do you have?",
         monthly_income = "How much money do you earn a month? (Salary) [It is in Niara currency]",
         number_of_jobs_held = "How many jobs do you have?",
         number_of_workdays_per_week = "How many days a week do you work?",
         education_level = "Education",
         yearly_housing_expense = "How much do you pay yearly for rent or mortage?",
         is_homeowner = "Do you own a house?",
         access_to_clean_water = "Do you have access to clean water?",
         access_to_hand_washing = "Do you have access to wash your hands?",
         food_security_status = "Do you feel like you have enough to eat?",
         nutrition_status = "Do you feel that what you eat supports your health needs?",
         fresh_produce_frequency = "- How often do you eat fresh fruit or vegetables?",
         is_healthy_food_expensive = "Is healthy food expensive?",
         transportation_barrier_to_healthy_food = "Why are you not eating healthy foods? - I don't have the transportation to go shopping for healthy food",
         time_barrier_to_healthy_food = "Why are you not eating healthy foods? - I don't have the time to shop for, and prepare healthy food",
         knowledge_barrier_to_healthy_food = "Why are you not eating healthy foods? - I don't know how to cook and prepare healthy meals",
         supplies_barrier_to_healthy_food = "Why are you not eating healthy foods? - I don't have the supplies and equipment I'd need to cook healthy food",
         household_barrier_to_healthy_food = "Why are you not eating healthy foods? - The others in my household do not eat healthy",
         motivation_barrier_to_healthy_food = "Why are you not eating healthy foods? - I am not ready to change my eating habits",
         other_barrier_to_healthy_food = "Why are you not eating healthy foods? - Other",
         access_to_computer = "Do you have access to computer/laptop?",
         access_to_internet = "Do you have access to internet?",
         quality_of_life = "How would you rate your quality of life?",
         capacity_for_work = "How satisfied are you with your capacity for work?",
         ability_to_perform_daily_activities = "How satisfied are you with your ability to perform your daily living activities?",
         energy_level = "Do you have enough energy for everyday life?",
         sleep_satisfaction_level = "How satisfied are you with your sleep?",
         need_for_medical_treatment_in_daily_life = "- How much do you need any medical treatment to function in your daily life?",
         life_enjoyment_level = "How much do you enjoy life?",
         negative_feelings_frequency = "How often do you have negative feelings, such as blue mood, despair, anxiety, or depression?",
         is_life_meaningful = "To what extent do you feel your life to be meaningful?",
         bodily_appearance_acceptance_level = "Are you able to accept your bodily appearance?",
         self_satisfaction_level = "How satisfied are you with yourself?",
         access_to_health_services_satisfaction_level = "How satisfied are you with your access to health services?",
         info_availability_level = "How available to you is the information that you need in your day-to-day life?",
         safety_satisfaction_level = "How safe do you feel in your daily life?",
         financial_security_level = "Have you enough money to meet your needs?",
         physical_environment_health_level = "How healthy is your physical environment?",
         living_condition_satisfaction_level = "How satisfied are you with the conditions of your living place?",
         transportation_satisfaction_level = "How satisfied are you with transportation?",
         personal_relationship_satisfaction_level = "How satisfied are you with your personal relationships",
         sex_life_satisfaction_level = "How satisfied are you with your sex life?",
         friend_support_satisfaction_level = "How satisfied are you with the support you get from your friends?",
         violence_communication_contact = "In the event of violence, such as a gun shooting or abuse, whom do you communicate with?",
         lost_a_parent = "Did you lose a parent through divorce, abandonment, death, or other reason?",
         parental_violence = "Did your parents or adults in your home ever hit, punch, beat, or threaten to harm each other?",
         family_members_in_jail = "Did you live with anyone who went to jail or prison?",
         emotional_abuse_by_adults_in_home = "Did a parent or adult in your home ever swear at you, insult you, or put you down?",
         witnessed_stabbing_or_shooting = "- Did you see or hear someone being stabbed or shot?",
         has_home_been_deliberately_destroyed = "- Did you experience the deliberate destruction of your home?",
         beaten_by_police_or_other_outside_home = "- Were you beaten up by soliders, police, militia, bandits, boko haram, or thugs?",
         age_of_hearing_loss_onset = "At what age did you begin experiencing hearing loss?",
         hearing_loss_support_needs = "What did you need to support your hearing loss experience?",
         age_when_learned_sign_language = "How old were you when you learned sign language?",
         want_to_learn_sign_language = "Do you want to learn sign language?",
         sign_language_fluency = "How fluent are you in sign language?",
         where_learned_sign_language = "Where did you learn sign language?",
         use_hearing_aids = "What tools or devices do you use for your hearing? - Hearing Aids",
         use_cochlear_implants = "What tools or devices do you use for your hearing? - Cochlear Implants",
         use_other_hearing_support_tool = "What tools or devices do you use for your hearing? - Other",
         age_first_got_hearing_aid = "What age did you get your hearing aid?",
         still_wear_hearing_aid = "Do you still wear hearing aids?",
         access_to_hearing_aid_batteries_or_repair = "Do you have access to battery replacements or repairs for your assisted hearing devices?",
         age_of_cochlear_implant_surgery = "What age did you have your cochlear implant surgery?",
         cochlear_implant_frequency_of_use = "- How often do you use cochlear implant?",
         access_to_speech_or_auditory_therapy = "Do you have access to speech and/or auditory therapy?",
         deaf_community_support_services_in_area = "List all the locations in your area that offer services, gatherings, and events related to the deaf community (including centers, agencies, churches, festivals, schools, gatherings, etc).",
         three_most_significant_challenges_to_deaf = "What are the three most significant challenges you believe deaf people face in your area?",
         is_transportation_affordable = "Is transportation affordable for you?",
         are_hearing_orgs_accessible = "Do you find hearing organizations accessible for you to join? (e.g., do they provide interpreters?)",
         communication_access_cost_responsibility = "Who is responsible for covering the cost of communication access, such as interpreters?",
         can_get_communication_access_at_work = "If you require communication access at work, can you get it?",
         can_get_communication_access_at_school = "If you require communication access at school, can you get it?",
         can_get_social_communication_access = "If you require communication access at social events (e.g., festivals, weddings, religious gatherings), can you get it?",
         can_get_healthcare_communication_access = "If you require communication access in healthcare, can you get it?",
         can_communicate_with_doctor = "Can you communicate with your doctor?",
         can_afford_needed_healthcare = "Can you afford the healthcare you need?",
         has_typhoid = "Do you have any other health disorders or issues? - Typhoid",
         has_malaria = "Do you have any other health disorders or issues? - Malaria",
         has_HIV_or_AIDS = "Do you have any other health disorders or issues? - HIV/AIDS",
         has_cancer = "Do you have any other health disorders or issues? - Cancer",
         has_diabetes = "Do you have any other health disorders or issues? - Diabetes",
         has_high_blood_pressure = "Do you have any other health disorders or issues? - High Blood Pressure",
         has_liver_disease = "Do you have any other health disorders or issues? - Liver disease",
         has_ulcers = "Do you have any other health disorders or issues? - Ulcers",
         has_other_health_issues = "Do you have any other health disorders or issues? - Other",
         malaria_protection_method = "How do you protect yourself from Malaria?",
         is_satisfied_with_health = "How satisfied are you with your health?",
         can_make_independent_health_decisions = "Can you independently decide what you need for your health?",
         enough_interpreters = "Are there enough interpreters?",
         interpreters_qualified_and_clear = "Are interpreters qualified and clear?",
         trusts_interpreters = "Do you trust interpreters to work with you?",
         filing_lack_of_svc_complaints_by_form_is_helpful = "Do you feel that filling out a form to make a complaint about lack of accommodation service is helpful?",
         confidence_in_info_access_in_disaster_disease_crisis = "In a crisis like natural disasters or disease, are you confident about accessing necessary information and communication?",
         where_to_locate_info_access_in_disaster_disease_crisis = "In a crisis like natural disasters or disease, where do you get necessary information and communication?",
         discrimination_frequency = "Throughout your lifetime, how often have you felt discriminated against by others because you are deaf?",
         communication_with_family_method = "How do you communicate with your family members?")

# clean up data formats (date, factor, numeric)

data_clean <- data_clean %>%
  mutate(DOB = as.Date(DOB, format = "%m/%d/%Y")) %>%
  mutate_at(vars("religion",
              "gender_identity",
              "relationship_status",
              "employment_status",
              "education_level",
              "access_to_clean_water",
              "access_to_hand_washing",
              "food_security_status",
              "nutrition_status",
              "fresh_produce_frequency",
              "access_to_computer",
              "access_to_internet",
              "quality_of_life",
              "capacity_for_work",
              "ability_to_perform_daily_activities",
              "energy_level",
              "sleep_satisfaction_level",
              "need_for_medical_treatment_in_daily_life",
              "life_enjoyment_level",
              "negative_feelings_frequency",
              "is_life_meaningful",
              "bodily_appearance_acceptance_level",
              "self_satisfaction_level",
              "access_to_health_services_satisfaction_level",
              "info_availability_level",
              "safety_satisfaction_level",
              "financial_security_level",
              "physical_environment_health_level",
              "living_condition_satisfaction_level",
              "transportation_satisfaction_level",
              "personal_relationship_satisfaction_level",
              "sex_life_satisfaction_level",
              "friend_support_satisfaction_level",
              "witnessed_stabbing_or_shooting",
              "has_home_been_deliberately_destroyed",
              "beaten_by_police_or_other_outside_home",
              "want_to_learn_sign_language",
              "sign_language_fluency",
              "access_to_hearing_aid_batteries_or_repair",
              "cochlear_implant_frequency_of_use",
              "access_to_speech_or_auditory_therapy",
              "can_get_communication_access_at_work",
              "can_get_communication_access_at_school",
              "can_get_social_communication_access",
              "can_get_healthcare_communication_access",
              "can_communicate_with_doctor",
              "is_satisfied_with_health",
              "interpreters_qualified_and_clear",
              "trusts_interpreters",
              "confidence_in_info_access_in_disaster_disease_crisis",
              "discrimination_frequency"), as.factor) %>%
  
  mutate_at(c("number_of_jobs_held",
              "number_of_workdays_per_week",
              "age_of_hearing_loss_onset",
              "age_when_learned_sign_language",
              "age_first_got_hearing_aid",
              "age_of_cochlear_implant_surgery"), as.integer) %>%
  
  mutate_at(c("monthly_income",
              "yearly_housing_expense"), as.numeric)

# convert yes/no or response/no response columns to Boolean format

convert_values_to_boolean <- function(df, column_name) {
  df[[column_name]] <- ifelse(is.na(df[[column_name]]), FALSE,
                              ifelse(df[[column_name]] == "No", FALSE, TRUE))
  return(df)
}

data_clean_with_boolean <- data_clean %>%
  convert_values_to_boolean("is_LGBTQ") %>%
  convert_values_to_boolean("is_partner_deaf") %>%
  convert_values_to_boolean("is_homeowner") %>%
  convert_values_to_boolean("is_healthy_food_expensive") %>%
  convert_values_to_boolean("lost_a_parent") %>%  
  convert_values_to_boolean("parental_violence") %>%
  convert_values_to_boolean("family_members_in_jail") %>%
  convert_values_to_boolean("emotional_abuse_by_adults_in_home") %>%
  convert_values_to_boolean("use_hearing_aids") %>%
  convert_values_to_boolean("use_cochlear_implants") %>%
  convert_values_to_boolean("use_other_hearing_support_tool") %>%
  convert_values_to_boolean("still_wear_hearing_aid") %>%
  convert_values_to_boolean("is_transportation_affordable") %>%
  convert_values_to_boolean("are_hearing_orgs_accessible") %>%
  convert_values_to_boolean("can_afford_needed_healthcare") %>%
  convert_values_to_boolean("has_typhoid") %>%
  convert_values_to_boolean("has_malaria") %>%
  convert_values_to_boolean("has_HIV_or_AIDS") %>%
  convert_values_to_boolean("has_cancer") %>%
  convert_values_to_boolean("has_diabetes") %>%
  convert_values_to_boolean("has_high_blood_pressure") %>%
  convert_values_to_boolean("has_liver_disease") %>%
  convert_values_to_boolean("has_ulcers") %>%
  convert_values_to_boolean("has_other_health_issues") %>%
  convert_values_to_boolean("enough_interpreters") %>%
  convert_values_to_boolean("filing_lack_of_svc_complaints_by_form_is_helpful") %>%
  convert_values_to_boolean("transportation_barrier_to_healthy_food") %>%
  convert_values_to_boolean("time_barrier_to_healthy_food") %>%
  convert_values_to_boolean("knowledge_barrier_to_healthy_food") %>%
  convert_values_to_boolean("supplies_barrier_to_healthy_food") %>%
  convert_values_to_boolean("household_barrier_to_healthy_food") %>%
  convert_values_to_boolean("motivation_barrier_to_healthy_food")

write.csv(data_clean_with_boolean, "data.csv")


## load edited data ----
data_edited <- read_excel("data_edited.xlsx", sheet = "data_edited")

# dates were not read in properly from above statement. Needed to convert DOB to numeric, then set to date using the Excel origin date
data_edited$DOB <- as.Date(as.numeric(data_edited$DOB), origin = "1899-12-30")

data_column_descriptors <- read_xlsx("data_edited.xlsx", sheet = "column_descriptors")
         
# define data formats (date, factor)
data_edited <- data_edited %>%
  mutate(response_ID = as.character(response_ID),
         DOB = as.Date(DOB, format = "%m/%d/%Y")) %>%
  mutate_at(vars("religion",
                 "gender_identity",
                 "relationship_status",
                 "employment_status",
                 "education_level",
                 "access_to_clean_water",
                 "access_to_hand_washing",
                 "food_security_status",
                 "nutrition_status",
                 "fresh_produce_frequency",
                 "access_to_computer",
                 "access_to_internet",
                 "quality_of_life",
                 "capacity_for_work",
                 "ability_to_perform_daily_activities",
                 "energy_level",
                 "sleep_satisfaction_level",
                 "need_for_medical_treatment_in_daily_life",
                 "life_enjoyment_level",
                 "negative_feelings_frequency",
                 "is_life_meaningful",
                 "bodily_appearance_acceptance_level",
                 "self_satisfaction_level",
                 "access_to_health_services_satisfaction_level",
                 "info_availability_level",
                 "safety_satisfaction_level",
                 "financial_security_level",
                 "physical_environment_health_level",
                 "living_condition_satisfaction_level",
                 "transportation_satisfaction_level",
                 "personal_relationship_satisfaction_level",
                 "sex_life_satisfaction_level",
                 "friend_support_satisfaction_level",
                 "witnessed_stabbing_or_shooting",
                 "has_home_been_deliberately_destroyed",
                 "beaten_by_police_or_other_outside_home",
                 "want_to_learn_sign_language",
                 "sign_language_fluency",
                 "access_to_hearing_aid_batteries_or_repair",
                 "cochlear_implant_frequency_of_use",
                 "access_to_speech_or_auditory_therapy",
                 "can_get_communication_access_at_work",
                 "can_get_communication_access_at_school",
                 "can_get_social_communication_access",
                 "can_get_healthcare_communication_access",
                 "can_communicate_with_doctor",
                 "is_satisfied_with_health",
                 "interpreters_qualified_and_clear",
                 "trusts_interpreters",
                 "confidence_in_info_access_in_disaster_disease_crisis",
                 "discrimination_frequency"), as.factor)

date_today <-  Sys.Date() 
print(date_today)

# create data frame with all rows containing date of birth (remove rows where DOB = NA)
data_edited_age_analysis <- data_edited[!is.na(data_edited$DOB),]


## Age analysis data load ----

# compute age based on DOB
data_edited_age_analysis$age <- age_calc(data_edited_age_analysis$DOB,
                                         date_today,
                                         units = "years")

# create column to group respondants by age (over or under 50)
data_edited_age_analysis$age_group <- ifelse(data_edited_age_analysis$age > 50, "Over 50", "Under 50")
View(data_edited_age_analysis)  


write_xlsx(data_edited_age_analysis, "data_edited_age_analysis.xlsx")


age_analysis_summary_stats <- data_edited_age_analysis %>%
  summarise(
    average_age = mean(age),
    count_over_50 = sum(age_group == "Over 50"),
    count_under_50 = sum(age_group == "Under 50")
  )

write_xlsx(age_analysis_summary_stats, "age_analysis_summary_stats.xlsx")


## Gender analysis data load ----

# create data frame with all rows containing gender
data_edited_gender_analysis <- data_edited[!is.na(data_edited$gender_identity),]

write_xlsx(data_edited_gender_analysis, "data_edited_gender_analysis.xlsx")


## pivot longer on entire data frame ----
data_long_factors <- pivot_longer(data_edited, cols = c("access_to_clean_water",
                                                "access_to_hand_washing",
                                                "food_security_status",
                                                "nutrition_status",
                                                "fresh_produce_frequency",
                                                "access_to_computer",
                                                "access_to_internet",
                                                "quality_of_life",
                                                "capacity_for_work",
                                                "ability_to_perform_daily_activities",
                                                "energy_level",
                                                "sleep_satisfaction_level",
                                                "need_for_medical_treatment_in_daily_life",
                                                "life_enjoyment_level",
                                                "negative_feelings_frequency",
                                                "is_life_meaningful",
                                                "bodily_appearance_acceptance_level",
                                                "self_satisfaction_level",
                                                "access_to_health_services_satisfaction_level",
                                                "info_availability_level",
                                                "safety_satisfaction_level",
                                                "financial_security_level",
                                                "physical_environment_health_level",
                                                "living_condition_satisfaction_level",
                                                "transportation_satisfaction_level",
                                                "personal_relationship_satisfaction_level",
                                                "sex_life_satisfaction_level",
                                                "friend_support_satisfaction_level",
                                                "witnessed_stabbing_or_shooting",
                                                "has_home_been_deliberately_destroyed",
                                                "beaten_by_police_or_other_outside_home",
                                                "want_to_learn_sign_language",
                                                "sign_language_fluency",
                                                "access_to_hearing_aid_batteries_or_repair",
                                                "cochlear_implant_frequency_of_use",
                                                "access_to_speech_or_auditory_therapy",
                                                "can_get_communication_access_at_work",
                                                "can_get_communication_access_at_school",
                                                "can_get_social_communication_access",
                                                "can_get_healthcare_communication_access",
                                                "can_communicate_with_doctor",
                                                "is_satisfied_with_health",
                                                "interpreters_qualified_and_clear",
                                                "trusts_interpreters",
                                                "confidence_in_info_access_in_disaster_disease_crisis",
                                                "discrimination_frequency"),
                          names_to='question_name_factor',
                          values_to='value_factor')

data_long_boolean <- pivot_longer(data_edited, cols = c("is_LGBTQ",
                                              "is_partner_deaf",
                                              "is_homeowner",
                                              "is_healthy_food_expensive",
                                              "lost_a_parent",  
                                              "parental_violence",
                                              "family_members_in_jail",
                                              "emotional_abuse_by_adults_in_home",
                                              "use_hearing_aids",
                                              "use_cochlear_implants",
                                              "use_other_hearing_support_tool",
                                              "still_wear_hearing_aid",
                                              "is_transportation_affordable",
                                              "are_hearing_orgs_accessible",
                                              "can_afford_needed_healthcare",
                                              "has_typhoid",
                                              "has_malaria",
                                              "has_HIV_or_AIDS",
                                              "has_cancer",
                                              "has_diabetes",
                                              "has_high_blood_pressure",
                                              "has_liver_disease",
                                              "has_ulcers",
                                              "has_other_health_issues",
                                              "enough_interpreters",
                                              "filing_lack_of_svc_complaints_by_form_is_helpful",
                                              "no_transport_to_buy_healthy_food",
                                              "no_time_to_buy_or_prep_healthy_food",
                                              "do_not_know_how_to_cook_healthy_food",
                                              "no_supplies_or_equipment_to_make_healthy_food",
                                              "no_one_in_household_eats_healthy_food",
                                              "not_ready_to_change_eating_habits"),
                          names_to='question_name_boolean',
                          values_to='value_boolean')

data_long_integers <- pivot_longer(data_edited, cols = c("number_of_jobs_held",
                                              "number_of_workdays_per_week",
                                              "age_of_hearing_loss_onset",
                                              "age_when_learned_sign_language",
                                              "age_first_got_hearing_aid",
                                              "age_of_cochlear_implant_surgery"),
                          names_to='question_name_integer',
                          values_to='value_integer')




## create separate data frames based on LIKERT answer choices ----


## LIKERT I: Never - Infrequently - Sometimes - Frequently - Always ----

 # ALL ----
data_edited_Likert_I <- data_edited[c("response_ID",
                                       "languages",
                                       "DOB",
                                       "best_communication_contact_method",
                                       "best_emergency_contact_method",
                                       "tribal_affiliation",
                                       "religion",
                                       "gender_identity",
                                       "is_LGBTQ",
                                       "relationship_status",
                                       "is_partner_deaf",
                                       "employment_status",
                                       "job_description",
                                       "monthly_income",
                                       "number_of_jobs_held",
                                       "number_of_workdays_per_week",
                                       "education_level",
                                       "yearly_housing_expense",
                                       "is_homeowner",
                                      "age_of_hearing_loss_onset",
                                      "hearing_loss_support_needs",
                                      "age_when_learned_sign_language",
                                      "where_learned_sign_language",
                                      "age_first_got_hearing_aid",
                                      "age_of_cochlear_implant_surgery",
                                      "deaf_community_support_services_in_area",
                                      "three_most_significant_challenges_to_deaf",
                                      "communication_access_cost_responsibility",
                                      "where_to_locate_info_access_in_disaster_disease_crisis",
                                      "communication_with_family_method",
                                      
                                       "access_to_clean_water",
                                       "access_to_hand_washing",
                                       "food_security_status",
                                       "fresh_produce_frequency",
                                       "negative_feelings_frequency",
                                       "access_to_computer",
                                       "access_to_internet",
                                       "can_get_communication_access_at_school",
                                       "can_get_social_communication_access",
                                       "can_get_healthcare_communication_access",
                                       "can_make_independent_health_decisions")]

write_xlsx(data_edited_Likert_I, "data_edited_Likert_I.xlsx")

data_edited_Likert_I_long <- pivot_longer(data_edited_Likert_I, cols = c("access_to_clean_water",
                                                        "access_to_hand_washing",
                                                        "food_security_status",
                                                        "fresh_produce_frequency",
                                                        "negative_feelings_frequency",
                                                        "access_to_computer",
                                                        "access_to_internet",
                                                        "can_get_communication_access_at_school",
                                                        "can_get_social_communication_access",
                                                        "can_get_healthcare_communication_access",
                                                        "can_make_independent_health_decisions"),
                                  names_to='question_name',
                                  values_to='value')

write_xlsx(data_edited_Likert_I_long, "data_edited_Likert_I_long.xlsx")

Likert_I_counts <- data_edited_Likert_I_long %>%
  group_by(question_name, value) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Never", "Infrequently", "Sometimes", "Frequently", "Always"))

Likert_I_counts_wide <- pivot_wider(Likert_I_counts, names_from = value, values_from = count)

Likert_I_counts_wide <- Likert_I_counts_wide[, c(1,5,4,6,3,2)]

write_xlsx(Likert_I_counts_wide, "Likert_I_counts.xlsx")

 # AGE ANALYSIS Likert I ----
age_analysis_data_edited_Likert_I <- data_edited_age_analysis[c("response_ID",
                                      "languages",
                                      "DOB",
                                      "age",
                                      "age_group",
                                      "best_communication_contact_method",
                                      "best_emergency_contact_method",
                                      "tribal_affiliation",
                                      "religion",
                                      "gender_identity",
                                      "is_LGBTQ",
                                      "relationship_status",
                                      "is_partner_deaf",
                                      "employment_status",
                                      "job_description",
                                      "monthly_income",
                                      "number_of_jobs_held",
                                      "number_of_workdays_per_week",
                                      "education_level",
                                      "yearly_housing_expense",
                                      "is_homeowner",
                                      "age_of_hearing_loss_onset",
                                      "hearing_loss_support_needs",
                                      "age_when_learned_sign_language",
                                      "where_learned_sign_language",
                                      "age_first_got_hearing_aid",
                                      "age_of_cochlear_implant_surgery",
                                      "deaf_community_support_services_in_area",
                                      "three_most_significant_challenges_to_deaf",
                                      "communication_access_cost_responsibility",
                                      "where_to_locate_info_access_in_disaster_disease_crisis",
                                      "communication_with_family_method",
                                      
                                      "access_to_clean_water",
                                      "access_to_hand_washing",
                                      "food_security_status",
                                      "fresh_produce_frequency",
                                      "negative_feelings_frequency",
                                      "access_to_computer",
                                      "access_to_internet",
                                      "can_get_communication_access_at_school",
                                      "can_get_social_communication_access",
                                      "can_get_healthcare_communication_access",
                                      "can_make_independent_health_decisions")]

age_analysis_data_edited_Likert_I_long <- pivot_longer(age_analysis_data_edited_Likert_I, cols = c("access_to_clean_water",
                                                                         "access_to_hand_washing",
                                                                         "food_security_status",
                                                                         "fresh_produce_frequency",
                                                                         "negative_feelings_frequency",
                                                                         "access_to_computer",
                                                                         "access_to_internet",
                                                                         "can_get_communication_access_at_school",
                                                                         "can_get_social_communication_access",
                                                                         "can_get_healthcare_communication_access",
                                                                         "can_make_independent_health_decisions"),
                                          names_to='question_name',
                                          values_to='value')

write_xlsx(age_analysis_data_edited_Likert_I_long, "age_analysis_data_edited_Likert_I_long.xlsx")

age_analysis_Likert_I_counts <- age_analysis_data_edited_Likert_I_long %>%
  group_by(question_name, value, age_group) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Never", "Infrequently", "Sometimes", "Frequently", "Always"))

age_analysis_Likert_I_counts_wide <- pivot_wider(age_analysis_Likert_I_counts, names_from = value, values_from = count)

age_analysis_Likert_I_counts_wide <- age_analysis_Likert_I_counts_wide[, c(1,5,4,6,3,2)]

write_xlsx(age_analysis_Likert_I_counts_wide, "age_analysis_Likert_I_counts.xlsx")

 # GENDER ANALYSIS Likert I ----

gender_analysis_data_edited_Likert_I <- data_edited_gender_analysis[c("response_ID",
                                                                   "languages",
                                                                   
                                                                   "best_communication_contact_method",
                                                                   "best_emergency_contact_method",
                                                                   "tribal_affiliation",
                                                                   "religion",
                                                                   "gender_identity",
                                                                   "is_LGBTQ",
                                                                   "relationship_status",
                                                                   "is_partner_deaf",
                                                                   "employment_status",
                                                                   "job_description",
                                                                   "monthly_income",
                                                                   "number_of_jobs_held",
                                                                   "number_of_workdays_per_week",
                                                                   "education_level",
                                                                   "yearly_housing_expense",
                                                                   "is_homeowner",
                                                                   "age_of_hearing_loss_onset",
                                                                   "hearing_loss_support_needs",
                                                                   "age_when_learned_sign_language",
                                                                   "where_learned_sign_language",
                                                                   "age_first_got_hearing_aid",
                                                                   "age_of_cochlear_implant_surgery",
                                                                   "deaf_community_support_services_in_area",
                                                                   "three_most_significant_challenges_to_deaf",
                                                                   "communication_access_cost_responsibility",
                                                                   "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                   "communication_with_family_method",
                                                                   
                                                                   "access_to_clean_water",
                                                                   "access_to_hand_washing",
                                                                   "food_security_status",
                                                                   "fresh_produce_frequency",
                                                                   "negative_feelings_frequency",
                                                                   "access_to_computer",
                                                                   "access_to_internet",
                                                                   "can_get_communication_access_at_school",
                                                                   "can_get_social_communication_access",
                                                                   "can_get_healthcare_communication_access",
                                                                   "can_make_independent_health_decisions")]

gender_analysis_data_edited_Likert_I_long <- pivot_longer(gender_analysis_data_edited_Likert_I, cols = c("access_to_clean_water",
                                                                                                   "access_to_hand_washing",
                                                                                                   "food_security_status",
                                                                                                   "fresh_produce_frequency",
                                                                                                   "negative_feelings_frequency",
                                                                                                   "access_to_computer",
                                                                                                   "access_to_internet",
                                                                                                   "can_get_communication_access_at_school",
                                                                                                   "can_get_social_communication_access",
                                                                                                   "can_get_healthcare_communication_access",
                                                                                                   "can_make_independent_health_decisions"),
                                                       names_to='question_name',
                                                       values_to='value')

write_xlsx(gender_analysis_data_edited_Likert_I_long, "gender_analysis_data_edited_Likert_I_long.xlsx")

gender_analysis_Likert_I_counts <- gender_analysis_data_edited_Likert_I %>%
  group_by(question_name, value, gender_identity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Never", "Infrequently", "Sometimes", "Frequently", "Always"))

gender_analysis_Likert_I_counts_wide <- pivot_wider(gender_analysis_Likert_I_counts, names_from = value, values_from = count)

write_xlsx(gender_analysis_Likert_I_counts_wide, "gender_analysis_Likert_I_counts_wide.xlsx")


## LIKERT II: Poor - Fair - Good - Very Good - Excellent ----
 # ALL ----
data_edited_Likert_II <- data_edited[c("response_ID",
                                       "languages",
                                       "DOB",
                                       "best_communication_contact_method",
                                       "best_emergency_contact_method",
                                       "tribal_affiliation",
                                       "religion",
                                       "gender_identity",
                                       "is_LGBTQ",
                                       "relationship_status",
                                       "is_partner_deaf",
                                       "employment_status",
                                       "job_description",
                                       "monthly_income",
                                       "number_of_jobs_held",
                                       "number_of_workdays_per_week",
                                       "education_level",
                                       "yearly_housing_expense",
                                       "is_homeowner",
                                       "age_of_hearing_loss_onset",
                                       "hearing_loss_support_needs",
                                       "age_when_learned_sign_language",
                                       "where_learned_sign_language",
                                       "age_first_got_hearing_aid",
                                       "age_of_cochlear_implant_surgery",
                                       "deaf_community_support_services_in_area",
                                       "three_most_significant_challenges_to_deaf",
                                       "communication_access_cost_responsibility",
                                       "where_to_locate_info_access_in_disaster_disease_crisis",
                                       "communication_with_family_method",
                                       
                                       "nutrition_status")]

data_edited_Likert_II_long <- pivot_longer(data_edited_Likert_II, cols = c("nutrition_status"),
                                          names_to='question_name',
                                          values_to='value')

write_xlsx(data_edited_Likert_II_long, "data_edited_Likert_II_long.xlsx")

Likert_II_counts <- data_edited_Likert_II_long %>%
  group_by(question_name, value) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Poor", "Fair", "Good", "Very Good", "Excellent"))

Likert_II_counts_wide <- pivot_wider(Likert_II_counts, names_from = value, values_from = count)

Likert_II_counts_wide <- Likert_II_counts_wide[, c(1,5,3,4,2)]

write_xlsx(Likert_II_counts_wide, "Likert_II_counts.xlsx")

 # AGE ANALYSIS Likert II ----
age_analysis_data_edited_Likert_II <- data_edited_age_analysis[c("response_ID",
                                                                "languages",
                                                                "DOB",
                                                                "age",
                                                                "age_group",
                                                                "best_communication_contact_method",
                                                                "best_emergency_contact_method",
                                                                "tribal_affiliation",
                                                                "religion",
                                                                "gender_identity",
                                                                "is_LGBTQ",
                                                                "relationship_status",
                                                                "is_partner_deaf",
                                                                "employment_status",
                                                                "job_description",
                                                                "monthly_income",
                                                                "number_of_jobs_held",
                                                                "number_of_workdays_per_week",
                                                                "education_level",
                                                                "yearly_housing_expense",
                                                                "is_homeowner",
                                                                "age_of_hearing_loss_onset",
                                                                "hearing_loss_support_needs",
                                                                "age_when_learned_sign_language",
                                                                "where_learned_sign_language",
                                                                "age_first_got_hearing_aid",
                                                                "age_of_cochlear_implant_surgery",
                                                                "deaf_community_support_services_in_area",
                                                                "three_most_significant_challenges_to_deaf",
                                                                "communication_access_cost_responsibility",
                                                                "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                "communication_with_family_method",
                                                                
                                                                "nutrition_status")]

age_analysis_data_edited_Likert_II_long <- pivot_longer(age_analysis_data_edited_Likert_II, cols = c("nutrition_status"),
                                                       names_to='question_name',
                                                       values_to='value') %>%
  mutate(value = trimws(value))

write_xlsx(age_analysis_data_edited_Likert_II_long, "age_analysis_data_edited_Likert_II_long.xlsx")

unique(data_edited_Likert_II_long$value)

# Clean up the 'value' column
age_analysis_data_edited_Likert_II_long <- age_analysis_data_edited_Likert_II_long %>%
  mutate(value = str_replace(value, "(?i)^Very[^[:alnum:]]*Good$", "Very Good"))

age_analysis_Likert_II_counts <- age_analysis_data_edited_Likert_II_long %>%
  mutate(value = gsub("[^[:print:]]", " ", value)) %>%  # Remove non-printable characters
  group_by(question_name, value, age_group) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Poor", "Fair", "Good", "Very Good", "Excellent"))

View(age_analysis_Likert_II_counts)

age_analysis_Likert_II_counts_wide <- pivot_wider(age_analysis_Likert_II_counts, names_from = value, values_from = count)
View(age_analysis_Likert_II_counts_wide)

age_analysis_Likert_II_counts_wide <- age_analysis_Likert_II_counts_wide[, c(1,2,6,4,5,7,3)]

write_xlsx(age_analysis_Likert_II_counts_wide, "age_analysis_Likert_II_counts.xlsx")


 # GENDER ANALYSIS Likert II ----

gender_analysis_data_edited_Likert_II <- data_edited_gender_analysis[c("response_ID",
                                                                      "languages",
                                                                      
                                                                      "best_communication_contact_method",
                                                                      "best_emergency_contact_method",
                                                                      "tribal_affiliation",
                                                                      "religion",
                                                                      "gender_identity",
                                                                      "is_LGBTQ",
                                                                      "relationship_status",
                                                                      "is_partner_deaf",
                                                                      "employment_status",
                                                                      "job_description",
                                                                      "monthly_income",
                                                                      "number_of_jobs_held",
                                                                      "number_of_workdays_per_week",
                                                                      "education_level",
                                                                      "yearly_housing_expense",
                                                                      "is_homeowner",
                                                                      "age_of_hearing_loss_onset",
                                                                      "hearing_loss_support_needs",
                                                                      "age_when_learned_sign_language",
                                                                      "where_learned_sign_language",
                                                                      "age_first_got_hearing_aid",
                                                                      "age_of_cochlear_implant_surgery",
                                                                      "deaf_community_support_services_in_area",
                                                                      "three_most_significant_challenges_to_deaf",
                                                                      "communication_access_cost_responsibility",
                                                                      "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                      "communication_with_family_method",
                                                                      "nutrition_status")]

gender_analysis_data_edited_Likert_II_long <- pivot_longer(gender_analysis_data_edited_Likert_II, cols = c("nutrition_status"),
                                                          names_to='question_name',
                                                          values_to='value')

gender_analysis_data_edited_Likert_II_long <- gender_analysis_data_edited_Likert_II_long %>%
  mutate(value = str_replace(value, "(?i)^Very[^[:alnum:]]*Good$", "Very Good"))

write_xlsx(gender_analysis_data_edited_Likert_II_long, "gender_analysis_data_edited_Likert_II_long.xlsx")

gender_analysis_Likert_II_counts <- gender_analysis_data_edited_Likert_II_long %>%
  mutate(value = gsub("[^[:print:]]", " ", value)) %>%  # Remove non-printable characters
  group_by(question_name, value, gender_identity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Poor", "Fair", "Good", "Very Good", "Excellent"))

gender_analysis_Likert_II_counts_wide <- pivot_wider(gender_analysis_Likert_II_counts, names_from = value, values_from = count)
View(gender_analysis_Likert_II_counts_wide)

write_xlsx(gender_analysis_Likert_II_counts_wide, "gender_analysis_Likert_II_counts_wide.xlsx")


## LIKERT III:  Very Poor - Poor - Neither poor nor good - Good - Very Good ----

data_edited_Likert_III <- data_edited[c("response_ID",
                                        "languages", 
                                        "DOB",
                                        "best_communication_contact_method",
                                        "best_emergency_contact_method",
                                        "tribal_affiliation",
                                        "religion",
                                        "gender_identity",
                                        "is_LGBTQ",
                                        "relationship_status",
                                        "is_partner_deaf",
                                        "employment_status",
                                        "job_description",
                                        "monthly_income",
                                        "number_of_jobs_held",
                                        "number_of_workdays_per_week",
                                        "education_level",
                                        "yearly_housing_expense",
                                        "is_homeowner",
                                        "age_of_hearing_loss_onset",
                                        "hearing_loss_support_needs",
                                        "age_when_learned_sign_language",
                                        "where_learned_sign_language",
                                        "age_first_got_hearing_aid",
                                        "age_of_cochlear_implant_surgery",
                                        "deaf_community_support_services_in_area",
                                        "three_most_significant_challenges_to_deaf",
                                        "communication_access_cost_responsibility",
                                        "where_to_locate_info_access_in_disaster_disease_crisis",
                                        "communication_with_family_method",
                                        
                                        "quality_of_life",
                                        "sign_language_fluency")]


data_edited_Likert_III_long <- pivot_longer(data_edited_Likert_III, cols = c("quality_of_life",
                                                                  "sign_language_fluency"),
                                           names_to='question_name',
                                           values_to='value')

write_xlsx(data_edited_Likert_III_long, "data_edited_Likert_III_long.xlsx")

Likert_III_counts <- data_edited_Likert_III_long %>%
  group_by(question_name, value) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Very Poor", "Poor", "Neither poor nor good", "Good", "Very Good"))

Likert_III_counts_wide <- pivot_wider(Likert_III_counts, names_from = value, values_from = count)

Likert_III_counts_wide <- Likert_III_counts_wide[, c(1,6,4,3,2,5)]

write_xlsx(Likert_III_counts_wide, "Likert_III_counts.xlsx")



 # AGE ANALYSIS Likert III ----

age_analysis_data_edited_Likert_III <- data_edited_age_analysis[c("response_ID",
                                                                "languages",
                                                                "DOB",
                                                                "age",
                                                                "age_group",
                                                                "best_communication_contact_method",
                                                                "best_emergency_contact_method",
                                                                "tribal_affiliation",
                                                                "religion",
                                                                "gender_identity",
                                                                "is_LGBTQ",
                                                                "relationship_status",
                                                                "is_partner_deaf",
                                                                "employment_status",
                                                                "job_description",
                                                                "monthly_income",
                                                                "number_of_jobs_held",
                                                                "number_of_workdays_per_week",
                                                                "education_level",
                                                                "yearly_housing_expense",
                                                                "is_homeowner",
                                                                "age_of_hearing_loss_onset",
                                                                "hearing_loss_support_needs",
                                                                "age_when_learned_sign_language",
                                                                "where_learned_sign_language",
                                                                "age_first_got_hearing_aid",
                                                                "age_of_cochlear_implant_surgery",
                                                                "deaf_community_support_services_in_area",
                                                                "three_most_significant_challenges_to_deaf",
                                                                "communication_access_cost_responsibility",
                                                                "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                "communication_with_family_method",
                                                                
                                                                "quality_of_life",
                                                                "sign_language_fluency")]

age_analysis_data_edited_Likert_III_long <- pivot_longer(age_analysis_data_edited_Likert_III, cols = c("quality_of_life",
                                                                                                   "sign_language_fluency"),
                                                       names_to='question_name',
                                                       values_to='value')

write_xlsx(age_analysis_data_edited_Likert_III_long, "age_analysis_data_edited_Likert_III_long.xlsx")

age_analysis_Likert_III_counts <- age_analysis_data_edited_Likert_III_long %>%
  group_by(question_name, value, age_group) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Very Poor", "Poor", "Neither poor nor good", "Good", "Very Good"))

age_analysis_Likert_III_counts_wide <- pivot_wider(age_analysis_Likert_III_counts, names_from = value, values_from = count)

age_analysis_Likert_III_counts_wide <- age_analysis_Likert_III_counts_wide[, c(1,2,7,5,4,3,6)]

write_xlsx(age_analysis_Likert_III_counts_wide, "age_analysis_Likert_III_counts.xlsx")

 # GENDER ANALYSIS Likert III ----

gender_analysis_data_edited_Likert_III <- data_edited_gender_analysis[c("response_ID",
                                                                       "languages",
                                                                       
                                                                       "best_communication_contact_method",
                                                                       "best_emergency_contact_method",
                                                                       "tribal_affiliation",
                                                                       "religion",
                                                                       "gender_identity",
                                                                       "is_LGBTQ",
                                                                       "relationship_status",
                                                                       "is_partner_deaf",
                                                                       "employment_status",
                                                                       "job_description",
                                                                       "monthly_income",
                                                                       "number_of_jobs_held",
                                                                       "number_of_workdays_per_week",
                                                                       "education_level",
                                                                       "yearly_housing_expense",
                                                                       "is_homeowner",
                                                                       "age_of_hearing_loss_onset",
                                                                       "hearing_loss_support_needs",
                                                                       "age_when_learned_sign_language",
                                                                       "where_learned_sign_language",
                                                                       "age_first_got_hearing_aid",
                                                                       "age_of_cochlear_implant_surgery",
                                                                       "deaf_community_support_services_in_area",
                                                                       "three_most_significant_challenges_to_deaf",
                                                                       "communication_access_cost_responsibility",
                                                                       "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                       "communication_with_family_method",
                                                                       
                                                                       "quality_of_life",
                                                                       "sign_language_fluency")]

gender_analysis_data_edited_Likert_III_long <- pivot_longer(gender_analysis_data_edited_Likert_III, cols = c("quality_of_life",
                                                                                                             "sign_language_fluency"),
                                                           names_to='question_name',
                                                           values_to='value')

write_xlsx(gender_analysis_data_edited_Likert_III_long, "gender_analysis_data_edited_Likert_III_long.xlsx")
View(gender_analysis_data_edited_Likert_III_long)

gender_analysis_Likert_III_counts <- gender_analysis_data_edited_Likert_III_long %>%
  mutate(value = gsub("[^[:print:]]", " ", value)) %>%  # Remove non-printable characters
  group_by(question_name, value, gender_identity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Very Poor", "Poor", "Neither poor nor good", "Good", "Very Good"))

gender_analysis_Likert_III_counts_wide <- pivot_wider(gender_analysis_Likert_III_counts, names_from = value, values_from = count)
View(gender_analysis_Likert_III_counts_wide)

gender_analysis_Likert_III_counts_wide <- gender_analysis_Likert_III_counts_wide[, c(1,2,7,5,4,3,6)]

write_xlsx(gender_analysis_Likert_III_counts_wide, "gender_analysis_Likert_III_counts_wide.xlsx")


## LIKERT IV: Very dissatisfied - Fairly dissatisfied - Neither satisfied nor dissatisfied - Satisfied - Very satisfied ----
 # ALL ----
data_edited_Likert_IV <- data_edited[c("response_ID",
                                       "languages",
                                       "DOB",
                                       "best_communication_contact_method",
                                       "best_emergency_contact_method",
                                       "tribal_affiliation",
                                       "religion",
                                       "gender_identity",
                                       "is_LGBTQ",
                                       "relationship_status",
                                       "is_partner_deaf",
                                       "employment_status",
                                       "job_description",
                                       "monthly_income",
                                       "number_of_jobs_held",
                                       "number_of_workdays_per_week",
                                       "education_level",
                                       "yearly_housing_expense",
                                       "is_homeowner",
                                       "age_of_hearing_loss_onset",
                                       "hearing_loss_support_needs",
                                       "age_when_learned_sign_language",
                                       "where_learned_sign_language",
                                       "age_first_got_hearing_aid",
                                       "age_of_cochlear_implant_surgery",
                                       "deaf_community_support_services_in_area",
                                       "three_most_significant_challenges_to_deaf",
                                       "communication_access_cost_responsibility",
                                       "where_to_locate_info_access_in_disaster_disease_crisis",
                                       "communication_with_family_method",
                                       
                                       "capacity_for_work",
                                       "ability_to_perform_daily_activities",
                                       "sleep_satisfaction_level",
                                       "self_satisfaction_level",
                                       "access_to_health_services_satisfaction_level",
                                       "living_condition_satisfaction_level",
                                       "transportation_satisfaction_level",
                                       "personal_relationship_satisfaction_level",
                                       "sex_life_satisfaction_level",
                                       "friend_support_satisfaction_level",
                                       "is_satisfied_with_health")]

data_edited_Likert_IV_long <- pivot_longer(data_edited_Likert_IV, cols = c("capacity_for_work",
                                                                 "ability_to_perform_daily_activities",
                                                                 "sleep_satisfaction_level",
                                                                 "self_satisfaction_level",
                                                                 "access_to_health_services_satisfaction_level",
                                                                 "living_condition_satisfaction_level",
                                                                 "transportation_satisfaction_level",
                                                                 "personal_relationship_satisfaction_level",
                                                                 "sex_life_satisfaction_level",
                                                                 "friend_support_satisfaction_level",
                                                                 "is_satisfied_with_health"),
                                            names_to='question_name',
                                            values_to='value')

write_xlsx(data_edited_Likert_IV_long, "data_edited_Likert_IV_long.xlsx")

Likert_IV_counts <- data_edited_Likert_IV_long %>%
  group_by(question_name, value) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Very dissatisfied", "Fairly dissatisfied", "Neither satisfied nor dissatisfied", "Satisfied", "Very satisfied"))

Likert_IV_counts_wide <- pivot_wider(Likert_IV_counts, names_from = value, values_from = count)

Likert_IV_counts_wide <- Likert_IV_counts_wide[, c(1,5,2,3,4,6)]

write_xlsx(Likert_IV_counts_wide, "Likert_IV_counts.xlsx")

 # AGE ANALYSIS Likert IV ----

age_analysis_data_edited_Likert_IV <- data_edited_age_analysis[c("response_ID",
                                                                  "languages",
                                                                  "DOB",
                                                                  "age",
                                                                  "age_group",
                                                                  "best_communication_contact_method",
                                                                  "best_emergency_contact_method",
                                                                  "tribal_affiliation",
                                                                  "religion",
                                                                  "gender_identity",
                                                                  "is_LGBTQ",
                                                                  "relationship_status",
                                                                  "is_partner_deaf",
                                                                  "employment_status",
                                                                  "job_description",
                                                                  "monthly_income",
                                                                  "number_of_jobs_held",
                                                                  "number_of_workdays_per_week",
                                                                  "education_level",
                                                                  "yearly_housing_expense",
                                                                  "is_homeowner",
                                                                  "age_of_hearing_loss_onset",
                                                                  "hearing_loss_support_needs",
                                                                  "age_when_learned_sign_language",
                                                                  "where_learned_sign_language",
                                                                  "age_first_got_hearing_aid",
                                                                  "age_of_cochlear_implant_surgery",
                                                                  "deaf_community_support_services_in_area",
                                                                  "three_most_significant_challenges_to_deaf",
                                                                  "communication_access_cost_responsibility",
                                                                  "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                  "communication_with_family_method",
                                                                  
                                                                 "capacity_for_work",
                                                                 "ability_to_perform_daily_activities",
                                                                 "sleep_satisfaction_level",
                                                                 "self_satisfaction_level",
                                                                 "access_to_health_services_satisfaction_level",
                                                                 "living_condition_satisfaction_level",
                                                                 "transportation_satisfaction_level",
                                                                 "personal_relationship_satisfaction_level",
                                                                 "sex_life_satisfaction_level",
                                                                 "friend_support_satisfaction_level",
                                                                 "is_satisfied_with_health")]

age_analysis_data_edited_Likert_IV_long <- pivot_longer(age_analysis_data_edited_Likert_IV, cols = c("capacity_for_work",
                                                        "ability_to_perform_daily_activities",
                                                        "sleep_satisfaction_level",
                                                        "self_satisfaction_level",
                                                        "access_to_health_services_satisfaction_level",
                                                        "living_condition_satisfaction_level",
                                                        "transportation_satisfaction_level",
                                                        "personal_relationship_satisfaction_level",
                                                        "sex_life_satisfaction_level",
                                                        "friend_support_satisfaction_level",
                                                        "is_satisfied_with_health"),

                                                         names_to='question_name',
                                                         values_to='value')

write_xlsx(age_analysis_data_edited_Likert_IV_long, "age_analysis_data_edited_Likert_IV_long.xlsx")

age_analysis_Likert_IV_counts <- age_analysis_data_edited_Likert_IV_long %>%
  group_by(question_name, value, age_group) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Very dissatisfied", "Fairly dissatisfied", "Neither satisfied nor dissatisfied", "Satisfied", "Very satisfied"))

age_analysis_Likert_IV_counts_wide <- pivot_wider(age_analysis_Likert_IV_counts, names_from = value, values_from = count)
View(age_analysis_Likert_IV_counts_wide)

age_analysis_Likert_IV_counts_wide <- age_analysis_Likert_IV_counts_wide[, c(1,2,6,3,4,5,7)]

write_xlsx(age_analysis_Likert_IV_counts_wide, "age_analysis_Likert_IV_counts.xlsx")

 # GENDER ANALYSIS Likert IV ----

gender_analysis_data_edited_Likert_IV <- data_edited_gender_analysis[c("response_ID",
                                                                        "languages",
                                                                        
                                                                        "best_communication_contact_method",
                                                                        "best_emergency_contact_method",
                                                                        "tribal_affiliation",
                                                                        "religion",
                                                                        "gender_identity",
                                                                        "is_LGBTQ",
                                                                        "relationship_status",
                                                                        "is_partner_deaf",
                                                                        "employment_status",
                                                                        "job_description",
                                                                        "monthly_income",
                                                                        "number_of_jobs_held",
                                                                        "number_of_workdays_per_week",
                                                                        "education_level",
                                                                        "yearly_housing_expense",
                                                                        "is_homeowner",
                                                                        "age_of_hearing_loss_onset",
                                                                        "hearing_loss_support_needs",
                                                                        "age_when_learned_sign_language",
                                                                        "where_learned_sign_language",
                                                                        "age_first_got_hearing_aid",
                                                                        "age_of_cochlear_implant_surgery",
                                                                        "deaf_community_support_services_in_area",
                                                                        "three_most_significant_challenges_to_deaf",
                                                                        "communication_access_cost_responsibility",
                                                                        "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                        "communication_with_family_method",
                                                                        
                                                                       "capacity_for_work",
                                                                       "ability_to_perform_daily_activities",
                                                                       "sleep_satisfaction_level",
                                                                       "self_satisfaction_level",
                                                                       "access_to_health_services_satisfaction_level",
                                                                       "living_condition_satisfaction_level",
                                                                       "transportation_satisfaction_level",
                                                                       "personal_relationship_satisfaction_level",
                                                                       "sex_life_satisfaction_level",
                                                                       "friend_support_satisfaction_level",
                                                                       "is_satisfied_with_health")]

gender_analysis_data_edited_Likert_IV_long <- pivot_longer(gender_analysis_data_edited_Likert_IV, cols = c("capacity_for_work",
                                                                                                             "ability_to_perform_daily_activities",
                                                                                                             "sleep_satisfaction_level",
                                                                                                             "self_satisfaction_level",
                                                                                                             "access_to_health_services_satisfaction_level",
                                                                                                             "living_condition_satisfaction_level",
                                                                                                             "transportation_satisfaction_level",
                                                                                                             "personal_relationship_satisfaction_level",
                                                                                                             "sex_life_satisfaction_level",
                                                                                                             "friend_support_satisfaction_level",
                                                                                                             "is_satisfied_with_health"),
                                                            names_to='question_name',
                                                            values_to='value')

write_xlsx(gender_analysis_data_edited_Likert_IV_long, "gender_analysis_data_edited_Likert_IV_long.xlsx")

gender_analysis_Likert_IV_counts <- gender_analysis_data_edited_Likert_IV_long %>%
  group_by(question_name, value, gender_identity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Very dissatisfied", "Fairly dissatisfied", "Neither satisfied nor dissatisfied", "Satisfied", "Very satisfied"))

gender_analysis_Likert_IV_counts_wide <- pivot_wider(gender_analysis_Likert_IV_counts, names_from = value, values_from = count)
View(gender_analysis_Likert_IV_counts_wide)

gender_analysis_Likert_IV_counts_wide <- gender_analysis_Likert_IV_counts_wide[, c(1,2,6,3,4,5,7)]

write_xlsx(gender_analysis_Likert_IV_counts_wide, "gender_analysis_Likert_IV_counts_wide.xlsx")

## LIKERT V: Not at all - Slightly - Somewhat - To a great extent - Completely ----

 # ALL ----
data_edited_Likert_V <- data_edited[c("response_ID",
                                       "languages",
                                       "DOB",
                                       "best_communication_contact_method",
                                       "best_emergency_contact_method",
                                       "tribal_affiliation",
                                       "religion",
                                       "gender_identity",
                                       "is_LGBTQ",
                                       "relationship_status",
                                       "is_partner_deaf",
                                       "employment_status",
                                       "job_description",
                                       "monthly_income",
                                       "number_of_jobs_held",
                                       "number_of_workdays_per_week",
                                       "education_level",
                                       "yearly_housing_expense",
                                       "is_homeowner",
                                      "age_of_hearing_loss_onset",
                                      "hearing_loss_support_needs",
                                      "age_when_learned_sign_language",
                                      "where_learned_sign_language",
                                      "age_first_got_hearing_aid",
                                      "age_of_cochlear_implant_surgery",
                                      "deaf_community_support_services_in_area",
                                      "three_most_significant_challenges_to_deaf",
                                      "communication_access_cost_responsibility",
                                      "where_to_locate_info_access_in_disaster_disease_crisis",
                                      "communication_with_family_method",
                                      
                                      "energy_level",
                                      "bodily_appearance_acceptance_level",
                                      "info_availability_level",
                                      "financial_security_level",
                                      "access_to_hearing_aid_batteries_or_repair",
                                      "access_to_speech_or_auditory_therapy",
                                      "can_communicate_with_doctor",
                                      "interpreters_qualified_and_clear",
                                      "trusts_interpreters",
                                      "confidence_in_info_access_in_disaster_disease_crisis")]

data_edited_Likert_V_long <- pivot_longer(data_edited_Likert_V, cols = c( "energy_level",
                                                                          "bodily_appearance_acceptance_level",
                                                                          "info_availability_level",
                                                                          "financial_security_level",
                                                                          "access_to_hearing_aid_batteries_or_repair",
                                                                          "access_to_speech_or_auditory_therapy",
                                                                          "can_communicate_with_doctor",
                                                                          "interpreters_qualified_and_clear",
                                                                          "trusts_interpreters",
                                                                          "confidence_in_info_access_in_disaster_disease_crisis"),
                                          names_to='question_name',
                                          values_to='value')

write_xlsx(data_edited_Likert_V_long, "data_edited_Likert_V_long.xlsx")

Likert_V_counts <- data_edited_Likert_V_long %>%
  group_by(question_name, value) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Not at all", "Slightly", "Somewhat", "To a great extent", "Completely"))

Likert_V_counts_wide <- pivot_wider(Likert_V_counts, names_from = value, values_from = count)
View(Likert_V_counts_wide)

Likert_V_counts_wide <- Likert_V_counts_wide[, c(1,3,4,5,6,2)]

write_xlsx(Likert_V_counts_wide, "Likert_V_counts.xlsx")

 # AGE ANALYSIS Likert V ----
age_analysis_data_edited_Likert_V <- data_edited_age_analysis[c("response_ID",
                                                                 "languages",
                                                                 "DOB",
                                                                 "age",
                                                                 "age_group",
                                                                 "best_communication_contact_method",
                                                                 "best_emergency_contact_method",
                                                                 "tribal_affiliation",
                                                                 "religion",
                                                                 "gender_identity",
                                                                 "is_LGBTQ",
                                                                 "relationship_status",
                                                                 "is_partner_deaf",
                                                                 "employment_status",
                                                                 "job_description",
                                                                 "monthly_income",
                                                                 "number_of_jobs_held",
                                                                 "number_of_workdays_per_week",
                                                                 "education_level",
                                                                 "yearly_housing_expense",
                                                                 "is_homeowner",
                                                                 "age_of_hearing_loss_onset",
                                                                 "hearing_loss_support_needs",
                                                                 "age_when_learned_sign_language",
                                                                 "where_learned_sign_language",
                                                                 "age_first_got_hearing_aid",
                                                                 "age_of_cochlear_implant_surgery",
                                                                 "deaf_community_support_services_in_area",
                                                                 "three_most_significant_challenges_to_deaf",
                                                                 "communication_access_cost_responsibility",
                                                                 "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                 "communication_with_family_method",
                                                                 
                                                                "energy_level",
                                                                "bodily_appearance_acceptance_level",
                                                                "info_availability_level",
                                                                "financial_security_level",
                                                                "access_to_hearing_aid_batteries_or_repair",
                                                                "access_to_speech_or_auditory_therapy",
                                                                "can_communicate_with_doctor",
                                                                "interpreters_qualified_and_clear",
                                                                "trusts_interpreters",
                                                                "confidence_in_info_access_in_disaster_disease_crisis")]

age_analysis_data_edited_Likert_V_long <- pivot_longer(age_analysis_data_edited_Likert_V, cols = c("energy_level",
                                                                                                     "bodily_appearance_acceptance_level",
                                                                                                     "info_availability_level",
                                                                                                     "financial_security_level",
                                                                                                     "access_to_hearing_aid_batteries_or_repair",
                                                                                                     "access_to_speech_or_auditory_therapy",
                                                                                                     "can_communicate_with_doctor",
                                                                                                     "interpreters_qualified_and_clear",
                                                                                                     "trusts_interpreters",
                                                                                                     "confidence_in_info_access_in_disaster_disease_crisis"),
                                                        
                                                        names_to='question_name',
                                                        values_to='value')

write_xlsx(age_analysis_data_edited_Likert_V_long, "age_analysis_data_edited_Likert_V_long.xlsx")

age_analysis_Likert_V_counts <- age_analysis_data_edited_Likert_V_long %>%
  group_by(question_name, value, age_group) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Not at all", "Slightly", "Somewhat", "To a great extent", "Completely"))

age_analysis_Likert_V_counts_wide <- pivot_wider(age_analysis_Likert_V_counts, names_from = value, values_from = count)
View(age_analysis_Likert_V_counts_wide)

age_analysis_Likert_V_counts_wide <- age_analysis_Likert_V_counts_wide[, c(1,2,4,5,6,7,3)]

write_xlsx(age_analysis_Likert_V_counts_wide, "age_analysis_Likert_V_counts.xlsx")

 # GENDER ANALYSIS Likert V ----

gender_analysis_data_edited_Likert_V <- data_edited_gender_analysis[c("response_ID",
                                                                       "languages",
                                                                       
                                                                       "best_communication_contact_method",
                                                                       "best_emergency_contact_method",
                                                                       "tribal_affiliation",
                                                                       "religion",
                                                                       "gender_identity",
                                                                       "is_LGBTQ",
                                                                       "relationship_status",
                                                                       "is_partner_deaf",
                                                                       "employment_status",
                                                                       "job_description",
                                                                       "monthly_income",
                                                                       "number_of_jobs_held",
                                                                       "number_of_workdays_per_week",
                                                                       "education_level",
                                                                       "yearly_housing_expense",
                                                                       "is_homeowner",
                                                                       "age_of_hearing_loss_onset",
                                                                       "hearing_loss_support_needs",
                                                                       "age_when_learned_sign_language",
                                                                       "where_learned_sign_language",
                                                                       "age_first_got_hearing_aid",
                                                                       "age_of_cochlear_implant_surgery",
                                                                       "deaf_community_support_services_in_area",
                                                                       "three_most_significant_challenges_to_deaf",
                                                                       "communication_access_cost_responsibility",
                                                                       "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                       "communication_with_family_method",
                                                                       
                                                                      "energy_level",
                                                                      "bodily_appearance_acceptance_level",
                                                                      "info_availability_level",
                                                                      "financial_security_level",
                                                                      "access_to_hearing_aid_batteries_or_repair",
                                                                      "access_to_speech_or_auditory_therapy",
                                                                      "can_communicate_with_doctor",
                                                                      "interpreters_qualified_and_clear",
                                                                      "trusts_interpreters",
                                                                      "confidence_in_info_access_in_disaster_disease_crisis")]

gender_analysis_data_edited_Likert_V_long <- pivot_longer(gender_analysis_data_edited_Likert_V, cols = c("energy_level",
                                                                                                           "bodily_appearance_acceptance_level",
                                                                                                           "info_availability_level",
                                                                                                           "financial_security_level",
                                                                                                           "access_to_hearing_aid_batteries_or_repair",
                                                                                                           "access_to_speech_or_auditory_therapy",
                                                                                                           "can_communicate_with_doctor",
                                                                                                           "interpreters_qualified_and_clear",
                                                                                                           "trusts_interpreters",
                                                                                                           "confidence_in_info_access_in_disaster_disease_crisis"),
                                                           names_to='question_name',
                                                           values_to='value')

write_xlsx(gender_analysis_data_edited_Likert_V_long, "gender_analysis_data_edited_Likert_V_long.xlsx")

gender_analysis_Likert_V_counts <- gender_analysis_data_edited_Likert_V_long %>%
  mutate(value = gsub("[^[:print:]]", " ", value)) %>%  # Remove non-printable characters
  group_by(question_name, value, gender_identity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Not at all", "Slightly", "Somewhat", "To a great extent", "Completely"))

gender_analysis_Likert_V_counts_wide <- pivot_wider(gender_analysis_Likert_V_counts, names_from = value, values_from = count)
View(gender_analysis_Likert_V_counts_wide)

gender_analysis_Likert_V_counts_wide <- gender_analysis_Likert_V_counts_wide[, c(1,2,4,5,6,7,3)]

write_xlsx(gender_analysis_Likert_V_counts_wide, "gender_analysis_Likert_V_counts_wide.xlsx")


## LIKERT VI: Not at all - A small amount - A moderate amount - A great deal - An extreme amount ----

 # ALL ----

data_edited_Likert_VI <- data_edited[c("response_ID",
                                      "languages",
                                      "DOB",
                                      "best_communication_contact_method",
                                      "best_emergency_contact_method",
                                      "tribal_affiliation",
                                      "religion",
                                      "gender_identity",
                                      "is_LGBTQ",
                                      "relationship_status",
                                      "is_partner_deaf",
                                      "employment_status",
                                      "job_description",
                                      "monthly_income",
                                      "number_of_jobs_held",
                                      "number_of_workdays_per_week",
                                      "education_level",
                                      "yearly_housing_expense",
                                      "is_homeowner",
                                      "age_of_hearing_loss_onset",
                                      "hearing_loss_support_needs",
                                      "age_when_learned_sign_language",
                                      "where_learned_sign_language",
                                      "age_first_got_hearing_aid",
                                      "age_of_cochlear_implant_surgery",
                                      "deaf_community_support_services_in_area",
                                      "three_most_significant_challenges_to_deaf",
                                      "communication_access_cost_responsibility",
                                      "where_to_locate_info_access_in_disaster_disease_crisis",
                                      "communication_with_family_method",
                                      
                                      "need_for_medical_treatment_in_daily_life",
                                      "life_enjoyment_level",
                                      "is_life_meaningful")]

data_edited_Likert_VI_long <- pivot_longer(data_edited_Likert_VI, cols = c( "need_for_medical_treatment_in_daily_life",
                                                                            "life_enjoyment_level",
                                                                            "is_life_meaningful"),
                                          names_to='question_name',
                                          values_to='value')

write_xlsx(data_edited_Likert_VI_long, "data_edited_Likert_VI_long.xlsx")

Likert_VI_counts <- data_edited_Likert_VI_long %>%
  group_by(question_name, value) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Not at all", "A small amount", "A moderate amount", "A great deal", "An extreme amount"))

Likert_VI_counts_wide <- pivot_wider(Likert_VI_counts, names_from = value, values_from = count)

Likert_VI_counts_wide <- Likert_VI_counts_wide[, c(1,6,4,3,2,5)]

write_xlsx(Likert_VI_counts_wide, "Likert_VI_counts.xlsx")
                                      

 # AGE ANALYSIS Likert VI ----
age_analysis_data_edited_Likert_VI <- data_edited_age_analysis[c("response_ID",
                                                                "languages",
                                                                "DOB",
                                                                "age",
                                                                "age_group",
                                                                "best_communication_contact_method",
                                                                "best_emergency_contact_method",
                                                                "tribal_affiliation",
                                                                "religion",
                                                                "gender_identity",
                                                                "is_LGBTQ",
                                                                "relationship_status",
                                                                "is_partner_deaf",
                                                                "employment_status",
                                                                "job_description",
                                                                "monthly_income",
                                                                "number_of_jobs_held",
                                                                "number_of_workdays_per_week",
                                                                "education_level",
                                                                "yearly_housing_expense",
                                                                "is_homeowner",
                                                                "age_of_hearing_loss_onset",
                                                                "hearing_loss_support_needs",
                                                                "age_when_learned_sign_language",
                                                                "where_learned_sign_language",
                                                                "age_first_got_hearing_aid",
                                                                "age_of_cochlear_implant_surgery",
                                                                "deaf_community_support_services_in_area",
                                                                "three_most_significant_challenges_to_deaf",
                                                                "communication_access_cost_responsibility",
                                                                "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                "communication_with_family_method",
                                                                
                                                                "need_for_medical_treatment_in_daily_life",
                                                                "life_enjoyment_level",
                                                                "is_life_meaningful")]

age_analysis_data_edited_Likert_VI_long <- pivot_longer(age_analysis_data_edited_Likert_VI, cols = c( "need_for_medical_treatment_in_daily_life",
                                                                                                    "life_enjoyment_level",
                                                                                                    "is_life_meaningful"),
                                                       
                                                       names_to='question_name',
                                                       values_to='value')

write_xlsx(age_analysis_data_edited_Likert_VI_long, "age_analysis_data_edited_Likert_VI_long.xlsx")

age_analysis_Likert_VI_counts <- age_analysis_data_edited_Likert_VI_long %>%
  group_by(question_name, value, age_group) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Not at all", "A small amount", "A moderate amount", "A great deal", "An extreme amount"))

age_analysis_Likert_VI_counts_wide <- pivot_wider(age_analysis_Likert_VI_counts, names_from = value, values_from = count)
View(age_analysis_Likert_VI_counts_wide)

age_analysis_Likert_VI_counts_wide <- age_analysis_Likert_VI_counts_wide[, c(1,2,7,5,4,3,6)]

write_xlsx(age_analysis_Likert_VI_counts_wide, "age_analysis_Likert_VI_counts.xlsx")

 # GENDER ANALYSIS Likert VI ----

gender_analysis_data_edited_Likert_VI <- data_edited_gender_analysis[c("response_ID",
                                                                      "languages",
                                                                      
                                                                      "best_communication_contact_method",
                                                                      "best_emergency_contact_method",
                                                                      "tribal_affiliation",
                                                                      "religion",
                                                                      "gender_identity",
                                                                      "is_LGBTQ",
                                                                      "relationship_status",
                                                                      "is_partner_deaf",
                                                                      "employment_status",
                                                                      "job_description",
                                                                      "monthly_income",
                                                                      "number_of_jobs_held",
                                                                      "number_of_workdays_per_week",
                                                                      "education_level",
                                                                      "yearly_housing_expense",
                                                                      "is_homeowner",
                                                                      "age_of_hearing_loss_onset",
                                                                      "hearing_loss_support_needs",
                                                                      "age_when_learned_sign_language",
                                                                      "where_learned_sign_language",
                                                                      "age_first_got_hearing_aid",
                                                                      "age_of_cochlear_implant_surgery",
                                                                      "deaf_community_support_services_in_area",
                                                                      "three_most_significant_challenges_to_deaf",
                                                                      "communication_access_cost_responsibility",
                                                                      "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                      "communication_with_family_method",
                                                                      
                                                                      "need_for_medical_treatment_in_daily_life",
                                                                      "life_enjoyment_level",
                                                                      "is_life_meaningful")]

gender_analysis_data_edited_Likert_VI_long <- pivot_longer(gender_analysis_data_edited_Likert_VI, cols = c( "need_for_medical_treatment_in_daily_life",
                                                                                                          "life_enjoyment_level",
                                                                                                          "is_life_meaningful"),
                                                          names_to='question_name',
                                                          values_to='value')

write_xlsx(gender_analysis_data_edited_Likert_VI_long, "gender_analysis_data_edited_Likert_VI_long.xlsx")

gender_analysis_Likert_VI_counts <- gender_analysis_data_edited_Likert_VI_long %>%
  group_by(question_name, value, gender_identity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Not at all", "A small amount", "A moderate amount", "A great deal", "An extreme amount"))

gender_analysis_Likert_VI_counts_wide <- pivot_wider(gender_analysis_Likert_VI_counts, names_from = value, values_from = count)
View(gender_analysis_Likert_VI_counts_wide)

gender_analysis_Likert_VI_counts_wide <- gender_analysis_Likert_VI_counts_wide[, c(1,2,7,5,4,3,6)]

write_xlsx(gender_analysis_Likert_VI_counts_wide, "gender_analysis_Likert_VI_counts_wide.xlsx")


## LIKERT VII: Not at all - A little of the time - A fair amount of the time - All of the time ----

 # ALL ----
data_edited_Likert_VII <- data_edited[c("response_ID",
                                       "languages",
                                       "DOB",
                                       "best_communication_contact_method",
                                       "best_emergency_contact_method",
                                       "tribal_affiliation",
                                       "religion",
                                       "gender_identity",
                                       "is_LGBTQ",
                                       "relationship_status",
                                       "is_partner_deaf",
                                       "employment_status",
                                       "job_description",
                                       "monthly_income",
                                       "number_of_jobs_held",
                                       "number_of_workdays_per_week",
                                       "education_level",
                                       "yearly_housing_expense",
                                       "is_homeowner",
                                       "age_of_hearing_loss_onset",
                                       "hearing_loss_support_needs",
                                       "age_when_learned_sign_language",
                                       "where_learned_sign_language",
                                       "age_first_got_hearing_aid",
                                       "age_of_cochlear_implant_surgery",
                                       "deaf_community_support_services_in_area",
                                       "three_most_significant_challenges_to_deaf",
                                       "communication_access_cost_responsibility",
                                       "where_to_locate_info_access_in_disaster_disease_crisis",
                                       "communication_with_family_method",
                                       
                                       "discrimination_frequency")]

data_edited_Likert_VII_long <- pivot_longer(data_edited_Likert_VII, cols = c("discrimination_frequency"),
                                           names_to='question_name',
                                           values_to='value')

write_xlsx(data_edited_Likert_VII_long, "data_edited_Likert_VII_long.xlsx")

Likert_VII_counts <- data_edited_Likert_VII_long %>%
  group_by(question_name, value) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Not at all", "A little of the time", "A fair amount of the time", "All of the time"))

Likert_VII_counts_wide <- pivot_wider(Likert_VII_counts, names_from = value, values_from = count)

Likert_VII_counts_wide <- Likert_VII_counts_wide[, c(1,5,3,2,4)]

write_xlsx(Likert_VII_counts_wide, "Likert_VII_counts.xlsx")


 # AGE ANALYSIS Likert VII ----
age_analysis_data_edited_Likert_VII <- data_edited_age_analysis[c("response_ID",
                                                                 "languages",
                                                                 "DOB",
                                                                 "age",
                                                                 "age_group",
                                                                 "best_communication_contact_method",
                                                                 "best_emergency_contact_method",
                                                                 "tribal_affiliation",
                                                                 "religion",
                                                                 "gender_identity",
                                                                 "is_LGBTQ",
                                                                 "relationship_status",
                                                                 "is_partner_deaf",
                                                                 "employment_status",
                                                                 "job_description",
                                                                 "monthly_income",
                                                                 "number_of_jobs_held",
                                                                 "number_of_workdays_per_week",
                                                                 "education_level",
                                                                 "yearly_housing_expense",
                                                                 "is_homeowner",
                                                                 "age_of_hearing_loss_onset",
                                                                 "hearing_loss_support_needs",
                                                                 "age_when_learned_sign_language",
                                                                 "where_learned_sign_language",
                                                                 "age_first_got_hearing_aid",
                                                                 "age_of_cochlear_implant_surgery",
                                                                 "deaf_community_support_services_in_area",
                                                                 "three_most_significant_challenges_to_deaf",
                                                                 "communication_access_cost_responsibility",
                                                                 "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                 "communication_with_family_method",
                                                                 
                                                                 "discrimination_frequency")]

age_analysis_data_edited_Likert_VII_long <- pivot_longer(age_analysis_data_edited_Likert_VII, cols = c( "discrimination_frequency"),
                                                        
                                                        names_to='question_name',
                                                        values_to='value')

write_xlsx(age_analysis_data_edited_Likert_VII_long, "age_analysis_data_edited_Likert_VII_long.xlsx")

age_analysis_Likert_VII_counts <- age_analysis_data_edited_Likert_VII_long %>%
  group_by(question_name, value, age_group) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Not at all", "A little of the time", "A fair amount of the time", "All of the time"))

age_analysis_Likert_VII_counts_wide <- pivot_wider(age_analysis_Likert_VII_counts, names_from = value, values_from = count)
View(age_analysis_Likert_VII_counts_wide)

age_analysis_Likert_VII_counts_wide <- age_analysis_Likert_VII_counts_wide[, c(1,2,7,5,4,3,6)]

write_xlsx(age_analysis_Likert_VII_counts_wide, "age_analysis_Likert_VII_counts.xlsx")

 # GENDER ANALYSIS Likert VII ----

gender_analysis_data_edited_Likert_VII <- data_edited_gender_analysis[c("response_ID",
                                                                       "languages",
                                                                       
                                                                       "best_communication_contact_method",
                                                                       "best_emergency_contact_method",
                                                                       "tribal_affiliation",
                                                                       "religion",
                                                                       "gender_identity",
                                                                       "is_LGBTQ",
                                                                       "relationship_status",
                                                                       "is_partner_deaf",
                                                                       "employment_status",
                                                                       "job_description",
                                                                       "monthly_income",
                                                                       "number_of_jobs_held",
                                                                       "number_of_workdays_per_week",
                                                                       "education_level",
                                                                       "yearly_housing_expense",
                                                                       "is_homeowner",
                                                                       "age_of_hearing_loss_onset",
                                                                       "hearing_loss_support_needs",
                                                                       "age_when_learned_sign_language",
                                                                       "where_learned_sign_language",
                                                                       "age_first_got_hearing_aid",
                                                                       "age_of_cochlear_implant_surgery",
                                                                       "deaf_community_support_services_in_area",
                                                                       "three_most_significant_challenges_to_deaf",
                                                                       "communication_access_cost_responsibility",
                                                                       "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                       "communication_with_family_method",
                                                                       
                                                                       "discrimination_frequency")]

gender_analysis_data_edited_Likert_VII_long <- pivot_longer(gender_analysis_data_edited_Likert_VII, cols = c("discrimination_frequency"),
                                                           names_to='question_name',
                                                           values_to='value')

write_xlsx(gender_analysis_data_edited_Likert_VII_long, "gender_analysis_data_edited_Likert_VII_long.xlsx")

gender_analysis_Likert_VII_counts <- gender_analysis_data_edited_Likert_VII_long %>%
  group_by(question_name, value, gender_identity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Not at all", "A little of the time", "A fair amount of the time", "All of the time"))

gender_analysis_Likert_VII_counts_wide <- pivot_wider(gender_analysis_Likert_VII_counts, names_from = value, values_from = count)
View(gender_analysis_Likert_VII_counts_wide)

gender_analysis_Likert_VII_counts_wide <- gender_analysis_Likert_VII_counts_wide[, c(1,2,6,4,3,5)]

write_xlsx(gender_analysis_Likert_VII_counts_wide, "gender_analysis_Likert_VII_counts_wide.xlsx")



## LIKERT VIII: Not at all - Slightly - Moderately - Very - Extremely ----

 # ALL ----
data_edited_Likert_VIII <- data_edited[c("response_ID",
                                       "languages",
                                       "DOB",
                                       "best_communication_contact_method",
                                       "best_emergency_contact_method",
                                       "tribal_affiliation",
                                       "religion",
                                       "gender_identity",
                                       "is_LGBTQ",
                                       "relationship_status",
                                       "is_partner_deaf",
                                       "employment_status",
                                       "job_description",
                                       "monthly_income",
                                       "number_of_jobs_held",
                                       "number_of_workdays_per_week",
                                       "education_level",
                                       "yearly_housing_expense",
                                       "is_homeowner",
                                       "age_of_hearing_loss_onset",
                                       "hearing_loss_support_needs",
                                       "age_when_learned_sign_language",
                                       "where_learned_sign_language",
                                       "age_first_got_hearing_aid",
                                       "age_of_cochlear_implant_surgery",
                                       "deaf_community_support_services_in_area",
                                       "three_most_significant_challenges_to_deaf",
                                       "communication_access_cost_responsibility",
                                       "where_to_locate_info_access_in_disaster_disease_crisis",
                                       "communication_with_family_method",
                                       
                                       "safety_satisfaction_level",
                                       "physical_environment_health_level")]

data_edited_Likert_VIII_long <- pivot_longer(data_edited_Likert_VIII, cols = c("safety_satisfaction_level",
                                                                               "physical_environment_health_level"),
                                            names_to='question_name',
                                            values_to='value')

write_xlsx(data_edited_Likert_VIII_long, "data_edited_Likert_VIII_long.xlsx")

Likert_VIII_counts <- data_edited_Likert_VIII_long %>%
  group_by(question_name, value) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Not at all", "Slightly", "Moderately", "Very", "Extremely"))

Likert_VIII_counts_wide <- pivot_wider(Likert_VIII_counts, names_from = value, values_from = count)

Likert_VIII_counts_wide <- Likert_VIII_counts_wide[, c(1,4,5,3,6,2)]

write_xlsx(Likert_VIII_counts_wide, "Likert_VIII_counts.xlsx")

 # AGE ANALYSIS Likert VIII ----
age_analysis_data_edited_Likert_VIII <- data_edited_age_analysis[c("response_ID",
                                                                  "languages",
                                                                  "DOB",
                                                                  "age",
                                                                  "age_group",
                                                                  "best_communication_contact_method",
                                                                  "best_emergency_contact_method",
                                                                  "tribal_affiliation",
                                                                  "religion",
                                                                  "gender_identity",
                                                                  "is_LGBTQ",
                                                                  "relationship_status",
                                                                  "is_partner_deaf",
                                                                  "employment_status",
                                                                  "job_description",
                                                                  "monthly_income",
                                                                  "number_of_jobs_held",
                                                                  "number_of_workdays_per_week",
                                                                  "education_level",
                                                                  "yearly_housing_expense",
                                                                  "is_homeowner",
                                                                  "age_of_hearing_loss_onset",
                                                                  "hearing_loss_support_needs",
                                                                  "age_when_learned_sign_language",
                                                                  "where_learned_sign_language",
                                                                  "age_first_got_hearing_aid",
                                                                  "age_of_cochlear_implant_surgery",
                                                                  "deaf_community_support_services_in_area",
                                                                  "three_most_significant_challenges_to_deaf",
                                                                  "communication_access_cost_responsibility",
                                                                  "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                  "communication_with_family_method",
                                                                  
                                                                  "safety_satisfaction_level",
                                                                  "physical_environment_health_level")]

age_analysis_data_edited_Likert_VIII_long <- pivot_longer(age_analysis_data_edited_Likert_VIII, cols = c("safety_satisfaction_level",
                                                                                                       "physical_environment_health_level"),
                                                         
                                                         names_to='question_name',
                                                         values_to='value')

write_xlsx(age_analysis_data_edited_Likert_VIII_long, "age_analysis_data_edited_Likert_VIII_long.xlsx")

age_analysis_Likert_VIII_counts <- age_analysis_data_edited_Likert_VIII_long %>%
  group_by(question_name, value, age_group) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Not at all", "Slightly", "Moderately", "Very", "Extremely"))

age_analysis_Likert_VIII_counts_wide <- pivot_wider(age_analysis_Likert_VIII_counts, names_from = value, values_from = count)
View(age_analysis_Likert_VIII_counts_wide)

age_analysis_Likert_VIII_counts_wide <- age_analysis_Likert_VIII_counts_wide[, c(1,2,5,6,4,7,3)]

write_xlsx(age_analysis_Likert_VIII_counts_wide, "age_analysis_Likert_VIII_counts.xlsx")

 # GENDER ANALYSIS Likert VIII ----

gender_analysis_data_edited_Likert_VIII <- data_edited_gender_analysis[c("response_ID",
                                                                       "languages",
                                                                       
                                                                       "best_communication_contact_method",
                                                                       "best_emergency_contact_method",
                                                                       "tribal_affiliation",
                                                                       "religion",
                                                                       "gender_identity",
                                                                       "is_LGBTQ",
                                                                       "relationship_status",
                                                                       "is_partner_deaf",
                                                                       "employment_status",
                                                                       "job_description",
                                                                       "monthly_income",
                                                                       "number_of_jobs_held",
                                                                       "number_of_workdays_per_week",
                                                                       "education_level",
                                                                       "yearly_housing_expense",
                                                                       "is_homeowner",
                                                                       "age_of_hearing_loss_onset",
                                                                       "hearing_loss_support_needs",
                                                                       "age_when_learned_sign_language",
                                                                       "where_learned_sign_language",
                                                                       "age_first_got_hearing_aid",
                                                                       "age_of_cochlear_implant_surgery",
                                                                       "deaf_community_support_services_in_area",
                                                                       "three_most_significant_challenges_to_deaf",
                                                                       "communication_access_cost_responsibility",
                                                                       "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                       "communication_with_family_method",
                                                                       
                                                                       "safety_satisfaction_level",
                                                                       "physical_environment_health_level")]

gender_analysis_data_edited_Likert_VIII_long <- pivot_longer(gender_analysis_data_edited_Likert_VIII, cols = c("safety_satisfaction_level",
                                                                                                            "physical_environment_health_level"),
                                                           names_to='question_name',
                                                           values_to='value')

write_xlsx(gender_analysis_data_edited_Likert_VIII_long, "gender_analysis_data_edited_Likert_VIII_long.xlsx")

gender_analysis_Likert_VIII_counts <- gender_analysis_data_edited_Likert_VIII_long %>%
  group_by(question_name, value, gender_identity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Not at all", "Slightly", "Moderately", "Very", "Extremely"))

gender_analysis_Likert_VIII_counts_wide <- pivot_wider(gender_analysis_Likert_VIII_counts, names_from = value, values_from = count)
View(gender_analysis_Likert_VIII_counts_wide)

gender_analysis_Likert_VIII_counts_wide <- gender_analysis_Likert_VIII_counts_wide[, c(1,2,5,6,4,7,3)]

write_xlsx(gender_analysis_Likert_VIII_counts_wide, "gender_analysis_Likert_VIII_counts_wide.xlsx")



## LIKERT IX: Never - Once - A few times - Many times ----

 # ALL ----
data_edited_Likert_IX <- data_edited[c("response_ID",
                                       "languages",
                                       "DOB",
                                       "best_communication_contact_method",
                                       "best_emergency_contact_method",
                                       "tribal_affiliation",
                                       "religion",
                                       "gender_identity",
                                       "is_LGBTQ",
                                       "relationship_status",
                                       "is_partner_deaf",
                                       "employment_status",
                                       "job_description",
                                       "monthly_income",
                                       "number_of_jobs_held",
                                       "number_of_workdays_per_week",
                                       "education_level",
                                       "yearly_housing_expense",
                                       "is_homeowner",
                                       "age_of_hearing_loss_onset",
                                       "hearing_loss_support_needs",
                                       "age_when_learned_sign_language",
                                       "where_learned_sign_language",
                                       "age_first_got_hearing_aid",
                                       "age_of_cochlear_implant_surgery",
                                       "deaf_community_support_services_in_area",
                                       "three_most_significant_challenges_to_deaf",
                                       "communication_access_cost_responsibility",
                                       "where_to_locate_info_access_in_disaster_disease_crisis",
                                       "communication_with_family_method",
                                       
                                       "witnessed_stabbing_or_shooting",
                                       "has_home_been_deliberately_destroyed",
                                       "beaten_by_police_or_other_outside_home")]

data_edited_Likert_IX_long <- pivot_longer(data_edited_Likert_IX, cols = c("witnessed_stabbing_or_shooting",
                                                                           "has_home_been_deliberately_destroyed",
                                                                           "beaten_by_police_or_other_outside_home"),
                                             names_to='question_name',
                                             values_to='value')

write_xlsx(data_edited_Likert_IX_long, "data_edited_Likert_IX_long.xlsx")

Likert_IX_counts <- data_edited_Likert_IX_long %>%
  group_by(question_name, value) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Never", "Once", "A few times", "Many times"))

Likert_IX_counts_wide <- pivot_wider(Likert_IX_counts, names_from = value, values_from = count)

Likert_IX_counts_wide <- Likert_IX_counts_wide[, c(1,4,5,2,3)]

write_xlsx(Likert_IX_counts_wide, "Likert_IX_counts.xlsx")


 # AGE ANALYSIS Likert IX ----
age_analysis_data_edited_Likert_IX <- data_edited_age_analysis[c("response_ID",
                                                                   "languages",
                                                                   "DOB",
                                                                   "age",
                                                                   "age_group",
                                                                   "best_communication_contact_method",
                                                                   "best_emergency_contact_method",
                                                                   "tribal_affiliation",
                                                                   "religion",
                                                                   "gender_identity",
                                                                   "is_LGBTQ",
                                                                   "relationship_status",
                                                                   "is_partner_deaf",
                                                                   "employment_status",
                                                                   "job_description",
                                                                   "monthly_income",
                                                                   "number_of_jobs_held",
                                                                   "number_of_workdays_per_week",
                                                                   "education_level",
                                                                   "yearly_housing_expense",
                                                                   "is_homeowner",
                                                                   "age_of_hearing_loss_onset",
                                                                   "hearing_loss_support_needs",
                                                                   "age_when_learned_sign_language",
                                                                   "where_learned_sign_language",
                                                                   "age_first_got_hearing_aid",
                                                                   "age_of_cochlear_implant_surgery",
                                                                   "deaf_community_support_services_in_area",
                                                                   "three_most_significant_challenges_to_deaf",
                                                                   "communication_access_cost_responsibility",
                                                                   "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                   "communication_with_family_method",
                                                                   
                                                                 "witnessed_stabbing_or_shooting",
                                                                 "has_home_been_deliberately_destroyed",
                                                                 "beaten_by_police_or_other_outside_home")]

age_analysis_data_edited_Likert_IX_long <- pivot_longer(age_analysis_data_edited_Likert_IX, cols = c("witnessed_stabbing_or_shooting",
                                                                                                     "has_home_been_deliberately_destroyed",
                                                                                                     "beaten_by_police_or_other_outside_home"),
                                                          
                                                          names_to='question_name',
                                                          values_to='value')

write_xlsx(age_analysis_data_edited_Likert_IX_long, "age_analysis_data_edited_Likert_IX_long.xlsx")

age_analysis_Likert_IX_counts <- age_analysis_data_edited_Likert_IX_long %>%
  group_by(question_name, value, age_group) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Never", "Once", "A few times", "Many times"))

age_analysis_Likert_IX_counts_wide <- pivot_wider(age_analysis_Likert_IX_counts, names_from = value, values_from = count)
View(age_analysis_Likert_IX_counts_wide)

age_analysis_Likert_IX_counts_wide <- age_analysis_Likert_IX_counts_wide[, c(1,2,5,6,3,4)]

write_xlsx(age_analysis_Likert_IX_counts_wide, "age_analysis_Likert_IX_counts.xlsx")



 # GENDER ANALYSIS Likert IX ----

gender_analysis_data_edited_Likert_IX <- data_edited_gender_analysis[c("response_ID",
                                                                         "languages",
                                                                         
                                                                         "best_communication_contact_method",
                                                                         "best_emergency_contact_method",
                                                                         "tribal_affiliation",
                                                                         "religion",
                                                                         "gender_identity",
                                                                         "is_LGBTQ",
                                                                         "relationship_status",
                                                                         "is_partner_deaf",
                                                                         "employment_status",
                                                                         "job_description",
                                                                         "monthly_income",
                                                                         "number_of_jobs_held",
                                                                         "number_of_workdays_per_week",
                                                                         "education_level",
                                                                         "yearly_housing_expense",
                                                                         "is_homeowner",
                                                                         "age_of_hearing_loss_onset",
                                                                         "hearing_loss_support_needs",
                                                                         "age_when_learned_sign_language",
                                                                         "where_learned_sign_language",
                                                                         "age_first_got_hearing_aid",
                                                                         "age_of_cochlear_implant_surgery",
                                                                         "deaf_community_support_services_in_area",
                                                                         "three_most_significant_challenges_to_deaf",
                                                                         "communication_access_cost_responsibility",
                                                                         "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                         "communication_with_family_method",
                                                                         
                                                                       "witnessed_stabbing_or_shooting",
                                                                       "has_home_been_deliberately_destroyed",
                                                                       "beaten_by_police_or_other_outside_home")]

gender_analysis_data_edited_Likert_IX_long <- pivot_longer(gender_analysis_data_edited_Likert_IX, cols = c("witnessed_stabbing_or_shooting",
                                                                                                               "has_home_been_deliberately_destroyed",
                                                                                                               "beaten_by_police_or_other_outside_home"),
                                                             names_to='question_name',
                                                             values_to='value')

write_xlsx(gender_analysis_data_edited_Likert_IX_long, "gender_analysis_data_edited_Likert_IX_long.xlsx")

gender_analysis_Likert_IX_counts <- gender_analysis_data_edited_Likert_IX_long %>%
  group_by(question_name, value, gender_identity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Never", "Once", "A few times", "Many times"))

gender_analysis_Likert_IX_counts_wide <- pivot_wider(gender_analysis_Likert_IX_counts, names_from = value, values_from = count)
View(gender_analysis_Likert_IX_counts_wide)

gender_analysis_Likert_IX_counts_wide <- gender_analysis_Likert_IX_counts_wide[, c(1,2,5,6,3,4)]

write_xlsx(gender_analysis_Likert_IX_counts_wide, "gender_analysis_Likert_IX_counts_wide.xlsx")

## LIKERT X: Very Unsatisfied - Unsatisfied - Neutral - Satisfied - Very Satisfied ----

 # ALL ----
data_edited_Likert_X <- data_edited[c("response_ID",
                                       "languages",
                                       "DOB",
                                       "best_communication_contact_method",
                                       "best_emergency_contact_method",
                                       "tribal_affiliation",
                                       "religion",
                                       "gender_identity",
                                       "is_LGBTQ",
                                       "relationship_status",
                                       "is_partner_deaf",
                                       "employment_status",
                                       "job_description",
                                       "monthly_income",
                                       "number_of_jobs_held",
                                       "number_of_workdays_per_week",
                                       "education_level",
                                       "yearly_housing_expense",
                                       "is_homeowner",
                                      "age_of_hearing_loss_onset",
                                      "hearing_loss_support_needs",
                                      "age_when_learned_sign_language",
                                      "where_learned_sign_language",
                                      "age_first_got_hearing_aid",
                                      "age_of_cochlear_implant_surgery",
                                      "deaf_community_support_services_in_area",
                                      "three_most_significant_challenges_to_deaf",
                                      "communication_access_cost_responsibility",
                                      "where_to_locate_info_access_in_disaster_disease_crisis",
                                      "communication_with_family_method",
                                      
                                      "can_get_communication_access_at_work")]
                                      
data_edited_Likert_X_long <- pivot_longer(data_edited_Likert_X, cols = c("can_get_communication_access_at_work"),
                                           names_to='question_name',
                                           values_to='value')      

write_xlsx(data_edited_Likert_X_long, "data_edited_Likert_X_long.xlsx")

Likert_X_counts <- data_edited_Likert_X_long %>%
  group_by(question_name, value) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Very Unsatisfied", "Unsatisfied", "Neutral", "Satisfied", "Very Satisfied"))

Likert_X_counts_wide <- pivot_wider(Likert_X_counts, names_from = value, values_from = count)

Likert_X_counts_wide <- Likert_X_counts_wide[, c(1,6,4,2,3,5)]

write_xlsx(Likert_X_counts_wide, "Likert_X_counts.xlsx")


 # AGE ANALYSIS Likert X ----
age_analysis_data_edited_Likert_X <- data_edited_age_analysis[c("response_ID",
                                                                 "languages",
                                                                 "DOB",
                                                                 "age",
                                                                 "age_group",
                                                                 "best_communication_contact_method",
                                                                 "best_emergency_contact_method",
                                                                 "tribal_affiliation",
                                                                 "religion",
                                                                 "gender_identity",
                                                                 "is_LGBTQ",
                                                                 "relationship_status",
                                                                 "is_partner_deaf",
                                                                 "employment_status",
                                                                 "job_description",
                                                                 "monthly_income",
                                                                 "number_of_jobs_held",
                                                                 "number_of_workdays_per_week",
                                                                 "education_level",
                                                                 "yearly_housing_expense",
                                                                 "is_homeowner",
                                                                 "age_of_hearing_loss_onset",
                                                                 "hearing_loss_support_needs",
                                                                 "age_when_learned_sign_language",
                                                                 "where_learned_sign_language",
                                                                 "age_first_got_hearing_aid",
                                                                 "age_of_cochlear_implant_surgery",
                                                                 "deaf_community_support_services_in_area",
                                                                 "three_most_significant_challenges_to_deaf",
                                                                 "communication_access_cost_responsibility",
                                                                 "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                 "communication_with_family_method",
                                                                 
                                                                "can_get_communication_access_at_work")]

age_analysis_data_edited_Likert_X_long <- pivot_longer(age_analysis_data_edited_Likert_X, cols = c("can_get_communication_access_at_work"),
                                                        
                                                        names_to='question_name',
                                                        values_to='value')

write_xlsx(age_analysis_data_edited_Likert_X_long, "age_analysis_data_edited_Likert_X_long.xlsx")

age_analysis_Likert_X_counts <- age_analysis_data_edited_Likert_X_long %>%
  group_by(question_name, value, age_group) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Very Unsatisfied", "Unsatisfied", "Neutral", "Satisfied", "Very Satisfied"))

age_analysis_Likert_X_counts_wide <- pivot_wider(age_analysis_Likert_X_counts, names_from = value, values_from = count)
View(age_analysis_Likert_X_counts_wide)

age_analysis_Likert_X_counts_wide <- age_analysis_Likert_X_counts_wide[, c(1,2,7,5,3,4,6)]

write_xlsx(age_analysis_Likert_X_counts_wide, "age_analysis_Likert_X_counts.xlsx")


 # GENDER ANALYSIS Likert X ----

gender_analysis_data_edited_Likert_X <- data_edited_gender_analysis[c("response_ID",
                                                                       "languages",
                                                                       
                                                                       "best_communication_contact_method",
                                                                       "best_emergency_contact_method",
                                                                       "tribal_affiliation",
                                                                       "religion",
                                                                       "gender_identity",
                                                                       "is_LGBTQ",
                                                                       "relationship_status",
                                                                       "is_partner_deaf",
                                                                       "employment_status",
                                                                       "job_description",
                                                                       "monthly_income",
                                                                       "number_of_jobs_held",
                                                                       "number_of_workdays_per_week",
                                                                       "education_level",
                                                                       "yearly_housing_expense",
                                                                       "is_homeowner",
                                                                       "age_of_hearing_loss_onset",
                                                                       "hearing_loss_support_needs",
                                                                       "age_when_learned_sign_language",
                                                                       "where_learned_sign_language",
                                                                       "age_first_got_hearing_aid",
                                                                       "age_of_cochlear_implant_surgery",
                                                                       "deaf_community_support_services_in_area",
                                                                       "three_most_significant_challenges_to_deaf",
                                                                       "communication_access_cost_responsibility",
                                                                       "where_to_locate_info_access_in_disaster_disease_crisis",
                                                                       "communication_with_family_method",
                                                                       
                                                                      "can_get_communication_access_at_work")]

gender_analysis_data_edited_Likert_X_long <- pivot_longer(gender_analysis_data_edited_Likert_X, cols = c("can_get_communication_access_at_work"),
                                                           names_to='question_name',
                                                           values_to='value')

write_xlsx(gender_analysis_data_edited_Likert_X_long, "gender_analysis_data_edited_Likert_X_long.xlsx")

gender_analysis_Likert_X_counts <- gender_analysis_data_edited_Likert_X_long %>%
  group_by(question_name, value, gender_identity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(value %in% c("Very Unsatisfied", "Unsatisfied", "Neutral", "Satisfied", "Very Satisfied"))

gender_analysis_Likert_X_counts_wide <- pivot_wider(gender_analysis_Likert_X_counts, names_from = value, values_from = count)
View(gender_analysis_Likert_X_counts_wide)

gender_analysis_Likert_X_counts_wide <- gender_analysis_Likert_X_counts_wide[, c(1,2,7,5,3,4,6)]

write_xlsx(gender_analysis_Likert_X_counts_wide, "gender_analysis_Likert_X_counts_wide.xlsx")




## Why did women respond to Likert questions less than men?

Likert_I_gender_analysis_DNRs <- read_excel("gender_analysis_data_edited_Likert_I_long.xlsx")
View(Likert_I_gender_analysis_DNRs)

Likert_I_gender_analysis_DNRs <- Likert_I_gender_analysis_DNRs %>%
  filter(value == "Did not respond")

Likert_I_gender_analysis_DNRs_wide <- pivot_wider(Likert_I_gender_analysis_DNRs, names_from = question_name, values_from = value)
View(Likert_I_gender_analysis_DNRs_wide)

write_xlsx(Likert_I_gender_analysis_DNRs_wide, "Likert_I_gender_analysis_DNRs_wide.xlsx")



## COMMUNICATION TOP AND BOTTOM 2 ----

data_communication <- read_excel("top_bottom_2_for_chart.xlsx", sheet = "communication")

data_communication <- pivot_longer(data_communication,
                                   cols = c("top_2_excl_dnr", "bottom_2_excl_dnr"),
                                   names_to = "top_or_bottom",
                                   values_to = "value")

write_xlsx(data_communication, "communication_long.xlsx")

## HEALTH TOP AND BOTTOM 2 ----

data_health <- read_excel("top_bottom_2_for_chart.xlsx", sheet = "health")

data_health <- pivot_longer(data_health,
                                   cols = c("top_2_excl_dnr", "bottom_2_excl_dnr"),
                                   names_to = "top_or_bottom",
                                   values_to = "value")

write_xlsx(data_health, "health_long.xlsx")

## COMMUNITY TOP AND BOTTOM 2 ----

data_community <- read_excel("top_bottom_2_for_chart.xlsx", sheet = "community")

data_community <- pivot_longer(data_community,
                            cols = c("top_2_excl_dnr", "bottom_2_excl_dnr"),
                            names_to = "top_or_bottom",
                            values_to = "value")

write_xlsx(data_community, "community_long.xlsx")

## COMMUNITY TOP AND BOTTOM 2 ----

data_basic_needs <- read_excel("top_bottom_2_for_chart.xlsx", sheet = "basic needs")

data_basic_needs <- pivot_longer(data_basic_needs,
                               cols = c("top_2_excl_dnr", "bottom_2_excl_dnr"),
                               names_to = "top_or_bottom",
                               values_to = "value")

write_xlsx(data_basic_needs, "basic_needs_long.xlsx")

## SAFETY TOP AND BOTTOM 2 ----

data_safety <- read_excel("top_bottom_2_for_chart.xlsx", sheet = "safety")

data_safety <- pivot_longer(data_safety,
                                 cols = c("top_2_excl_dnr", "bottom_2_excl_dnr"),
                                 names_to = "top_or_bottom",
                                 values_to = "value")

write_xlsx(data_safety, "safety_long.xlsx")


## ALL TOP AND BOTTOM 2 ----

data_ALL <- read_excel("top_bottom_2_for_chart.xlsx", sheet = "ALL")

data_ALL <- pivot_longer(data_ALL,
                            cols = c("top_2_excl_dnr", "bottom_2_excl_dnr"),
                            names_to = "top_or_bottom",
                            values_to = "value")

write_xlsx(data_ALL, "data_ALL.xlsx")
