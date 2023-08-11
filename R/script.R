
library(readxl)
library(tidyverse)

cbimsurvey <- read_excel("cbimsurvey.xlsx")
View(cbimsurvey)

questions <- names(cbimsurvey %>% select(`Respect Towards Women`:`Girl Raped Or Raped Because Didnot Clearly Say No`))

section_1_quiz_names <- names(cbimsurvey %>% select(`Respect Towards Women`:`Stopping Kids Do Harmful Things`))

section_2_quiz_names <- names(cbimsurvey %>% select(`Name Calling Or Insulting Them`:`Using Their Creditcard Cash Without Permission`))

section_3_quiz_names <- names(cbimsurvey %>% select(`Making Disrespective Comments About Girls Body`:`Pressure Girl Have Sex Intimate Without Consent`))

section_4_quiz_names <- names(cbimsurvey %>% select(`Boy Not Have Tofight For Respect`:`Girl Raped Or Raped Because Didnot Clearly Say No`))

section_5_quiz_names <- names(cbimsurvey %>% select(`Age Range`:`Self Description`))

pre_cbimsurvey <- cbimsurvey %>% filter(Category == "pre-programme")

post_cbimsurvey <- cbimsurvey %>% filter(Category == "post-programme")




choices_section_a <- data.frame(choice_code = 1:3, choice_label = c("Choice A", "Choice B", "Choice C"))
choices_section_b <- data.frame(choice_code = 1:5, choice_label = c("Choice 1", "Choice 2", "Choice 3", "Choice 4", "Choice 5"))
choices_section_c <- data.frame(choice_code = 1:5, choice_label = c("Option 1", "Option 2", "Option 3", "Option 4", "Option 5"))
choices_section_d <- data.frame(choice_code = 1:5, choice_label = c("Option A", "Option B", "Option C", "Option D", "Option E"))


data_merged <- cbimsurvey %>%
  left_join(cbimsurvey, choices_section_a, by = c(`"Respect Towards Women"` = "choice_code")) %>%
  left_join(choices_section_b, by = c("section_b_question1_choice" = "choice_code")) %>%
  left_join(choices_section_c, by = c("section_c_question1_choice" = "choice_code")) %>%
  left_join(choices_section_d, by = c("section_d"))


data_combined <- data_merged %>%
  group_by(participant_id) %>%
  summarize(
    section_a_combined = paste(section_a_question1_choice_label, section_a_question2_choice_label, sep = " | "),
    section_b_combined = paste(section_b_question1_choice_label, section_b_question2_choice_label, ..., sep = " | "),  # Continue for all section B questions
    section_c_combined = paste(section_c_question1_choice_label, section_c_question2_choice_label, ..., sep = " | ")  # Continue for all section C questions
  )


data_long <- data_combined %>%
  gather(section, combined_responses, starts_with("section_")) %>%
  separate(combined_responses, into = c("question", "responses"), sep = " \\| ", remove = FALSE)



library(recipes)
blueprint <- cbimsurvey %>% select(`Pims Id`, Category, `Respect Towards Women`:`Girl Raped Or Raped Because Didnot Clearly Say No`) %>%
  recipe(`Pims Id` ~ .) %>%
  step_dummy(all_nominal(), one_hot = TRUE)
cbimsurvey %>% select(`Pims Id`, Category, `Respect Towards Women`:`Girl Raped Or Raped Because Didnot Clearly Say No`) %>%
  prep(blueprint) %>% bake(new_data = data, keep_unused_levels = TRUE)

names <- cbimsurvey %>% select(`Respect Towards Women`:`Girl Raped Or Raped Because Didnot Clearly Say No`) %>%
  names()
data <- cbimsurvey %>% select(`Pims Id`, Category, `Respect Towards Women`:`Girl Raped Or Raped Because Didnot Clearly Say No`)
blue_print <- recipe(`Pims Id` ~ ., data = data) %>%
  step_dummy(all_nominal(), one_hot = TRUE)

data_transformed <- blue_print %>% prep(data) %>%
  bake(data)

cbimsurvey %>% filter(!is.na(`Respect Towards Women`)) %>% View()



library(recipes)
library(dplyr)

# Sample data with multiple category_encoded columns
data <- data.frame(
  participant_id = c(1, 2, 3, 4),
  col1_encoded = c(3, 1, 5, 2),
  col2_encoded = c(2, 4, 1, 3)
)

str_c("quiz_", names(cbimsurvey[, 12]))

data_renamed <- cbimsurvey %>%
  rename(
    "quiz_Respect Towards Women" = `Respect Towards Women`,
    "quiz_Respect Towards Women" = `Stopping Kids Do Harmful Things`,
    "quiz_Name Calling Or Insulting Them" = `Name Calling Or Insulting Them`,
    `Ugly Or Stupid`,
    `Making Fun Ofthem` = str_c("quiz_", names(`Making Fun Ofthem`)),
    `Telling Them What Todo` = str_c("quiz_", names(`Telling Them What Todo`)),
    `Telling Friends To See Or Talkto` = str_c("quiz_", names(`Telling Friends To See Or Talkto`)),
    `Pressuring Their Partners Not To Breakup` = str_c("quiz_", names(`Pressuring Their Partners Not To Breakup`)),
    `Not Listening Them` = str_c("quiz_", names(`Not Listening Them`)),
    `Convince Them To Have Sex` = str_c("quiz_", names(`Convince Them To Have Sex`)),
    `Preventing Them Leaving A Room` = str_c("quiz_", names(`Preventing Them Leaving A Room`)),
    `Distracting Spying Onthem` = str_c("quiz_", names(`Distracting Spying Onthem`)),
    `Being Physically Intimate Without Consent` = str_c("quiz_", names(`Being Physically Intimate Without Consent`)),
    `Constanly Contacting Them To Find Out There Whereabout` = str_c("quiz_", names(`Constanly Contacting Them To Find Out There Whereabout`)),
    `Threating To Hit Them` = str_c("quiz_", names(`Threating To Hit Them`)),
    `Forcing Them To Have Sex` = str_c("quiz_", names(`Forcing Them To Have Sex`)),
    `Pressuring Them Skip Their Activities` = str_c("quiz_", names(`Pressuring Them Skip Their Activities`)),
    `Constantly Asking Them To Pay For Activities Meal Othergifts` = str_c("quiz_", names(`Constantly Asking Them To Pay For Activities Meal Othergifts`)),
    `Constantly Interrupting Them At Work` = str_c("quiz_", names(`Constantly Interrupting Them At Work`)),
    `Using Their Creditcard Cash Without Permission` = str_c("quiz_", names(`Using Their Creditcard Cash Without Permission`)),
    `Making Disrespective Comments About Girls Body` = str_c("quiz_", names(`Making Disrespective Comments About Girls Body`)),
    `Spreading Rumors Abt Girls` = str_c("quiz_", names(`Spreading Rumors Abt Girls`)),
    `Fighting Or Threatening Agirl` = str_c("quiz_", names(`Fighting Or Threatening Agirl`)),
    `Doing Unwelcome Actions` = str_c("quiz_", names(`Doing Unwelcome Actions`)),
    `Pushing Grabbing Hittingor Hurting A Girl` = str_c("quiz_", names(`Pushing Grabbing Hittingor Hurting A Girl`)),
    `Showing People Sexual Content Of A Girl Without Herconsent` = str_c("quiz_", names(`Showing People Sexual Content Of A Girl Without Herconsent`)),
    `Telling Jokes That Disrespect Women` = str_c("quiz_", names(`Telling Jokes That Disrespect Women`)),
    `Taking Advantage Of Drunk Girl` = str_c("quiz_", names(`Taking Advantage Of Drunk Girl`)),
    `Pressure Girl Have Sex Intimate Without Consent` = str_c("quiz_", names(`Pressure Girl Have Sex Intimate Without Consent`)),
    `Boy Not Have Tofight For Respect` = str_c("quiz_", names(`Boy Not Have Tofight For Respect`)),
    `Girl Wearing Revealing Clothes Deserves Comment` = str_c("quiz_", names(`Girl Wearing Revealing Clothes Deserves Comment`)),
    `Bothered When Guy Acts Girlish` = str_c("quiz_", names(`Bothered When Guy Acts Girlish`)),
    `Guys With Lot Of Money Are Manlier` = str_c("quiz_", names(`Guys With Lot Of Money Are Manlier`)),
    `Guy Who Ask For Help Looks Weak` = str_c("quiz_", names(`Guy Who Ask For Help Looks Weak`)),
    `Guy Should Pay For Most Of Things Ina Good Dating` = str_c("quiz_", names(`Guy Should Pay For Most Of Things Ina Good Dating`)),
    `Guys Should Only Hookup Or Havesex With Girls` = str_c("quiz_", names(`Guys Should Only Hookup Or Havesex With Girls`)),
    `Respect Who Backs Down From Fight` = str_c("quiz_", names(`Respect Who Backs Down From Fight`)),
    `Guy Should Share Household Chores` = str_c("quiz_", names(`Guy Should Share Household Chores`)),
    `Girl Raped Or Raped Because Didnot Clearly Say No` = str_c("quiz_", names(`Girl Raped Or Raped Because Didnot Clearly Say No`)),
  )

# Lookup table for decoding
lookup_table <- data.frame(
  encoded_value = c(1, 2, 3),
  category_label = c("I wasn’t involved in this group for the past three months", 
                     "Yes, my coach talked to us about this", 
                     "No, my coach didn’t talk to us about this")
)

lookup_table <- data.frame(
  encoded_value = c(1, 2, 3, 4, 5),
  category_label = c("2","5 abusive", "3", "1 not abusive", "4")
)

lookup_table <- data.frame(
  encoded_value = c(1, 2, 3, 4, 5),
  category_label = c("Very unlikely", "Not Sure", "Unlikely", "Likely", "Very likely")
)

lookup_table <- data.frame(
  encoded_value = c(1, 2, 3, 4, 5),
  category_label = c("Agree", "Not sure", "Strongly disagree", "Strongly agree", "Disagree")
)

# Create a recipe to decode multiple category_encoded columns
rec <- recipe(participant_id ~ ., data = data) %>%
  step_mutate(
    starts_with("col") + "_decoded",
    !!!syms(paste0("lookup_table$category_label[match(", starts_with("col"), "_encoded, lookup_table$encoded_value)]"))
  )

# Apply the recipe using bake()
data_transformed <- bake(rec, data = data)

print(data_transformed)





















# Sample lookup table
lookup_table <- data.frame(
  encoded_value = c(1, 2, 3),
  category = c("Category A", "Category B", "Category C")
)


decoded_data <- data %>%
  left_join(lookup_table, by = c("encoded_column" = "encoded_value")) %>%
  select(-encoded_column) %>%
  rename(decoded_column = category)
