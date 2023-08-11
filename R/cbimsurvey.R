
#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
cbimsurvey_Section_a_list <- function(data) {
  cbimsurvey_renamed <- data %>%
    rename(
      "Pims_Id" = `Pims Id`,
      "quizA_Respect Towards Women" = `Respect Towards Women`,
      "quizA_Stopping Kids Do Harmful Things" = `Stopping Kids Do Harmful Things`
    )

  melted_data <- cbimsurvey_renamed %>%
    select(Pims_Id, Category, `quizA_Respect Towards Women`, `quizA_Stopping Kids Do Harmful Things`) %>%
    pivot_longer(
      cols = starts_with("quizA"),
      names_to = "Question",
      values_to = "Choice",
      names_prefix = "quizA_"
    )

  look_up_table <- c("I wasn’t involved in this group for the past three months" = "1 a",
                     "Yes, my coach talked to us about this" = "2 a",
                     "No, my coach didn’t talk to us about this" = "3 a")
  melted_data$Decoded_choice <- look_up_table[melted_data$Choice]

  choices_combined <- melted_data %>%
    group_by(Pims_Id, Question) %>%
    summarize(Combined_Choices = paste(Decoded_choice, collapse = " | "))

  return(choices_combined)

}


#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
cbimsurvey_Section_b_list <- function(data) {
  cbimsurvey_renamed <- data %>%
    rename(
      "quizB_Name Calling Or Insulting Them" = `Name Calling Or Insulting Them`,
      "quizB_Ugly Or Stupid" = `Ugly Or Stupid`,
      "quizB_Making Fun Ofthem" = `Making Fun Ofthem`,
      "quizB_Telling Them What Todo" = `Telling Them What Todo`,
      "quizB_Telling Friends To See Or Talkto" = `Telling Friends To See Or Talkto`,
      "quizB_Pressuring Their Partners Not To Breakup" = `Pressuring Their Partners Not To Breakup`,
      "quizB_Not Listening Them" = `Not Listening Them`,
      "quizB_Convince Them To Have Sex" = `Convince Them To Have Sex`,
      "quizB_Preventing Them Leaving A Room" = `Preventing Them Leaving A Room`,
      "quizB_Distracting Spying Onthem" = `Distracting Spying Onthem`,
      "quizB_Being Physically Intimate Without Consent" = `Being Physically Intimate Without Consent`,
      "quizB_Threating To Hit Them" = `Threating To Hit Them`,
      "quizB_Forcing Them To Have Sex" = `Forcing Them To Have Sex`,
      "quizB_Pressuring Them Skip Their Activities" = `Pressuring Them Skip Their Activities`,
      OB
      "quizB_Constanly Contacting Them To Find Out There Whereabout" = `Constanly Contacting Them To Find Out There Whereabout`,
      "quizB_Constantly Interrupting Them At Work" = `Constantly Interrupting Them At Work`,
      "quizB_Using Their Creditcard Cash Without Permission" = `Using Their Creditcard Cash Without Permission`,
      "quizC_Making Disrespective Comments About Girls Body" = `Making Disrespective Comments About Girls Body`,
      "quizC_Spreading Rumors Abt Girls" = `Spreading Rumors Abt Girls`,
      "quizC_Fighting Or Threatening Agirl" = `Fighting Or Threatening Agirl`,
      "quizB_Constantly Asking Them To Pay For Activities Meal Othergifts" = `Constantly Asking Them To Pay For Activities Meal Othergifts`,
      "quizC_Doing Unwelcome Actions" = `Doing Unwelcome Actions`,
      "quizC_Pushing Grabbing Hittingor Hurting A Girl" = `Pushing Grabbing Hittingor Hurting A Girl`,
      "quizC_Showing People Sexual Content Of A Girl Without Herconsent" = `Showing People Sexual Content Of A Girl Without Herconsent`,
      "quizC_Telling Jokes That Disrespect Women" = `Telling Jokes That Disrespect Women`,
      "quizC_Taking Advantage Of Drunk Girl" = `Taking Advantage Of Drunk Girl`,
      "quizC_Pressure Girl Have Sex Intimate Without Consent" = `Pressure Girl Have Sex Intimate Without Consent`,
      "quizD_Boy Not Have Tofight For Respect" = `Boy Not Have Tofight For Respect`,
      "quizD_Girl Wearing Revealing Clothes Deserves Comment" = `Girl Wearing Revealing Clothes Deserves Comment`,
      "quizD_Bothered When Guy Acts Girlish" = `Bothered When Guy Acts Girlish`,
      "quizD_Guys With Lot Of Money Are Manlier" = `Guys With Lot Of Money Are Manlier`,
      "quizD_Guy Who Ask For Help Looks Weak" = `Guy Who Ask For Help Looks Weak`,
      "quizD_Guy Should Pay For Most Of Things Ina Good Dating" = `Guy Should Pay For Most Of Things Ina Good Dating`,
      "quizD_Guys Should Only Hookup Or Havesex With Girls" = `Guys Should Only Hookup Or Havesex With Girls`,
      "quizD_Respect Who Backs Down From Fight" = `Respect Who Backs Down From Fight`,
      "quizD_Guy Should Share Household Chores" = `Guy Should Share Household Chores`,
      "quizD_Girl Raped Or Raped Because Didnot Clearly Say No" = `Girl Raped Or Raped Because Didnot Clearly Say No`,
    )

  melted_data <- cbimsurvey_renamed %>%
    select(Pims_Id, Category, `quizB_Name Calling Or Insulting Them`:`quizB_Using Their Creditcard Cash Without Permission`) %>%
    pivot_longer(
      cols = starts_with("quizB"),
      names_to = "Question",
      values_to = "Choice",
      names_prefix = "quizB_"
    )

  look_up_table <- c("1 not abusive" = "not abusive", "2" = "2 b", "3" = "3 b", "4" = "4 b", "5 abusive" = "abusive")
  melted_data$Decoded_choice <- look_up_table[melted_data$Choice]

  choices_combined <- melted_data %>%
    group_by(Pims_Id, Question) %>%
    summarize(Combined_Choices = paste(Decoded_choice, collapse = " | "))

  return(choices_combined)

}


#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
cbimsurvey_Section_c_list <- function(data) {
  cbimsurvey_renamed <- data %>%
    rename(
      "Pims_Id" = `Pims Id`,
      "quizA_Respect Towards Women" = `Respect Towards Women`,
      "quizA_Stopping Kids Do Harmful Things" = `Stopping Kids Do Harmful Things`,
      "quizB_Name Calling Or Insulting Them" = `Name Calling Or Insulting Them`,
      "quizB_Ugly Or Stupid" = `Ugly Or Stupid`,
      "quizB_Making Fun Ofthem" = `Making Fun Ofthem`,
      "quizB_Telling Them What Todo" = `Telling Them What Todo`,
      "quizB_Telling Friends To See Or Talkto" = `Telling Friends To See Or Talkto`,
      "quizB_Pressuring Their Partners Not To Breakup" = `Pressuring Their Partners Not To Breakup`,
      "quizB_Not Listening Them" = `Not Listening Them`,
      "quizB_Convince Them To Have Sex" = `Convince Them To Have Sex`,
      "quizB_Preventing Them Leaving A Room" = `Preventing Them Leaving A Room`,
      "quizB_Distracting Spying Onthem" = `Distracting Spying Onthem`,
      "quizB_Being Physically Intimate Without Consent" = `Being Physically Intimate Without Consent`,
      "quizB_Threating To Hit Them" = `Threating To Hit Them`,
      "quizB_Forcing Them To Have Sex" = `Forcing Them To Have Sex`,
      "quizB_Pressuring Them Skip Their Activities" = `Pressuring Them Skip Their Activities`,
      "quizB_Constanly Contacting Them To Find Out There Whereabout" = `Constanly Contacting Them To Find Out There Whereabout`,
      "quizB_Constantly Interrupting Them At Work" = `Constantly Interrupting Them At Work`,
      "quizB_Using Their Creditcard Cash Without Permission" = `Using Their Creditcard Cash Without Permission`,
      "quizC_Making Disrespective Comments About Girls Body" = `Making Disrespective Comments About Girls Body`,
      "quizC_Spreading Rumors Abt Girls" = `Spreading Rumors Abt Girls`,
      "quizC_Fighting Or Threatening Agirl" = `Fighting Or Threatening Agirl`,
      "quizB_Constantly Asking Them To Pay For Activities Meal Othergifts" = `Constantly Asking Them To Pay For Activities Meal Othergifts`,
      "quizC_Doing Unwelcome Actions" = `Doing Unwelcome Actions`,
      "quizC_Pushing Grabbing Hittingor Hurting A Girl" = `Pushing Grabbing Hittingor Hurting A Girl`,
      "quizC_Showing People Sexual Content Of A Girl Without Herconsent" = `Showing People Sexual Content Of A Girl Without Herconsent`,
      "quizC_Telling Jokes That Disrespect Women" = `Telling Jokes That Disrespect Women`,
      "quizC_Taking Advantage Of Drunk Girl" = `Taking Advantage Of Drunk Girl`,
      "quizC_Pressure Girl Have Sex Intimate Without Consent" = `Pressure Girl Have Sex Intimate Without Consent`,
      "quizD_Boy Not Have Tofight For Respect" = `Boy Not Have Tofight For Respect`,
      "quizD_Girl Wearing Revealing Clothes Deserves Comment" = `Girl Wearing Revealing Clothes Deserves Comment`,
      "quizD_Bothered When Guy Acts Girlish" = `Bothered When Guy Acts Girlish`,
      "quizD_Guys With Lot Of Money Are Manlier" = `Guys With Lot Of Money Are Manlier`,
      "quizD_Guy Who Ask For Help Looks Weak" = `Guy Who Ask For Help Looks Weak`,
      "quizD_Guy Should Pay For Most Of Things Ina Good Dating" = `Guy Should Pay For Most Of Things Ina Good Dating`,
      "quizD_Guys Should Only Hookup Or Havesex With Girls" = `Guys Should Only Hookup Or Havesex With Girls`,
      "quizD_Respect Who Backs Down From Fight" = `Respect Who Backs Down From Fight`,
      "quizD_Guy Should Share Household Chores" = `Guy Should Share Household Chores`,
      "quizD_Girl Raped Or Raped Because Didnot Clearly Say No" = `Girl Raped Or Raped Because Didnot Clearly Say No`,
    )

  melted_data <- cbimsurvey_renamed %>%
    select(Pims_Id, Category, starts_with("quizC")) %>%
    pivot_longer(
      cols = starts_with("quizC"),
      names_to = "Question",
      values_to = "Choice",
      names_prefix = "quizC_"
    )

  look_up_table <- c("Very unlikey" = "very_unlikely", "Unlikely" = "unlikey", "Not Sure" = "not_sure", "Likely" = "likely", "Very likely" = "very_likely")
  melted_data$Decoded_choice <- look_up_table[melted_data$Choice]

  choices_combined <- melted_data %>%
    group_by(Pims_Id, Question) %>%
    summarize(Combined_Choices = paste(Decoded_choice, collapse = " | "))

  return(choices_combined)

}


#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
cbimsurvey_Section_d_list <- function(data) {
  cbimsurvey_renamed <- data %>%
    rename(
      "Pims_Id" = `Pims Id`,
      "quizA_Respect Towards Women" = `Respect Towards Women`,
      "quizA_Stopping Kids Do Harmful Things" = `Stopping Kids Do Harmful Things`,
      "quizB_Name Calling Or Insulting Them" = `Name Calling Or Insulting Them`,
      "quizB_Ugly Or Stupid" = `Ugly Or Stupid`,
      "quizB_Making Fun Ofthem" = `Making Fun Ofthem`,
      "quizB_Telling Them What Todo" = `Telling Them What Todo`,
      "quizB_Telling Friends To See Or Talkto" = `Telling Friends To See Or Talkto`,
      "quizB_Pressuring Their Partners Not To Breakup" = `Pressuring Their Partners Not To Breakup`,
      "quizB_Not Listening Them" = `Not Listening Them`,
      "quizB_Convince Them To Have Sex" = `Convince Them To Have Sex`,
      "quizB_Preventing Them Leaving A Room" = `Preventing Them Leaving A Room`,
      "quizB_Distracting Spying Onthem" = `Distracting Spying Onthem`,
      "quizB_Being Physically Intimate Without Consent" = `Being Physically Intimate Without Consent`,
      "quizB_Threating To Hit Them" = `Threating To Hit Them`,
      "quizB_Forcing Them To Have Sex" = `Forcing Them To Have Sex`,
      "quizB_Pressuring Them Skip Their Activities" = `Pressuring Them Skip Their Activities`,
      "quizB_Constanly Contacting Them To Find Out There Whereabout" = `Constanly Contacting Them To Find Out There Whereabout`,
      "quizB_Constantly Interrupting Them At Work" = `Constantly Interrupting Them At Work`,
      "quizB_Using Their Creditcard Cash Without Permission" = `Using Their Creditcard Cash Without Permission`,
      "quizC_Making Disrespective Comments About Girls Body" = `Making Disrespective Comments About Girls Body`,
      "quizC_Spreading Rumors Abt Girls" = `Spreading Rumors Abt Girls`,
      "quizC_Fighting Or Threatening Agirl" = `Fighting Or Threatening Agirl`,
      "quizB_Constantly Asking Them To Pay For Activities Meal Othergifts" = `Constantly Asking Them To Pay For Activities Meal Othergifts`,
      "quizC_Doing Unwelcome Actions" = `Doing Unwelcome Actions`,
      "quizC_Pushing Grabbing Hittingor Hurting A Girl" = `Pushing Grabbing Hittingor Hurting A Girl`,
      "quizC_Showing People Sexual Content Of A Girl Without Herconsent" = `Showing People Sexual Content Of A Girl Without Herconsent`,
      "quizC_Telling Jokes That Disrespect Women" = `Telling Jokes That Disrespect Women`,
      "quizC_Taking Advantage Of Drunk Girl" = `Taking Advantage Of Drunk Girl`,
      "quizC_Pressure Girl Have Sex Intimate Without Consent" = `Pressure Girl Have Sex Intimate Without Consent`,
      "quizD_Boy Not Have Tofight For Respect" = `Boy Not Have Tofight For Respect`,
      "quizD_Girl Wearing Revealing Clothes Deserves Comment" = `Girl Wearing Revealing Clothes Deserves Comment`,
      "quizD_Bothered When Guy Acts Girlish" = `Bothered When Guy Acts Girlish`,
      "quizD_Guys With Lot Of Money Are Manlier" = `Guys With Lot Of Money Are Manlier`,
      "quizD_Guy Who Ask For Help Looks Weak" = `Guy Who Ask For Help Looks Weak`,
      "quizD_Guy Should Pay For Most Of Things Ina Good Dating" = `Guy Should Pay For Most Of Things Ina Good Dating`,
      "quizD_Guys Should Only Hookup Or Havesex With Girls" = `Guys Should Only Hookup Or Havesex With Girls`,
      "quizD_Respect Who Backs Down From Fight" = `Respect Who Backs Down From Fight`,
      "quizD_Guy Should Share Household Chores" = `Guy Should Share Household Chores`,
      "quizD_Girl Raped Or Raped Because Didnot Clearly Say No" = `Girl Raped Or Raped Because Didnot Clearly Say No`,
    )

  melted_data <- cbimsurvey_renamed %>%
    select(Pims_Id, Category, starts_with("quizD")) %>%
    pivot_longer(
      cols = starts_with("quizD"),
      names_to = "Question",
      values_to = "Choice",
      names_prefix = "quizD_"
    )

  look_up_table <- c("Strongly agree" = "strong_agree", "Agree" = "agree", "Not sure" = "not_sure4", "Disagree" = "disagree", "Strongly disagree" = "strong_disagree")
  melted_data$Decoded_choice <- look_up_table[melted_data$Choice]

  choices_combined <- melted_data %>%
    group_by(Pims_Id, Question) %>%
    summarize(Combined_Choices = paste(Decoded_choice, collapse = " | "))

  return(choices_combined)

}


#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
cbimsurvey_Combined_Sections_lists <- function(data) {
  # Load your datasets into data frames (replace 'file_path' with actual file paths)
  section_a <- cbimsurvey_Section_a_list(data)
  section_b <- cbimsurvey_Section_b_list(data)
  section_c <- cbimsurvey_Section_c_list(data)
  section_d <- cbimsurvey_Section_d_list(data)

  # Merge data frames using the common column
  combined_df <- rbind(section_a, section_b, section_c, section_d)

  # split Combined_Choices column
  df_split <- separate(combined_df, col = 'Combined_Choices',
                       into = c("post_programme", "pre_programme"),
                       sep = "\\|", extra = "drop", fill = "right")

  # Remove leading and trailing white spaces from the new columns
  df_split$post_programme <- trimws(df_split$post_programme)
  df_split$pre_programme <- trimws(df_split$pre_programme)

  # Original dataset
  cbimsurvey <- data

  # Create workbook and save original data and results in different sheets
  wb <- createWorkbook()

  addWorksheet(wb, sheetName = "cbimsurvey")
  writeData(wb, sheet = "cbimsurvey", x = cbimsurvey)
  addWorksheet(wb, sheetName = "pre-post")
  writeData(wb, sheet = "pre-post", x = df_split)

  saveWorkbook(wb, "updated_cbimsurvey_report.xlsx")
  finale_updated_cbimsurvey <- openXL("updated_cbimsurvey_report.xlsx")

  return(finale_updated_cbimsurvey)

}
