#' Active OVCs filtering
#'
#' @param data registration_list dataset
#'
#' @return A data frame of active OVCs
#' @export
#'
#' @examples active <- active_list(reglist)
#'
#' @author Jeremy Katush
active_list <- function(data) {
  active <- data %>% filter(exit_status == "ACTIVE")
  return(active)
}


#' Kilifi site Active OVCs
#'
#' @param data registration_list dataset
#'
#' @return A data frame of Kilifi site active OVCs
#' @export
#'
#' @examples active_klf <- active_Kilifi_list(reglist)
#'
#' @author Jeremy Katush
active_Kilifi_list <- function(data) {
  active_kilifi <- data %>% filter(exit_status == "ACTIVE",
                          ward == c("Kibarani","Mnarani","Tezo","Sokoni"))
  return(active_kilifi)
}


#' Gede site Active OVCs
#'
#' @param data registration_list dataset
#'
#' @return A data frame of Gede site active OVCs
#' @export
#'
#' @examples active_gede <- active_Gede_list(reglist)
active_Gede_list <- function(data) {
  active <- data %>% filter(exit_status == "ACTIVE",
                            ward == c("Dabaso", "Matsangoni", "Watamu"))
  return(active)
}


#' Malindi site Active OVCs
#'
#' @param data registration_list dataset
#'
#' @return A data frame of Malindi site active OVCs
#' @export
#'
#' @examples active_mld <- active_Malindi_list(reglist)
#'
#' @author Jeremy Katush
active_Malindi_list <- function(data) {
  active <- data %>% filter(exit_status == "ACTIVE",
                            ward == c("Ganda", "Jilore", "Kaksingiri West",
                                      "Shella", "Kakuyuni", "Malindi Town"))
  return(active)
}

#' Active and Unique OVCs
#'
#' @param data registration_list dataset
#'
#' @return A data frame of active and unique OVCs (in the order respectively)
#' @export
#'
#' @examples active_unique <- active_Unique_list(reglist)
#'
#' @author Jeremy Katush
active_Unique_list <- function(data) {
  active_unique <- data %>% filter(exit_status == "ACTIVE") %>%
    distinct(cpims_ovc_id, .keep_all = TRUE) %>%
    as.data.frame()
}

#' Active CALHIV OVCs
#'
#' @param data registration_list dataset
#'
#' @return A data frame of Active CALHIV OVCs
#' @export
#'
#' @examples calhiv <- calhiv_list(reglist)
#'
#' @author Jeremy Katush
calhiv_list <- function(data) {
  active_unique <- data %>% filter(exit_status == "ACTIVE") %>%
    distinct(cpims_ovc_id, .keep_all = TRUE) %>%
    as.data.frame()
  calhiv <- active_unique %>% filter(ovchivstatus == "POSITIVE")
  return(calhiv)
}




#' Positive OVCs list
#'
#' @param data registration_list dataset
#'
#' @return A data frame of POSITIVE OVCs
#' @export
#'
#' @examples positive <- positive_list(reglist)
#'
#' @author Jeremy Katush
positive_list <- function(data) {
  positive <- data %>% filter(exit_status == "ACTIVE",
                              ward == c("Dabaso", "Matsangoni", "Watamu"),
                              ovchivstatus == "POSITIVE")
  return(positive)
}


#' Children of People Living with HIV (PLHIV)
#'
#' @param data registration_list dataset
#'
#' @return A data frame of Children of PLHIV
#' @export
#'
#' @examples child_plhiv <- plhiv_Children_list(reglist)
#'
#' @author Jeremy Katush
plhiv_Children_list <- function(data) {
  children_of_plhiv <- data %>% filter(exit_status == "ACTIVE",
                                       ward == c("Dabaso", "Matsangoni", "Watamu"),
                                       caregiverhivstatus == "POSITIVE")
  return(children_of_plhiv)
}


#' "HEI NOT KNOWN" OVCs
#'
#' @param data registration_list dataset
#'
#' @return A data frame of Active "HEI NOT KNOWN" OVCs
#' @export
#'
#' @examples hei <- hei_list(reglist)
#'
#' @author Jeremy Katush
hei_list <- function(data) {
  hei <- data %>% filter(exit_status == "ACTIVE",
                         ward == c("Dabaso", "Matsangoni", "Watamu"),
                         ovchivstatus == "HEI NOT KNOWN")
  return(hei)
}


#' First Quarter OVCs
#'
#' @param data list_of_ovcs_served_report dataset
#'
#' @return A data frame of First quarter served OVCs
#' @export
#'
#' @examples q1 <- first_Quarter_list(listOfOvcsServed)
#'
#' @author Jeremy Katush
first_Quarter_list <- function(data) {
  q1 <- data %>% mutate(month = month(date_of_service)) %>%
    filter(month == c(1,2,3))
  return(q1)
}


#' Second Quarter OVCs
#'
#' @param data list_of_ovcs_served_report dataset
#'
#' @return A data frame of Second quarter served OVCs
#' @export
#'
#' @examples q2 <- second_Quarter_list(listOfOvcsServed)
#'
#' @author Jeremy Katush
second_Quarter_list <- function(data) {
  q2 <- data %>% mutate(month = month(date_of_service)) %>%
    filter(month == c(4,5,6))
  return(q2)
}


#' Third Quarter OVCs
#'
#' @param data list_of_ovcs_served_report dataset
#'
#' @return A data frame of Third quarter served OVCs
#' @export
#'
#' @examples q3 <- third_Quarter_list(listOfOvcsServed)
#'
#' @author Jeremy Katush
third_Quarter_list <- function(data) {
  q3 <- data %>% mutate(month = month(date_of_service)) %>%
    filter(month == c(4, 5, 6))
  return(q3)
}


#' Fourth Quarter OVCs
#'
#' @param data list_of_ovcs_served_report dataset
#'
#' @return A data frame of Third quarter served OVCs
#' @export
#'
#' @examples q4 <- fourth_Quarter_list(listOfOvcsServed)
#'
#' @author Jeremy Katush
fourth_Quarter_list <- function(data) {
  q4 <- data %>% mutate(month = month(date_of_service)) %>%
    filter(month == c(7,8,9))
  return(q4)
}



#' Valid Caseplan by Ward
#'
#' @param data Caseplan dataset
#' @param ward Ward column inside the Caseplan dataset
#'
#' @return A dataframe of valid caseplan by ward
#' @export
#'
#' @examples valid_caseplan <- valid_Caseplan_by_ward(caseplan.xlx, "Sokoni")
valid_Caseplan_by_ward <- function(data, ward) {
  selected_data <- data[data$ward == ward, ]
  return(selected_data)
}


#' Valid Caseplan by Domain
#'
#' @param data Caseplan dataset
#' @param domain Domain column inside the Caseplan dataset
#'
#' @return A dataframe of valid caseplan by Domain
#' @export
#'
#' @examples valid_caseplan <- valid_Caseplan_by_domain(caseplan.xlsx, "Sokoni")
valid_Caseplan_by_domain <- function(data, domain) {
  selected_data <- data[data$domain == domain, ]
  return(selected_data)
}


#' Valid Caseplan by Ward and Domain
#'
#' @param data Caseplan dataset
#' @param ward Ward column
#' @param domains Domains column
#'
#' @return A data frame of valid caseplan by ward and domains
#' @export
#'
#' @examples valid_casplan <- valid_Caseplan_by_ward_and_domain(caseplan.xlsx, "Sokoni", "Stable")
valid_Caseplan_by_ward_and_domain <- function(data, ward, domains) {
  selected_data <- data[data$ward == ward & data$domains == domains, ]
  return(selected_data)
}
