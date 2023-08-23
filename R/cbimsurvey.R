
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
#' @examples active_unique <- active_Unique_OVC_list(reglist)
#'
#' @author Jeremy Katush
active_Unique_OVC_list <- function(data) {
  active_unique <- data %>% filter(exit_status == "ACTIVE") %>%
    distinct(cpims_ovc_id, .keep_all = TRUE) %>%
    as.data.frame()
}


#' Active and Unique OVCs
#'
#' @param data registration_list dataset
#'
#' @return A data frame of active and unique Caregivers (in the order respectively)
#' @export
#'
#' @examples active_unique <- active_Unique_CG_list(reglist)
#'
#' @author Jeremy Katush
active_Unique_CG_list <- function(data) {
  active_unique <- data %>% filter(exit_status == "ACTIVE") %>%
    distinct(caregiver_id, .keep_all = TRUE) %>%
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


#' GEDE Positive OVCs list
#'
#' @param data registration_list dataset
#'
#' @return A data frame of POSITIVE OVCs
#' @export
#'
#' @examples positive <- positive_Gede_list(reglist)
#'
#' @author Jeremy Katush
positive_Gede_list <- function(data) {
  positive <- data %>% filter(exit_status == "ACTIVE",
                              ward == c("Dabaso", "Matsangoni", "Watamu"),
                              ovchivstatus == "POSITIVE")
  return(positive)
}


#' KILIFI Positive OVCs list
#'
#' @param data registration_list dataset
#'
#' @return A data frame of POSITIVE OVCs
#' @export
#'
#' @examples positive <- positive_Kilifi_list(reglist)
#'
#' @author Jeremy Katush
positive_Kilifi_list <- function(data) {
  positive <- data %>% filter(exit_status == "ACTIVE",
                              ward == c("Sokoni", "Kibarani", "Mnarani", "Tezo"),
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


#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
reglist_Combined_lists <- function(data) {
  active_list <- active_list(data)
  active_Kilifi_list <- active_Kilifi_list(data)
  active_Gede_list <- active_Gede_list(data)
  active_Malindi_list <- active_Malindi_list(data)
  active_Unique_OVC_list <- active_Unique_OVC_list(data)
  active_Unique_CG_list <- active_Unique_CG_list(data)
  calhiv_list <- calhiv_list(data)
  positive_Gede_list <- positive_Gede_list(data)
  positive_Kilifi_list <- positive_Kilifi_list(data)
  plhiv_Children_list <- plhiv_Children_list(data)
  hei_list <- hei_list(data)

  wb <- createWorkbook()

  addWorksheet(wb, sheetName = "active")
  writeData(wb, sheet = "active", x = active_list)
  addWorksheet(wb, sheetName = "active Kilifi")
  writeData(wb, sheet = "active Kilifi", x = active_Kilifi_list)
  addWorksheet(wb, sheetName = "active Gede")
  writeData(wb, sheet = "active Gede", x = active_Gede_list)
  addWorksheet(wb, sheetName = "active Malindi")
  writeData(wb, sheet = "active Malindi", x = active_Malindi_list)
  addWorksheet(wb, sheetName = "active Unique OVC")
  writeData(wb, sheet = "active Unique OVC", x = active_Unique_OVC_list)
  addWorksheet(wb, sheetName = "active Unique CG")
  writeData(wb, sheet = "active Unique CG", x = active_Unique_CG_list)
  addWorksheet(wb, sheetName = "calhiv")
  writeData(wb, sheet = "calhiv", x = calhiv_list)
  addWorksheet(wb, sheetName = "positive Gede")
  writeData(wb, sheet = "positive Gede", x = positive_Gede_list)
  addWorksheet(wb, sheetName = "positive Kilifi")
  writeData(wb, sheet = "positive Kilifi", x = positive_Kilifi_list)
  addWorksheet(wb, sheetName = "plhiv Children")
  writeData(wb, sheet = "plhiv Children", x = plhiv_Children_list)
  addWorksheet(wb, sheetName = "HEI")
  writeData(wb, sheet = "HEI", x = hei_list)

  saveWorkbook(wb, "updated_reglist_report.xlsx")
  finale_updated_reglist <- openXL("updated_reglist_report.xlsx")

  return(finale_updated_reglist)
}


