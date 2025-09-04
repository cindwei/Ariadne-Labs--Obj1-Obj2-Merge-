# load libraries
library(tidyverse)
library(googlesheets4)

gs4_auth(scope = "https://www.googleapis.com/auth/spreadsheets.readonly")

create_module_map <- function(spreadsheet_url, sheet_tab){
  crosswalk <- read_sheet(spreadsheet_url, sheet=sheet_tab)
  module_map <- crosswalk[!is.na(crosswalk$"Objective 1  variable name (mother / infant 1)") & 
                       !is.na(crosswalk$"Objective 2 variable name") & 
                       (is.na(crosswalk$"Response Options (if they differ)...4") | crosswalk$"Response Options (if they differ)...11" == ""), 
                     c("Objective 1  variable name (mother / infant 1)", "Objective 2 variable name")]
  return(module_map)
}

create_merged_data <- function(screening_check, obj1_module, obj2_module, module_map, merged_name){
  # identify crossover infants from 02 screening check
  crossover_infants <- screening_check %>%
    select(obj1snap_record_id, infant_num, obj1_yn_1, obj1_id_infant_1, record_id) %>%
    filter(obj1_yn_1, !is.na(obj1_id_infant_1))
  # keep only infants from obj1 that are crossed over
  obj1_crossover_rows <- semi_join(obj1_module, crossover_infants, by = c("record_id"="obj1snap_record_id", "infant_num"))
  # join o2 record ids to o1 data
  obj1_crossover_rows <- obj1_crossover_rows %>%
    left_join(
      crossover_infants %>%
        select(o2_record_id = record_id, obj1snap_record_id,infant_num),
      by = c("record_id" = "obj1snap_record_id", "infant_num"="infant_num")
    )
  
  # create variable map with 2 cols with obj1 vars and corresponding obj2 vars
  var_map <- tibble(
    obj1 = module_map[[1]],
    obj2 = module_map[[2]]
  )
  
  # check for errors in the crosswalk
  missing_obj1 <- setdiff(var_map$obj1, names(obj1_module))
  missing_obj2 <- setdiff(var_map$obj2, names(obj2_module))
  if (length(missing_obj1) > 0) {
    warning("The following obj1 variables are missing from obj1_module: ", paste(missing_obj1, collapse = ", "))
  }
  if (length(missing_obj2) > 0) {
    warning("The following obj2 variables are missing from obj2_module: ", paste(missing_obj2, collapse = ", "))
  }
  
  # merge on record ids and infant_num
  obj1_mapped_cols <- obj1_crossover_rows %>%
    select(o2_record_id, infant_num, all_of(var_map$obj1))
  merged <- obj2_module %>%
    left_join(obj1_mapped_cols, by = c("record_id"="o2_record_id", "infant_num"))
  
  # fill in missing values in obj2
  for (i in seq_len(nrow(var_map))) {
    var1 <- var_map$obj1[i]
    var2 <- var_map$obj2[i]
    merged[[var2]] <- if_else(
      is.na(merged[[var2]]),
      merged[[var1]],
      merged[[var2]]
    )
  }
  # get rid of obj1 columns
  final_merged <- merged %>%
    select(-all_of(var_map$obj1))
  
  return(final_merged)
}

### example usage
#sheet_url="https://docs.google.com/spreadsheets/d/1VO1gq269094Vz8QEgOnzRRgum_OVfHPvU1i6m2pE0Pg/edit?usp=sharing"
#module_map<-create_module_map(sheet_url, "Infant anthro")
#create_merged_data(O2_screening_check_a, O1_module_l_snapshot_infant_anthro, O2_infant_anthropometrics,module_map)

