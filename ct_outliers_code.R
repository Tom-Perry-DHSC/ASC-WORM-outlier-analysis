analyst <- "Tom Perry"
description <- "Capacity Tracker Outlier Analysis of all current statistics collected (other than absence headcounts!)"

# __________________________________________________________________________________
# 
# -------- Isolate data based on Change_in_reporting if there is a response --------
# __________________________________________________________________________________

#' This function takes the full provider_data taken directly from CT responses and reduces to only new responses. Also converts to long form data so outlier analysis is easier
#'
#' @param wide_data 
#'
#' @return A long-format dataframe containing all new responses from CT, using only \code{wide_data}
#' 
#' @export
#'
#' @examples OA_reduce_CT_data(provider_data)
OA_reduce_CT_data <- function(wide_data){
  reduced <- wide_data %>% 
    # Get only new responses and reduce the file size by filtering for Change_in_reporting the hourly stats we introduced
    filter(UpdatedWeek == 1) %>% 
    select(ExtractDate, CqcId, Setting, contains("_Headcount"), contains("Hours"), contains("Days")) %>% 
    select(!contains("_UpdatedOnly")) %>% 
    pivot_longer(
      cols = c(contains("_Headcount"), contains("Hours"), contains("Days")),
      names_to = "Temp",
      values_to = "Value"
    ) %>% 
    # remove entries that are either not collected or not responded to in that extract
    filter(!is.na(Value)) %>% 
    # relabelling by categories
    mutate(
      "Employee_Type" = ifelse(str_detect(Temp, "Agency"), "Ag", "DE"),
      Temp = str_remove(Temp, "DirectlyEmployed_"),
      Temp = str_remove(Temp, "Agency_"),
      Temp = str_remove(Temp, "Agency"),
      Breakdown = ifelse(
        str_detect(Temp, "Headcount"),
        "Headcount",
        ifelse(
          str_detect(Temp, "Hours"),
          "Hours",
          "Days"
        )
      ),
      Temp = str_remove(Temp, "Headcount"),
      Temp = str_remove(Temp, "Hours"),
      Temp = str_remove(Temp, "Days"),
      "Detail" = ifelse(
        str_detect(Temp, "Absence"),
        "Absence",
        ifelse(
          str_detect(Temp, "Overtime"),
          "Overtime",
          "Total"
        )
      ),
    ) %>% 
    select(ExtractDate, CqcId, Setting, Breakdown, Employee_Type, Detail, Value)
  
  return(reduced)
}

# __________________________
# 
# -------- Outliers --------
# __________________________

#' Code takes long format data, finds outliers within that and then returns only the outliers (for smaller file sizes)
#'
#' @param data is the long format data that is to be analysed for outliers
#' @param tolerances is a datset which is imported in the main function from a csv saved in Workforce Reporting / Inputs
#'
#' @return The long format data from \code{data}, with TRUE if marked as an outlier. Outliers are decided by the \code{tolerances} file
#' @export
#'
#' @examples 
#' OA_get_outliers(provider_data, mytolerances)
#' OA_get_outliers(provider_data, "tolerances.csv")
OA_get_outliers <- function(data, tolerances){
  output <- data %>% 
    # add the tolerances for each category of data
    left_join(tolerances) %>%
    # find the first and last responses
    group_by(CqcId, Setting, Employee_Type, Breakdown, Detail) %>% 
    mutate("Cum_Value" = cumsum(Value)) %>% 
    filter(Cum_Value > 0) %>% 
    mutate(
      # find the first/last if not time the provider responds so it is not treated as an outlier
      "Change_in_reporting" = ifelse(
        Cum_Value > 0 & (lag(Cum_Value) == 0 | is.na(lag(Cum_Value))),
        "First",
        ifelse(
          Cum_Value == max(Cum_Value) & (lag(Cum_Value) < Cum_Value | is.na(lag(Cum_Value))),
          "Final",
          NA
        )
      ),
      # compare every instance to the previous and denote if an outlier (+ type/why)
      "Change_on_prev" = ifelse(
        is.na(Change_in_reporting) | Change_in_reporting == "Final",
        ifelse(
          Value / lag(Value) - 1 > Tol_perc & Value - lag(Value) > Tol_raw,
          "Outlier_increase",
          ifelse(
            Value / lag(Value) - 1 < 1 / Tol_perc & Value - lag(Value) < -Tol_raw,
            "Outlier_decrease",
            NA
          )
        ),
        NA
      ),
      # compare to the 2nd previous entry if possible
      # if it doesn't exist, then the resulting looked up NA will be neither TRUE nor FALSE in the ifelse
      "Change_on_prev_2" = ifelse(
        is.na(Change_in_reporting) | Change_in_reporting == "Final",
        ifelse(
          Value / lag(Value, 2) - 1 > Tol_perc & Value - lag(Value, 2) > Tol_raw,
          "Outlier_increase_2",
          ifelse(
            Value / lag(Value, 2) - 1 < 1 / Tol_perc & Value - lag(Value, 2) < -Tol_raw,
            "Outlier_decrease_2",
            NA
          )
        ),
        NA
      ),
      # use change_on_prev to decide if it is an outlier based on the previous value
      "Outlier_on_prev" = ifelse(
        is.na(Change_in_reporting) | Change_in_reporting == "Final",
        ifelse(
          str_detect(Change_on_prev, "Outlier"),
          TRUE,
          NA
        ),
        NA
      ),
      "Outlier_on_prev_2" = ifelse(
        (is.na(Change_in_reporting) | Change_in_reporting == "Final") & is.na(lag(Change_in_reporting,2)),
        ifelse(
          str_detect(Change_on_prev_2, "Outlier"),
          TRUE,
          NA
        ),
        NA
      ),
      # decide if the last reported (non-zero) figure is an outlier
      "Outlier_for_final" = ifelse(
        Change_in_reporting == "Final" & Outlier_on_prev & Outlier_on_prev_2,
        TRUE,
        NA
      ),
      "Outlier" = ifelse(
        # standard outliers
        (
          str_detect(Change_on_prev, "Outlier") &
            str_detect(lead(Change_on_prev), "Outlier") &
            str_detect(Change_on_prev_2, "Outlier") &
            is.na(lead(Change_on_prev_2))
        ) |
          # final responses that have no data to prove given afterwards
          # just to rule out rogue results we can't explain
          (
            Outlier_for_final & is.na(lag(Change_on_prev))
          ),
        TRUE,
        NA
      )
    ) %>% 
    ungroup() %>% 
    select(ExtractDate, CqcId, Setting, Breakdown, Employee_Type, Detail, Value, Outlier) %>%
    filter(Outlier)
  
  return(output)
}

# ___________________________________________
# 
# -------- Convert back to wide data --------
# ___________________________________________

#' Converts the outliers which were returned as long format, into wide format, also attaching the usual column names
#'
#' @param outliers needs to be in long format
#' @param myprefix automatically will prefix OutlierFlag_ post-wide-format conversion, but can specify another
#'
#' @return
#' @export
#'
#' @examples
OA_get_outliers_wide_form <- function(outliers, myprefix = "OutlierFlag_"){
  wide_form <- outliers %>% 
    mutate(
      "Field" = 
        ifelse(
          Breakdown == "Headcount",
          # headcount labelling
          ifelse(
            Employee_Type == "DE",
            "DirectlyEmployed_Headcount",
            "Agency_Headcount"
          ),
          ifelse(
            Breakdown == "Hours",
            # hourly labelling
            ifelse(
              Employee_Type == "DE",
              ifelse(
                Detail == "Total",
                "HoursPaid",
                ifelse(
                  Detail == "Overtime",
                  "HoursOvertime",
                  ifelse(
                    Detail == "Absence",
                    "HoursAbsence",
                    NA
                  )
                )
              ),
              "HoursAgency"
            ),
            ifelse(
              Breakdown == "Days",
              # daily labelling
              "DaysAbsence",
              NA
            )
          )
        )
    ) %>% 
    select("Update_date" = ExtractDate, CqcId, Setting, Field, Outlier) %>% 
    pivot_wider(names_from = "Field", values_from = "Outlier", names_prefix = myprefix) # was  , names_prefix = "OutlierFlag_"
  
  return(wide_form)
}

# ____________________________________________________
# 
# -------- Add the flags on for each category --------
# ____________________________________________________

#' Turns outliers into a flag, labelling each outlier in a new flag column. Again only returns outliers for file size
#'
#' @param wide_data 
#' @param wide_outliers 
#'
#' @return
#' @export
#'
#' @examples
OA_get_outliers_flagged <- function(wide_data, wide_outliers){
  flagged <- wide_data %>% 
    # Get only old responses and reduce the file size by filtering for Change_in_reporting the hourly stats we introduced
    select(ExtractDate, UpdatedWeek, CqcId, Setting) %>% 
    mutate(
      "Update_date" = ifelse(UpdatedWeek, ExtractDate, NA)
    ) %>% 
    select(-UpdatedWeek) %>% 
    # fill in the dates for the time of last update then format into date form
    group_by(CqcId, Setting) %>%
    fill(Update_date, .direction = "down") %>% 
    ungroup() %>% 
    mutate(Update_date = as.Date(Update_date, origin = "1970-01-01")) %>% 
    # attach the outlier flag
    left_join(wide_outliers) %>% 
    select(ExtractDate, CqcId, Setting, contains("OutlierFlag_"))
  # filter for results with at least one outlier
  flagged <- flagged[rowSums(is.na(flagged)) < ncol(flagged) - 3, ]
  
  return(flagged)
}

# _______________________________________________
# 
# -------- Produce the corrected dataset --------
# _______________________________________________

#' This takes the uncorrected provider data and attaches the flags, any known correction. First removes erroneous data, adds the confirmed corrections and then fills where no update has been provided. Requires a change where if no outliers in one category are found, it plays up
#'
#' @param wide_data 
#' @param wide_flagged 
#' @param wide_known_outliers 
#'
#' @return
#' @export
#'
#' @examples
OA_get_corrections <- function(wide_data, wide_flagged, wide_known_outliers){
  corrected <- wide_data %>% 
    # remove unnecessary columns
    select(!contains("_UpdatedOnly"), -DirectlyEmployed_CovidAbsence, -DirectlyEmployed_GeneralAbsence, -DirectlyEmployed_TotalAbsence) %>% 
    # add the flags for the outliers
    left_join(wide_flagged) %>% 
    # remove the last updated columns for the moment
    select(!contains("Updated")) %>%
    # convert the flags to be false if not an outlier
    mutate(
      across(
        c(contains("OutlierFlag_")),
        .fns = ~ ifelse(is.na(.x), FALSE, .x)
      ),
      # remove outliers (if TRUE flag)
      across(
        c(
          contains("Headcount") & !contains("OutlierFlag_"),
          contains("Absence") & !contains("OutlierFlag_"),
          contains("Hours") & !contains("OutlierFlag_")
        ),
        .fns = ~ ifelse(eval(parse(text = paste0("OutlierFlag_", deparse(substitute(.x))))),
                        NA,
                        .x)
      )
    ) %>% 
    # clean the dataframe
    select(!contains("OutlierFlag_")) %>% 
    # SECTION TO ADD IN THE KNOWN OUTLIERS - needs checking
    # adds the known outliers we have manually recorded
    left_join(wide_known_outliers %>% rename("ExtractDate" = Update_date)) %>%
    # select() %>% 
    mutate(
      across(
        c(
          contains("Headcount") & !contains("Corrected_"),
          contains("Absence") & !contains("Corrected_"),
          contains("Hours") & !contains("Corrected_")
        ),
        .fns = ~ ifelse(!is.na(eval(parse(text = paste0("Corrected_", deparse(substitute(.x)))))),
                        eval(parse(text = paste0("Corrected_", deparse(substitute(.x))))),
                        .x)
      )
    ) %>%
    # fill in the empty removed outlier slots with carried forward results
    group_by(CqcId, Setting) %>% 
    fill(
      everything(),
      .direction = "down"
    ) %>%
    ungroup() %>% 
    # add the absence headcounts and other info back on, then reorder
    left_join(wide_data %>% select(ExtractDate, CqcId, Setting, contains("DirectlyEmployed") & contains("Absence") & !contains("UpdatedOnly"), UpdatedMonth, UpdatedWeek)) %>% 
    select(ExtractDate, contains("Updated"), !contains("Extract") & !contains("Updated") & !contains("Corrected_"))
  
  return(corrected)
}

# ______________________________
# 
# -------- Full Process --------
# ______________________________

#' The whole process of the outlier analysis process in one useful function
#'
#' @param wide_data 
#'
#' @return
#' @export
#'
#' @examples
OA_full_process <- function(wide_data){
  
  # Import the stuff we need whose location won't change
  my_tolerances <- import_tolerances_csv()
  print("Read in tolerances")
  long_known_outliers <- import_known_outliers_csv()
  wide_known_outliers <- OA_get_outliers_wide_form(long_known_outliers, myprefix = "Corrected_")
  print("Read in tolerances")
  
  # Run the software
  provider_data_reduced <- OA_reduce_CT_data(wide_data)
  print("Reduced complete")
  long_outliers <- OA_get_outliers(provider_data_reduced, my_tolerances)
  print("Outliers complete")
  wide_outliers <- OA_get_outliers_wide_form(long_outliers)
  print("Wide outliers complete")
  wide_flagged <- OA_get_outliers_flagged(wide_data, wide_outliers)
  print("Wide flagged complete")
  wide_corrected <- OA_get_corrections(wide_data, wide_flagged, wide_known_outliers)
  print("Wide corrected complete")
  
  return(wide_corrected)
}