#' Clean up raw conductivity-temperature, CT, data file from the HOBO CT Logger
#'
#' This function loads a raw CT file. It searches a directory for the correct logger's serial number,
#' plucks out the correct file or files and stacks all related files into one loaded csv.
#' It removes the first row of each relevant file containing logger information, then retains column headers.
#' Unnecessary columns are removed, and the remaining columns are renamed as needed for simplicity and consistency.
#' Any rows containing NA's are dropped.
#'
#' @param path Path to the input files
#' @param ct_serial Logger serial number used in naming the input file
#' @param tf_write Logical parameter indicating whether to save output files in an output folder. No default.
#' @param recursive_tf Logical parameter indicating whether to search within folders at the file path. Default = FALSE
#' @return A cleaned dataframe of CT logger data
#' @export
CT_cleanup <- function(path, ct_serial, tf_write = FALSE, recursive_tf = FALSE) {

  file.names.Cal<-basename(list.files(path, pattern = c(ct_serial,"csv$", recursive = recursive_tf))) #list all csv file names in the folder and subfolders

  condCal <- file.names.Cal %>%
    purrr::map_dfr(~ readr::read_csv(file.path(path, .), skip=1, col_names=T)) # read all csv files at the file path, skipping 1 line of metadata

  condCal<-condCal %>%
    dplyr::select(contains('Date'), contains(ct_serial), contains("High Range"), contains("Temp")) %>% # Filter specified probe by Serial number
    dplyr::mutate(Serial=paste0("CT_",ct_serial)) %>% # add column for CT serial number
    dplyr::rename(date=contains("Date"),
                  TempInSitu=contains("Temp"),
                  E_Conductivity=contains("High Range")) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(date = lubridate::mdy_hms(date))

  # condCal$date <- condCal$date %>%
  #   readr::parse_datetime(format = "%m/%d/%y %H:%M:%S %p", # Convert 'date' to date and time vector type
  #                         na = character(),
  #                         locale = default_locale(),
  #                         trim_ws = TRUE)



  ############################################################
  ### Nonlinear Temperature Compensation
  ############################################################

  # # https://www.aqion.de/site/112
  # condCal<-condCal %>%
  #   mutate(A = (1.37023 * (TempInSitu - 20)) + 8.36 * (10^(-4) * ((TempInSitu - 20)^2))) %>%
  #   mutate(B = 109 + TempInSitu) %>%
  #   mutate(Sp_Conductance = 0.889 * (10^(A/B)) * E_Conductivity) %>%
  #   dplyr::select(-c(A,B)) # remove intermediate columns

  # conditional write.csv at output path
  if(tf_write == TRUE) {
    write.csv(condCal, paste0(output.path,'/',Data_ID,'_tidy.csv'))
  }

  return(condCal) # return dataframe
}
