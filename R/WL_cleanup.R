#' Clean up raw depth data file from the HOBO water level Logger
#'
#' This function loads a raw water level file. It searches a directory for the correct logger's serial number,
#' plucks out the correct file or files and stacks all related files into one loaded csv.
#' It removes the first row of each relevant file containing logger information, then retains column headers.
#' Unnecessary columns are removed, and the remaining columns are renamed as needed for simplicity and consistency.
#' Any rows containing NA's are dropped.
#'
#' @param data.path Path to the input files
#' @param output.path Path for the output files
#' @param wl.serial Logger serial number used in naming the input file
#' @param tf_write Logical parameter indicating whether to save output files in an output folder. No default.
#' @param recursive_tf Logical parameter indicating whether to search within folders at the file path. Default = FALSE
#' @return A cleaned dataframe of pH logger data
#' @export
WL_cleanup <- function(data.path, wl.serial, output.path, tf_write = FALSE, recursive_tf = FALSE) {

  file.names.wl<-basename(list.files(data.path, pattern = c(wl.serial,"csv$"), recursive = recursive_tf)) #list all csv file names in the folder and subfolders

  depthLog <- file.names.wl %>%
    purrr::map_dfr(~ readr::read_csv(file.path(data.path, .), skip=1, col_names=T)) # read all csv files at the file path, skipping 1 line of metadata

  depthLog<-depthLog %>%
    dplyr::select(contains('Date'), contains(wl.serial), contains("Temp"),contains("Abs Pres"), contains("Water Level")) %>% # Filter specified probe by Serial number
    dplyr::mutate(Serial=paste0("WL_",wl.serial)) %>% # add column for CT serial number
    dplyr::rename(date=contains("Date"),
                  TempInSitu=contains("Temp"),
                  AbsPressure=contains("Abs Pres"),
                  Depth=contains("Water Level")) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(date = lubridate::mdy_hms(date),
                  Pres_dbar = AbsPressure / 10) # Absolute Pressure recorded in kPa. 1 kPa = 10dbar

  # depthLog$date <- depthLog$date %>%
  #   readr::parse_datetime(format = "%m/%d/%y %H:%M:%S %p", # Convert 'date' to date and time vector type
  #                         na = character(),
  #                         locale = default_locale(),
  #                         trim_ws = TRUE)

  # conditional write.csv at output path
  if(tf_write == TRUE) {
    write.csv(depthLog, paste0(output.path,'/Pressure_',wl.serial,'_tidy.csv'))
  }

  return(depthLog)
}
