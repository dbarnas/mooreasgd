#' Clean up raw conductivity-temperature, CT, data file from the HOBO CT Logger
#'
#' This function loads a raw CT file. It searches a directory for the correct logger's serial number,
#' plucks out the correct file or files and stacks all related files into one loaded csv.
#' It removes the first row of each relevant file containing logger information, then retains column headers.
#' Unnecessary columns are removed, and the remaining columns are renamed as needed for simplicity and consistency.
#' Any rows containing NA's are dropped.
#'
#' @param path Path to the input files
#' @param WL_serial Logger serial number used in naming the input file
#' @param recursive_tf Logical parameter indicating whether to search within folders at the file path. Default = FALSE
#' @return A cleaned dataframe of pH logger data
#' @export
CT_cleanup <- function(path, ct_serial, recursive_tf = FALSE) {

  file.names.Cal<-basename(list.files(path, pattern = c(ct_serial,"csv$", recursive = recursive_tf))) #list all csv file names in the folder and subfolders

  condCal <- file.names.Cal %>%
    purrr::map_dfr(~ readr::read_csv(file.path(path, .), skip=1, col_names=T)) # read all csv files at the file path, skipping 1 line of metadata

  condCal<-condCal %>%
    dplyr::select(contains('Date'), contains(ct_serial), contains("High Range"), contains("Temp")) %>% # Filter specified probe by Serial number
    dplyr::mutate(Serial=paste0("CT_",ct_serial)) %>% # add column for CT serial number
    dplyr::rename(date=contains("Date"),
                  TempInSitu=contains("Temp"),
                  E_Conductivity=contains("High Range")) %>%
    tidyr::drop_na()

  condCal$date <- condCal$date %>%
    readr::parse_datetime(format = "%m/%d/%y %H:%M:%S %p", # Convert 'date' to date and time vector type
                          na = character(),
                          locale = default_locale(),
                          trim_ws = TRUE)

  return(condCal)
}
