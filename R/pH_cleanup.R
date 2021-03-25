#' Clean up raw pH data file from the HOBO pH Logger
#'
#' This function loads a raw pH file. It searches a directory for the correct logger's serial number,
#' plucks out the correct file or files and stacks all related files into one loaded csv.
#' It removes the first two rows of each relevant file containing logger information, then retains column headers.
#' Unnecessary columns are removed, and the remaining columns are renamed as needed for simplicity and consistency.
#' Any rows containing NA's are dropped.
#'
#' @param path Path to the input files
#' @param pH_serial Logger serial number used in naming the input file
#' @param recursive_tf Logical parameter indicating whether to search within folders at the file path. Default = FALSE
#' @return A cleaned dataframe of pH logger data
#' @export
pH_cleanup <- function(path, pH_serial, recursive_tf = FALSE) {

  # list all csv file names in the folder and subfolders
  file.names.Cal<-basename(list.files(path, pattern = c(pH_serial,"csv$", recursive = recursive_tf)))

  # read all csv files at the file path, skipping 1 line of metadata
  pHLog <- file.names.Cal %>%
    purrr::map_dfr(~ readr:::read_csv(file.path(path, .), skip=2, col_names=T))

  # clean file: only select useful columns, create serial# column, rename columns
  pHLog<-pHLog %>%
    dplyr::select(contains('Date'), contains(Serial), contains("temp"), mV, pH) %>%
    dplyr::mutate(Serial=paste0("pH_",Serial)) %>%
    dplyr::rename(date=contains("Date"),
                  TempInSitu=contains("Temp")) %>%
    tidyr::drop_na()

  return(pHLog)
}
