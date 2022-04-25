#' Clean up raw pH data file from the HOBO pH Logger
#'
#' This function loads a raw pH file. It searches a directory for the correct logger's serial number,
#' plucks out the correct file or files and stacks all related files into one loaded csv.
#' It removes the first two rows of each relevant file containing logger information, then retains column headers.
#' Unnecessary columns are removed, and the remaining columns are renamed as needed for simplicity and consistency.
#' Any rows containing NA's are dropped.
#'
#' @param data.path Path to the input files
#' @param output.path Path for the output files
#' @param path.pattern Character string or object referencing a character string that identifies pH logger data within the specified file path
#' @param pH.serial Logger serial number used in naming the input file
#' @param tf_write Logical parameter indicating whether to save output files in an output folder. Default = FALSE.
#' @param recursive_tf Logical parameter indicating whether to search within folders at the file path. Default = FALSE
#' @return For every imported pH data file, one tidied file is exported and returned
#' @export
pH_cleanup <- function(data.path, pH.serial, output.path, tf_write = FALSE, recursive_tf = FALSE) {

  # list all csv file names in the folder and subfolders
  file.names.Cal<-basename(list.files(data.path, pattern = c(pH.serial,"csv$", recursive = recursive_tf)))

  # read all csv files at the file path, skipping 1 line of metadata
  pHLog <- file.names.Cal %>%
    purrr::map_dfr(~ readr:::read_csv(file.path(data.path, .), skip=2, col_names=T))

  # clean file: only select useful columns, create serial# column, rename columns
  pHLog<-pHLog %>%
    dplyr::select(contains('Date'), contains(pH.serial), contains("temp"), mV, pH) %>%
    dplyr::mutate(Serial=paste0("pH_",pH.serial)) %>%
    dplyr::rename(date=contains("Date"),
                  TempInSitu=contains("Temp")) %>%
    tidyr::drop_na()

  # conditional write.csv at output path
  if(tf_write == TRUE) {
    write.csv(pHLog, paste0(output.path,'/pH_',pH.serial,'_tidy.csv'))
  }

  return(pHLog) # return a list of dataframes
}
