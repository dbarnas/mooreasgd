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
#' @param tf.write Logical parameter indicating whether to save output files in an output folder. Default = FALSE.
#' @param tf.recursive Logical parameter indicating whether to search within folders at the file path. Default = FALSE
#' @return For every imported pH data file, one tidied file is exported and returned
#' @export
pH_cleanup <- function(data.path, output.path, path.pattern, tf.write = FALSE, tf.recursive = FALSE) {

  # list all csv file names in the folder and subfolders
  file.names.full<-basename(list.files(data.path, pattern = path.pattern, recursive = tf.recursive)) #list all csv file names in the folder and subfolders


  # Create an empty dataframe to store all subsequent tidied df's into
  full_df <- tibble::tibble(
    date = as_datetime(NA),
    LoggerID = as.character(),
    mV = as.numeric(),
    pH = as.numeric(),
    TempInSitu = as.numeric())


  # For each listed file:
  ## Load the dataframe,
  ## Tidy the data,
  ## Create a column for temperature-compensated specific conductance, and
  ## Export the new file to an output folder
  for(i in 1:length(file.names.full)) {
    Data_ID<-file.names.full[[i]]

    file.names<-basename(list.files(data.path, pattern = c(Data_ID, "csv$"), recursive = tf.recursive)) #list all csv file names in the folder and subfolders

    pHLog <- file.names %>%
      purrr::map_dfr(~ readr::read_csv(file.path(data.path, .), skip=1, col_names=T)) # read all csv files at the file path, skipping 1 line of metadata and bind together

    Data_ID<-stringr::str_split_fixed(Data_ID,".csv",2)[1,1] # remove ".csv" from the file name

    pHLog<-pHLog %>%
      dplyr::select(contains('Date'), contains("Temp"), contains("mV"), contains("pH")) %>%
      dplyr::mutate(LoggerID=Data_ID) %>%  # add column for file ID
      dplyr::rename(date=contains("Date"),
                    TempInSitu=contains("Temp"),
                    mV=contains("mv"),
                    pH=contains("pH")) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(date = lubridate::mdy_hms(date))

    full_df <- full_df %>%
      rbind(pHLog)

    # conditional write csv at output path
    if(tf.write == TRUE) {
      if(exists('output.path') == T){
        write_csv(pHLog, paste0(output.path,'/',Data_ID,'_tidypH.csv'))
      } else if(exists('output.path') == F) {
        write_csv(pHLog, paste0(data.path,'/',Data_ID,'_tidypH.csv'))
      }
    }
  }


  return(full_df) # return a list of dataframes
}
