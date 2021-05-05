#' Import, clean up, and export multiple raw conductivity-temperature, CT, data files from the HOBO CT Logger
#'
#' This function loads all raw .csv files within the data.path folder.
#' It removes the first row of each file containing logger information, then retains column headers.
#' Unnecessary columns are removed, and the remaining columns are renamed as needed for simplicity and consistency.
#' Any rows containing NA's are dropped.
#'
#' @param data.path Path to the input files
#' @param output.path Path for the output files
#' @param wl.serial Character string containing serial ID for a particular probe.  Used to filter from full dataset before returning final dataframe
#' @param tf_write Logical parameter indicating whether to save output files in an output folder. No default.
#' @param tf_recursive Logical parameter indicating whether to search within folders at the data.path. Default = FALSE
#' @return For every imported CT data file, one tidied file with temperature-compensated conductance is exported and returned
#' @export
CT_roundup<-function(data.path, output.path, wl.serial = FALSE, tf_write = FALSE, tf_recursive = FALSE){

  # Create a list of all files within the directory folder
  file.names.wl<-basename(list.files(data.path, pattern = c("Pressure","csv$"), recursive = tf_recursive)) #list all csv file names in the folder and subfolders

  # Create an empty dataframe to store all subsequent tidied df's into
  full_df <- tibble::tibble(
    date = as_datetime(NA),
    List.ID = as.character(),
    AbsPressure=as.numeric(),
    Depth=as.numeric(),
    TempInSitu = as.numeric())

  # For each listed file:
  ## Load the dataframe,
  ## Tidy the data,
  ## Create a column for temperature-compensated specific conductance, and
  ## Export the new file to an output folder
  for(i in 1:length(file.names.wl)) {
    Data_ID<-file.names.wl[[i]]

    file.names<-basename(list.files(data.path, pattern = c(Data_ID, "csv$"), recursive = tf_recursive)) #list all csv file names in the folder and subfolders

    depthLog <- file.names %>%
      purrr::map_dfr(~ readr::read_csv(file.path(data.path, .), skip=1, col_names=T)) # read all csv files at the file path, skipping 1 line of metadata and bind together

    depthLog<-depthLog %>%
      dplyr::select(contains('Date'), contains("Temp"),contains("Abs Pres"), contains("Water Level")) %>%
      dplyr::mutate(List.ID=Data_ID) %>%  # add column for file ID
      dplyr::rename(date=contains("Date"),
                    TempInSitu=contains("Temp"),
                    AbsPressure=contains("Abs Pres"),
                    Depth=contains("Water Level")) %>%
      tidyr::drop_na() %>%
      tidyr::separate(col = 'List.ID', into = c('List.ID',NA), sep = ".csv", remove = T) %>%  # remove the '.csv'
      dplyr::mutate(date = lubridate::mdy_hms(date))

    # Format date and time
    # condCal$date <- condCal$date %>%
    #   readr::parse_datetime(format = "%m/%d/%y %H:%M:%S %p", # Convert 'date' to date and time vector type
    #                         na = character(),
    #                         locale = default_locale(),
    #                         trim_ws = TRUE)



    if(tf_write != FALSE) {
      write.csv(depthLog, paste0(output.path,'/',Data_ID,'_WaterLevel.csv'))
    }
  }

  if(wl.serial!=FALSE) {
    pattern <- grep(x = full_df$List.ID, pattern = wl.serial, value = TRUE)
    full_df <- full_df %>%
      dplyr::filter(List.ID == pattern[2])
  }

  full_df <- full_df %>%
    rename(Serial = List.ID)

  return(full_df) # return a list of dataframes
}
