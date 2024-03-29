#' Import, clean up, and export multiple raw conductivity-temperature, CT, data files from the HOBO CT Logger
#'
#' This function loads all raw .csv files within the data.path folder.
#' It removes the first row of each file containing logger information, then retains column headers.
#' Unnecessary columns are removed, and the remaining columns are renamed as needed for simplicity and consistency.
#' Any rows containing NA's are dropped.
#'
#' @param data.path Path to the input files
#' @param output.path Path for the output files
#' @param path.pattern Character string or object referencing a character string that identifies CT logger data within the specified file path
#' @param tf.write Logical parameter indicating whether to save output files in an output folder. Default = FALSE.
#' @param tf.recursive Logical parameter indicating whether to search within folders at the data.path. Default = FALSE
#' @param hobo.cal Logical parameter indicating whether the .hobo file has been pre-calibrated using HOBOware software and exported as a processed .csv. Default = FALSE
#' @param cond.range Character string or object indicating whether conductivity was recorded in High ("high"), Low ("low"), or both ("highlow") ranges. Default = "high"
#' @return For every imported CT data file, one tidied file with temperature-compensated conductance is exported and returned
#' @export
CT_cleanup<-function(data.path, output.path, path.pattern, tf.write = FALSE, tf.recursive = FALSE, hobo.cal = FALSE, cond.range = "high"){

  if(hobo.cal == FALSE){
  # Create a list of all files within the directory folder
  file.names.full<-basename(list.files(data.path, pattern = path.pattern, recursive = tf.recursive)) #list all csv file names in the folder and subfolders

  # Create an empty dataframe to store all subsequent tidied df's into
  full_df <- tibble::tibble(
    date = lubridate::as_datetime(NA),
    LoggerID = as.character(),
    TempInSitu = as.numeric(),
    E_Conductivity = as.numeric())

  # For each listed file:
  ## Load the dataframe,
  ## Tidy the data,
  ## Create a column for temperature-compensated specific conductance, and
  ## Export the new file to an output folder
  for(i in 1:length(file.names.full)) {
    Data_ID<-file.names.full[[i]]

    file.names<-basename(list.files(data.path, pattern = c(Data_ID, "csv$"), recursive = tf.recursive)) #list all csv file names in the folder and subfolders

    condCal <- file.names %>%
      purrr::map_dfr(~ readr::read_csv(file.path(data.path, .), skip=1, col_names=T)) # read all csv files at the file path, skipping 1 line of metadata and bind together

    Data_ID<-stringr::str_split_fixed(Data_ID,".csv",2)[1,1] # remove ".csv" from the file name

    if(cond.range == "high"){
    condCal<-condCal %>%
      dplyr::select(contains('Date'), contains("High Range"), contains("Temp")) %>%
      dplyr::mutate(LoggerID=Data_ID) %>%  # add column for file ID
      dplyr::rename(date=contains("Date"),
                    TempInSitu=contains("Temp"),
                    E_Conductivity=contains("High Range")) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(date = if_else(str_length(date) >= 15, lubridate::mdy_hms(date), lubridate::mdy_hm(date))) # if date includes seconds, mdy_hms, else mdy_hm
    } else if(cond.range == "low") {
      condCal<-condCal %>%
        dplyr::select(contains('Date'), contains("Low Range"), contains("Temp")) %>%
        dplyr::mutate(LoggerID=Data_ID) %>%  # add column for file ID
        dplyr::rename(date=contains("Date"),
                      TempInSitu=contains("Temp"),
                      E_Conductivity=contains("Low Range")) %>%
        tidyr::drop_na() %>%
        dplyr::mutate(date = if_else(str_length(date) >= 15, lubridate::mdy_hms(date), lubridate::mdy_hm(date))) # if date includes seconds, mdy_hms, else mdy_hm
    } else if(cond.range == "highlow") {
      condCal<-condCal %>%
        dplyr::select(contains('Date'), contains("High Range"), contains("Low Range"), contains("Temp")) %>%
        dplyr::mutate(LoggerID=Data_ID) %>%  # add column for file ID
        dplyr::rename(date=contains("Date"),
                      TempInSitu=contains("Temp"),
                      E_Conductivity_High=contains("High Range"),
                      E_Conductivity_Low=contains("Low Range")) %>%
        tidyr::drop_na() %>%
        dplyr::mutate(date = if_else(str_length(date) >= 15, lubridate::mdy_hms(date), lubridate::mdy_hm(date))) # if date includes seconds, mdy_hms, else mdy_hm
    }

    full_df <- full_df %>%
      dplyr::full_join(condCal) # save your dataframes into a larger df

    # conditional write.csv at output path
    if(tf.write == TRUE) {
      if(exists('output.path') == T){
      write_csv(condCal, paste0(output.path,'/',Data_ID,'_tidyCT.csv'))
      } else if(exists('output.path') == F) {
      write_csv(condCal, paste0(data.path,'/',Data_ID,'_tidyCT.csv'))
      }
    }
   }
} else { # end of if(hobo.cal == FALSE)

    # Create a list of all files within the directory folder
  file.names.full<-basename(list.files(data.path, pattern = path.pattern, recursive = tf.recursive)) #list all csv file names in the folder and subfolders

    # Create an empty dataframe to store all subsequent tidied df's into
    full_df <- tibble::tibble(
      date = as_datetime(NA),
      LoggerID = as.character(),
      TempInSitu = as.numeric(),
      E_Conductivity = as.numeric(),
      Salinity_psu = as.numeric())

    # For each listed file:
    ## Load the dataframe,
    ## Tidy the data,
    ## Create a column for temperature-compensated specific conductance, and
    ## Export the new file to an output folder
    for(i in 1:length(file.names.full)) {
      Data_ID<-file.names.full[[i]]

      file.names<-basename(list.files(data.path, pattern = c(Data_ID, "csv$"), recursive = tf.recursive)) #list all csv file names in the folder and subfolders

      Data_ID<-stringr::str_split_fixed(Data_ID,".csv",2)[1,1] # remove ".csv" from the file name before renaming

      condCal <- file.names %>%
        purrr::map_dfr(~ readr::read_csv(file.path(data.path, .), skip=1, col_names=T)) # read all csv files at the file path, skipping 1 line of metadata and bind together
    if(cond.range == "high") {
      condCal<-condCal %>%
        dplyr::select(contains('Date'), contains("High Range"), contains("Temp"), contains("Salinity")) %>%
        dplyr::mutate(LoggerID=Data_ID) %>%  # add column for file ID
        dplyr::rename(date=contains("Date"),
                      TempInSitu=contains("Temp"),
                      E_Conductivity=contains("High Range"),
                      Salinity_psu=contains("Salinity")) %>%
        tidyr::drop_na() %>%
        dplyr::mutate(date = if_else(str_length(date) >= 15, lubridate::mdy_hms(date), lubridate::mdy_hm(date))) # if date includes seconds, mdy_hms, else mdy_hm
    } else if(cond.range == "low") {
      condCal<-condCal %>%
        dplyr::select(contains('Date'), contains("Low Range"), contains("Temp"), contains("Salinity")) %>%
        dplyr::mutate(LoggerID=Data_ID) %>%  # add column for file ID
        dplyr::rename(date=contains("Date"),
                      TempInSitu=contains("Temp"),
                      E_Conductivity=contains("Low Range"),
                      Salinity_psu=contains("Salinity")) %>%
        tidyr::drop_na() %>%
        dplyr::mutate(date = if_else(str_length(date) >= 15, lubridate::mdy_hms(date), lubridate::mdy_hm(date))) # if date includes seconds, mdy_hms, else mdy_hm
    } else if(cond.range == "highlow") {
      condCal<-condCal %>%
        dplyr::select(contains('Date'),contains("Low Range"), contains("High Range"), contains("Temp"), contains("Salinity")) %>%
        dplyr::mutate(LoggerID=Data_ID) %>%  # add column for file ID
        dplyr::rename(date=contains("Date"),
                      TempInSitu=contains("Temp"),
                      E_Conductivity_High=contains("High Range"),
                      E_Conductivity_Low=contains("Low Range"),
                      Salinity_psu=contains("Salinity")) %>%
        tidyr::drop_na() %>%
        dplyr::mutate(date = if_else(str_length(date) >= 15, lubridate::mdy_hms(date), lubridate::mdy_hm(date))) # if date includes seconds, mdy_hms, else mdy_hm
    }

      full_df <- full_df %>%
        dplyr::full_join(condCal) # save your dataframes into a larger df

      # conditional write.csv at output path
      if(tf.write == TRUE) {
        if(exists(output.path) == T){
          write_csv(condCal, paste0(output.path,'/',Data_ID,'_tidyCT.csv'))
        } else {
          write_csv(condCal, paste0(data.path,'/',Data_ID,'_tidyCT.csv'))
        }
      }
    }
} # end of if(hobo.csv == TRUE)

  return(full_df) # return full dataframe
}
