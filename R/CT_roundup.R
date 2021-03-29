#' Import, clean up, and export multiple raw conductivity-temperature, CT, data files from the HOBO CT Logger
#'
#' This function loads all raw .csv files within the data.path folder.
#' It removes the first row of each file containing logger information, then retains column headers.
#' Unnecessary columns are removed, and the remaining columns are renamed as needed for simplicity and consistency.
#' Any rows containing NA's are dropped.
#'
#' @param data.path Path to the input files
#' @param output.path Path for the ouput files
#' @param tf_write Logical parameter indicating whether to save output files in an output folder. No default.
#' @param tf_recursive Logical parameter indicating whether to search within folders at the data.path. Default = FALSE
#' @return For every imported CT data file, one tidied file with temperature-compensated conductance is exported and returned
#' @export
CT_roundup<-function(data.path, output.path, tf_write, tf_recursive = FALSE){

  path<-data.path

  # Create a list of all files within the directory folder
  file.names.Cal<-basename(list.files(path, pattern = c("csv$", recursive = tf_recursive))) #list all csv file names in the folder and subfolders

  # For each listed file:
  ## Load the dataframe,
  ## Tidy the data,
  ## Create a column for temperature-compensated specific conductance, and
  ## Export the new file to an output folder
  for(i in 1:length(file.names.Cal)) {
    Data_ID<-file.names.Cal[[i]]

    file.names<-basename(list.files(path, pattern = c(Data_ID, "csv$", recursive = tf_recursive))) #list all csv file names in the folder and subfolders

    condCal <- file.names %>%
      purrr::map_dfr(~ readr::read_csv(file.path(path, .), skip=1, col_names=T)) # read all csv files at the file path, skipping 1 line of metadata

    condCal<-condCal %>%
      dplyr::select(contains('Date'), contains("High Range"), contains("Temp")) %>%
      dplyr::mutate(List.ID=Data_ID) %>%  # add column for file ID
      dplyr::rename(date=contains("Date"),
                    TempInSitu=contains("Temp"),
                    E_Conductivity=contains("High Range")) %>%
      tidyr::drop_na() %>%
      tidyr::separate(col = 'file.id', into = c('file.id',NA), sep = ".csv", remove = T) # remove the '.csv'

    # Format date and time
    condCal$date <- condCal$date %>%
      readr::parse_datetime(format = "%m/%d/%y %H:%M:%S %p", # Convert 'date' to date and time vector type
                            na = character(),
                            locale = default_locale(),
                            trim_ws = TRUE)

    ############################################################
    ### Nonlinear Temperature Compensation
    ############################################################

    # https://www.aqion.de/site/112
    condCal<-condCal %>%
      mutate(A = (1.37023 * (TempInSitu - 20)) + 8.36 * (10^(-4) * ((TempInSitu - 20)^2))) %>%
      mutate(B = 109 + TempInSitu) %>%
      mutate(Sp_Conductance = 0.889 * (10^(A/B)) * E_Conductivity) %>%
      dplyr::select(-c(A,B))

    listofdfs[[i]] <- conCal # save your dataframes into the list

    if(tf_write == TRUE) {
      write.csv(condCal, paste0(output.path,'/',Data_ID,'_SpConductance.csv'))
    }
  }
}
