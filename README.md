# mooreasgd

A package to clean, organize, and analyze data from Onset HOBO loggers. Functions have been created for and tested on HOBO U24-002-C Conductivity-Temperature, HOBO MX2501 pH, and HOBO U20 Water Level loggers.

### Available functions
- CT_cleanup  
- CT_one_cal  
- CT_two_cal  
- WL_cleanup  
- pH_cleanup


### Install and Load _mooreasgd_

install.packages('devtools')
devtools::install_github("dbarnas/mooreasgd")
library(mooreasgd)
