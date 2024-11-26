# Install packages required through full analysis
# Installing specific versions except where it threw an error

if(!require(remotes)) install.packages('remotes')
library(remotes)

remotes::install_version('dplyr',
                         version = '1.1.4')
install.packages('fields') # version 16.3
remotes::install_version('GGally',
                         version = '2.2.1')
remotes::install_version('ggplot2',
                         version = '3.5.1')
remotes::install_version('raster',
                         version = '3.6.26')
install.packages('sf') # version 1.0.19
remotes::install_version('sfheaders',
                         version = '0.4.4')
remotes::install_version('soilDB',
                         version = '2.8.4')
install.packages('terra') # version 1.7.78
remotes::install_version('tidyr',
                         version = '1.3.1')
