################# Installing packages needed ################

#' Please run this script before the training session to install the required packages
if(length(grep("pacman", installed.packages())) == 0) install.packages("pacman")
library(pacman)

# Installing the needed packages in advance
p_load(tidyverse, # basic suite of packages
       glue, # useful for building string (notably for url)
       rvest, # scraping and manipulating html pages
       polite, # scraping ethically
       RSelenium) # scraping by interacting with RSelenium
