##### script medium exercise #####

# Goal: 
# You want to know what happenned to the files which were EU legislative priorities in 2023-2024
# So you want to know what is the state of play for different legislation packages. 

# Let's first load all the packages essential for web scraping
library(rvest)
library(xml2)
library(httr)
library(stringr)
library(dplyr)

# 1. We are going to list all relevant procedures. 
# In the EU, once proposed each piece of legislation has a procedure number, including 'COD'. 
# Go to this page that lists the legislative files which were priorities for 2023-24: https://oeil.secure.europarl.europa.eu/oeil/popups/thematicnote.do?id=41380&l=en 
# You have to scrape this page to obtain a dataframe, in which there will be the title, number and url towards the specific page of each procedure

# First, read the html page
url <- "https://oeil.secure.europarl.europa.eu/oeil/popups/thematicnote.do?id=41380&l=en"
page <- read_html(url)

# Select all the names of the files
name_procedure <- html_elements (page,css=".ep-table-cell-xxl .ep_name") %>%
  html_text()
# Clean these names: remove tabs and special characters, and extra blank space. 
# You can use two functions from the stringr package: str_replace_all and str_squish
# Don't forget to print the resulting vector to check the result!
clean_name_procedure <- str_replace_all(name_procedure, "\t", "") %>%
  str_replace_all(., "\r", "") %>%
  str_replace_all(., "\n", "") %>%
  str_squish()

# Select all the procedure numbers of the files
number_procedure <- html_elements (page,css=".ep-table-cell-s a") %>%
  html_text()
# Like previously, don't forget to clean white space and special character
clean_number_procedure <- str_replace_all(number_procedure, "\t", "") %>%
  str_replace_all(., "\r", "") %>%
  str_replace_all(., "\n", "") %>%
  str_squish()

# Select all the links towards the procedure pages
link_procedure <- html_elements (page,css=".ep-table-cell-s a") %>%
  html_attr("href")
# Check one or two links - can you copy paste them in a browser and access the page?
# Is there anything missing in the URL? How could you fix this?
# Search manually a procedure here to find out how urls are made: https://oeil.secure.europarl.europa.eu/oeil/search/search.do?searchTab=y
# Tip: you can use the paste function 
clean_link_procedure <- paste("https://oeil.secure.europarl.europa.eu", link_procedure, sep="")

# Put all the information in a data frame
# Tip: use the data.frame function
my_procedures <- data.frame(clean_name_procedure,
                            clean_number_procedure,
                            clean_link_procedure)
# Tip: you may want to rename the columns at this point. For this you can use the function colnames. 
colnames(my_procedures) <- c("name",
                             "number",
                             "link")

# 2. Now you have listed the names of all relevant procedures, and the links to access them.
# You are only interest in procedures having COD in their name. 
# Create a data frame that contains only procedure with 'COD' in their number. 
# Tip: you can use the the grepl function.
my_procedures2 <- my_procedures[grepl("COD", my_procedures$number),]
# Check whether you now have the right number of procedures. 
# You should now have 160. 

# 4. Take this single URL link: https://oeil.secure.europarl.europa.eu/oeil/popups/ficheprocedure.do?reference=2021/0433(CNS)&l=en
# It is one of the ones you have listed
# In a separate data frame (which will have only one line), scrape
# - the date at which the legislative file was published
# - the date at which the final act was published in the official journal

# Let's read the page. 
page2 <- read_html("https://oeil.secure.europarl.europa.eu/oeil/popups/ficheprocedure.do?reference=2021/0433(CNS)&l=en")

# 4.b. Date of publication of legislative proposal
# Tip: first, select all the dates. 
# Then, select the names of all the events to which they correspond. 
# Finally, select your event of interest with grepl (use for example "proposal")

# Select all key dates
table_key_dates <- html_nodes(page2,
                              "#key_events-data .ep-table-cell-s") %>%
  html_text()

# Select all the key events
table_key_events <- html_nodes(page2,
                              "#key_events-data .ep-table-cell-xl") %>%
  html_text()
# Select the right date and clean your character string!
date_publication_proposal <- table_key_dates[table_key_events %>% #taking the right element
                                        grep("proposal",.)] %>%
  gsub ("\r","",.) %>%
  gsub("\n","",.) %>%
  gsub("\t","",.)

# 4.c Date of publication in the Official Journal
date_publication_official_journal <- table_key_dates[table_key_events %>% #taking the right element
                                               grep("Official",.)] %>%
  gsub ("\r","",.) %>%
  gsub("\n","",.) %>%
  gsub("\t","",.)

# 4.d. Put everything in a data frame 
my_first_scrap <- data.frame(key_players_rapporteur,
                             date_publication_proposal,
                             date_publication_official_journal)

####### Date of adoption or publication in the official journal??? (there are not much processes already completed####

# 5. Write a function that automates the scraping you did at question (generalize your code!). 
# For each URL, the function has to scrape the same three pieces of information. 
# Run that function and store the results in a data frame that also contains the number of the procedures and their URLs.

# 5.a Write the function. You can find explanations about creating a function in R here: 
# https://www.r-bloggers.com/2022/04/how-to-create-your-own-functions-in-r/

# Tip: In the function, you can use the matrix function to bind the different information together
# At the end,write return(created_matrix). This indicates to R that it is the output of the function. 

# Tip: You can clean the two character strings at the same time. You can use the function mutate for this. 
# Otherwise, you can just clean them one after the other. 

# Tip: some of the info you are looking for may not be on all pages. 
# use the function length to check it, and write "To check" if the information is not found. 
# Why is some info missing on some pages?

MY_SCRAPER <- function (x) {
  my_page <- read_html(x)
  
  # Select all the key events
  table_key_events <- html_nodes(my_page,
                                 "#key_events-data .ep-table-cell-xl") %>%
    html_text()
  
  # Select all the key dates
  table_key_dates <- html_nodes(my_page,
                                "#key_events-data .ep-table-cell-s") %>%
    html_text()
  
  # Date publication proposal
  date_publication_proposal <- table_key_dates[table_key_events %>% #
                                                 grep("proposal",.)] 
  
  # Date of publication in the Official Journal
  date_publication_official_journal <- table_key_dates[table_key_events %>% #taking the right element
                                                         grep("adopted",.)] 
  date_publication_official_journal <- ifelse(length(date_publication_official_journal) != 1,
                                              "To check",
                                              date_publication_official_journal) 
  
  # Binding the different pieces of information together
  my_data <- data.frame(date_publication_proposal,
                      date_publication_official_journal) %>%
    mutate(across(everything(), ~ gsub("\r", "", gsub("\n", "", gsub("\t", "", .)))))
  
  
  return(my_data)
}  


# 5.b Test the function. To do this, run the function on the first element of the links vector.
MY_SCRAPER(my_procedures2$link[129])

# 5.c Run the function. Use as input the list of URLs that you made previously. 
# You will need to use the lapply() function to apply you function to this list of URLs.
# Once you ran the function, you need to bind the results together. 
# Otherwise, you just have a list of separate data frames for each procedure. You can use do.call. 
results <- lapply (my_procedures2$link, MY_SCRAPER)
my_results_clean <- do.call("rbind",results)%>%as.data.frame

# 5.d Bind the result of your scraping with you original dataframe containing the links. 
# Tip: we can do this because here, we are sure that our input links (and procedures) are in the same order as the results.
# Otherwise, scrape the reference procedure on the page to make sure. 
my_results_complete <- cbind(my_procedures2,
                             my_results_clean)

# 6. Calculate the duration of each legislative process (in days) in a new column of your data frame. 
# Calculate it as the number of days between the legislative proposal and the publication in the official journal.
# Tip: you have to tell R that you are working with dates. 
# Search which function allows to do this!
my_results_complete$date_publication_proposal <- as.Date(my_results_complete$date_publication_proposal,
                                                         "%d/%m/%Y")
my_results_complete$date_publication_official_journal <- as.Date(my_results_complete$date_publication_official_journal,
                                                                 "%d/%m/%Y")
my_results_complete$duration_legislative_process<- my_results_complete$date_publication_official_journal - 
  my_results_complete$date_publication_proposal
# Let's look for the longer process. 
max(my_results_complete$duration_legislative_process, na.rm = TRUE)
