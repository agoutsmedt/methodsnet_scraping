##### script medium exercise #####

# Goal: 
# You want to know what happened to the files which were EU legislative priorities in 2023-2024
# So you want to know what is the state of play for different legislation packages. 

# Let's first load packages we will need
install.packages("pacman")
pacman::p_load(tidyverse, 
               plyr,
               rvest, 
               polite)

# 1. We are going to list all relevant procedures
# In the EU, once proposed each piece of legislation has a procedure number, including 'COD'. 
# Go to this page that lists the legislative files which were priorities for 2023-24: https://oeil.secure.europarl.europa.eu/oeil/popups/thematicnote.do?id=41380&l=en 
# You have to scrape this page to obtain a data frame, in which there will be the title, number and url towards the specific page of each procedure

# Let's be polite! 
website_ep <- "https://oeil.secure.europarl.europa.eu"
session <- polite::bow(website_ep, 
                       user_agent = "polite R package - used for academic training by Marine Bardou (marine.bardou@uclouvain.be)")

# First, read the html page
url <- "https://oeil.secure.europarl.europa.eu/oeil/popups/thematicnote.do?id=41380&l=en"
page <- read_html(url)

# Select all the names of the files
name_procedure <- html_elements(page,css = ".ep-table-cell-xxl .ep_name") %>%
  html_text2() # Extract the text 
# html_text2() allows also to clean by removing tabs and extra blank spaces. 
# It is as well possible to withdraw specific characters like the ones for tabs. 

# We still have some "\r" which is a code for returning to the beginning of the line. 
# You can use two functions from the stringr package: str_remove_all() to remove all the "\r" and str_squish() to remove all the resulting extra white spaces.
# Don't forget to print the resulting vector to check the result!
clean_name_procedure <- name_procedure %>%
  str_remove_all(., "\r") %>%
  str_squish()

# Select all the procedure numbers of the files
clean_number_procedure <- html_elements(page, css = ".ep-table-cell-s .ep_name") %>%
  html_text2() %>% 
  str_remove_all(., "\r") %>% # Remove all the line returns
  str_squish() # Remove all the extra white spaces

# Select all the links towards the procedure pages
link_procedure <- html_elements(page, css = ".ep-table-cell-s a") %>%
  html_attr("href")

# Check one or two links - can you copy paste them in a browser and access the page?
# Is there anything missing in the URL? How could you fix this?
# Search manually a procedure here to find out how urls are made: https://oeil.secure.europarl.europa.eu/oeil/search/search.do?searchTab=y
# Tip: you can use the paste function 
clean_link_procedure <- paste0("https://oeil.secure.europarl.europa.eu", link_procedure)

# Check the length of the vectors. They should all be the same.
if(length(clean_name_procedure) == length(clean_number_procedure) & length(clean_number_procedure) == length(link_procedure)){
  message("All vectors have the same length")
}

# Put all the information in a tibble (the tidyverse format for data frames)
my_procedures <- tibble(name = clean_name_procedure,
                        number = clean_number_procedure,
                        link = clean_link_procedure)

# 2. Now you have listed the names of all relevant procedures, and the links to access them
# You are only interest in procedures having COD in their name. 
# Create a data frame that contains only procedure with 'COD' in their number. 
# Tip: you can use the the str_detect() function of stringr.
my_procedures_cod <- my_procedures %>% 
  filter(str_detect(number, "COD"))
# Check whether you now have the right number of procedures. 
# You should now have 160. 

# 3 Take this single URL link: https://oeil.secure.europarl.europa.eu/oeil/popups/ficheprocedure.do?reference=2021/0433(CNS)&l=en
# It is one of the ones you have listed
# In a separate data frame (which will have only one line, and three columns), scrape:
# - the status of the procedure (i.e. at which stage it is)
# - the date at which the legislative file was published
# - the date at which the EP took its decision 

# Let's read the page. 
procedure_page <- read_html("https://oeil.secure.europarl.europa.eu/oeil/popups/ficheprocedure.do?reference=2021/0433(CNS)&l=en")

# 3.a. Status of the procedure (observe: here the css selector is very "human readable")
procedure_status <- html_elements(procedure_page,
                              ".procedure-status") %>%
  html_text2()

# 3.b. Date of publication of legislative proposal
# Tip: first, select all the dates. 
# Then, select the names of all the events to which they correspond. 
# Finally, select your event of interest with grepl (use for example "proposal")

# Select all key dates
key_dates <- html_elements(procedure_page,
                              "#key_events-data .ep-table-cell-s .ep_name") %>%
  html_text() 

# Select all the key events
key_events <- html_elements(procedure_page,
                              "#key_events-data .ep-table-cell-xl .ep_name") %>%
  html_text() 

# Select the right date and clean your character string by selecting the right row
date_publication_proposal <- key_dates[str_which(key_events, "proposal")] 

# 3.c Date of EP decision
date_EP_decision <- key_dates[str_which(key_events, "Decision")]

# 3.d. Put everything in tibble
my_first_scrap <- tibble(procedure_status = procedure_status,
                         date_publication_proposal = date_publication_proposal,
                         date_EP_decision = date_EP_decision)

# 4. Write a function that automates the scraping you did at question 3. (generalize your code!). 
# For each URL, the function has to scrape the same three pieces of information. 
# Run that function and store the results in a data frame that also contains the number of the procedures and their URLs.

# 5.a Write the function. You can find explanations about creating a function in R here: 
# https://www.r-bloggers.com/2022/04/how-to-create-your-own-functions-in-r/

# Tip: In the function, you can use the tibble() function to bind the different information together
# At the end, write return(created_data_frame). This indicates to R that it is the output of the function. 

# Tip: some of the info you are looking for may not be on all pages. 
# Use the function length() to check whether your code found something, and write "To check" if the information is not found. 
# Why is some info missing on some pages?

scraping_all_pages <- function (url) {
  my_page <- read_html(url)
  Sys.sleep(session$delay) # Be polite!
  
  # Select the procedure status
  procedure_status <- html_elements(my_page,
                                 ".procedure-status") %>%
    html_text2()
  
  # Select all the key events
  key_dates <- html_elements(my_page,
                                 "#key_events-data .ep-table-cell-s .ep_name") %>%
    html_text()
  
  # Select all the key dates
  key_events <- html_elements(my_page,
                                "#key_events-data .ep-table-cell-xl .ep_name") %>%
    html_text()
  
  # Date publication proposal
  date_publication_proposal <- key_dates[str_which(key_events, "proposal")] 
  
  # Date of EP decision
  date_EP_decision <- key_dates[str_which(key_events, "Decision")]
  
  date_EP_decision <- ifelse(length(date_EP_decision) != 1,
                                              "To check",
                                              date_EP_decision) 
  
  # Binding the different pieces of information together
  my_data <- tibble(link = url, # the url will serve as a reference to join the new dataset with my_procedures_cod
                    procedure_status = procedure_status,
                    date_publication_proposal = date_publication_proposal,
                    date_EP_decision = date_EP_decision)
  
  return(my_data)
}  


# 5.b Test the function. To do this, run the function on one of the links (only one!)
scraping_all_pages(my_procedures_cod$link[129])

# 5.c Run the function. Use as input the list of URLs that you made previously. 
# You will need to use the lapply() function to apply you function to this list of URLs.
# We can test this on the ten first links
results <- lapply(my_procedures_cod$link[1:10], scraping_all_pages)
# Once you ran the function, you need to bind the results together by "horizontally" (i.e. above each other)
# Otherwise, you just have a list of separate data frames for each procedure. 
# Which R function allows you to do this?
# You can use bind_rows() from dplyr to aggregate all the tibbles.
my_results_clean <- results %>% 
  bind_rows()

# 5.d Bind the results of your scraping with your original dataframe containing the links. 
# Tip: we can do a bind_col() because here, we are sure that our input links (and procedures) are in the same order as the results.
# Otherwise, more generally, it is preferable to have a common identifier in each table and to use a join function.
my_results_complete <- my_procedures_cod %>% 
  join(my_results_clean, by = "link")


# 6. Calculate the duration of each legislative process (in days) in a new column of your data frame. 
# Calculate it as the number of days between the legislative proposal and the EP decision.
# Tip: you have to tell R that you are working with dates. 
# Search which function allows to do this!
my_results_complete$date_publication_proposal <- as.Date(my_results_complete$date_publication_proposal,
                                                         "%d/%m/%Y")
my_results_complete$date_EP_decision <- as.Date(my_results_complete$date_EP_decision,
                                                                 "%d/%m/%Y")
# What happens to the cases where the date of EP decision is not yet available?
# Pay attention when calculating the duration!
my_results_complete$duration_legislative_process<- my_results_complete$date_EP_decision - 
  my_results_complete$date_publication_proposa
# Let's look for the longer process. 
max(my_results_complete$duration_legislative_process, 
    na.rm = TRUE)
my_results_complete$link[which.max(my_results_complete$duration_legislative_process)]
# So When did the procedure start?
