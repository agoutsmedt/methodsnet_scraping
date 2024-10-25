######## Scraping BCE Occasional papers ##########

# 25/10/2024 14h50 relecture Marine
# EDIT qu'est ce qui est de l'énoncé et du corrigé? 
# EDIT est ce qu'il ne ferait pas sens de tout mettre dans un dataframe?
# A aucun moment on ne créait l'objet all_data, donc j'en est déduis qu'il manquait. 
# Je l'ai ajouté.

# Loading necessary packages for web scraping and text analysis
install.packages(pacman)
pacman::p_load(tidyverse,     # Data manipulation
               rvest,         # Web scraping
               RSelenium,     # Automating browser interactions
               polite,        # Polite scraping (respecting robots.txt)
               tidytext)      # Text mining and analysis

# Define the ECB domain URL for scraping
ecb_domain <- "https://www.ecb.europa.eu/"
# Create a polite session object for the ECB domain to ensure responsible scraping
session <- polite::bow(ecb_domain, 
                       user_agent = "polite R package - used for academic training by Aurélien Goutsmedt (aurelien.goutsmedt@uclouvain.be)")
#### EDIT: ai changé le nom (c'était Marine Bardou)

# Here is the full path to the page listing occasional papers. 
ecb_pub_path <- str_c(ecb_domain, "press/research-publications/occasional-papers/html/index.en.html")
# How would you handle scraping this? What are the big issues. 

## Setting up RSelenium for browser automation ---------
# Start a Firefox browser session for interacting with the webpage
remDr <- rsDriver(browser = "firefox", port = 4444L, chromever = NULL)
browser <- remDr[["client"]]

# Navigate to the ECB publication page
browser$navigate(ecb_pub_path)

# Wait for any delay specified by the polite session (based on server settings)
Sys.sleep(session$delay) 

# Accept cookies banner (if exists) to continue scraping
browser$findElement("link text", "I do not accept the use of cookies")$clickElement()

# Let's have a first look at the code of the page
pub_page <- browser$getPageSource()[[1]] %>% 
  read_html()

# We extract the paper identifiers (number) and look at them
id <- pub_page %>% 
  html_elements(".category") %>% 
  html_text
### EDIT: id n'étit pas assigné à un objet (il était uniquement mentionné dan sla fonction print ci dessous)

print(id)
# Obviously, we just have the first papers, and we need to scroll down to get all the papers

## Manipulating the page to ensure all content is loaded ---------

# The first step is to scroll the page progressively to load dynamic content. Then, we need to click on the "Details" buttons to get the abstracts.

# Function to scroll the page progressively to load dynamic content
progressive_scroll <- function(browser, scroll_step = 100) {
  # Get initial scroll height of the page
  current_height <- browser$executeScript("return document.body.scrollHeight")
  
  # Set a variable for the scrolling position
  scroll_position <- 0
  
  # Continue scrolling until the end of the page
  while (scroll_position < current_height) {
    # Scroll down by 'scroll_step' pixels
    browser$executeScript(paste0("window.scrollBy(0,", scroll_step, ");"))
    
    Sys.sleep(runif(1, max = 0.2)) # Wait for the content to load (adjust this if the page is slower to load)
    scroll_position <- scroll_position + scroll_step # Update the scroll position
    current_height <- browser$executeScript("return document.body.scrollHeight") # Get the updated scroll height after scrolling (in case more content is loaded)
  }
}

# Scroll the ECB page to ensure all dynamic content is visible
progressive_scroll(browser, scroll_step = 1000)

# We identify and click the necessary "Details" buttons
# We need a complex css selector to open the Details menu, but not "Annexes" or others.
buttons <- browser$findElements("css selector", ".ecb-langSelector+ .accordion .header:nth-child(1) .title")
for(i in seq_along(buttons)){
  buttons[[i]]$clickElement() # Click each button to reveal hidden content
  Sys.sleep(runif(1, max = 0.2)) # Add a slight pause to avoid errors and overloading the website
}

## Extracting data from the page ---------
# Now, extract relevant information from the page: publication date, title, pdf url, authors, abstract, JEL codes, and network information

# Get the page source after interaction and read it into an HTML structure
pub_page <- browser$getPageSource()[[1]] %>% 
  read_html()

# Extract the publication dates
date <- pub_page %>% 
  html_elements(".loaded > dt") %>% 
  html_text()

# Extract the paper identifiers (number)
id <- pub_page %>% 
  html_elements(".category") %>% 
  html_text

# Extract the titles of the papers
title <- pub_page %>%
  html_elements(".category+ .title a") %>% 
  html_text()

# Extract the URLs for getting the papers pdf
url <- pub_page %>%
  html_elements(".category+ .title a") %>% 
  html_attr("href")

### Handling multiple authors ---------
# Extract authors (as each paper may have multiple authors listed, we will need to differentiate them)
papers_all_authors <- pub_page %>% 
  html_elements(".authors") 

# Extract author names from the list of papers' authors (each paper may have several authors)
authors_list <- map(papers_all_authors, ~ html_elements(., "a") %>% html_text())

# Flatten the list of authors into a single string per paper, separating authors by commas
authors <- map_chr(authors_list, ~str_flatten(., collapse = ", "))

## Extracting supplementary information (Abstracts, JEL Codes) ---------
supplementary_information <- pub_page %>% 
  html_elements(".category, .content-box > dl dt, .content-box > dl dd") %>% 
  html_text() %>% 
  tibble(text = .) %>% 
  mutate(id = if_else(str_detect(text, "^No\\. \\d+"), text, NA)) %>% 
  fill(id, .direction = "down") %>%    # Fill the paper ID down for every piece of information
  filter(text != id) %>%    # Filter out rows that only contain the ID
  mutate(info_type = str_extract(text, "^Abstract$|^JEL Code$|^Network$")) %>%  # Identify supplementary info place
  group_by(id) %>% 
  fill(info_type, .direction = "down") %>%   # Fill info type downwards
  filter(text != info_type) %>%  
  pivot_wider(names_from = info_type, values_from = text) # Reshape the data so each paper has its abstract, JEL codes, etc.

# EDIT: put all the extracted information in a single data frame
extracted_data <- tibble(date,
                         id,
                         title,
                         url,
                         authors)
all_data <- left_join(extracted_data,
                      supplementary_information,
                      by = "id")

## Analyzing text from titles and abstracts ---------
# Now we have all the papers, let's have a bit of analysis by looking at the most frequent terms in titles and abstracts

# Defining a list of stop words to filter out from text analysis
## EDIT: for that purpose, you can use lexicon == "SMART"
stopwords <- stop_words %>% filter(lexicon == "SMART") %>% pull(word)

# Unnest words from titles and count frequent terms (i.e. excluding stopwords)
all_data %>% 
  unnest_ngrams(word, title, n_min = 1, n = 3) %>%
  filter(! str_detect(word, str_c("\\b", stopwords, "\\b", collapse = "|"))) %>%
  count(word, sort = TRUE)

# Unnest words from abstracts and count frequent terms (excluding stopwords)
all_data %>% 
  unnest_ngrams(word, Abstract, n_min = 1, n = 3) %>%
  filter(! str_detect(word, str_c("\\b", stopwords, "\\b", collapse = "|"))) %>%
  count(word, sort = TRUE)

# Checking for the mention of 'climate change' in the abstracts
all_data %>% 
  unnest_ngrams(word, Abstract, n = 2) %>%
  mutate(mention_climate = str_detect(word, "climate change")) %>%
  distinct(id, mention_climate) %>%
  pull(mention_climate) %>% 
  sum(na.rm = TRUE)/nrow(all_data)  # Calculate proportion of papers mentioning 'climate change'
