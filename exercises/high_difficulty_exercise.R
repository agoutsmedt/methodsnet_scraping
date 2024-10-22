######## BCE ##########

# Let's first load packages we will need
pacman::p_load(tidyverse, 
               rvest, 
               polite)

ecb_domain <- "https://www.ecb.europa.eu/"
session <- polite::bow(ecb_domain, 
                       user_agent = "polite R package - used for academic training by Marine Bardou (marine.bardou@uclouvain.be)")
ecb_pub_path <- str_c(ecb_domain, "press/research-publications/occasional-papers/html/index.en.html")

remDr <- rsDriver(browser = "firefox",
                  port = 4444L,
                  chromever = NULL)
browser <- remDr[["client"]]
browser$navigate(ecb_pub_path)
Sys.sleep(session$delay) 

browser$findElement("link text", "I do not accept the use of cookies")$clickElement()

# Manipulating the page to get all the information-----------

# We will first scroll down the page to load all the content
progressive_scroll <- function(browser, scroll_step = 500) {
  # Get initial scroll height of the page
  current_height <- browser$executeScript("return document.body.scrollHeight")
  
  # Set a variable for the scrolling position
  scroll_position <- 0
  
  # Continue scrolling until the end of the page
  while (scroll_position < current_height) {
    # Scroll down by 'scroll_step' pixels
    browser$executeScript(paste0("window.scrollBy(0,", scroll_step, ");"))
    
    # Wait for the content to load (adjust this if the page is slower to load)
    Sys.sleep(runif(1, max = 0.2))
    
    # Update the scroll position
    scroll_position <- scroll_position + scroll_step
    
    # Get the updated scroll height after scrolling (in case more content is loaded)
    current_height <- browser$executeScript("return document.body.scrollHeight")
  }
}

progressive_scroll(browser, scroll_step = 1000)

# We need to click on the "Details" buttons to get the abstracts
# We need a complex css selector to open the Details menu, but not "Annexes" or others.
buttons <- browser$findElements("css selector", ".ecb-langSelector+ .accordion .header:nth-child(1) .title")
for(i in seq_along(buttons)){
  buttons[[i]]$clickElement()
  Sys.sleep(runif(1, max = 0.2))
}


pub_page <- browser$getPageSource()[[1]] %>% 
  read_html()

dates <- pub_page %>% 
  html_elements(".loaded > dt") %>% 
  html_text()

numbers <- pub_page %>% 
  html_elements(".category") %>% 
  html_text

titles <- pub_page %>%
  html_elements(".category+ .title a") %>% 
  html_text()

# For authors, the problem is that you have several authors, and so you need to differentiate them
papers_all_authors <- pub_page %>% 
  html_elements(".authors") 

authors_list <- map(papers_all_authors, ~ html_elements(., "a") %>% html_text())

# We need to flatten the list of authors
authors <- map_chr(authors_list, ~str_flatten(., collapse = ", "))

supplementary_information <- pub_page %>% 
  html_elements(".category, .content-box > dl dt, .content-box > dl dd") %>% 
  html_text() %>% 
  tibble(text = .) %>% 
  mutate(number_paper = if_else(str_detect(text, "^No\\. \\d+"), text, NA)) %>% 
  fill(number_paper, .direction = "down") %>%
  filter(text != number_paper) %>%
  mutate(info_type = str_extract(text, "^Abstract$|^JEL Code$|^Network$")) %>%  # We identify Abstracts and other information
  group_by(number_paper) %>% 
  fill(info_type, .direction = "down") %>%
  filter(text != info_type) %>%  
  pivot_wider(names_from = info_type, values_from = text)

