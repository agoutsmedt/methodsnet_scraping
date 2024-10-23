# Goal: 
# To scrape and analyze the results of the 2024 United Kingdom general election 
# from the BBC website (i.e, https://www.bbc.com/news/election/2024/uk/regions/E92000001), focusing on the following:
# 1. The number of parties that won at least one seat.
# 2. The parties that gained or lost seats.
# 3. The average number of votes required per seat.
# 4. Comparing these metrics between the entire UK and England specifically.

# Step 1: Load necessary packages
# We use the pacman package to manage dependencies and load required libraries in one command
install.packages("pacman")
pacman::p_load(tidyverse, 
               rvest,      # For web scraping
               polite,     # To scrape politely
               ggplot2,    # For data visualization
               patchwork)  # For combining multiple plots

# Step 2: Scrape Election Data for the UK

# 2.1: Initialize a polite session for web scraping
bbc <- "https://www.bbc.com"
session <- polite::bow(bbc, 
                       user_agent = "polite R package - used for academic training by Thomas Laloux (Thomas.g.laloux@uclouvain.be)")

# 2.2: Define the URL of the 2024 UK election results page
url <- "https://www.bbc.com/news/election/2024/uk/results"

# 2.3: Read the HTML page
page <- read_html(url)

# 2.4: Extract data from the page
# Extract the names of the parties
page %>%
  html_elements(".e1j83d2f2") %>%
  html_text() -> party

# Extract the number of seats won by each party
page %>%
  html_elements(".ssrcss-a2di88-ResultValue") %>%
  html_text() -> seat 

# Extract the changes in seat numbers (gain/loss) per party
page %>%
  html_elements(".e1k9l0jz3:nth-child(2) .e1k9l0jz0") %>%
  html_text() -> change_seat 

# Extract the number of votes each party received
page %>%
  html_elements(".e1k9l0jz3:nth-child(3) .e1k9l0jz0") %>%
  html_text() -> number_vote 

# 2.5: Store the extracted data in a tibble and convert relevant columns to numeric types
Election_UK <- tibble(party, seat, change_seat, number_vote)

# Remove unnecessary variables from memory to keep the environment clean
rm(party, seat, change_seat, number_vote)

# 2.6: Clean the data
# - Remove "+" from seat changes, and convert seat counts and votes to numeric
Election_UK <- Election_UK %>%
  mutate(change_seat = gsub("\\+", "", change_seat) %>% as.numeric(),
         seat = as.numeric(seat),
         number_vote = as.numeric(gsub(",", "", number_vote)))

# Step 3: Analyze UK Election Data

# 3.1: Identify winning and losing parties by arranging them based on seat changes
Election_UK %>%
  arrange(-change_seat) %>%
  mutate(voteforsiege = number_vote / seat) %>%  # Calculate votes per seat
  select(-number_vote) %>%
  print()

# 3.2: Determine the number of parties that won at least one seat
length(Election_UK$party)

# Step 4: Visualize the number of votes per seat in the UK

# Create a bar plot for the number of votes per seat for each party
ggplot(Election_UK, aes(y = reorder(party, voteforsiege), x = voteforsiege)) +
  geom_col(fill = 'grey') +
  theme_bw() +
  ggtitle("Number of Votes per Seat in the UK") +
  scale_x_continuous(name = "Number of votes per seat", labels = scales::comma_format(big.mark = '.')) -> Figure_UK

Figure_UK

# Step 5: Scrape Election Data for England

# 5.1: Scrape the election results specific to England
url <- "https://www.bbc.com/news/election/2024/uk/regions/E92000001"
page <- read_html(url)

# 5.2: Extract the data similarly to the UK process
page %>%
  html_elements(".e1j83d2f2") %>%
  html_text() -> party

page %>%
  html_elements(".ssrcss-a2di88-ResultValue") %>%
  html_text() -> seat 

page %>%
  html_elements(".e1k9l0jz3:nth-child(2) .e1k9l0jz0") %>%
  html_text() -> change_seat 

page %>%
  html_elements(".e1k9l0jz3:nth-child(3) .e1k9l0jz0") %>%
  html_text() -> number_vote 

# Store data in a tibble and remove unnecessary variables
Election_EN <- tibble(party, seat, change_seat, number_vote)
rm(party, seat, change_seat, number_vote)

# Step 6: Analyze England Election Data

# 6.1: Repeat analysis steps for the England data (arrange by seat changes, calculate votes per seat)
Election_EN %>%
  arrange(-change_seat) %>%
  mutate(voteforsiege = number_vote / seat) %>%
  select(-number_vote) %>%
  print()

# 6.2: Filter for parties that won at least one seat and calculate votes per seat
Election_EN <- Election_EN %>%
  filter(seat > 0) %>%
  mutate(voteforsiege = number_vote / seat)

# Step 7: Visualize the number of votes per seat in England

# Create a bar plot for the number of votes per seat for each party in England
ggplot(Election_EN, aes(y = reorder(party, voteforsiege), x = voteforsiege)) +
  geom_col(fill = 'grey') +
  theme_bw() +
  ggtitle("Number of Votes per Seat in England") +
  scale_x_continuous(name = "Number of votes per seat", labels = scales::comma_format(big.mark = '.')) -> Figure_EN

Figure_EN

# Step 8: Compare UK and England Data

# 8.1: Create a comparison table with the number of parties and mean votes per seat for the UK and England
tibble(Country=c("England","UK"),
       Nparties=c(length(Election_EN$party),length(Election_UK$party)),
       voteforsiege=c(mean(Election_EN$voteforsiege),mean(Election_UK$voteforsiege))) %>%
  print()

# 8.2: Visualize the comparison between the UK and England
Figure_UK / Figure_EN &
  scale_x_continuous(limits = c(0, 840000), name = "Number of votes per seat", labels = scales::comma_format(big.mark = '.'))
