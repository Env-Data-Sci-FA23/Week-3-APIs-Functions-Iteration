library(tidyverse)
library(httr)
library(jsonlite)

unit_visitation <- function(park, start_month = "01", start_year, end_month = "12", end_year) {
raw_data <- httr::GET(url = paste0("https://irmaservices.nps.gov/v3/rest/stats/visitation?unitCodes=", park,
                                   "&startMonth=", start_month,
                                   "&startYear=", start_year,
                                   "&endMonth=", end_month,
                                   "&endYear-", end_year))
# convert content to text
extracted_data <- httr::content(raw_data, as = "text", encoding = "UTF-8")

# parse text from JSON to data frame
final_data <- jsonlite::fromJSON(extracted_data)

return(final_data)

}

#selecting parks
parks <- c("ROMO", "THRO", "DEVA")

#parse for years & binding
unit_data <- parks %>%
  map(~ unit_visitation(park = ., start_year = 2011, end_year = 2020)) %>%
  bind_rows()

# pivot wider
unit_wide <- unit_data %>%
  select(Month, Year, UnitCode, RecreationVisitors) %>%
  pivot_wider(., names_from = UnitCode, values_from = RecreationVisitors)

# narrow data frame by using pivot longer function
unit_long <- unit_wide %>%
  pivot_longer(cols = -c(Month, Year),
               names_to = "UnitCode",
               values_to = "RecreationVisitors")

# alien visitation
unit_alien <- unit_wide %>%
  mutate_at(.vars = parks, .funs = ~ (. + 70) )





