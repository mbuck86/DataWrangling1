# Data wrangling exercise 1: Basic data manipulation
library(tidyr)
library(dplyr)
library(stringr)

# 0. Load the data in RStudio
elec <- read.csv("refine_original.csv", stringsAsFactors = FALSE)

# 1. Clean up brand names

# First, make all the names all lower case so I don't have to deal with exceptions before figuring out string 
# replacement code

elec$company <- tolower(elec$company)

# Each instance of Phillips ends in 'ps', so to fix the Phillips names:
elec <- elec %>% mutate(company = sub(".*ps$", "phillips", company))

# Each instance of Akzo begins with 'ak', so to fix the Akzo names:
elec <- elec %>% mutate(company = sub("^ak.*", "akzo", company))

# Each instance of Unilever begins with "uni", so to fix the Unilever names:
elec <- elec %>% mutate(company = sub("^uni.*", "unilever", company))

# The Van Houten names are all fine, so I won't modify them here.

# 2. Separate product code and number
elec <- separate(elec, Product.code...number, into = c("product_code", "product_number"),sep = "-" )

# 3. Add product categories
# assign each product code to their respective category
product_codes <- c(p = "Smartphone", x = "Laptop", v = "TV", q = "Tablet")

# Create the product category column
elec <- elec %>% mutate(product_category = product_codes[product_code])

# 4. Add full address for geocoding
elec <- unite(elec, geocoding_address, c(address, city, country), sep = ", ")

# 5. Creating Dummy variables for companies and product categories

# Dummy variables for companies

elec$company <- paste0("company_", elec$company)

elec <- elec %>% 
  mutate(dummy = 1) %>%
  distinct()%>%
  spread(company, dummy, fill = 0)


# Dummy variables for product categories

elec$product_category <- tolower(elec$product_category) 
elec$product_category <- paste0("product_", elec$product_category)

elec <- elec %>% 
  mutate(dummy = 1) %>%
  distinct() %>%
  spread(product_category, dummy, fill = 0)


#Save the clean data frame as a clean csv file
write.csv(elec, file = "refine_clean.csv")


