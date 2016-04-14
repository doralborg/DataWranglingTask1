# Springboard: Foundations of Data Science Workshop
# Data wrangling exercies 1
# Author : Dora L. Borg

library(dplyr)
library(tidyr)


refine_original <- read.csv("refine_original.csv") # import csv file in rstudio
refine <- refine_original

#clean up the names

refine$company <- tolower(refine_original$company) %>% 
  gsub("0", "o", .) %>%
  sub("k z", "kz", .) %>%
  sub("lv", "lev", .) %>%
  sub("^([fp].+)", "philips", .)

# seperate product code and number

newcols = c("product_code","product_number")
refine <- separate(refine, "Product.code...number", 
           newcols, sep = "-", remove = FALSE)

# add product categories

refine$product_cat[refine$product_code == "x"] <- "Laptop"
refine$product_cat[refine$product_code == "q"] <- "Tablet"
refine$product_cat[refine$product_code == "p"] <- "Smartphone"
refine$product_cat[refine$product_code == "v"] <- "TV"

# add full address for geocoding

refine <- unite(refine,"full_address", address,city,country, sep = ",",remove = FALSE)


# create dummy variable for company and product category

refine <- refine %>%
  mutate(., company_philips = refine$company == "philips") %>%
  mutate(., company_akzo = refine$company == "akzo") %>%
  mutate(., company_van_houten = refine$company == "van houten") %>%
  mutate(., company_unilever = refine$company == "unilever")

refine <- refine %>%
  mutate(., product_smartphone = refine$product_cat == "Smartphone") %>%
  mutate(., product_tv = refine$product_cat == "TV") %>%
  mutate(., product_laptop = refine$product_cat == "Laptop") %>%
  mutate(., product_tablet = refine$product_cat == "Tablet")

# change the boolean to binary

refine$company_philips <- refine$company_philips + 0
refine$company_akzo <- refine$company_akzo + 0
refine$company_van_houten <- refine$company_van_houten + 0
refine$company_unilever <- refine$company_unilever + 0

refine$product_smartphone <- refine$product_smartphone + 0
refine$product_tv <- refine$product_tv + 0
refine$product_laptop <- refine$product_laptop + 0
refine$product_tablet <- refine$product_tablet + 0


# save new csv file

write.csv(refine, "refine_clean.csv", row.names = FALSE)
