# Script for determining if original edgelist provided by Susan matches that
# from the posted taxonomies.
library(tidyverse)
source("./R/xbrl_taxonomy_functions.R")

# original link provided by susan
load("./data/calc_link_2017.RData")

# # Load from Taxonomy_2017Amended.xlsx
# calc_link <- excel_to_df("./inst/extdata/Taxonomy_2017Amended.xlsx",
#                          "Calculation")
# # remove those rows that do not start with us-gaap
# calc_link <- calc_link[calc_link$prefix=="us-gaap", ]
# calc_link <- calc_link %>% select(parent, name) %>% rename(child = name) %>%
#     mutate(parent = sub("us-gaap:", "", parent))
# calc_link_amended_tx <- calc_link
# save(calc_link_amended_tx, file = "./data/calc_link_amended_tx.Rdata")

load("./data/calc_link_amended_tx.Rdata")

# diff between dataframes
# The only difference is that Susan's list does not have any of the root nodes,
# as in elements that have no parents (NA for parents)
calc_link_diff <- setdiff(calc_link_amended_tx, calc_link_2017)
sum(!is.na(calc_link_diff$parent))

# below shows Susan's list does not have any elements that the amended taxonomy
# does not have
setdiff(calc_link_2017, calc_link_amended_tx)

# root nodes in the calc_link_amended:
sum(is.na(calc_link_diff$parent))
