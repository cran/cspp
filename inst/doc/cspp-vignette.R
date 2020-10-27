## ----message = FALSE----------------------------------------------------------
# Load the package
library(cspp)

# Find variables based on a category
demo_variables <- get_var_info(categories = "demographics")

# Use these variables to get a full or subsetted version of the data
cspp_data <- get_cspp_data(vars = demo_variables$variable, 
                           years = seq(2000, 2010))


## -----------------------------------------------------------------------------
library(dplyr)
glimpse(cspp_data[1:15],)

## -----------------------------------------------------------------------------
# All variables
all_variables <- get_var_info()

# Full dataset
all_data <- get_cspp_data()

## -----------------------------------------------------------------------------
# Search for variables by name
get_var_info(var_names = c("pop","femal"))

## -----------------------------------------------------------------------------
# Search by name and description:
get_var_info(related_to = c("pop", "femal"))

## -----------------------------------------------------------------------------
# See variable categories:
unique(get_var_info()$category)

## -----------------------------------------------------------------------------
# Find variables by category:
var_cats <- get_var_info(categories = c("gun control", "labor"))

## ----eval = F-----------------------------------------------------------------
#  # Get subsetted data and save to dataframe
#  data <- get_cspp_data(vars = c("sess_length", "hou_majority", "term_length"),
#                        var_category = "demographics",
#                        states = c("NC", "VA", "GA"),
#                        years = seq(1995, 2004))

## -----------------------------------------------------------------------------
# Use get_var_info to generate variable vector inline
get_cspp_data(vars = get_var_info(related_to = "concealed carry")$variable,
              states = "NC",
              years = 1999)

## -----------------------------------------------------------------------------
# Simple dataframe for one variable
get_cites(var_names = "poptotal")

# Using get_var_info to return variable citations
get_cites(var_names = get_var_info(related_to = "concealed carry")$variable)

## ----eval=F-------------------------------------------------------------------
#  get_cites(var_names = "poptotal",
#           write_out = TRUE,
#           file_path = "~/path/to/file.csv",
#           format = "csv")

## ----fig.width=4, dpi = 140---------------------------------------------------
library(ggplot2) # optional, but needed to remove legend

# Generates a map of the percentage of the population over 65
generate_map(get_cspp_data(var_category = "demographics"),
             var_name = "pctpopover65") +
  theme(legend.position = "none")

## ----fig.width=4, dpi = 140, message=FALSE------------------------------------
library(dplyr)

generate_map(get_cspp_data(var_category = "demographics") %>%
                dplyr::filter(st.abb %in% c("NC", "VA", "SC")),
              var_name = "pctpopover65",
              poly_args = list(color = "black"),
              drop_NA_states = TRUE) +
  theme(legend.position = "none")

## ----fig.width=4, dpi = 140---------------------------------------------------
generate_map(get_cspp_data(var_category = "demographics") %>%
                dplyr::filter(st.abb %in% c("NC", "VA", "SC", "TN", "GA", "WV", "MS", "AL", "KY")),
              var_name = "pctpopover65",
              poly_args = list(color = "black"),
              drop_NA_states = TRUE) +
  scale_fill_gradient(low = "white", high = "red") +
  theme(legend.position = "none") +
  ggtitle("% Population Over 65")

## -----------------------------------------------------------------------------
# Returns dataframe of state dyads
head(get_network_data())

## -----------------------------------------------------------------------------
network.df <- get_network_data(category = c("Economic", "Political"))

names(network.df)

## -----------------------------------------------------------------------------
cspp_data <- get_cspp_data(vars = c("sess_length", "hou_majority"), years = seq(1999, 2000))

network.df <- get_network_data(category = "Distance Travel Migration",
                               merge_data  = cspp_data)

names(network.df)

library(dplyr)

head(cspp_data %>% arrange(st.abb))
# the merged value of Alaska's hou_majority value will be mean(c(-0.129, -0.115))


