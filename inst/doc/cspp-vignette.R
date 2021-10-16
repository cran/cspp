## ----eval=F-------------------------------------------------------------------
#  # For latest developmental verison:
#  library(devtools)
#  install_github("correlatesstatepolicy/cspp")
#  
#  # For CRAN version:
#  install.packages("cspp")

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
get_var_info(var_names = c("pop","femal")) %>% dplyr::glimpse()

## -----------------------------------------------------------------------------
# Search by name and description:
get_var_info(related_to = c("pop", "femal")) %>% dplyr::glimpse()

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
get_cites(var_names = "poptotal") %>% dplyr::glimpse()

# Using get_var_info to return variable citations
cite_ex <- get_cites(var_names = get_var_info(related_to = "concealed carry")$variable)
cite_ex$plaintext_cite[3:4]

## ----eval=F-------------------------------------------------------------------
#  get_cites(var_names = "poptotal",
#           write_out = TRUE,
#           file_path = "~/path/to/file.csv",
#           format = "csv")

## ----out.width='60%'----------------------------------------------------------
library(ggplot2) # optional, but needed to remove legend

# Generates a map of the percentage of the population over 65
generate_map(get_cspp_data(var_category = "demographics"),
             var_name = "pctpopover65") +
  ggplot2::theme(legend.position = "none")

## ----out.width='60%'----------------------------------------------------------
library(dplyr)

generate_map(get_cspp_data(var_category = "demographics") %>%
                dplyr::filter(st %in% c("NC", "VA", "SC")),
              var_name = "pctpopover65",
              poly_args = list(color = "black"),
              drop_NA_states = TRUE) +
  ggplot2::theme(legend.position = "none")

## ----out.width='60%'----------------------------------------------------------
generate_map(get_cspp_data(var_category = "demographics") %>%
                dplyr::filter(st %in% c("NC", "VA", "SC", "TN", "GA", "WV", "MS", "AL", "KY")),
              var_name = "pctpopover65",
              poly_args = list(color = "black"),
              drop_NA_states = TRUE) +
  ggplot2::scale_fill_gradient(low = "white", high = "red") +
  ggplot2::theme(legend.position = "none") +
  ggplot2::ggtitle("% Population Over 65")

## ----out.width="100%", dpi=180------------------------------------------------
# panel of all states' adoption of medical marijuana laws
cspp <- get_cspp_data(vars = "drugs_medical_marijuana")

# visualize panel:
plot_panel(cspp)

## ---- out.width="100%", dpi=180-----------------------------------------------
plot_panel(cspp_data = get_cspp_data(vars = "pollib_median"),
           colors = c("firebrick4", "steelblue2", "gray"),
           years = seq(1960, 2010)) +
  ggplot2::ggtitle("Policy liberalism")

## -----------------------------------------------------------------------------
# Returns dataframe of state dyads
get_network_data() %>% dplyr::glimpse()

## -----------------------------------------------------------------------------
network.df <- get_network_data(category = c("Economic", "Political"))

names(network.df)

## -----------------------------------------------------------------------------
cspp_data <- get_cspp_data(vars = c("sess_length", "hou_majority"), years = seq(1999, 2000))

network.df <- get_network_data(category = "Distance Travel Migration",
                               merge_data  = cspp_data)

names(network.df)

library(dplyr)

head(cspp_data %>% arrange(st))
# the merged value of Alaska's hou_majority value will be mean(c(-0.129, -0.115))


## ----message=F, warning=F, dpi=180--------------------------------------------
library(ggraph)
library(igraph)

network.df <- select(network.df, from = st.abb1, to = st.abb2, ACS_Migration) 

network.df %>% 
  filter(from %in% c("NC", "VA", "SC", "GA")) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout="fr") + 
  geom_edge_link(aes(edge_alpha = ACS_Migration), edge_color = "royalblue") + 
  geom_node_point() +
  geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
  theme_void() +
  theme(legend.position = "none")

## ----message=F, warning=F, dpi=180--------------------------------------------
network.df %>% 
  filter(from %in% c("NC")) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout="linear") + 
  geom_edge_arc(aes(edge_alpha = ACS_Migration), edge_color = "royalblue") + 
  geom_node_text(aes(label = name), size = 2) +
  theme_void() +
  theme(legend.position = "none")

