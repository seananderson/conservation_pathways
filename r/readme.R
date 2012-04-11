rm(list = ls())

# read in the wos and gnews data:
source("create_wos_gnews_dat.R")
wos_gnews_dat <- create_wos_gnews_dat()

# read in the env. response data:
source("create_response_dat.R")
# write it out to a .csv so we can refer to it for the results section
# values:
library(reshape)

wos_gnews_dat <- subset(wos_gnews_dat, year <= 2007)

# colours for the panels:
col.axis <- "grey50"
col.axis.label <- "grey50"
col.text <- "grey50"

# colours for the data types:
col.research <- "#D11B24"
col.public <- "#1C72C5"
col.stressor <- "#333333"
col.response <- "#159005"

# function to plot the fist panel of the figures
# and set up the rest:
source("conservation_panel_functions.R")
source("fig_letter.R")

# add the prop.text() binomial CIs to the wos and gnews series:
# creates the columns prop_ci_low and prop_ci_high
source("add_prop_test_cis.R")

# take out all data at and after 2008 to be consistent: 
# the figures:
source("plot_acid_rain_panels.R")
source("plot_elephant_panels.R")
source("plot_ddt_panels.R") 
#source("plot_ddt_panels.R")

# supplementary raw frequency figures:
source("raw_freq_plots.R")

# summary stats for the various news and research trends:
source("get_average_rates.R")

# the ratio of the two indices:
source("scaled_news_to_research.R")

