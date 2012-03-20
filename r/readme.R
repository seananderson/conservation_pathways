# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Oct 28, 2011
# Last modified: Mar 20, 2012
# Purpose:       all steps to plot the tracking conservation data
# ====================================================================
rm(list = ls())

# read in the wos and gnews data:
source("create_wos_gnews_dat.R")
wos_gnews_dat <- create_wos_gnews_dat()

# read in the env. response data:
source("create_response_dat.R")

# colours for the panels:
col.axis <- "grey50"
col.axis.label <- "grey50"
col.text <- "grey50"

# function to plot the fist panel of the figures
# and set up the rest:
source("conservation_panel_functions.R")
source("fig_letter.R")


# add the prop.text() binomial CIs to the wos and gnews series:
# creates the columns prop_ci_low and prop_ci_high
source("add_prop_test_cis.R")

# the figures:
source("plot_acid_rain_panels.R")
source("plot_elephant_panels.R")
source("plot_ddt_panels.R")

