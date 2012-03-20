# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Mar 20, 2012
# Last modified: Mar 20, 2012
# Purpose:       Add prop.test() binomial CIs to the gnews and wos
# series
# ====================================================================

# per million:
wos_gnews_dat$prop_ci_low <- NA
wos_gnews_dat$prop_ci_high <- NA
for(i in (1:nrow(wos_gnews_dat))[wos_gnews_dat$type == "Google News Archive"]) {
  wos_gnews_dat$prop_ci_low[i] <- 1e6*prop.test(wos_gnews_dat$freq[i], n = wos_gnews_dat$the_freq[i])$conf.int[1]
  wos_gnews_dat$prop_ci_high[i] <- 1e6*prop.test(wos_gnews_dat$freq[i], n = wos_gnews_dat$the_freq[i])$conf.int[2]
}
for(i in (1:nrow(wos_gnews_dat))[wos_gnews_dat$type == "Web of Science"]) {
  wos_gnews_dat$prop_ci_low[i] <- 1e6*prop.test(wos_gnews_dat$freq[i], n = wos_gnews_dat$wos_all_freq[i])$conf.int[1]
  wos_gnews_dat$prop_ci_high[i] <- 1e6*prop.test(wos_gnews_dat$freq[i], n = wos_gnews_dat$wos_all_freq[i])$conf.int[2]
}

