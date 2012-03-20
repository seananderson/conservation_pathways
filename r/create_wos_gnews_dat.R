# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Oct 28, 2011
# Last modified: Mar 20, 2012
# Purpose:       Build the derby WoS and GNews data.
# ====================================================================

create_wos_gnews_dat <- function() {
require(plyr)
require(reshape)
the_freq <- read.table("../data/gnews.the.freq.txt", col.names = c("year", "the_freq"))
#gscholar_pubs <- read.table("../data/gscholar.pubs.txt", col.names = c("year", "gscholar_freq"))
#wos_all <- read.table("../data/wos.all.freq.txt", sep = "\t", header = TRUE, col.names = c("year", "wos_all_freq", "ignore"))[,1:2]
wos_all <- read.table("../data/wos.all.languages.articles.sci.expanded.txt", sep = "\t", header = TRUE, col.names = c("year", "wos_all_freq", "ignore"))[,1:2]
wos_all <- wos_all[order(wos_all$year), ]
wos_all <- subset(wos_all, year <= 2010 & year >= 1940)
the_freq <- subset(the_freq, year <= 2010 & year >= 1940)
#wos_all <- ddply(wos_all, "year", summarize, wos_all_freq = sum(wos_all_freq))

enter_derby_data <- function(topic, gnews_file, wos_file) {
  gnews <- read.table(paste("../data/", gnews_file, sep = ""), col.names = c("year", "freq"))
  gnews <- merge(data.frame(year = seq(min(gnews$year), max(gnews$year))), gnews, all = TRUE)
  gnews$freq[is.na(gnews$freq)] <- 0
  gnews$type <- "Google News Archive"
  wos <- read.table(paste("../data/", wos_file, sep = ""), col.names = c("year", "freq", "ignore"), sep = "\t", header = TRUE)[,1:2]
  wos <- merge(data.frame(year = seq(min(wos$year), max(wos$year))), wos, all = TRUE)
  wos$freq[is.na(wos$freq)] <- 0
  wos$type <- "Web of Science"
# we're only going back to 1940 max, so cut off the rest here:
  wos <- subset(wos, year >= 1940 & year <= 2010)
  gnews <- subset(gnews, year >= 1940 & year <= 2010)
  temp <- rbind(gnews, wos)
  temp$topic <- topic
  temp
}

d <- enter_derby_data("acid rain", "gnews.acidrain.txt", "wos.acidrain.txt")
d <- rbind(d, enter_derby_data("ivory", "gnews.ivory.txt", "wos.ivory.txt"))
d <- rbind(d, enter_derby_data("ddt", "gnews.ddt.txt", "wos.ddt.txt"))

d <- merge(d, the_freq, all = TRUE)
#d <- merge(d, gscholar_pubs, all.x = TRUE)
d <- merge(d, wos_all, all = TRUE)
d <- d[order(d$topic, d$type, d$year), ]

# missing 2009 and 2010 of "the_freq", remove them:
# TODO, do we have this somewhere?
d <- subset(d, !is.na(the_freq))

# scaling:
gnews_temp <- transform(subset(d, type == "Google News Archive"), freq_scaled = freq / the_freq * 1e6) 
wos_temp <- transform(subset(d, type == "Web of Science"), freq_scaled = freq / wos_all_freq * 1e6) 
d <- rbind(gnews_temp, wos_temp)

d <- na.omit(d)
row.names(d) <- NULL # cleaning up

return(d)
}
