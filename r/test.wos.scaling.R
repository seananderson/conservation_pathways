# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Dec 23, 2011
# Last modified: Dec 24, 2011
# Purpose:       What's the effect of choosing different scaling
# frequencies for the WoS data? Is gscholar reasonable to use for wos?
# ====================================================================

create_wos_gnews_dat2 <- function() {
require(plyr)
require(reshape)
the_freq <- read.table("../data/gnews.the.freq.txt", col.names = c("year", "the_freq"))
wos_conservation <- read.table("../data/wos.all.freq.txt", sep = "\t", header = TRUE, col.names = c("year", "wos_all_freq", "ignore"))[,1:2]
wos_conservation <- wos_conservation[order(wos_conservation$year), ]
wos_conservation <- subset(wos_conservation, year < 2011)
wos_conservation <- ddply(wos_conservation, "year", summarize, wos_all_freq = sum(wos_all_freq))
gscholar_pubs <- read.table("../data/gscholar.pubs.txt", col.names = c("year", "gscholar_freq"))

enter_derby_data2 <- function(topic, gnews_file, wos_file) {
  gnews <- read.table(paste("../data/", gnews_file, sep = ""), col.names = c("year", "freq"))
  gnews$type <- "Google News Archive"
  wos <- read.table(paste("../data/", wos_file, sep = ""), col.names = c("year", "freq"))
  wos$type <- "Web of Science"
  temp <- rbind(gnews, wos)
  temp$topic <- topic
  temp
}

d <- enter_derby_data2("acid rain", "gnews.acidrain.txt", "wos.acidrain.txt")
d <- rbind(d, enter_derby_data2("ivory", "gnews.ivory.txt", "wos.ivory.txt"))
d <- rbind(d, enter_derby_data2("ddt", "gnews.ddt.txt", "wos.ddt.txt"))

d <- merge(d, the_freq, all.x = TRUE)
d <- merge(d, gscholar_pubs, all.x = TRUE)
d <- merge(d, wos_conservation , all.x = TRUE)
d <- d[order(d$topic, d$type, d$year), ]

# scaling:
gnews_temp <- transform(subset(d, type == "Google News Archive"), freq_scaled = freq / the_freq * 1e6) 
gnews_temp$scale_type <- "Google the"
wos_temp <- transform(subset(d, type == "Web of Science"), freq_scaled = freq / gscholar_freq * 1e6) 
wos_temp$scale_type <- "G Scholar"
wos_temp2 <- transform(subset(d, type == "Web of Science"), freq_scaled = freq / wos_all_freq * 1e6)
wos_temp2$scale_type <- "WoS all"
d <- rbind(gnews_temp, wos_temp, wos_temp2)

row.names(d) <- NULL # cleaning up
return(d)
}

###
library(reshape)
library(ggplot2)
d <- create_wos_gnews_dat2()
d <- subset(d, year >= 1940)
d <- d[-which(d$topic == "ivory" & d$year < 1960), ] # erratic data before this which messes up the graph scaling
d <- ddply(d, c("scale_type", "topic"), transform, freq_scaled_1 = freq_scaled / max(freq_scaled, na.rm = TRUE))
p <- ggplot(d, aes(year, freq_scaled_1, colour = scale_type)) + geom_line() + facet_grid(type~topic, scale = "free_y") + ylab("Frequency scaled to 1")
ggsave("../fig/comparison-of-wos-scaling.pdf")


junk <- subset(d, topic == "acid rain")
junk.melt <- subset(melt(junk[,c("year", "type", "gscholar_freq", "wos_all_freq", "scale_type")], id.vars = c("year", "type"), measure.vars = c("wos_all_freq", "gscholar_freq")), type != "Google News Archive" & year >=1940)

junk.melt$scaled <- "Raw data"
junk.melt.2 <- ddply(junk.melt, "variable", transform, value = value/max(value))
junk.melt.2$scaled <- "Scaled to 1"
junk.melt <- rbind(junk.melt, junk.melt.2)

p <- ggplot(junk.melt, aes(year, value, colour = variable)) + geom_line() + facet_wrap(~scaled, scales = "free_y")
print(p)
ggsave("../fig/WoS-scaling-variables.pdf")

