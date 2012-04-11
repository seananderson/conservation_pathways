# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Apr 07, 2012
# Last modified: Apr 07, 2012
# Purpose:       look at the ratio of the two indices
# ====================================================================

wos_gnews_dat_wide <- cast(wos_gnews_dat, topic + year ~ type, value = "freq_scaled")
names(wos_gnews_dat_wide) <- c("topic", "year", "gnews", "wos")
wos_gnews_dat_wide <- transform(wos_gnews_dat_wide, news_per_research = gnews/wos)
write.csv(wos_gnews_dat_wide, file = "../data/wos_gnews_dat.csv")
pdf("../fig/news_per_research.pdf", width = 8, height = 3.0)
par(mfrow = c(1,3), cex = 0.7, mar = c(4,4, 1.5, .5), oma = c(0,0,0,0))
with(subset(wos_gnews_dat_wide, topic == "acid rain" & year <= 2007), plot(year, news_per_research, type = "o"));mtext("acid rain")
with(subset(wos_gnews_dat_wide, topic == "ivory" & year <= 2007), plot(year, news_per_research, type = "o"));mtext("ivory")
with(subset(wos_gnews_dat_wide, topic == "ddt" & year <= 2007), plot(year, news_per_research, type = "o"));mtext("ddt")
dev.off()


print("acid rain")
print(summary(na.omit(subset(wos_gnews_dat_wide, topic == "acid rain" & year > acid_rain_xlim[1] & wos!= 0))$news_per_research))

print("ddt")
print(summary(na.omit(subset(wos_gnews_dat_wide, topic == "ddt" & year > ddt_xlim[1] & wos!= 0))$news_per_research))

print("ivory")
print(summary(na.omit(subset(wos_gnews_dat_wide, topic == "ivory" & year > elephant_xlim[1] & wos!= 0))$news_per_research))
