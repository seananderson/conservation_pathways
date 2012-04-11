# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Apr 07, 2012
# Last modified: Apr 07, 2012
# Purpose:       Get average rates to describe the news and research
# trends in general
# ====================================================================


#acid_rain_xlim <- c(1960, 2011)
#ddt_xlim <- c(1937, 2008)
#elephant_xlim <- c(1955, 2008)

print("acid rain")
print("news")
print(summary(subset(wos_gnews_dat, topic == "acid rain" & year > acid_rain_xlim[1] & year < acid_rain_xlim[2] & type == "Google News Archive")$freq_scaled))
print("science")
print(summary(subset(wos_gnews_dat, topic == "acid rain" & year > acid_rain_xlim[1] & year < acid_rain_xlim[2] & type == "Web of Science")$freq_scaled))

print("ivory")
print("news")
print(summary(subset(wos_gnews_dat, topic == "ivory" & year > elephant_xlim[1] & year < elephant_xlim[2] & type == "Google News Archive")$freq_scaled))
print("science")
print(summary(subset(wos_gnews_dat, topic == "ivory" & year > elephant_xlim[1] & year < elephant_xlim[2] & type == "Web of Science")$freq_scaled))

print("ddt")
print("news")
print(summary(subset(wos_gnews_dat, topic == "ddt" & year > ddt_xlim[1] & year < ddt_xlim[2] & type == "Google News Archive")$freq_scaled))
print("science")
print(summary(subset(wos_gnews_dat, topic == "ddt" & year > ddt_xlim[1] & year < ddt_xlim[2] & type == "Web of Science")$freq_scaled))
