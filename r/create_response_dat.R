# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Dec 12, 2011
# Last modified: Dec 23, 2011
# Purpose:       create the response data
# ====================================================================

##################
# acid rain data:
ph <- read.csv("../data/raw-data/Acid-rain-dep-1978-2010.csv", header = T, comment.char = "#", na.strings = "-9")
require(plyr)
ph <- transform(ph, State = substr(SiteID, 1, 2))
ph <- transform(ph, ph.low = ifelse(pH < 5, TRUE, FALSE))

# median jacknife plot:
#plot(ph$Year, ph$pH, type = "n");for(state.i in unique(ph$State)) {x<-ddply(subset(ph, !State %in% state.i), "Year", summarize, mean.ph =  median(pH, na.rm = TRUE));with(x, lines(Year, mean.ph, col = "#00000020"))}

# which states to show:
#out <- ddply(ph, "State", summarize, med.ph = median(pH, na.rm = TRUE), slope.ph = as.numeric(coef(lm(pH~Year))[2]), num.obs = length(pH))
#out <- out[order(out$slope.ph), ]
#out <- subset(out, num.obs >= 100)
#out[order(out$med.ph), ]
# so, most negative slope = OR, most positive slope = IL, lowest
# median = OH, highest median = ID
ph2 <- subset(ph, State %in% c("OR", "IL", "OH", "ID"))
ph2.summary <- ddply(ph2, c("Year","State"), function(x) { 
  q <- quantile(x$pH, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  data.frame(q0.25 = q[1], q0.50 = q[2], q0.75 = q[3])
})
ph2.summary.overall <- ddply(ph, c("Year"), function(x) { 
  q <- quantile(x$pH, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  data.frame(q0.25 = q[1], q0.50 = q[2], q0.75 = q[3])
})
ph2.summary.overall$State <- "US"
ph2.summary.overall <- ph2.summary.overall[,c("Year", "State", "q0.25", "q0.50", "q0.75")]
ph2.summary <- rbind(ph2.summary, ph2.summary.overall)
ph2.summary$lty <- 2
ph2.summary[ph2.summary$State == "US", "lty"] <- 1

ph.OK <- ddply(ph, "Year", summarize, num.OK = 1 - sum(ph.low, na.rm = T)/length(ph.low))

#ph <- ddply(ph, c("Year", "State"), function(x) { 
#ph <- ddply(ph, c("Year"), function(x) { 
  #q <- quantile(x$pH, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  #data.frame(q0.25 = q[1], q0.50 = q[2], q0.75 = q[3])
#})
#names(ph) <- c("year", "y_lower", "y", "y_upper")
#ph$id <- "United States"
#ph <- ph[,c("year", "id", "y", "y_lower", "y_upper")]

##################
# ddt data:
ddt.ev <- read.table("../data/MtEvererstConcDDT.csv", sep = ",", comment.char = "#", header = TRUE)
ddt.ev <- transform(ddt.ev, conc.frac = conc./max(conc.))
ddt.ev <- transform(ddt.ev, conc.frac.sd = conc.st.dev/max(conc.))

ddt.fish.swe <- read.table("../data/reponse.ddt.fish.sapota.2006.and.lindell.2001.txt", comment.char = "#", header = TRUE)
ddt.fish.swe <- subset(ddt.fish.swe, system == "lake.Vattern.Sweden")
ddt.fish.swe <- transform(ddt.fish.swe, conc.frac = DDT.ug.g_1.m.l./max(DDT.ug.g_1.m.l.))
ddt.global <- read.table("../data/ddt.global.usage.txt", sep = "\t", header = TRUE)
ddt.polar <- read.table("../data/response.ddt.polar.braune2005.txt", sep = "\t", header = TRUE)
ddt.polar <- transform(ddt.polar, conc.frac = DDT.ng_g.ww/max(DDT.ng_g.ww))
ddt.polar <- transform(ddt.polar, conc.frac.sd = St.dev/max(DDT.ng_g.ww))

##################
# ivory data:
elephant_pop <- read.table("../data/response.african.elephant.population.est.txt", sep = "\t", comment.char = "#", header = TRUE, strip.white = TRUE)[,-8]
ivory_export_recent <- read.table("../data/response.elephant.export.1989-2009.txt", sep = "\t", comment.char = "#", header = TRUE, strip.white = TRUE)
ivory_export_early <- read.table("../data/response.elephant.export.1950-1987.txt", sep = "\t", comment.char = "#", header = TRUE, strip.white = TRUE)
elephant_pop_modeled <- read.table("../data/response.elephant.abundance.milner-gulland.txt", sep = "\t", comment.char = "#", header = TRUE, strip.white = TRUE)
ivory_export_early_raw <- read.table("../data/response.elephant.export.1950-1987.raw.txt", sep = "\t", comment.char = "#", header = TRUE, strip.white = TRUE, na.string = "NA")

