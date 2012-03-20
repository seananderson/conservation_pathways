# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Dec 01, 2011
# Last modified: Dec 01, 2011
# Purpose:       test evidence for correlation at lags
# ====================================================================




library(plyr)
library(reshape)
library(nlme)

d.relative <- ddply(d, c("type", "topic"), function(x) {
  if(unique(x$type == "Google News Archive")) rel_freq <- x$freq / x$the_freq
  if(unique(x$type == "Web of Science")) rel_freq <- x$freq / x$gscholar_freq
  if(unique(x$type == "Google Books")) rel_freq <- x$freq / x$gbooks_words
  if(unique(x$type == "Response")) rel_freq <- x$freq
  data.frame(year = x$year, freq = rel_freq)
})

d.wide <- cast(d.relative, year +topic ~ type, value = "freq")
names(d.wide) <- gsub(" ", "_", names(d.wide))



#d.wide <-d.wide[-which(d.wide$Web_of_Science == 0 & d.wide$topic == "ddt" & !is.na(d.wide$Web_of_Science)), ]
#d.wide <-d.wide[-which(d.wide$Web_of_Science == 0 & d.wide$topic == "acid rain" & !is.na(d.wide$Web_of_Science)), ]

junk <- ddply(d.wide, "topic",transform,                                                                                  
  wos_minus5 = c(Web_of_Science[6:(length(Web_of_Science))], rep(NA, 5)),
  wos_minus4 = c(Web_of_Science[5:(length(Web_of_Science))], rep(NA, 4)),
  wos_minus3 = c(Web_of_Science[4:(length(Web_of_Science))], rep(NA, 3)),
  wos_minus2 = c(Web_of_Science[3:(length(Web_of_Science))], rep(NA, 2)),
  wos_minus1 = c(Web_of_Science[2:(length(Web_of_Science))], rep(NA, 1)),
  wos_plus0 = c(Web_of_Science[1:(length(Web_of_Science))]),
  wos_plus1 = c(rep(NA, 1), Web_of_Science[1:(length(Web_of_Science)-1)]),
  wos_plus2 = c(rep(NA, 2), Web_of_Science[1:(length(Web_of_Science)-2)]),
  wos_plus3 = c(rep(NA, 3), Web_of_Science[1:(length(Web_of_Science)-3)]),
  wos_plus4 = c(rep(NA, 4), Web_of_Science[1:(length(Web_of_Science)-4)]),
  wos_plus5 = c(rep(NA, 5), Web_of_Science[1:(length(Web_of_Science)-5)])
)  

junk$Google_Books <- NULL
junk$Response <- NULL # TODO temporary until we get the full response data
junk <- na.omit(junk)

# TODO note this
junk <-junk[-which(junk$year <= 1970 & junk$topic == "acid rain"), ]
junk <-junk[-which(junk$year <= 1940 & junk$topic == "ddt"), ]
junk <-junk[-which(junk$year <= 1960 & junk$topic == "ivory"), ]


d.long <- melt(junk[,-c(3, 4)], id.vars = c("year","topic"))
d.long <- merge(d.long, junk[,c("year", "topic", "Google_News_Archive")])

# TODO take out 0s from web of science for now...
d.long <- d.long[-which(d.long$value == 0), ]
#d.long <- d.long[-which(d.long$Google_News_Archive == 0), ]


scale.dat <- function(x){    
  #x <- log(x+.5)
  #x <- log(x)
  x <- x - mean(x)
  #x <- x / (2 * sd(x))
  x <- x / (sd(x))
  x
}

d.long <- ddply(d.long, c("topic", "variable"), transform, scaled_value = scale.dat(value))
d.long <- ddply(d.long, c("topic", "variable"), transform, Google_News_Archive_scaled = scale.dat(Google_News_Archive))

d.long <- ddply(d.long, c("topic", "variable"), transform, cor = as.numeric(cor.test(Google_News_Archive_scaled, scaled_value)$estimate), ci.l = as.numeric(cor.test(Google_News_Archive_scaled, scaled_value)$conf.int[1]), ci.u = as.numeric(cor.test(Google_News_Archive_scaled, scaled_value)$conf.int[2]))


#d.long <- ddply(d.long, c("topic", "variable"), transform, gls.coef = {
#m <- gls(Google_News_Archive_scaled~scaled_value, correlation = corAR1(form=~year))
#coef(m)[2]
##browser()
#} ,
#gls.l = {
#m <- gls(Google_News_Archive_scaled~scaled_value, correlation = corAR1(form=~year))
#confint(m)[2,1]
#},
#gls.u = {
#m <- gls(Google_News_Archive_scaled~scaled_value, correlation = corAR1(form=~year))
#confint(m)[2,2]
#}
#)

# reverse the order:
d.long <- ddply(d.long, c("topic", "variable"), transform, gls.coef = {
m <- gls(scaled_value~Google_News_Archive_scaled, correlation = corAR1(form=~year))
coef(m)[2]
#browser()
} ,
gls.l = {
m <- gls(scaled_value~Google_News_Archive_scaled, correlation = corAR1(form=~year))
confint(m)[2,1]
},
gls.u = {
m <- gls(scaled_value~Google_News_Archive_scaled, correlation = corAR1(form=~year))
confint(m)[2,2]
}
)





d.long <- ddply(d.long, c("topic", "variable"), transform, gls.coef.noAR = {
m <- gls(Google_News_Archive_scaled~scaled_value)
coef(m)[2]
} ,
gls.l.noAR = {
m <- gls(Google_News_Archive_scaled~scaled_value)
confint(m)[2,1]
},
gls.u.noAR = {
m <- gls(Google_News_Archive_scaled~scaled_value)
confint(m)[2,2]
}
)

d.long <- ddply(d.long, c("topic", "variable"), transform, Google_News_Archive_scaled_diff = c(NA, diff(Google_News_Archive_scaled)), scaled_value_diff = c(NA, diff(scaled_value)))

d.long <-
  ddply(d.long, c("topic", "variable"), transform, r_diff = {
corr <- cor.test(Google_News_Archive_scaled, scaled_value_diff)
corr$estimate
},
r_diff.l = {
corr <- cor.test(Google_News_Archive_scaled, scaled_value_diff)
corr$conf.int[1]
} ,
r_diff.u = {
corr <- cor.test(Google_News_Archive_scaled, scaled_value_diff)
corr$conf.int[2]
}
)

library(ggplot2)
p <- ggplot(d.long, aes(Google_News_Archive_scaled, scaled_value, col = cor))  + stat_smooth(method = "lm", col = "#000000") + geom_point() + facet_grid(topic~variable)
ggsave("../fig/gg-scatter-lags-lm.pdf", plot = p, width = 13, height = 5)

d.gls.summarize <- ddply(d.long, c("topic", "variable"), summarize, gls.coef = unique(gls.coef), gls.l = unique(gls.l), gls.u = unique(gls.u))
d.gls.summarize$lag <- rep(seq(-5, 5, 1), 3)

d.cor.summarize <- ddply(d.long, c("topic", "variable"), summarize, r.coef = unique(r_diff), l = unique(r_diff.l), u = unique(r_diff.u))
d.cor.summarize$lag <- rep(seq(-5, 5, 1), 3)

d.gls.noAR.summarize <- ddply(d.long, c("topic", "variable"), summarize, gls.coef = unique(gls.coef.noAR), l = unique(gls.l.noAR), u = unique(gls.u.noAR))
d.gls.noAR.summarize$lag <- rep(seq(-5, 5, 1), 3)

p <- ggplot(d.gls.summarize, aes(lag, gls.coef)) + geom_pointrange(aes(x = lag, y = gls.coef, ymin = gls.l, ymax = gls.u)) + facet_wrap( ~ topic, ncol = 1) + geom_abline(aes(intercept = 0, slope = 0),col = "#00000060")
ggsave("../fig/gg-gls-lags.pdf", plot = p, width = 6, height = 7)

p <- ggplot(d.gls.noAR.summarize, aes(lag, gls.coef)) + geom_pointrange(aes(x = lag, y = gls.coef, ymin = l, ymax = u)) + facet_wrap( ~ topic, ncol = 1) + geom_abline(aes(intercept = 0, slope = 0),col = "#00000060")
ggsave("../fig/gg-gls-noAR-lags.pdf", plot = p, width = 6, height = 7)

d.ggplot <- melt(junk[,c("year","topic", "Google_News_Archive", "Web_of_Science")], id.vars = c("year", "topic"))
d.ggplot <- ddply(d.ggplot, c("topic", "variable"), transform, value_scaled = scale.dat(value))
p <- ggplot(d.ggplot, aes(year, value_scaled, col = variable)) +  facet_wrap( ~ topic, scales = "free", ncol = 1) + geom_line() + ylab("scaled log frequency") + xlab("Year")
ggsave("../fig/gg-ts.pdf", plot = p, width = 8, height = 7)

d.ggplot <- ddply(d.ggplot, c("topic", "variable"), transform, value_scaled_diff = c(NA, diff(value_scaled)))

p <- ggplot(d.ggplot, aes(year, value_scaled_diff, col = variable)) +  facet_wrap( ~ topic, scales = "free_y", ncol = 1) + geom_line() + ylab("diff scaled log frequency") + xlab("Year") 
ggsave("../fig/gg-ts-diff.pdf", plot = p, width = 8, height = 7)

p <- ggplot(d.cor.summarize, aes(lag, r.coef)) + geom_pointrange(aes(x = lag, y = r.coef, ymin = l, ymax = u)) + facet_wrap( ~ topic, ncol = 1) + geom_abline(aes(intercept = 0, slope = 0),col = "#00000060")
ggsave("../fig/gg-diff-cor-lags.pdf", plot = p, width = 6, height = 7)



######
#ARMA structure: 
ivory <- subset(d.long, topic == "ivory" & variable == "wos_plus0")
ddt <- subset(d.long, topic == "ddt" & variable == "wos_plus0")
rain <- subset(d.long, topic == "acid rain" & variable == "wos_plus0")

#sapply(1:3, function(p) {sapply(0:3, function(q) {AIC(with(ddt, gls(value~Google_News_Archive, correlation = corARMA(p=p,q=q,form=~year))))})})
#sapply(1:3, function(p) {sapply(0:3, function(q) {AIC(with(ivory, gls(value~Google_News_Archive, correlation = corARMA(p=p,q=q,form=~year))))})})
#sapply(1:3, function(p) {sapply(0:3, function(q) {AIC(with(rain, gls(value~Google_News_Archive, correlation = corARMA(p=p,q=q,form=~year))))})})

#acid rain, p = 1, q = 0
#ddt: p = 3, q = 2
#ivory: p = 1, q = 0

#m1 <- with(ddt, gls(value~Google_News_Archive, correlation = corAR1(form=~year)))
#m2 <- with(ddt, gls(value~Google_News_Archive, correlation = corARMA(p=3, q = 2, form=~year)))
#acf(residuals(m1, type = "normalized"))
#plot(residuals(m1, type = "normalized"))
#acf(residuals(m2, type = "normalized"))

