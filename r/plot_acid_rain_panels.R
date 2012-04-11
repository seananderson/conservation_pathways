# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Dec 17, 2011
# Last modified: Apr 07, 2012
# Purpose:       Make the acid rain figure
# ====================================================================

acid_rain_xlim <- c(1960, 2011)
acid_rain_annotations <- read.table("../data/acid.rain.policy.dates.txt", header = TRUE, stringsAsFactors = FALSE, sep = "\t")
acid_rain_annotations$label <- sub("\\\\n", "\\\n", acid_rain_annotations$label)

pdf("../fig/acid-rain-ts5.pdf", width = 3.4, height = 3.8)
plot_conservation_panels(topic = "acid rain", xlim = acid_rain_xlim, wos.axis2 = seq(0, 3, 0.4)*100, gnews.axis2 = seq(0, 3, 0.3)*1000, annotate_df = acid_rain_annotations, ylabel = "Science publications\n(per million)", gnews_multiplier = 0.1, ylim = c(0, 120))
text(1989.5, 800, "News", col = "grey40", cex = .9, pos = 4)
text(1992, 500, "Research", col = "grey40", cex = .9, pos = 4)

#abline(v = 2008)
plot(1,1, type = "n", xlim = acid_rain_xlim, ylim = c(0, 39), axes = FALSE, yaxs = "i")
axis(2, col = col.axis, col.axis = col.axis.label)
nox <- read.table("../data/nox.so2.usa.txt", header = TRUE)
with(nox, lines(Year, NOx, type = "l", lty = 2, col = col.stressor))
text(2000, 24, "NOx", col = col.text, pos = 4, cex =0.9)
with(nox, lines(Year, SO2, type = "l", lty = 1, col = col.stressor))
text(2000, 11.5, expression(SO[2]), col = col.text, pos = 4, cex =0.9)
#fig_letter("(b) Stressor")
fig_letter(expression(paste(bold("B"), " Stressor")))
add_ylabel("US emissions\n(million t)")
box(col = col.axis)
with(acid_rain_annotations, add_annotation(year, label, print.label = FALSE))

plot(1,1, type = "n", xlim = acid_rain_xlim, ylim = c(3.9, 6.1), axes = FALSE, yaxs = "i")

# don't plot the US line here, it will be done by jacknife below
ph2.summary[ph2.summary$lty == 1, "lty"] <- 0
d_ply(ph2.summary, "State", transform, lines(Year, q0.50, col = col.response, lty = lty), text(2009.1, q0.50[length(q0.50)], unique(State), pos = 4, col = "grey40", cex = 0.8))

#points(c(1964, 1970, 1970), c(2.1, 3.91, 4.02), pch = 19, col = "#00000090")
points(c(1964), c(2.1), pch = 19, col = "grey25", cex = 0.8)
#text(1964.1, 2.1, "NE US low", pos = 4, col = "grey40", cex = 0.9)
text(1961, 4.4, "New Hampshire\nannual mean", pos = 4, col = "grey40", cex = 0.9)
text(2000.2, 4.15, "Annual mean\nby state", pos = 4, col = "grey40", cex = 0.9)

#for(state.i in unique(ph$State)) {x<-ddply(subset(ph, !State %in% state.i), "Year", summarize, mean.ph =  median(pH, na.rm = TRUE));with(x, lines(Year, mean.ph, col = "#00000017", lwd = 0.5))}
for(state.i in unique(ph$State)) {x<-ddply(subset(ph, !State %in% state.i), "Year", summarize, mean.ph =  median(pH, na.rm = TRUE));with(x, lines(Year, mean.ph, col = paste(col.response, "17", sep = ""), lwd = 0.5))}

abline(h = 5.6, col = "#00000070", lty = 3, lwd = 1.1)

likens <- read.table("../data/Likens-and-Bormann-NH-pH.txt", header = TRUE, sep = "\t")
with(likens, lines(year, pH, lty = 6, col = col.response))

axis(2, col = col.axis, col.axis = col.axis.label, at = seq(4, 5.5, .5))
axis(1, col = col.axis, col.axis = col.axis.label)
box(col = col.axis)
add_ylabel("Rain pH\n")
#fig_letter("(c) Response")
fig_letter(expression(paste(bold("C"), " Response")))
add_xlabel()
with(acid_rain_annotations, add_annotation(year, label, print.label = FALSE))

dev.off()

