# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Mar 23, 2012
# Last modified: Mar 24, 2012
# Purpose:       look at raw numerator and denominator frequencies,
# possibly for the supplement
# ====================================================================


pdf("../fig/raw-denominators.pdf", width = 3.4, height = 3.4)
par(mfrow = c(2, 1), cex = 0.55, mar = c(0, 0, 0, 0), oma = c(3, 4.45, .75, .5))
par(tcl = -0.2)
par(mgp = c(3, 0.5, 0))
par(las = 1)
with(subset(wos_gnews_dat, topic == "ddt" & type == "Google News Archive"), plot(year, the_freq/1e6, type = "l", axes = FALSE, yaxs = "i",xlim = c(1950, 2010),  ylim = c(0, 80)))
box(col = "grey50")
#axis(1, col = "grey50", labels = FALSE)
axis(2, col = "grey50", col.axis = "grey40")
fig_letter(expression(paste(bold("A"))))
  text(par("usr")[1] + (par("usr")[2] - par("usr")[1]) * 0.040, par("usr")[4] - (par("usr")[4] - par("usr")[3]) * 0.1, "Instances of the word", pos = 4, col = "grey30")
  text(par("usr")[1] + (par("usr")[2] - par("usr")[1]) * 0.040, par("usr")[4] - (par("usr")[4] - par("usr")[3]) * 0.18, "\"the\" in Google News Archives", pos = 4, col = "grey30")
par(xpd = NA)
mtext("Instances per year\n(millions)", side = 2, outer = FALSE, line = 2.1, las = 3, cex = 0.7, col = "grey30")
par(xpd = FALSE)

with(subset(wos_gnews_dat, topic == "ddt" & type == "Web of Science"), plot(year, wos_all_freq/1e6, type = "l", axes = FALSE, xlim = c(1950, 2010), ylim = c(0, 1), yaxs = "i"))
box(col = "grey50")
axis(1, col = "grey50", col.axis = "grey40")
axis(2, col = "grey50", at = seq(0, 0.8, 0.2), col.axis = "grey40")
fig_letter(expression(paste(bold("B"))))
  text(par("usr")[1] + (par("usr")[2] - par("usr")[1]) * 0.040, par("usr")[4] - (par("usr")[4] - par("usr")[3]) * 0.1, "Number of publications", pos = 4, col = "grey30")
  text(par("usr")[1] + (par("usr")[2] - par("usr")[1]) * 0.040, par("usr")[4] - (par("usr")[4] - par("usr")[3]) * 0.18, "indexed in Web of Science", pos = 4, col = "grey30")
par(xpd = NA)
mtext("Publications per year\n(millions)", side = 2, outer = FALSE, line = 2.1, las = 3, cex = 0.7, col = "grey30")
par(xpd = FALSE)
mtext("Year", side = 1, outer = FALSE, line = 2.0, cex = 0.7, col = "grey30")
dev.off()


# for the search terms:
pdf("../fig/raw-numerators.pdf", width = 6.0, height = 4.5)
par(mfrow = c(3, 2), cex = 0.55, mar = c(0, 3, 0, 0), oma = c(3.5, .7, 2.75, 2.5))
par(tcl = -0.2)
par(mgp = c(3, 0.5, 0))
par(las = 1)
panel <- 0
topics <- c("acid rain", "ddt", "ivory")
for(topic_i in topics) {
  for(type_j in c("Web of Science",  "Google News Archive")) {
    panel <- panel + 1
    with(subset(wos_gnews_dat, topic == topic_i & type == type_j), plot(year, freq, type = "l", xlim = c(1940, 2010), yaxs = "i", ylim = c(0, max(freq)*1.06), axes = FALSE))
box(col = "grey50")
yaxis_at <- NULL
if(topic_i == "ddt" & type_j == "Google News Archive") yaxis_at <- seq(0, 600, 150)
if(topic_i == "ivory" & type_j == "Google News Archive") yaxis_at <- seq(0, 100, 20)
if(topic_i == "ddt" & type_j == "Web of Science") yaxis_at <- seq(0, 125, 25)
if(is.null(yaxis_at))
axis(2, col = "grey50", col.axis = "grey40")
else
axis(2, col = "grey50", col.axis = "grey40", at = yaxis_at)
if(panel %in% c(5, 6)) axis(1, col = "grey50", labels = TRUE, col.axis = "grey40")
panel_letter <- LETTERS[panel]
  text(par("usr")[1] + (par("usr")[2] - par("usr")[1]) * 0.005, par("usr")[4] - (par("usr")[4] - par("usr")[3]) * 0.1, substitute(panel_letter, list(panel_letter = panel_letter)), pos = 4, col = "grey30", font = 2)
#if(topic_i == "acid rain") topic_title <- "Acid rain"
#if(topic_i == "ddt") topic_title <- "DDT"
#if(topic_i == "ivory") topic_title <- "Ivory"

  #text(par("usr")[1] + (par("usr")[2] - par("usr")[1]) * 0.005, par("usr")[4] - (par("usr")[4] - par("usr")[3]) * 0.1, substitute(topic_title, list(topic_title = topic_title)), pos = 4, col = "grey30", font = 1)

  #text(par("usr")[1] + (par("usr")[2] - par("usr")[1]) * 0.005, par("usr")[4] - (par("usr")[4] - par("usr")[3]) * 0.1, substitute(topic_title, list(topic_title = topic_title)), pos = 4, col = "grey30", font = 1)
  }
}
mtext("Year", side = 1, line = 2.4, outer = TRUE, col = "grey30", cex = 0.7)
par(xpd = NA)
mtext("Raw frequency per year", side = 2, line = -0.5, outer = TRUE, col = "grey30", las = 3, cex = 0.7)
par(xpd = FALSE)

mtext("Web of Science", side = 3, line = 0.6, outer= TRUE, col = "grey30", las = 1, cex = 0.7, adj = 0.25)
mtext("Google News Archives", side = 3, line = 0.6, outer= TRUE, col = "grey30", las = 1, cex = 0.7, adj = 0.85)

mtext("Acid rain", side = 4, line = 0.6, outer= TRUE, col = "grey30", las = 3, cex = 0.7, adj = 0.9)
mtext("DDT", side = 4, line = 0.6, outer= TRUE, col = "grey30", las = 3, cex = 0.7, adj = 0.5)
mtext("Ivory", side = 4, line = 0.6, outer= TRUE, col = "grey30", las = 3, cex = 0.7, adj = 0.12)


dev.off()
