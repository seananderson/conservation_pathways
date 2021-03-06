add_ylabel <- function(text, line = 2.1, cex = 0.55, col = "grey30") {
  par(xpd = NA)
  mtext(text, side = 2, line = line, cex = cex, las = 0, col = col)
  par(xpd= FALSE)
}
add_xlabel <- function(text = "Year", line = 2, cex = 0.55, col = "grey30") {
  par(xpd = NA)
  mtext(text, side = 1, line = line, cex = cex,  col = col)
  par(xpd= FALSE)
}

add_annotation <- function(year, label, print.label = TRUE, col.text = "grey35", col.abline = "#00000020", add.lines = TRUE) {
  par(xpd = FALSE)
  if(add.lines) abline(v = year, col = col.abline, lty = 1, lwd = 1.5)
  par(xpd = NA)
  if(print.label)
    text(year, par("usr")[4]*1.05, label, offset = 0, srt = 90, pos = 4, col = col.text)
  par(xpd = FALSE)
}

add_annotation_horizontal <- function(year, label, print.label = TRUE, col.text = "grey35", col.abline = "#00000020", add.lines = TRUE, ypos = 1.05, yseg = 1.05) {
  par(xpd = FALSE)
  if(add.lines) abline(v = year, col = col.abline, lty = 1, lwd = 1.5)
  par(xpd = NA)
  if(print.label)
    text(year, par("usr")[4]*ypos, label, offset = -0.7, srt = 0, pos = 4, col = col.text)
  segments(year, par("usr")[4], year, par("usr")[4]*yseg, col=col.abline, lwd = 1.5)
  par(xpd = FALSE)
}

  
plot_conservation_panels <- function(topic = c("acid rain", "ivory", "ddt") , xlim = c(1950, 2011), oma.upper = 8.5, wos.axis2 = NULL, gnews.axis2 = NULL, col.axis = "grey50", col.axis.labels = "grey50", annotate_df = NULL, gnews_multiplier = 1, ylim = NULL, ylabel = "Frequency", ylabel_news = "News articles\n(per million)", wos.axis2.labels = NULL, prop.test_ci_col = "#00000020") {
## annotate_df should have columns of year and label

topic <- topic[1]

par(mfrow = c(3, 1), cex = 0.55, mar = c(0, 0, 0, 0), oma = c(3, 4.25, oma.upper, 4.2))
par(tcl = -0.2)
par(mgp = c(3, 0.5, 0))
par(las = 1)

junk <- wos_gnews_dat[wos_gnews_dat$topic == topic & wos_gnews_dat$year >= xlim[1] & wos_gnews_dat$year <= xlim[2], ]
#if(is.null(ylim)) {
if(unique(junk$topic) != "ddt")
  with(subset(junk, type == "Web of Science"), plot(year, freq_scaled, type = "l", xlim = xlim, axes = FALSE, col = col.research, yaxs = "i", ylim = c(0, max(freq_scaled, na.rm = TRUE) * 1.1)))
else {
  with(subset(junk, type == "Web of Science" & year < 1960), plot(year, freq_scaled, type = "l", xlim = xlim, axes = FALSE, col = col.research, lty = 1, lwd = 1.05, yaxs = "i", ylim = c(0, max(freq_scaled, na.rm = TRUE) * 1.1)))
  #with(subset(junk, type == "Web of Science" & year < 1960), points(year, freq_scaled,bg = "white", pch = 21, cex = .9, lwd = .7 ))
  with(subset(junk, type == "Web of Science" & year > 1960), lines(year, freq_scaled, lty = "11", col = col.research, lwd = 1.3))
}

#} else {
  #with(subset(junk, type == "Web of Science"), plot(year, freq_scaled, type = "l", xlim = xlim, axes = FALSE, yaxs = "i", ylim = ylim)) }
if(!is.null(wos.axis2))
  axis(2, col = col.axis, at = wos.axis2, labels = wos.axis2.labels, col.axis = col.axis.labels)
else
  axis(2, col = col.axis, col.axis = col.axis.labels)
  
#box(col = col.axis)

### add prop.test() cis?
#with(subset(junk, type == "Web of Science"), polygon(c(year, rev(year)), c(prop_ci_low, rev(prop_ci_high)), border = NA, col = prop.test_ci_col))
###


if(!is.null(annotate_df)) d_ply(annotate_df, "year", transform, add_annotation_horizontal(year, label, ypos = ypos, yseg = yseg))
#fig_letter("A Research and news")
fig_letter(expression(paste(bold("A"), " Research and news")))
#par(xpd = NA)
#mtext("Relative frequency", side = 2, line = 3, adj = -8.5, cex = 0.55, las = 0, col = "grey30")
#par(xpd = FALSE)

#browser()
#if(topic == "ddt") with(subset(junk, type == "Web of Science" & year > 1960), points(year, freq_scaled, pch = 21, bg = "#FFFFFF80", cex = 0.5, col = "#00000060"))

par(new = TRUE)
with(subset(junk, type == "Google News Archive"), plot(year, freq_scaled, axes = FALSE, type = "l", xlim = xlim, col = col.public, lty = 2,  yaxs = "i", ylim = c(0, max(freq_scaled, na.rm = TRUE) * 1.1)))
if(!is.null(gnews.axis2))
  axis(4, col = col.axis, at = gnews.axis2, col.axis = col.axis)
else
  axis(4, col = col.axis, col.axis = col.axis)
par(xpd = NA)
mtext(ylabel_news, side = 4, line = 3.1, cex = 0.55, las = 0, col = "grey30")
par(xpd= FALSE)

### add prop.test() cis?
#with(subset(junk, type == "Google News Archive"), polygon(c(year, rev(year)), c(prop_ci_low, rev(prop_ci_high)), border = NA, col = prop.test_ci_col))
###

box(col = col.axis)
#d_ply(annotate_df, "year", transform, add_annotation(year, label, print.label = FALSE))

par(xpd = NA)
mtext(ylabel, side = 2, line = 2.1, cex = 0.55, las = 0, col = "grey30")
par(xpd= FALSE)

}


