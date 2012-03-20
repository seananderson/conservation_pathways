add_ylabel <- function(text, line = 2, cex = 0.55, col = "grey30") {
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
  
plot_conservation_panels <- function(topic = c("acid rain", "ivory", "ddt") , xlim = c(1950, 2011), oma.upper = 8.5, wos.axis2 = NULL, gnews.axis2 = NULL, col.axis = "grey50", col.axis.labels = "grey50", annotate_df = NULL, gnews_multiplier = 1, ylim = NULL, ylabel = "Frequency", ylabel_news = "News articles\n(per million)", wos.axis2.labels = NULL) {
## annotate_df should have columns of year and label

topic <- topic[1]

par(mfrow = c(3, 1), cex = 0.55, mar = c(0, 0, 0, 0), oma = c(3, 4.2, oma.upper, 4.2))
par(tcl = -0.2)
par(mgp = c(3, 0.5, 0))
par(las = 1)

junk <- wos_gnews_dat[wos_gnews_dat$topic == topic & wos_gnews_dat$year >= xlim[1] & wos_gnews_dat$year <= xlim[2], ]
#if(is.null(ylim)) {
  with(subset(junk, type == "Web of Science"), plot(year, freq_scaled, type = "l", xlim = xlim, axes = FALSE, yaxs = "i", ylim = c(0, max(freq_scaled) * 1.1)))
#} else {
  #with(subset(junk, type == "Web of Science"), plot(year, freq_scaled, type = "l", xlim = xlim, axes = FALSE, yaxs = "i", ylim = ylim)) }
if(!is.null(wos.axis2))
  axis(2, col = col.axis, at = wos.axis2, labels = wos.axis2.labels, col.axis = col.axis.labels)
else
  axis(2, col = col.axis, col.axis = col.axis.labels)
  
box(col = col.axis)

if(!is.null(annotate_df)) d_ply(annotate_df, "year", transform, add_annotation(year, label))
fig_letter("(a) Research and news")
#par(xpd = NA)
#mtext("Relative frequency", side = 2, line = 3, adj = -8.5, cex = 0.55, las = 0, col = "grey30")
#par(xpd = FALSE)

par(new = TRUE)
with(subset(junk, type == "Google News Archive"), plot(year, freq_scaled, axes = FALSE, type = "l", xlim = xlim, lty = 2,  yaxs = "i", ylim = c(0, max(freq_scaled) * 1.1)))
if(!is.null(gnews.axis2))
  axis(4, col = col.axis, at = gnews.axis2, col.axis = col.axis)
else
  axis(4, col = col.axis, col.axis = col.axis)
par(xpd = NA)
mtext(ylabel_news, side = 4, line = 3, cex = 0.55, las = 0, col = "grey30")
par(xpd= FALSE)

# TODO temporary hardcoded text:
#text(1990, 880, "News", col = "grey40", cex = .9, pos = 4)
#text(1992, 340, "Research", col = "grey40", cex = .9, pos = 4)
## end

box(col = col.axis)
#d_ply(annotate_df, "year", transform, add_annotation(year, label, print.label = FALSE))

par(xpd = NA)
mtext(ylabel, side = 2, line = 2, cex = 0.55, las = 0, col = "grey30")
par(xpd= FALSE)

}


