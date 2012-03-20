# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Dec 21, 2011
# Last modified: Dec 27, 2011
# Purpose:       Make the DDT figure
# ====================================================================

ddt_xlim <- c(1937, 2007)

ddt_annotations <- read.table("../data/ddt.policy.dates.txt", header = TRUE, stringsAsFactors = FALSE, sep = "\t", strip.white = TRUE)
ddt_annotations$label <- gsub("\\\\n", "\\\n", ddt_annotations$label)
ddt_annotations <- transform(ddt_annotations, year = start_year + (end_year - start_year)/2)
ddt_annotations$col <- "#5A5A5A" # colour for the country names
ddt_annotations$col2 <- "#5A5A5A93" # colour for the "begins" or "bans"

# the shaded rectangles:
add_ddt_rects <- function(add.text = FALSE) {
  with(ddt_annotations,rect(start_year, par("usr")[3], end_year, par("usr")[4], col = paste(col, "10", sep = ""), border = paste(col, "36", sep = ""), lwd = 1.3))
  par(xpd = NA)

## jittering labels so they don't overlap
   if(add.text) {
     jitter_ddt_annotations <- ddt_annotations
     jitter_ddt_annotations[jitter_ddt_annotations$label == "Sweden", "start_year"] <- 1947.8
     jitter_ddt_annotations[jitter_ddt_annotations$label == "Canada", "start_year"] <- 1944.9
     jitter_ddt_annotations[jitter_ddt_annotations$label == "US", "start_year"] <- 1942

## setting up for two-toned annotations
     the_labels1_begins <- as.expression(parse(text=paste(jitter_ddt_annotations$label,"~phantom(begins)",sep="")))
     the_labels1_bans <- as.expression(parse(text=paste(jitter_ddt_annotations$label,"~phantom(bans)",sep="")))

     the_labels2_begins <- as.expression(parse(text=paste("phantom(", jitter_ddt_annotations$label, ")","~begins",sep="")))
     the_labels2_bans <- as.expression(parse(text=paste("phantom(", jitter_ddt_annotations$label, ")","~bans",sep="")))

## "begins" annotations
     with(jitter_ddt_annotations, text(start_year, par("usr")[4]*1.05, the_labels1_begins, offset = 0, srt = 90, pos = 4, col = col))
     with(jitter_ddt_annotations, text(start_year, par("usr")[4]*1.05, the_labels2_begins, offset = 0, srt = 90, pos = 4, col = col2))

## "bans" annotations
     with(jitter_ddt_annotations, text(end_year, par("usr")[4]*1.05, the_labels1_bans, offset = 0, srt = 90, pos = 4, col = col))
     with(jitter_ddt_annotations, text(end_year, par("usr")[4]*1.05, the_labels2_bans, offset = 0, srt = 90, pos = 4, col = col2))
   }
  par(xpd = FALSE)
}

pdf("../fig/ddt-ts2.pdf", width = 3.4, height = 3.8)
plot_conservation_panels(topic = "ddt", xlim = ddt_xlim, annotate_df = NULL, gnews_multiplier = 40, ylabel = "Science pub. (per million)\n", ylim = c(0, 3400), wos.axis2 = seq(0, 7000, 2000), wos.axis2.labels = seq(0, 7000, 2000), gnews.axis2 = seq(0, 70, 20))
text(1990, 6.5, "Research", col = "grey40", cex = .9, pos = 4)
arrows(1996, 4000, 1996, 1000, length = 0.05, lwd = 0.9, col = "grey40")
text(1971.5, 45, "News", col = "grey40", cex = .9, pos = 4)

add_ddt_rects(add.text = TRUE)

# global usage:
plot(1,1, type = "n", xlim = ddt_xlim, ylim = c(0, 97), axes = FALSE, yaxs = "i")
with(ddt.global, lines(year, Kt.a))
with(ddt.global, points(year, Kt.a, pch = 21, bg = "white", cex = 0.9, lwd = .8))

add_ddt_rects()

box(col = col.axis)
axis(2, col = col.axis, col.axis = col.axis.label)
add_ylabel("Global DDT\nusage (Kt/a)")
fig_letter("(b) First-order response")

plot(1,1, type = "n", xlim = ddt_xlim, ylim = c(0, 120), axes = FALSE, yaxs = "i")
with(ddt.ev, lines(year, conc.frac*100, lty = 2))
with(ddt.ev, polygon(c(year, rev(year)), c(conc.frac*100 + conc.frac.sd*100, rev(conc.frac*100 - conc.frac.sd*100)), col = "#00000020", border = NA))
#text(2000, 34, "Everest ice core", col = "grey40", cex = 0.9)
#text(2000, 34, "Everest ice core", col = "grey40", cex = 0.9)
text(1967.5, 27, "Everest ice core", col = "grey40", cex = 0.9, pos = 2)
with(ddt.fish.swe, lines(year, conc.frac*100, lty = 1))
text(1967, 78, "Arctic char\nLake Vattern, Sweden", pos = 2, col = "grey40", cex = 0.9)

with(ddt.polar, lines(year, conc.frac*100, lty = 3, lwd = 1.0))
with(ddt.polar, segments(year, conc.frac*100 + conc.frac.sd*100, year, conc.frac*100 - conc.frac.sd*100, lty = 1, lwd = 0.6, col = "grey30"))
with(ddt.polar, points(year, conc.frac*100, bg = "white", pch = 21, cex= .9, lwd = 0.7))
text(1983.6, 70, "Polar bears\nHudson Bay, Canada", pos = 4, col = "grey40", cex = 0.9)

#text(1967, 60, "Arctic char\nLake Vattern, Sweden", pos = 2, col = "grey40", cex = 0.9)



axis(2, col = col.axis, col.axis = col.axis.label, at = seq(0, 100, 25))

box(col = col.axis)
fig_letter("(c) Second-order response")

add_ylabel("Percent of maximum\nDDT concentration")
add_xlabel()
axis(1, col = col.axis, col.axis = col.axis.label)
axis(1, col = col.axis, col.axis = col.axis.label, at = 2008)

add_ddt_rects()
dev.off()

