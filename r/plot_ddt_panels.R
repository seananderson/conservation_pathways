# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Dec 21, 2011
# Last modified: Apr 09, 2012
# Purpose:       Make the DDT figure
# ====================================================================

ddt_xlim <- c(1937, 2008)

ddt_annotations <- read.table("../data/ddt.policy.dates.txt", header = TRUE, stringsAsFactors = FALSE, sep = "\t", strip.white = TRUE)
ddt_annotations$label <- gsub("\\\\n", "\\\n", ddt_annotations$label)
ddt_annotations <- transform(ddt_annotations, year = start_year + (end_year - start_year)/2)
ddt_annotations$col <- "#5A5A5A" # colour for the country names
ddt_annotations$col2 <- "#5A5A5A93" # colour for the "begins" or "bans"

# the shaded rectangles:
add_ddt_rects <- function(add.text = FALSE) {
  with(ddt_annotations, rect(start_year, par("usr")[3], end_year, par("usr")[4], col = paste(col, "10", sep = ""), border = paste(col, "36", sep = ""), lwd = 1.5))
  #with(ddt_annotations, rect(start_year, par("usr")[3], end_year, par("usr")[4], col = paste(col, "10", sep = ""), border = FALSE, lwd = 1.5))
  #with(ddt_annotations, segments(end_year, par("usr")[4], end_year, par("usr")[4]*ypos * 0.96, col= "red", lwd = 1.5))
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

### vertically:
# "begins" annotations
     #with(jitter_ddt_annotations, text(start_year, par("usr")[4]*1.05, the_labels1_begins, offset = 0, srt = 90, pos = 4, col = col))
     #with(jitter_ddt_annotations, text(start_year, par("usr")[4]*1.05, the_labels2_begins, offset = 0, srt = 90, pos = 4, col = col2))

# "bans" annotations
     #with(jitter_ddt_annotations, text(end_year, par("usr")[4]*1.05, the_labels1_bans, offset = 0, srt = 90, pos = 4, col = col))
     #with(jitter_ddt_annotations, text(end_year, par("usr")[4]*1.05, the_labels2_bans, offset = 0, srt = 90, pos = 4, col = col2))


### Try horizontally:
## "begins" annotations
     with(ddt_annotations, text(start_year, par("usr")[4]*ypos, the_labels1_begins, offset = -0.15, srt = 0, pos = 4, col = col))
     with(ddt_annotations, text(start_year, par("usr")[4]*ypos, the_labels2_begins, offset = -0.15, srt = 0, pos = 4, col = col2))
  with(ddt_annotations, segments(start_year, par("usr")[4], start_year, par("usr")[4]*ypos * 0.96, col= "#00000020", lwd = 1.5))

## "bans" annotations
     with(ddt_annotations, text(end_year, par("usr")[4]*ypos*1.007, the_labels1_bans, offset = -0.15, srt = 0, pos = 4, col = col))
     with(ddt_annotations, text(end_year, par("usr")[4]*ypos*1.007, the_labels2_bans, offset = -0.15, srt = 0, pos = 4, col = col2))
  with(ddt_annotations, segments(end_year, par("usr")[4], end_year, par("usr")[4]*ypos * 0.96, col= "#00000020", lwd = 1.5))

   }
  par(xpd = FALSE)
}

pdf("../fig/ddt-ts5.pdf", width = 3.4, height = 3.8)

### scale some data to save space:
wos_gnews_dat_saved <- wos_gnews_dat

wos_gnews_dat[wos_gnews_dat$type == "Web of Science" & wos_gnews_dat$topic == "ddt", "freq_scaled"] <- wos_gnews_dat[wos_gnews_dat$type == "Web of Science" & wos_gnews_dat$topic == "ddt", "freq_scaled"] / 10
wos_gnews_dat[wos_gnews_dat$type == "Web of Science" & wos_gnews_dat$topic == "ddt", "prop_ci_low"] <- wos_gnews_dat[wos_gnews_dat$type == "Web of Science" & wos_gnews_dat$topic == "ddt", "prop_ci_low"] / 10
wos_gnews_dat[wos_gnews_dat$type == "Web of Science" & wos_gnews_dat$topic == "ddt", "prop_ci_high"] <- wos_gnews_dat[wos_gnews_dat$type == "Web of Science" & wos_gnews_dat$topic == "ddt", "prop_ci_high"] / 10
###

wos_gnews_dat[wos_gnews_dat$type == "Web of Science" & wos_gnews_dat$topic == "ddt" & wos_gnews_dat$year == 1960, "freq_scaled" ] <- NA
wos_gnews_dat[wos_gnews_dat$type == "Web of Science" & wos_gnews_dat$topic == "ddt" & wos_gnews_dat$year >= 1960, "freq_scaled" ] <- wos_gnews_dat[wos_gnews_dat$type == "Web of Science" & wos_gnews_dat$topic == "ddt" & wos_gnews_dat$year >= 1960, "freq_scaled"] * 5

#wos_gnews_dat[wos_gnews_dat$type == "Web of Science" & wos_gnews_dat$topic == "ddt" & wos_gnews_dat$year == 1960, "prop_ci_low" ] <- NA
wos_gnews_dat[wos_gnews_dat$type == "Web of Science" & wos_gnews_dat$topic == "ddt" & wos_gnews_dat$year >= 1960, "prop_ci_low" ] <- wos_gnews_dat[wos_gnews_dat$type == "Web of Science" & wos_gnews_dat$topic == "ddt" & wos_gnews_dat$year >= 1960, "prop_ci_low"] * 5

#wos_gnews_dat[wos_gnews_dat$type == "Web of Science" & wos_gnews_dat$topic == "ddt" & wos_gnews_dat$year == 1960, "prop_ci_high" ] <- NA
wos_gnews_dat[wos_gnews_dat$type == "Web of Science" & wos_gnews_dat$topic == "ddt" & wos_gnews_dat$year >= 1960, "prop_ci_high" ] <- wos_gnews_dat[wos_gnews_dat$type == "Web of Science" & wos_gnews_dat$topic == "ddt" & wos_gnews_dat$year >= 1960, "prop_ci_high"] * 5


plot_conservation_panels(topic = "ddt", xlim = ddt_xlim, annotate_df = NULL, gnews_multiplier = 40, ylabel = "Science publications\n(per 10 or 5 million)", ylim = c(0, 3400), wos.axis2 = seq(0, 700, 200), wos.axis2.labels = seq(0, 700, 200), gnews.axis2 = seq(0, 70, 20))
#text(1990, 6.5, "Research", col = "grey40", cex = .9, pos = 4)
text(1993.5, 14, "Research", col = "grey40", cex = .9, pos = 4)
text(1993.5, 8.2, "per 5 million", col = "grey40", cex = .9, pos = 4)

text(1958, 10.5, "Research", col = "grey40", cex = .9, pos = 4)
text(1958, 5, "per 10 million", col = "grey40", cex = .9, pos = 4)

arrows(1996, 4000, 1996, 1000, length = 0.05, lwd = 0.9, col = "grey40")
text(1970.2, 56, "News", col = "grey40", cex = .9, pos = 4)
#abline(v = 1960, lty = 2)

### scale it back:
wos_gnews_dat <- wos_gnews_dat_saved
rm(wos_gnews_dat_saved)

add_ddt_rects(add.text = TRUE)
#with(ddt_annotations, add_annotation(start_year, label, print.label = FALSE))
#with(ddt_annotations, add_annotation(end_year, label, print.label = FALSE))


#####
par(xpd = NA)
     text(1962, par("usr")[4]*1.8, "Silent Spring\npublished", offset = -0.15, srt = 0, pos = 4, col =  unique(ddt_annotations$col))
  segments(1962, par("usr")[4], 1962, par("usr")[4]*1.8* 0.925, col= "#00000020", lwd = 1.5)
par(xpd = FALSE)

# global usage:
plot(1,1, type = "n", xlim = ddt_xlim, ylim = c(0, 97), axes = FALSE, yaxs = "i")
with(ddt.global, lines(year, Kt.a, col = col.stressor))
with(ddt.global, points(year, Kt.a, pch = 21, bg = "white", cex = 0.9, lwd = .8, col = col.stressor))

add_ddt_rects()

box(col = col.axis)
axis(2, col = col.axis, col.axis = col.axis.label)
add_ylabel("Global DDT\nusage (Kt/a)")
#fig_letter("(b) Stressor")
fig_letter(expression(paste(bold("B"), " Stressor")))

plot(1,1, type = "n", xlim = ddt_xlim, ylim = c(0, 120), axes = FALSE, yaxs = "i")
with(ddt.ev, lines(year, conc.frac*100, lty = 2, col = col.response))
with(ddt.ev, polygon(c(year, rev(year)), c(conc.frac*100 + conc.frac.sd*100, rev(conc.frac*100 - conc.frac.sd*100)), col = paste(col.response, "20", sep = ""), border = NA))
#text(2000, 34, "Everest ice core", col = "grey40", cex = 0.9)
#text(2000, 34, "Everest ice core", col = "grey40", cex = 0.9)
text(1967.5, 27, "Everest ice core", col = "grey40", cex = 0.9, pos = 2)
with(ddt.fish.swe, lines(year, conc.frac*100, lty = 1, col = col.response))
text(1967, 78, "Arctic char\nLake Vattern, Sweden", pos = 2, col = "grey40", cex = 0.9)

with(ddt.polar, lines(year, conc.frac*100, lty = 3, lwd = 1.0, col = col.response))
#with(ddt.polar, segments(year, conc.frac*100 + conc.frac.sd*100, year, conc.frac*100 - conc.frac.sd*100, lty = 1, lwd = 0.6, col = "grey30"))
with(ddt.polar, segments(year, conc.frac*100 + conc.frac.sd*100, year, conc.frac*100 - conc.frac.sd*100, lty = 1, lwd = 0.6, col = col.response))
with(ddt.polar, points(year, conc.frac*100, bg = "white", pch = 21, cex= .9, lwd = 0.7, col = col.response))
text(1983.6, 70, "Polar bears\nHudson Bay, Canada", pos = 4, col = "grey40", cex = 0.9)

#text(1967, 60, "Arctic char\nLake Vattern, Sweden", pos = 2, col = "grey40", cex = 0.9)



axis(2, col = col.axis, col.axis = col.axis.label, at = seq(0, 100, 25))

box(col = col.axis)
#fig_letter("(c) Response")
fig_letter(expression(paste(bold("C"), " Response")))


add_ylabel("Percent of maximum\nDDT concentration")
add_xlabel()
axis(1, col = col.axis, col.axis = col.axis.label)
axis(1, col = col.axis, col.axis = col.axis.label, at = 2008)

add_ddt_rects()
dev.off()

