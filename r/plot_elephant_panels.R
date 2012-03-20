# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Dec 21, 2011
# Last modified: Mar 14, 2012
# Purpose:       Make the ivory figure
# ====================================================================


elephant_xlim <- c(1955, 2008)
elephant_annotations <- read.table("../data/elephant.policy.dates.txt", header = TRUE, stringsAsFactors = FALSE, sep = "\t")
elephant_annotations$label <- gsub("\\\\n", "\\\n", elephant_annotations$label)

pdf("../fig/ivory-ts2.pdf", width = 3.4, height = 3.8)
plot_conservation_panels(topic = "ivory", xlim = elephant_xlim, wos.axis2 = seq(0, 70, 20), wos.axis2.labels = seq(0, 70, 20), annotate_df = elephant_annotations, gnews_multiplier = 10, ylabel = "Science publications\n(per million)", gnews.axis2 = seq(0, 40, 10))
text(1966.7, 16, "Research", col = "grey40", cex = .9, pos = 4)
text(1988.5, 18, "News", col = "grey40", cex = .9, pos = 4)

# response panels:
# exports:
plot(1,1, type = "n", xlim = elephant_xlim, ylim = c(0, 1.450), axes = FALSE, yaxs = "i")
#with(ivory_export_early, lines(Year,  Total.ivory.export..tonnes., type = "l", lty = 1))

#lty.to.plot <- names(ivory_export_early_raw)[-1]
#lty.to.plot[3] <- 0 # don't plot the dashed one - "very incomplete"
lty.to.plot <- c(0, 0, 2, 1)

for(i in 1:4) with(ivory_export_early_raw, lines(year, ivory_export_early_raw[,i+1] , type = "l", lty = lty.to.plot[i], lwd = 1))

#with(ivory_export_recent, lines(Year,  Total.multiplied.by.10..estimate.of.export./1e6, type = "l", lty = 3, lwd = 1.6))
with(ivory_export_recent, points(Year,  Total.multiplied.by.10..estimate.of.export./1e6, type = "p", lty = 3, lwd = .5, pch = "."))
with(ivory_export_recent, lines(Year,  Total.multiplied.by.10..estimate.of.export./1e6, type = "l", lty = 1, lwd = .3, col = "#00000025"))
#text(1954, 0.75, "Milner-Gulland and\nBeddington 1993", col = "grey40", cex = .9, pos = 4)
text(1952.5, 0.58, "Pearce (1989)\ntrade estimates", col = "grey40", cex = .9, pos = 4)
text(1978.9, 1.15, "IUCN\ntrade estimates", col = "grey40", cex = .9, pos = 4)
text(1986.7, 0.47, "Estimated illegal exports\n(seizures x 10)", col = "grey40", cex = .9, pos = 4)

axis(2, col = col.axis, col.axis = col.axis.label, at = seq(0, 1.4, .4))
box(col = col.axis)
add_ylabel("African ivory\nexports (1000 t)")
fig_letter("(b) First-order response")
with(elephant_annotations, add_annotation(year, label, print.label = FALSE))

# elephant abundance:
plot(1,1, type = "n", xlim = elephant_xlim, ylim = c(0, max(elephant_pop_modeled$upper)* 1.06), axes = FALSE, yaxs = "i")
axis(2, col = col.axis, col.axis = col.axis.label, at = seq(0, 3, 1))
#with(elephant_pop, polygon(c(Year, rev(Year)), c(Pop..Estimate..lower./1e6, rev(Pop..Estimate..upper./1e6)), col = "lightgrey", border = FALSE))

#with(elephant_pop, lines(Year, Pop..Estimate..Number.of.elephants./1e6, type = "l", lty = 1))
with(elephant_pop_modeled, polygon(c(year, rev(year)), c(lower, rev(upper)), col = "#00000020", border = NA))
with(elephant_pop_modeled, lines(year, estimate))
with(elephant_pop, points(Year, Pop..Estimate..Number.of.elephants./1e6, cex = .7, pch = c(rep(21, 4), rep(19, 4))))
with(elephant_pop, segments(Year, Pop..Estimate..lower./1e6, Year, Pop..Estimate..upper./1e6, col = "black", lwd = c(rep(0.5, 4), rep(0.8, 4))))
box(col = col.axis)
#n <- nrow(elephant_pop)
#with(elephant_pop, segments(Year[1:(n-1)], Pop..Estimate..Number.of.elephants./1e6[1:(n-1)],Year[2:n], Pop..Estimate..Number.of.elephants./1e6[2:n], lty = 1))
fig_letter("(c) Second-order response")

add_ylabel("Elephant abundance\n(millions)")
with(elephant_annotations, add_annotation(year, label, print.label = FALSE))
add_xlabel()
axis(1, col = col.axis, col.axis = col.axis.label, at = seq(1950, 2000, 10))
axis(1, col = col.axis, col.axis = col.axis.label, at = seq(2008, 2008, 1))

text(1959, 1.5, "Modelled", col = "grey40", cex = .9, pos = 4)
text(1990, 1.2, "Annual\nestimates", col = "grey40", cex = .9, pos = 4)
#text(1995, 1.2, "African Elephant\nDatabase", col = "grey40", cex = .9, pos = 4)

dev.off()

