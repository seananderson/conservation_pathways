# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Purpose:       add a figure letter in bold
# ====================================================================

fig_letter <- function(letter) {
  #par(font = 2)
  #mtext(letter, side = 3, line = -1.3, adj = 0.025, cex = 0.7)

# left aligned on left:
  text(par("usr")[1] + (par("usr")[2] - par("usr")[1]) * 0.005, par("usr")[4] - (par("usr")[4] - par("usr")[3]) * 0.1, letter, pos = 4, col = "grey30")

# right aligned:
  #text(par("usr")[2] - (par("usr")[2] - par("usr")[1]) * 0.005, par("usr")[4] - (par("usr")[4] - par("usr")[3]) * 0.1, letter, pos = 2, col = "grey30")

# left aligned on right:
  #text(par("usr")[1] + (par("usr")[2] - par("usr")[1]) * 1, par("usr")[4] - (par("usr")[4] - par("usr")[3]) * 0.1, letter, pos = 2, col = "grey30")
  #par(font = 1)
}


