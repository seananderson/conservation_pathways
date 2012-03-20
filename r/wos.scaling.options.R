# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Dec 24, 2011
# Last modified: Dec 24, 2011
# Purpose:       what do the various wos scaling options look like?
# ====================================================================

wos.eng.art.sci <- read.table("~/Downloads/wos.english.articles.sci.expanded.txt", sep = "\t", header = TRUE, col.names = c("year", "publications", "x"))[,1:2]
wos.eng.art.sci$language <- "English"
wos.all.art.sci <- read.table("~/Downloads/wos.all.languages.articles.sci.expanded.txt", sep = "\t", header = TRUE, col.names = c("year", "publications", "x"))[,1:2]
wos.all.art.sci$language <- "All languages"

d <- rbind(wos.eng.art.sci, wos.all.art.sci)
p <- ggplot(d, aes(year, publications, colour = language)) + geom_line()
ggsave("english-vs-all.pdf")

acid.title <- read.table("~/Downloads/acid.rain.ti.english.articles.sci.expanded.txt", sep = "\t", header = TRUE, col.names = c("year", "publications", "x"))[,1:2]
acid.title$search <- "Title"
acid.topic <- read.table("~/Downloads/acid.rain.ts.english.articles.sci.expanded.txt", sep = "\t", header = TRUE, col.names = c("year", "publications", "x"))[,1:2]
acid.topic$search <- "Topic"
d.acid <- rbind(acid.title, acid.topic)
ggplot(d.acid, aes(year, publications, colour = search)) + geom_line()

ddt.title <- read.table("~/Downloads/ddt.ti.english.articles.sci.expanded.txt", sep = "\t", header = TRUE, col.names = c("year", "publications", "x"))[,1:2]
ddt.title$search <- "Title"
ddt.topic <- read.table("~/Downloads/ddt.ts.english.articles.sci.expanded.txt", sep = "\t", header = TRUE, col.names = c("year", "publications", "x"))[,1:2]
ddt.topic$search <- "Topic"

d.ddt <- rbind(ddt.title, ddt.topic)
ggplot(d.ddt, aes(year, publications, colour = search)) + geom_line()

d.ddt$topic <- "ddt"
d.acid$topic <- "acid rain"

d <- rbind(d.ddt, d.acid)

names(wos.all.art.sci)[2] <- "all.publications"
d <- merge(d, wos.all.art.sci[,c("year", "all.publications")])
d <- transform(d, scaled.publications = publications / all.publications)

p <- ggplot(d, aes(year, scaled.publications, colour = search)) + geom_line() + facet_wrap(~topic, ncol = 1, scales = "free_y")
ggsave("title-vs-topic.pdf")

