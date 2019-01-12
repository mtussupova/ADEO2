library(igraph)

setwd("~/Desktop/SocialNetwork")

football <- read.graph("football.gml", format = "gml")
karate <- read.graph("karate.gml", format = "gml")
dolphins <- read.graph("dolphins.gml", format = "gml")
polbooks <- read.graph("polbooks.gml", format = "gml")
wikipedia <- read.graph("wikipedia.gml", format = "gml")

community_an <- function(g){
  ml_com <- multilevel.community(g)
  ed_btw <- edge.betweenness.community(g)
  wlk_com <- walktrap.community(g)
  infm_com <- infomap.community(g)
  lbl <- c("multilvl", "edg_betw", "walktrap", "infomap")
  #modularity
  par(mfrow=c(1,2))
  mod_ml_com <- modularity(ml_com)
  mod_ed_btw <- modularity(ed_btw)
  mod_wlk_com <- modularity(wlk_com)
  mod_infm_com <- modularity(wlk_com)
  mod <- c(mod_ml_com, mod_ed_btw, mod_wlk_com, mod_infm_com)  
  barplot(mod, main = "Modularity", col = c("blue", "red", "yellow", "green"))
  legend("bottom", lbl, fill=c("blue", "red", "yellow", "green"))
  #compare
  comp_1 <- compare(ml_com, V(g)$value, method="nmi")
  comp_2 <- compare(ed_btw, V(g)$value, method="nmi")
  comp_3 <- compare(wlk_com, V(g)$value, method="nmi")
  comp_4 <- compare(infm_com, V(g)$value, method="nmi")
  comp <- c(comp_1, comp_2, comp_3, comp_4)
  barplot(comp, main = "Comparison", col = c("blue", "red", "yellow", "green")) 
  legend("bottom", lbl, fill=c("blue", "red", "yellow", "green"))
  par(mfrow=c(2,4))
  #membership
  mbr_ml <- membership(ml_com)
  mbr_eb <- membership(ed_btw)
  mbr_wlk <- membership(wlk_com)
  mbr_infmcom <- membership(infm_com)
  #plotting
  plot(ml_com, g, main = "1")
  plot(g, vertex.color = mbr_ml, main = "2")
  legend("bottom", "multilvl")
  plot(ed_btw, g, main = "1")
  plot(g, vertex.color = mbr_eb, main = "2")
  legend ("bottom", "ed_btw")
  plot(wlk_com, g, main = "1")
  plot(g, vertex.color = mbr_wlk, main = "2")
  legend ("bottom", "wlk_com")
  plot(infm_com, g, main = "1")
  plot(g, vertex.color = mbr_infmcom, main = "2")
  legend ("bottom", "infm_com")
}

community_an(karate)
community_an(football)
community_an(dolphins)

# ml <- multilevel.community(wikipedia) works for undirected graphs only

wlk_com <- walktrap.community(wikipedia)

mod_1 <- modularity(wlk_com)
mbr_1 <- membership(wlk_com)
par(mfrow=c(1,2))
plot(wlk_com, wikipedia, main = "Walktrap")


infm_com <- infomap.community(wikipedia)
mod_2 <- modularity(infm_com)
mbr_2 <- membership(infm_com)
plot(infm_com, wikipedia, main = "Infomap")

par(mfrow=c(1,2))

comp_1 <- compare(wlk_com, V(wikipedia)$value, method="nmi")
comp_2 <- compare(infm_com, V(wikipedia)$value, method="nmi")
lbl <- c("walktrap", "infomap")
comp <- c(comp_1, comp_2)
mod <- c(mod_1, mod_2)
barplot(comp, main = "Comparison", c("blue", "red", "yellow"))
legend("bottom", lbl, fill=c("blue", "red", "yellow"))
barplot(mod, main = "Modularity", col = c("blue", "red", "yellow"))
legend("bottom", lbl, fill=c("blue", "red", "yellow"))


