# chargement des packages nécessaires
library(igraph)
library(maptools)

# chargement des données 
load("data/dep.RData")

# extraction des coordonnées des centroides des departements
centres <- cbind(depGeom@data[,"CODE_DEPT"],
                 as.data.frame(coordinates(depGeom)))
names(centres)<-c("id","X","Y")

# création de la variable d'attractivité des départements :
# le nombre d'arrivées
attract <- aggregate(depMig$fij, by = list(depMig$dest), sum)
names(attract) <- c("id", "nbarrivee")

# Sélection flux entre les 6 départements recevant le plus de 
# migrations résidentielles à l'exeption des département 
# de l'IDF, de l'étranger (99) et des DOM (97)
# liste des départements à exclure
listExcl <- c("75", "92", "77", "78", "93", "94", "95", "97", "99")
# exclusion de ces départements
mig <- depMig[!depMig$dest %in% listExcl & 
                !depMig$orig %in% listExcl,]
# liste des départements à sélectionner 
listDep <- attract[order(attract$nbarrivee[!attract$id %in% listExcl], 
                         decreasing = T),][1:6,"id"]
# sélection de ces départements
mig <- mig[mig$orig %in% listDep & mig$dest %in% listDep, ]

# transformation des flux sélectionnés en graph
g <- data.frame(mig[,c("orig", "dest")])
g <- as.matrix(g)
g <- graph.edgelist(g, directed=TRUE)

# création de la liste des noeuds (vertex)
vert <- data.frame(id = V(g)$name)

# affectation des positions des coordonnées des centroides aux noeuds
coords <- centres[match(vert$id, table = centres$id), ]
coords <- as.matrix(coords[,2:3])
g$layout <- coords

# ajout d'un attribut  aux noeuds
V(g)$weights <- attract[match(vert$id, table = attract$i), "nbarrivee"]

# ajout d'un attribut d'épaisseur aux liens (edge)
x <- get.edgelist(g)
E(g)$fij <- mig[match(x = paste(x[,1],x[,2],sep="_"), 
                      table = paste(mig$orig,mig$dest,sep="_")),"fij"]

png("img/mig_test.png", width = 415, height = 450)
# affichage du fond de carte
plot(depGeom, col="pink")
# affichage du graph
par(mar=c(0,0,0,0),oma = c(0,0,0,0) )
plot.igraph(g, add=T, rescale = F, 
            vertex.size = (V(g)$weights)*100,
            edge.curved = 0.4, 
            edge.width =  E(g)$fij/250,
            edge.color = "red")
dev.off()


png("img/mig_final.png", width = 415, height = 450)
par(mar=c(0,0,0,0),oma = c(0,0,0,0) )
# Affichage de la carte des départements
plot(depGeom, col="black", bg = "grey", border = "#ffffff00")

# Titre et légende
mtext(text = " Migrations résidentielles majeures entre les \n départements les plus attractifs (hors IDF)\n",
      side = 3,adj=0, cex=1.5, line = -4.5)
mtext(text = "Source : Insee, migrations résidentielles 2003-2008 ",
      side = 1,adj=1,cex=0.8, line = -2)
mtext(text = " T. Giraud, 2014",side = 1,adj=0,cex=0.8, line = -2)

# ajout des flèches une par une
for (e in seq_len(ecount(g))) {
  graph2 = delete.edges(g, E(g)[(1:ecount(g))[-e]])
  plot(graph2, 
       vertex.size = (V(g)$weights)*250,
       edge.curved = 0.5,
       edge.width=E(graph2)$fij / 170,
       # nous limitons la taille des plus grosses flèches à 1.75 
       edge.arrow.size = if(E(graph2)$fij / 700 >= 1.75){
         1.75
       } else {
         E(graph2)$fij/700
       },
       edge.color = "red",
       vertex.label=NA, 
       add=TRUE, 
       rescale = F
  )
}
dev.off()

