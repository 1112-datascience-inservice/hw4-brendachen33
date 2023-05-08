library("FactoMineR")
data(iris)
# log transform 
log.ir <- as.matrix(log(iris[, 1:4]))
ir.species <- iris[, 5]

iris.ca <- CA(iris[, 1:4], graph = FALSE)
# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
#ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
#print(ir.pca)

#library(ggbiplot)
#g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species, choices = c(1,3))
#g <- g + scale_color_discrete(name = '')
#g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
#print(g)



ir.ca <- CA(log.ir, ncp = 3, graph = True)


# generate biplot of first two dimensions
plot.CA(ir.ca, axes = c(1,2), col.row = ir.species)