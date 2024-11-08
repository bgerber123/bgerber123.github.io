# Create a new spatial variable 

#Load libraries
library(raster)
library(MASS)
library(amt)
library(terra)
sh_forest <- get_sh_forest()

#Create new spatial layer and downsample
S=sh_forest
S.aggregate <- aggregate(S, fact=30)
names(S.aggregate) = "shrub.cover"

#Add new spatially correlated values into this new layer
n.samples <- ncell(S.aggregate)
set.seed(15435)  
random.values.matrix=mvrnorm(n=n.samples, mu=rep(0), Sigma=diag(1), empirical=TRUE)

phi=0.95 # 1 indicate high spatial correlation
n <- ncell(S.aggregate)
xy <- xyFromCell(S.aggregate, 1:n)
d <- as.matrix(dist(xy))		
Sigma <- exp(-d/10000/phi)
Sigma <- t(chol(Sigma))
values(S.aggregate) <- -1*scale(Sigma %*% random.values.matrix )
shrub.cover = S.aggregate

plot(shrub.cover, main="Shrub Cover")

saveRDS(shrub.cover,file="sp.layer.shrub")

sh_forest <- aggregate(S, fact=30)
forest.cover=sh_forest
plot(sh_forest)

values(forest.cover)[which(is.na(values(forest.cover)))]=0

saveRDS(forest.cover,file="sp.layer.forest")
