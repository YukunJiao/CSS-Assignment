library(FactoMineR)
library(explor)

MP <- read.csv("http://ollion.cnrs.fr/wp-content/uploads/2025/05/MediaMPs.csv")
str(MP)

dim(MP)
collapse::descr(MP)
summary(MP)

num <- sapply(MP, is.numeric)
num

res <- PCA(MP[, num], graph = FALSE)
res
