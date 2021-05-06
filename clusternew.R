## basic cluster analysis and PCA 
library(tidyverse)
library(DataExplorer)
library(viridis)
library(AER)
library(factoextra)
library(FactoMineR)

data("CASchools")

clusdata <- CASchools
str(clusdata)

##EDA automated
create_report(clusdata)

##select numeric features only

clus2 <- clusdata%>%
  select_if(is.numeric)

##scale data
scaleclus <- scale(clus2)

##optimal number of clusters
fviz_nbclust(scaleclus, kmeans,method='wss')


##final
mod <- kmeans(scaleclus, centers = 3, nstart=30)
fviz_cluster(mod,data=scaleclus)

##summarize clusters
clus2 %>%
  mutate(Cluster=mod$cluster) %>%
  group_by(Cluster)%>%
  summarise_all("mean")

##PCA  analysis - 2 components ~ +80%
pmod <- PCA(scaleclus)
#loadings.. PCA 1, construct maybe about socioeconomic status, PCA 2 maybe about resources
pmod$var$cor

##elbow rule keep two components
fviz_screeplot(pmod, ncp=10)


#evaluation contribution by variable to each component (biplot)
fviz_pca_var(pmod, col.var="contrib")

