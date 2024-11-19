# clustering
# kmeans 
set.seed(2)
km=kmeans(car.df.train.norm,5)
km$cluster
car.df.train.norm$cluster=km$cluster
View(car.df.train.norm)

# determine and visualize optimal number of clusters
library(factoextra)
fviz_nbclust(data, kmeans, method = "wss") 
fviz_nbclust(data, kmeans, method = "silhouette")
fviz_nbclust(data, kmeans, method = "gap_stat") 

# create cluster biplot
fviz_cluster(kmeans(data, centers = 3, iter.max = 100, nstart = 100), data = data)

# visualize clusters using original variables
clusters <- kmeans(data, centers = 3, iter.max = 100, nstart = 100)
Wine <- Wine |> mutate(cluster = clusters$cluster)
Wine |> ggplot(aes(x = Rating, y = Price, col = as.factor(cluster))) + geom_point()

