library(dplyr)       # for data manipulation
library(ggplot2)     # for data visualization
library(stringr)     # for string functionality
library(cluster)     # for general clustering algorithms
library(factoextra)  # for visualizing cluster results
library(readr)      # for reading
# Locate the project from either its root or Methods/Kclustering.
here::i_am("Methods/Kclustering/clustering.r")
df <- read_csv(here("data", "datasets", "Clustering_Dataset.csv"))
stopifnot(
  all(c("County", "population", "density", "phr", "outbreak") %in% names(df)),
  !anyNA(df$County),
  !anyDuplicated(df$County),
  all(is.finite(df$population)),
  all(df$population >= 0),
  all(is.finite(df$density))
)
df_raw <- df
County <- df$County
df <- df %>% select(-c("County", "phr", "outbreak"), -starts_with("M_"), -starts_with("MP_"), -starts_with("E_"), -starts_with("EPL_"), -starts_with("SPL_"), -starts_with("RPL_"), -starts_with("F_"))
df <- df[,colSums(is.na(df)) == 0]
df <- df %>% select(-c("EP_MINRTY", "EP_HISP", "EP_AFAM", "EP_POV150", "EP_UNINSUR"))
df <- as.data.frame(scale(df))  
pca_out <- prcomp(df)
summary(pca_out)
names(pca_out)
pca_out$center
pca_out$rotation
# inspect optimal clusters
fviz_nbclust(
  df,
  kmeans, 
  k.max = 25,
  method = "wss",
  diss = get_dist(df, method = "spearman")
)

fviz_nbclust(
  df, 
  kmeans, 
  method = "silhouette", 
  k.max = 25, 
  nstart = 25)

set.seed(100)
km <- kmeans(df, centers = 3, nstart = 25)
fviz_cluster(km, data = df, geom = "point", ellipse.type = "convex")

# get PCA coords
pca_coords <- as.data.frame(pca_out$x[, 1:2])
pca_coords$cluster <- factor(km$cluster)
pca_coords$outbreak <- df_raw$outbreak  # raw outbreak count

p <- fviz_cluster(km, data = df, geom = "point", ellipse.type = "convex") +
  theme_minimal()

# extract the PCA coords fviz already computed
plot_data <- p$data

# add outbreak from df_raw (make sure row order matches)
plot_data$outbreak <- df_raw$outbreak

# rebuild with sized points on top
p + 
  geom_point(data = plot_data %>% filter(outbreak > 0),
             aes(x = x, y = y, size = outbreak),
             color = "black", alpha = 0.7) +
  scale_size_continuous(range = c(2, 10), name = "Outbreak cases")