rm(list=ls())
df <-read_csv("intermed_cluster_data/Clustering_Dataset.csv")
County <- df$County

stopifnot(
  all(c("County", "population", "density", "phr", "outbreak") %in% names(df)),
  !anyNA(df$County),
  !anyDuplicated(df$County),
  all(is.finite(df$population)),
  all(df$population >= 0),
  all(is.finite(df$density))
)

#df$density <- df$population*1.0/df$AREA_SQMI
#df$density <- df$population/df$a
#df <- df %>% select(-c("County", "PHR", "outbreak", "AREA_SQMI"), -starts_with("M_"), -starts_with("MP_"), -starts_with("E_"), -starts_with("EPL_"), -starts_with("SPL_"), -starts_with("RPL_"), -starts_with("F_"))
df <- df %>% filter(!County %in% c("mcculloch", "mclennan", "mcmullen", "dewitt", "loving"))
df_raw <- df
df <- df %>% select(-c("County", "phr", "outbreak"), -starts_with("M_"), -starts_with("MP_"), -starts_with("E_"), -starts_with("EPL_"), -starts_with("SPL_"), -starts_with("RPL_"), -starts_with("F_"))
df <- df[,colSums(is.na(df)) == 0]
df <- df %>% select(-c("EP_MINRTY", "EP_HISP", "EP_AFAM", "EP_POV150", "EP_UNINSUR"))
df <- as.data.frame(scale(df))  

print(c("df should now have 249 rows and 31 columns. dim(df)=", dim(df)))
#### df should now have 249 rows and 31 columns
print(c("df_raw should now have 249 rows and 165 columns. dim(df_raw)=", dim(df_raw)))


pca_out <- prcomp(df)
summary(pca_out)
names(pca_out)
pca_out$center
pca_out$rotation

# Top ten factors influencing first principal component:
sort(abs(pca_out$rotation[, 1]), decreasing = TRUE) %>% head(10)
#  pct_white     pct_hispanic        EP_NOHSDP        EP_LIMENG 
# pct_uninsured pct_foreign_born      pct_poverty         EP_CROWD 
#EP_SNGPNT              cve 

pca_out$rotation[order(abs(pca_out$rotation[, 1]), decreasing = TRUE)[1:10],1]
# pct_white     pct_hispanic        EP_NOHSDP        EP_LIMENG 
# -0.3420209        0.3389457        0.3217543        0.2979115 
# pct_uninsured pct_foreign_born      pct_poverty         EP_CROWD 
# 0.2611374        0.2513370        0.2419572        0.2376305 
# EP_SNGPNT              cve 
# 0.2172861       -0.1948366 

# Top ten factors influencing second principal component:
sort(abs(pca_out$rotation[, 2]), decreasing = TRUE) %>% head(10)
#density      EP_MUNIT    population    enrollment      EP_ASIAN 
#pct_college median_income     EP_DISABL      EP_NOINT      EP_AGE65 

pca_out$rotation[order(abs(pca_out$rotation[, 2]), decreasing = TRUE)[1:10],2]
# density      EP_MUNIT    population    enrollment      EP_ASIAN   pct_college median_income 
# 0.3562591     0.3489906     0.3324605     0.3274613     0.3029555     0.2607086     0.2388396 
# EP_DISABL      EP_NOINT      EP_AGE65 
# -0.2275058    -0.2201343    -0.2098391 

# > max(abs(pca_out$rotation[,1]))
# [1] 0.3420209
# > which.max(abs(pca_out$rotation[,1]))
# pct_white 
# 10 

# inspect optimal clusters

p1 <-fviz_nbclust(
  df,
  kmeans, 
  k.max = 25,
  method = "wss",
  diss = get_dist(df, method = "spearman"),
)

p2 <-fviz_nbclust(
  df, 
  kmeans, 
  method = "silhouette", 
  k.max = 25, 
  nstart = 25,
)

png("results/cluster_optk.png", width = 12, height = 6, units = "in", res = 300)
grid.arrange(p1, p2, ncol = 2)
dev.off()

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
png("results/cluster_plot.png", width = 8, height = 8, units = "in", res = 300)

p + 
  geom_point(data = plot_data %>% filter(outbreak > 0),
             aes(x = x, y = y, size = outbreak),
             color = "black", alpha = 0.7) +
  scale_size_continuous(range = c(2, 10), name = "Measles cases")

dev.off()

# Analyzing results
# see centroid
centroids <- km$centers
print(centroids)
# see what loads heavily on PC1
pca_out <- prcomp(df)
names(pca_out)




# attach cluster assignments
df$cluster <- km$cluster
df$county <- df_raw$County
df
# look at means of original variables by cluster
sum <- df %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))

cluster_label <- df %>% select(c(cluster, county))


# Overlay map of Texas

# 1. Load data
df_map <- cluster_label

# 2. Load Texas shapefile directly from Census zip URL
texas <- st_read("/vsizip//vsicurl/https://www2.census.gov/geo/tiger/GENZ2021/shp/cb_2021_us_county_5m.zip") %>%
  filter(STATEFP == "48")

# 3. Standardize names
texas <- texas %>%
  mutate(county = tolower(str_trim(NAME))) %>% 
  filter(!county %in% c("mcculloch", "mclennan", "mcmullen", "dewitt", "loving"))

#df_map <- df_map %>%
#  mutate(
#    county = str_replace(county, regex(" County", ignore_case = TRUE), ""),
#    county = str_trim(county)
#  )

# 4. Merge shapefile with data & convert cluster to categorical/factor
merged <- texas %>%
  left_join(df_map, by = "county") %>%
  mutate(cluster = as.character(cluster))

# 5. Plot using ggplot2
ggplot(data = merged) +
  geom_sf(aes(fill = cluster), color = "white", size = 0.2) +
  scale_fill_brewer(
    palette = "Set1", 
    na.value = "lightgray", 
    name = "Cluster"
  ) +
  labs(title = "Texas Counties by Cluster") +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5,margin = ggplot2::margin(0, 0, 12, 0)),
    legend.position = c(0.85, 0.2)
  )





# Ensure numeric to character conversion
merged <- merged %>%
  mutate(
    cluster = as.character(as.integer(cluster)),
    CODE2023 = as.character(as.integer(CODE2023))
  )

# Define labels
code_labels <- c(
  '1' = 'Large central metro',
  '2' = 'Large fringe metro',
  '3' = 'Medium metro',
  '4' = 'Small metro',
  '5' = 'Micropolitan',
  '6' = 'Noncore'
)

# Extract unique valid values sorted
clusters <- sort(unique(na.omit(merged$cluster)))
codes    <- sort(unique(na.omit(merged$CODE2023)))

# Define color palettes matching matplotlib ('Reds', 'Greens', 'Blues')
cmap_names <- c('Reds', 'Greens', 'Blues')

# Create blending function for colors
get_color <- function(cluster, code) {
  if (is.na(cluster) || is.na(code) || !(cluster %in% clusters) || !(code %in% codes)) {
    return(rgb(0.83, 0.83, 0.83))
  }
  
  # Shade level calculation matching python formula: 0.95 - 0.65 * i / (N - 1)
  code_idx <- match(code, codes) - 1
  n_codes  <- length(codes)
  shade_val <- if (n_codes > 1) 0.95 - 0.65 * (code_idx / (n_codes - 1)) else 0.95
  
  # Map cluster index to palette name
  cl_idx <- match(cluster, clusters)
  pal_fun <- colorRampPalette(brewer.pal(9, cmap_names[cl_idx]))
  
  # Interpolate color based on shade value
  pal_fun(100)[round(shade_val * 99) + 1]
}

# Apply color mapping
merged <- merged %>%
  rowwise() %>%
  mutate(`_clr` = get_color(cluster, CODE2023)) %>%
  ungroup()

# Extract point centroids for outbreak points
pts <- merged %>%
  filter(!is.na(outbreak) & outbreak > 0) %>%
  mutate(
    coords = st_coordinates(st_point_on_surface(geometry)),
    `_x` = coords[, 1],
    `_y` = coords[, 2]
  )

vmax <- max(pts$outbreak, na.rm = TRUE)

# Custom legend construction matching grouped Python legend structure
legend_df <- expand.grid(CODE2023 = codes, cluster = clusters, stringsAsFactors = FALSE) %>%
  arrange(cluster, CODE2023) %>%
  rowwise() %>%
  mutate(
    `_clr` = get_color(cluster, CODE2023),
    label = paste0("  ", CODE2023, " · ", ifelse(CODE2023 %in% names(code_labels), code_labels[CODE2023], paste("CODE", CODE2023))),
    group_label = paste("Cluster", cluster)
  )

# Plotting using ggplot2
ggplot() +
  # Base map
  geom_sf(data = merged, aes(fill = `_clr`), color = "white", size = 0.4) +
  scale_fill_identity() +
  
  # Outbreak points
  geom_point(data = pts, aes(x = `_x`, y = `_y`, size = outbreak),
             shape = 21, fill = "navajowhite", color = "white", stroke = 0.8, alpha = 0.75) +
  scale_size_continuous(range = c(2, 12)) +
  
  # Outbreak labels with text stroke effect
  geom_shadowtext(data = pts, aes(x = `_x`, y = `_y`, label = as.integer(outbreak)),
                  color = "white", bg.color = "black", bg.r = 0.15,
                  size = 2.5, fontface = "bold") +
  
  # Aesthetics and layout
  labs(title = "Cluster Map with Outbreak Counts") +
  theme_void() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5, margin = margin(b = 12)),
    legend.position = "right"
  )



write.csv(cluster_label, "results/cluster_labels.csv", row.names = FALSE)
write.csv(sum, "results/cluster_means.csv", row.names = FALSE)

