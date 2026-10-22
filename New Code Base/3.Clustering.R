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




# attach cluster assignments
df$cluster <- km$cluster
df$county <- df_raw$County
df_raw$county <- df_raw$County

# look at means of original variables by cluster
sum <- df %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))

cluster_label <- df %>% select(c(cluster, county))


# Overlay map of Texas

# 1. Load data
df_map <- cluster_label

# 2. Load Texas shapefile directly from Census zip URL and standardize county names
#texas <- st_read("/vsizip//vsicurl/https://www2.census.gov/geo/tiger/GENZ2021/shp/cb_2021_us_county_5m.zip") %>%
#  filter(STATEFP == "48")
texas <-  st_read("/vsizip/input_data/cb_2021_us_county_5m.zip") %>% 
  filter(STATEFP == "48") %>%
  mutate(county = tolower(str_trim(NAME))) 
#%>%
#  filter(!county %in% c("mcculloch", "mclennan", "mcmullen", "dewitt", "loving"))

#################### ADD 2023 METROPOLITAN REGION ENCODING ################
# https://www.cdc.gov/nchs/data-analysis-tools/urban-rural.html #
# https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/OAE/urbanrural/NCHSUR2023_3156.csv #

urb_rur <- read_csv("input_data/NCHSUR2023_3156.csv")
# unify county names
clean_county <- function(x) {
  x %>%
    str_to_lower() %>%
    str_remove(regex(",?\\s*texas$", ignore_case = TRUE)) %>%
    str_remove(regex("\\s+county$", ignore_case = TRUE)) %>%
    str_squish()
}

urb_rur$county <- clean_county(urb_rur$CTYNAME)
urb_rur <- urb_rur %>% 
  filter(ST_ABBREV == "TX") 
#%>% 
#  filter(!county %in% c("mcculloch", "mclennan", "mcmullen", "dewitt", "loving"))



merged <- texas %>%
  left_join(df_map, by = "county") %>%
  left_join(urb_rur %>% select(county, CODE2023), by = "county") %>%
  left_join(df_raw %>% select(county, outbreak), by="county") %>%
  mutate(cluster = as.factor(as.integer(cluster)),
         CODE2023 = as.factor(as.integer(CODE2023))
  )



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




# Define labels
code_labels <- c(
  '1' = 'Large central metro',
  '2' = 'Large fringe metro',
  '3' = 'Medium metro',
  '4' = 'Small metro',
  '5' = 'Micropolitan',
  '6' = 'Noncore'
)





# 1. Enforce ALL 6 codes (1:6) and ALL 3 clusters (1:3)
codes    <- 1:6
clusters <- 1:3

# Define color palettes matching matplotlib ('Reds', 'Greens', 'Blues')
cmap_names <- c('Reds', 'Greens', 'Blues')

# 2. Vectorized color blending function
get_color <- function(cluster, code) {
  # Create output vector initialized with grey for missing/invalid cases
  out_colors <- rep(rgb(0.83, 0.83, 0.83), length(cluster))
  
  # Logical mask for valid observations
  valid <- !is.na(cluster) & !is.na(code) & (cluster %in% clusters) & (code %in% codes)
  
  if (!any(valid)) return(out_colors)
  
  # Compute color for valid rows
  cl_val   <- as.numeric(as.character(cluster[valid]))
  code_val <- as.numeric(as.character(code[valid]))
  
  code_idx  <- match(code_val, codes) - 1
  n_codes   <- length(codes)
  shade_val <- if (n_codes > 1) 0.95 - 0.65 * (code_idx / (n_codes - 1)) else 0.95
  
  cl_idx  <- match(cl_val, clusters)
  
  # Calculate colors per row
  for (i in seq_along(cl_idx)) {
    pal_fun <- colorRampPalette(brewer.pal(9, cmap_names[cl_idx[i]]))
    out_colors[which(valid)[i]] <- pal_fun(100)[round(shade_val[i] * 99) + 1]
  }
  
  out_colors
}

# -------------------------------------------------------------------------
# 3. Build full legend reference table with cluster header rows
# -------------------------------------------------------------------------
headers_df <- data.frame(
  CODE2023    = NA, 
  cluster     = clusters,
  colorcode   = "transparent", 
  lbl_text    = "",
  label       = paste0("**<span style='font-size:10pt;'>Cluster ", clusters, "</span>**"),
  group_label = paste("Cluster", clusters),
  key         = paste0("HEADER_CLUSTER_", clusters),
  is_header   = TRUE,
  stringsAsFactors = FALSE
)

legend_df <- expand.grid(CODE2023 = codes, cluster = clusters) %>%
  arrange(cluster, CODE2023) %>%
  mutate(
    colorcode   = get_color(cluster, CODE2023),
    lbl_text    = ifelse(as.character(CODE2023) %in% names(code_labels), 
                         code_labels[as.character(CODE2023)], 
                         paste("CODE", CODE2023)),
    label       = paste0("&nbsp;&nbsp;", CODE2023, " · ", lbl_text),
    group_label = paste("Cluster", cluster),
    key         = paste0("Cluster ", cluster, " - ", CODE2023, " · ", lbl_text),
    is_header   = FALSE
  )

full_legend_list <- list()
for (cl in clusters) {
  full_legend_list[[paste0("h_", cl)]] <- headers_df %>% filter(cluster == cl)
  full_legend_list[[paste0("d_", cl)]] <- legend_df %>% filter(cluster == cl)
}

other_header_df <- data.frame(
  CODE2023    = NA, 
  cluster     = NA,
  colorcode   = "transparent", 
  lbl_text    = "",
  label       = "**<span style='font-size:10pt;'>Other</span>**", 
  group_label = "Other",
  key         = "HEADER_OTHER", 
  is_header   = TRUE, 
  stringsAsFactors = FALSE
)

no_data_df <- data.frame(
  CODE2023    = NA, 
  cluster     = NA,
  colorcode   = "grey80", 
  lbl_text    = "No Data",
  label       = "&nbsp;&nbsp;No Data", 
  group_label = "Other",
  key         = "No Data", 
  is_header   = FALSE, 
  stringsAsFactors = FALSE
)

full_legend_list[["h_no"]] <- other_header_df
full_legend_list[["d_no"]] <- no_data_df

full_legend <- bind_rows(full_legend_list)

all_keys  <- full_legend$key
color_map <- setNames(full_legend$colorcode, full_legend$key)
label_map <- setNames(full_legend$label, full_legend$key)

# Build explicit named vectors for border stroke colors
border_map <- setNames(
  ifelse(full_legend$is_header, "transparent", "white"),
  full_legend$key
)


# -------------------------------------------------------------------------
# 4. Map 'merged' data frame to factor keys
# -------------------------------------------------------------------------
merged <- merged %>%
  mutate(
    lbl_text = ifelse(as.character(CODE2023) %in% names(code_labels), 
                      code_labels[as.character(CODE2023)], 
                      paste("CODE", CODE2023)),
    key_str  = ifelse(is.na(cluster) | is.na(CODE2023), 
                      "No Data", 
                      paste0("Cluster ", cluster, " - ", CODE2023, " · ", lbl_text)),
    plot_key = factor(key_str, levels = all_keys)
  )

# -------------------------------------------------------------------------
# 5. Extract point centroids for outbreaks
# -------------------------------------------------------------------------
pts <- merged %>%
  filter(!is.na(outbreak) & outbreak > 0) %>%
  mutate(
    coords = st_coordinates(st_point_on_surface(geometry)),
    `_x` = coords[, 1],
    `_y` = coords[, 2]
  )



# -------------------------------------------------------------------------
# 6. Final Plot
# -------------------------------------------------------------------------
png("results/cluster_Texas_map.png", width = 9, height = 8, units = "in", res = 300)


ggplot() +
  geom_sf(data = merged, aes(fill = plot_key), color = "white", linewidth = 0.4, show.legend = TRUE) +
  scale_fill_manual(
    values   = color_map,
    labels   = label_map,
    breaks   = all_keys,
    drop     = FALSE,
    na.value = "grey80",
    name     = "Urbanization Level"
  ) +
  geom_point(
    data = pts, 
    aes(x = `_x`, y = `_y`, size = outbreak),
    shape = 21, fill = "navajowhite", color = "white", stroke = 0.8, alpha = 0.75
  ) +
  #scale_size_continuous(range = c(4, 12), name = "Measles Cases") +
  scale_size_continuous(range = c(4, 12), guide = "none") +
  geom_text(
    data = pts, 
    aes(x = `_x`, y = `_y`, label = as.integer(outbreak)),
    color = "black", size = 2.8, fontface = "bold"
  ) +
  labs(title = "Cluster Map with Measles Case Counts") +
  theme_void() +
  theme(
    #plot.margin      = margin(t = 10, r = 20, b = 10, l = 10, unit = "pt"), # Added margin (Top, Right, Bottom, Left)
    plot.margin = ggplot2::margin(10, 20, 10, 10, unit="pt"),
    plot.title       = element_text(size = 15, face = "bold", hjust = 0.5, margin = ggplot2::margin(0, 0, 12, 0)),
    legend.position  = "right",
    legend.key.size  = unit(0.4, "cm"),
    legend.text      = element_markdown(size = 8),
    legend.title     = element_text(size = 10, face = "bold")
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(
        shape = NA,
        color = border_map[all_keys]
      ),
      ncol=1
    )#,
    #size = guide_legend(order = 2)
  )

dev.off()

write.csv(cluster_label, "results/cluster_labels.csv", row.names = FALSE)
write.csv(sum, "results/cluster_means.csv", row.names = FALSE)

