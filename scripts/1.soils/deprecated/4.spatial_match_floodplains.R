## Matching gSSURGO floodplain presence with PLS data spatially

rm(list = ls())

#### ILLINOIS ####
load('data/raw/soils/gssurgo_floodplain_030_700m.RData')

load('data/processed/PLS/illinois_format.RData')

# Format illinois data
illinois <- illinois |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species') |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Divide pls illinois into 20 sections
illinois1 <- illinois[1:3961,]
illinois2 <- illinois[3962:7923,]
illinois3 <- illinois[7924:11885,]
illinois4 <- illinois[11886:15847,]
illinois5 <- illinois[15848:19809,]
illinois6 <- illinois[19810:23771,]
illinois7 <- illinois[23772:27733,]
illinois8 <- illinois[27734:31695,]
illinois9 <- illinois[31696:35657,]
illinois10 <- illinois[35658:39619,]
illinois11 <- illinois[39620:43581,]
illinois12 <- illinois[43582:47543,]
illinois13 <- illinois[47544:51505,]
illinois14 <- illinois[51506:55467,]
illinois15 <- illinois[55468:59429,]
illinois16 <- illinois[59430:63391,]
illinois17 <- illinois[63392:67353,]
illinois18 <- illinois[67354:71315,]
illinois19 <- illinois[71316:75277,]
illinois20 <- illinois[75278:79232,]

# Select coordinates
coords_illinois1 <- dplyr::select(illinois1, y, x)
coords_illinois2 <- dplyr::select(illinois2, y, x)
coords_illinois3 <- dplyr::select(illinois3, y, x)
coords_illinois4 <- dplyr::select(illinois4, y, x)
coords_illinois5 <- dplyr::select(illinois5, y, x)
coords_illinois6 <- dplyr::select(illinois6, y, x)
coords_illinois7 <- dplyr::select(illinois7, y, x)
coords_illinois8 <- dplyr::select(illinois8, y, x)
coords_illinois9 <- dplyr::select(illinois9, y, x)
coords_illinois10 <- dplyr::select(illinois10, y, x)
coords_illinois11 <- dplyr::select(illinois11, y, x)
coords_illinois12 <- dplyr::select(illinois12, y, x)
coords_illinois13 <- dplyr::select(illinois13, y, x)
coords_illinois14 <- dplyr::select(illinois14, y, x)
coords_illinois15 <- dplyr::select(illinois15, y, x)
coords_illinois16 <- dplyr::select(illinois16, y, x)
coords_illinois17 <- dplyr::select(illinois17, y, x)
coords_illinois18 <- dplyr::select(illinois18, y, x)
coords_illinois19 <- dplyr::select(illinois19, y, x)
coords_illinois20 <- dplyr::select(illinois20, y, x)

# Add column names
colnames(coords_illinois1) <- colnames(coords_illinois2) <- colnames(coords_illinois3) <-
  colnames(coords_illinois4) <- colnames(coords_illinois5) <- colnames(coords_illinois6) <-
  colnames(coords_illinois7) <- colnames(coords_illinois8) <- colnames(coords_illinois9) <- 
  colnames(coords_illinois10) <- colnames(coords_illinois11) <- colnames(coords_illinois12) <-
  colnames(coords_illinois13) <- colnames(coords_illinois14) <- colnames(coords_illinois15) <-
  colnames(coords_illinois16) <- colnames(coords_illinois16) <- colnames(coords_illinois17) <-
  colnames(coords_illinois18) <- colnames(coords_illinois19) <- colnames(coords_illinois20) <- 
  c('lat', 'long')

# Subset the soil data to the lat/long extent of each pls subsection
# Then extract only the lat/long columns
coords_df_IL1 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois1$long) - 0.1 & x <= max(coords_illinois1$long) + 0.1 &
                  y >= min(coords_illinois1$lat) - 0.1 & y <= max(coords_illinois1$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL2 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois2$long) - 0.1 & x <= max(coords_illinois2$long) + 0.1 &
                  y >= min(coords_illinois2$lat) - 0.1 & y <= max(coords_illinois2$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL3 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois3$long) - 0.1 & x <= max(coords_illinois3$long) + 0.1 &
                  y >= min(coords_illinois3$lat) - 0.1 & y <= max(coords_illinois3$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL4 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois4$long) - 0.1 & x <= max(coords_illinois4$long) + 0.1 &
                  y >= min(coords_illinois4$lat) - 0.1 & y <= max(coords_illinois4$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL5 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois5$long) - 0.1 & x <= max(coords_illinois5$long) + 0.1 &
                  y >= min(coords_illinois5$lat) - 0.1 & y <= max(coords_illinois5$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL6 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois6$long) - 0.1 & x <= max(coords_illinois6$long) + 0.1 &
                  y >= min(coords_illinois6$lat) - 0.1 & y <= max(coords_illinois6$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL7 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois7$long) - 0.1 & x <= max(coords_illinois7$long) + 0.1 &
                  y >= min(coords_illinois7$lat) - 0.1 & y <= max(coords_illinois7$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL8 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois8$long) - 0.1 & x <= max(coords_illinois8$long) + 0.1 &
                  y >= min(coords_illinois8$lat) - 0.1 & y <= max(coords_illinois8$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL9 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois9$long) - 0.1 & x <= max(coords_illinois9$long) + 0.1 &
                  y >= min(coords_illinois9$lat) - 0.1 & y <= max(coords_illinois9$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL10 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois10$long) - 0.1 & x <= max(coords_illinois10$long) + 0.1 &
                  y >= min(coords_illinois10$lat) - 0.1 & y <= max(coords_illinois10$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL11 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois11$long) - 0.1 & x <= max(coords_illinois11$long) + 0.1 &
                  y >= min(coords_illinois11$lat) - 0.1 & y <= max(coords_illinois11$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL12 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois12$long) - 0.1 & x <= max(coords_illinois12$long) + 0.1 &
                  y >= min(coords_illinois12$lat) - 0.1 & y <= max(coords_illinois12$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL13 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois13$long) - 0.1 & x <= max(coords_illinois13$long) + 0.1 &
                  y >= min(coords_illinois13$lat) - 0.1 & y <= max(coords_illinois13$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL14 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois14$long) - 0.1 & x <= max(coords_illinois14$long) + 0.1 &
                  y >= min(coords_illinois14$lat) - 0.1 & y <= max(coords_illinois14$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL15 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois15$long) - 0.1 & x <= max(coords_illinois15$long) + 0.1 &
                  y >= min(coords_illinois15$lat) - 0.1 & y <= max(coords_illinois15$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL16 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois16$long) - 0.1 & x <= max(coords_illinois16$long) + 0.1 &
                  y >= min(coords_illinois16$lat) - 0.1 & y <= max(coords_illinois16$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL17 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois17$long) - 0.1 & x <= max(coords_illinois17$long) + 0.1 &
                  y >= min(coords_illinois17$lat) - 0.1 & y <= max(coords_illinois17$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL18 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois18$long) - 0.1 & x <= max(coords_illinois18$long) + 0.1 &
                  y >= min(coords_illinois18$lat) - 0.1 & y <= max(coords_illinois18$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL19 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois19$long) - 0.1 & x <= max(coords_illinois19$long) + 0.1 &
                  y >= min(coords_illinois19$lat) - 0.1 & y <= max(coords_illinois19$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL20 <- df_IL |>
  dplyr::filter(x >= min(coords_illinois20$long) - 0.1 & x <= max(coords_illinois20$long) + 0.1 &
                  y >= min(coords_illinois20$lat) - 0.1 & y <= max(coords_illinois20$lat) + 0.1) |>
  dplyr::select(y, x)

# Find the distance between each pair of points in the pls (1) and soil (2) dataframes
dists <- fields::rdist(coords_df_IL1, coords_illinois1)
# Find the  closest soil point to each PLS point 
# (should have length = nrow(pls subset))
closest_points <- apply(dists, 2, which.min)
# Immediately delete dists because it's huge
rm(dists)

# Make new dataframe of corresponding soil data to the PLS points
# We'll add to this subsequently
select_df_IL <- df_IL |>
  # Filter as done above to make sure indexing matches
  dplyr::filter(x >= min(coords_illinois1$long) - 0.1 & x <= max(coords_illinois1$long) + 0.1 &
                  y >= min(coords_illinois1$lat) - 0.1 & y <= max(coords_illinois1$lat) + 0.1) |>
  # subset by row numbers given by which.min
  dplyr::slice(closest_points)

# Column bind with PLS data
IL_flood_pls <- cbind(select_df_IL, illinois1)

# Remove unnecessary objects to keep memory usage lower
rm(closest_points, coords_df_IL1, coords_illinois1, select_df_IL, illinois1)

# Repeat all these steps for the remaining 19 subsets

# Subset 2
dists <- fields::rdist(coords_df_IL2, coords_illinois2)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois2$long) - 0.1 & x <= max(coords_illinois2$long) + 0.1 &
                  y >= min(coords_illinois2$lat) - 0.1 & y <= max(coords_illinois2$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois2)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL2, coords_illinois2, select_df_IL, illinois2)

# Subset 3
dists <- fields::rdist(coords_df_IL3, coords_illinois3)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois3$long) - 0.1 & x <= max(coords_illinois3$long) + 0.1 &
                  y >= min(coords_illinois3$lat) - 0.1 & y <= max(coords_illinois3$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois3)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL3, coords_illinois3, select_df_IL, illinois3)

# Subset 4
dists <- fields::rdist(coords_df_IL4, coords_illinois4)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois4$long) - 0.1 & x <= max(coords_illinois4$long) + 0.1 &
                  y >= min(coords_illinois4$lat) - 0.1 & y <= max(coords_illinois4$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois4)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL4, coords_illinois4, select_df_IL, illinois4)

# Subset 5
dists <- fields::rdist(coords_df_IL5, coords_illinois5)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois5$long) - 0.1 & x <= max(coords_illinois5$long) + 0.1 &
                  y >= min(coords_illinois5$lat) - 0.1 & y <= max(coords_illinois5$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois5)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL5, coords_illinois5, select_df_IL, illinois5)

# Subset 6
dists <- fields::rdist(coords_df_IL6, coords_illinois6)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois6$long) - 0.1 & x <= max(coords_illinois6$long) + 0.1 &
                  y >= min(coords_illinois6$lat) - 0.1 & y <= max(coords_illinois6$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois6)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL6, coords_illinois6, select_df_IL, illinois6)

# Subset 7
dists <- fields::rdist(coords_df_IL7, coords_illinois7)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois7$long) - 0.1 & x <= max(coords_illinois7$long) + 0.1 &
                  y >= min(coords_illinois7$lat) - 0.1 & y <= max(coords_illinois7$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois7)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL7, coords_illinois7, select_df_IL, illinois7)

# Subset 8
dists <- fields::rdist(coords_df_IL8, coords_illinois8)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois8$long) - 0.1 & x <= max(coords_illinois8$long) + 0.1 &
                  y >= min(coords_illinois8$lat) - 0.1 & y <= max(coords_illinois8$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois8)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL8, coords_illinois8, select_df_IL, illinois8)

# Subset 9
dists <- fields::rdist(coords_df_IL9, coords_illinois9)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois9$long) - 0.1 & x <= max(coords_illinois9$long) + 0.1 &
                  y >= min(coords_illinois9$lat) - 0.1 & y <= max(coords_illinois9$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois9)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL9, coords_illinois9, select_df_IL, illinois9)

# Subset 10
dists <- fields::rdist(coords_df_IL10, coords_illinois10)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois10$long) - 0.1 & x <= max(coords_illinois10$long) + 0.1 &
                  y >= min(coords_illinois10$lat) - 0.1 & y <= max(coords_illinois10$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois10)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL10, coords_illinois10, select_df_IL, illinois10)

# Subset 11
dists <- fields::rdist(coords_df_IL11, coords_illinois11)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois11$long) - 0.1 & x <= max(coords_illinois11$long) + 0.1 &
                  y >= min(coords_illinois11$lat) - 0.1 & y <= max(coords_illinois11$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois11)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL11, coords_illinois11, select_df_IL, illinois11)

# Subset 12
dists <- fields::rdist(coords_df_IL12, coords_illinois12)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois12$long) - 0.1 & x <= max(coords_illinois12$long) + 0.1 &
                  y >= min(coords_illinois12$lat) - 0.1 & y <= max(coords_illinois12$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois12)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL12, coords_illinois12, select_df_IL, illinois12)

# Subset 13
dists <- fields::rdist(coords_df_IL13, coords_illinois13)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois13$long) - 0.1 & x <= max(coords_illinois13$long) + 0.1 &
                  y >= min(coords_illinois13$lat) - 0.1 & y <= max(coords_illinois13$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois13)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL13, coords_illinois13, select_df_IL, illinois13)

# Subset 14
dists <- fields::rdist(coords_df_IL14, coords_illinois14)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois14$long) - 0.1 & x <= max(coords_illinois14$long) + 0.1 &
                  y >= min(coords_illinois14$lat) - 0.1 & y <= max(coords_illinois14$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois14)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL14, coords_illinois14, select_df_IL, illinois14)

# Subset 15
dists <- fields::rdist(coords_df_IL15, coords_illinois15)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois15$long) - 0.1 & x <= max(coords_illinois15$long) + 0.1 &
                  y >= min(coords_illinois15$lat) - 0.1 & y <= max(coords_illinois15$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois15)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL15, coords_illinois15, select_df_IL, illinois15)

# Subset 16
dists <- fields::rdist(coords_df_IL16, coords_illinois16)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois16$long) - 0.1 & x <= max(coords_illinois16$long) + 0.1 &
                  y >= min(coords_illinois16$lat) - 0.1 & y <= max(coords_illinois16$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois16)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL16, coords_illinois16, select_df_IL, illinois16)

# Subset 17
dists <- fields::rdist(coords_df_IL17, coords_illinois17)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois17$long) - 0.1 & x <= max(coords_illinois17$long) + 0.1 &
                  y >= min(coords_illinois17$lat) - 0.1 & y <= max(coords_illinois17$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois17)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL17, coords_illinois17, select_df_IL, illinois17)

# Subset 18
dists <- fields::rdist(coords_df_IL18, coords_illinois18)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois18$long) - 0.1 & x <= max(coords_illinois18$long) + 0.1 &
                  y >= min(coords_illinois18$lat) - 0.1 & y <= max(coords_illinois18$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois18)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL18, coords_illinois18, select_df_IL, illinois18)

# Subset 19
dists <- fields::rdist(coords_df_IL19, coords_illinois19)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois19$long) - 0.1 & x <= max(coords_illinois19$long) + 0.1 &
                  y >= min(coords_illinois19$lat) - 0.1 & y <= max(coords_illinois19$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois19)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL19, coords_illinois19, select_df_IL, illinois19)

# Subset 20
dists <- fields::rdist(coords_df_IL20, coords_illinois20)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL |>
  dplyr::filter(x >= min(coords_illinois20$long) - 0.1 & x <= max(coords_illinois20$long) + 0.1 &
                  y >= min(coords_illinois20$lat) - 0.1 & y <= max(coords_illinois20$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois20)
IL_flood_pls <- rbind(IL_flood_pls, temp)

rm(closest_points, coords_df_IL20, coords_illinois20, select_df_IL, illinois20)

# Update column names
colnames(IL_flood_pls) <- c('soil_x', 'soil_y', 'geomdesc', 'Floodplain',
                            'state', 'pls_x', 'pls_y', 
                            'L1_tree1', 'L1_tree2', 'L1_tree3', 'L1_tree4', 'uniqueID')

# Save IL soil. Confirmed in same order as illinois and illinois_ecosystem
IL_flood <- dplyr::select(IL_flood_pls, soil_x, soil_y, Floodplain, pls_x, pls_y)
save(IL_flood, file = 'data/processed/soils/processed_flood_il.RData')

#### INDIANA ####
rm(list = ls())

load('data/raw/soils/gssurgo_floodplain_030_700m.RData')

load('data/processed/PLS/indiana_format.RData')

# Format indiana data
indiana <- indiana |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species') |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Divide pls indiana into 20 sections
indiana1 <- indiana[1:3959,]
indiana2 <- indiana[3960:7919,]
indiana3 <- indiana[7920:11879,]
indiana4 <- indiana[11880:15839,]
indiana5 <- indiana[15840:19799,]
indiana6 <- indiana[19800:23759,]
indiana7 <- indiana[23760:27719,]
indiana8 <- indiana[27720:31679,]
indiana9 <- indiana[31680:35639,]
indiana10 <- indiana[35640:39599,]
indiana11 <- indiana[39600:43559,]
indiana12 <- indiana[43560:47519,]
indiana13 <- indiana[47520:51479,]
indiana14 <- indiana[51480:55439,]
indiana15 <- indiana[55440:59399,]
indiana16 <- indiana[59400:63359,]
indiana17 <- indiana[63360:67319,]
indiana18 <- indiana[67320:71279,]
indiana19 <- indiana[71280:75239,]
indiana20 <- indiana[75240:79177,]

# Select coordinates
coords_indiana1 <- dplyr::select(indiana1, y, x)
coords_indiana2 <- dplyr::select(indiana2, y, x)
coords_indiana3 <- dplyr::select(indiana3, y, x)
coords_indiana4 <- dplyr::select(indiana4, y, x)
coords_indiana5 <- dplyr::select(indiana5, y, x)
coords_indiana6 <- dplyr::select(indiana6, y, x)
coords_indiana7 <- dplyr::select(indiana7, y, x)
coords_indiana8 <- dplyr::select(indiana8, y, x)
coords_indiana9 <- dplyr::select(indiana9, y, x)
coords_indiana10 <- dplyr::select(indiana10, y, x)
coords_indiana11 <- dplyr::select(indiana11, y, x)
coords_indiana12 <- dplyr::select(indiana12, y, x)
coords_indiana13 <- dplyr::select(indiana13, y, x)
coords_indiana14 <- dplyr::select(indiana14, y, x)
coords_indiana15 <- dplyr::select(indiana15, y, x)
coords_indiana16 <- dplyr::select(indiana16, y, x)
coords_indiana17 <- dplyr::select(indiana17, y, x)
coords_indiana18 <- dplyr::select(indiana18, y, x)
coords_indiana19 <- dplyr::select(indiana19, y, x)
coords_indiana20 <- dplyr::select(indiana20, y, x)

# Add column names
colnames(coords_indiana1) <- colnames(coords_indiana2) <- colnames(coords_indiana3) <-
  colnames(coords_indiana4) <- colnames(coords_indiana5) <- colnames(coords_indiana6) <-
  colnames(coords_indiana7) <- colnames(coords_indiana8) <- colnames(coords_indiana9) <-
  colnames(coords_indiana10) <- colnames(coords_indiana11) <- colnames(coords_indiana12) <-
  colnames(coords_indiana13) <- colnames(coords_indiana14) <- colnames(coords_indiana15) <-
  colnames(coords_indiana16) <- colnames(coords_indiana17) <- colnames(coords_indiana18) <-
  colnames(coords_indiana19) <- colnames(coords_indiana20) <- c('lat', 'long')

# Subset the soil data to the lat/long extent of each pls subsection
# Then extract only the lat/long columns
coords_df_IN1 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana1$long) - 0.1 & x <= max(coords_indiana1$long) + 0.1 &
                  y >= min(coords_indiana1$lat) - 0.1 & y <= max(coords_indiana1$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN2 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana2$long) - 0.1 & x <= max(coords_indiana2$long) + 0.1 &
                  y >= min(coords_indiana2$lat) - 0.1 & y <= max(coords_indiana2$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN3 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana3$long) - 0.1 & x <= max(coords_indiana3$long) + 0.1 &
                  y >= min(coords_indiana3$lat) - 0.1 & y <= max(coords_indiana3$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN4 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana4$long) - 0.1 & x <= max(coords_indiana4$long) + 0.1 &
                  y >= min(coords_indiana4$lat) - 0.1 & y <= max(coords_indiana4$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN5 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana5$long) - 0.1 & x <= max(coords_indiana5$long) + 0.1 &
                  y >= min(coords_indiana5$lat) - 0.1 & y <= max(coords_indiana5$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN6 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana6$long) - 0.1 & x <= max(coords_indiana6$long) + 0.1 &
                  y >= min(coords_indiana6$lat) - 0.1 & y <= max(coords_indiana6$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN7 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana7$long) - 0.1 & x <= max(coords_indiana7$long) + 0.1 &
                  y >= min(coords_indiana7$lat) - 0.1 & y <= max(coords_indiana7$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN8 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana8$long) - 0.1 & x <= max(coords_indiana8$long) + 0.1 &
                  y >= min(coords_indiana8$lat) - 0.1 & y <= max(coords_indiana8$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN9 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana9$long) - 0.1 & x <= max(coords_indiana9$long) + 0.1 &
                  y >= min(coords_indiana9$lat) - 0.1 & y <= max(coords_indiana9$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN10 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana10$long) - 0.1 & x <= max(coords_indiana10$long) + 0.1 &
                  y >= min(coords_indiana10$lat) - 0.1 & y <= max(coords_indiana10$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN11 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana11$long) - 0.1 & x <= max(coords_indiana11$long) + 0.1 &
                  y >= min(coords_indiana11$lat) - 0.1 & y <= max(coords_indiana11$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN12 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana12$long) - 0.1 & x <= max(coords_indiana12$long) + 0.1 &
                  y >= min(coords_indiana12$lat) - 0.1 & y <= max(coords_indiana12$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN13 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana13$long) - 0.1 & x <= max(coords_indiana13$long) + 0.1 &
                  y >= min(coords_indiana13$lat) - 0.1 & y <= max(coords_indiana13$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN14 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana14$long) - 0.1 & x <= max(coords_indiana14$long) + 0.1 &
                  y >= min(coords_indiana14$lat) - 0.1 & y <= max(coords_indiana14$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN15 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana15$long) - 0.1 & x <= max(coords_indiana15$long) + 0.1 &
                  y >= min(coords_indiana15$lat) - 0.1 & y <= max(coords_indiana15$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN16 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana16$long) - 0.1 & x <= max(coords_indiana16$long) + 0.1 &
                  y >= min(coords_indiana16$lat) - 0.1 & y <= max(coords_indiana16$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN17 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana17$long) - 0.1 & x <= max(coords_indiana17$long) + 0.1 &
                  y >= min(coords_indiana17$lat) - 0.1 & y <= max(coords_indiana17$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN18 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana18$long) - 0.1 & x <= max(coords_indiana18$long) + 0.1 &
                  y >= min(coords_indiana18$lat) - 0.1 & y <= max(coords_indiana18$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN19 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana19$long) - 0.1 & x <= max(coords_indiana19$long) + 0.1 &
                  y >= min(coords_indiana19$lat) - 0.1 & y <= max(coords_indiana19$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN20 <- df_IN |>
  dplyr::filter(x >= min(coords_indiana20$long) - 0.1 & x <= max(coords_indiana20$long) + 0.1 &
                  y >= min(coords_indiana20$lat) - 0.1 & y <= max(coords_indiana20$lat) + 0.1) |>
  dplyr::select(y, x)

# Find the distance between each pair of pls (1) and soil (2) dataframes
dists <- fields::rdist(coords_df_IN1, coords_indiana1)
# Find the closest soil point to each PLS point
# (should have length = nrow(pls subset))
closest_points <- apply(dists, 2, which.min)
# Immediately delete dists because it's huge
rm(dists)

# Make new dataframe of corresponding soil data to the PLS pionts
# We'll add to this subsequently
select_df_IN <- df_IN |>
  # Filter as done above to make sure indexing matches
  dplyr::filter(x >= min(coords_indiana1$long) - 0.1 & x <= max(coords_indiana1$long) + 0.1 &
                  y >= min(coords_indiana1$lat) - 0.1 & y <= max(coords_indiana1$lat) + 0.1) |>
  # subset by row numbers given by which.min
  dplyr::slice(closest_points)

# Column bind with PLS data
IN_flood_pls <- cbind(select_df_IN, indiana1)

# Remove unnecessary objects to keep memory usage lower
rm(closest_points, coords_df_IN1, coords_indiana1, select_df_IN, indiana1)

# Repeat all these steps for the remaining 19 subsets

# Subset 2
dists <- fields::rdist(coords_df_IN2, coords_indiana2)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana2$long) - 0.1 & x <= max(coords_indiana2$long) + 0.1 &
                  y >= min(coords_indiana2$lat) - 0.1 & y <= max(coords_indiana2$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana2)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN2, coords_indiana2, select_df_IN, indiana2)

# Subset 3
dists <- fields::rdist(coords_df_IN3, coords_indiana3)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana3$long) - 0.1 & x <= max(coords_indiana3$long) + 0.1 &
                  y >= min(coords_indiana3$lat) - 0.1 & y <= max(coords_indiana3$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana3)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN3, coords_indiana3, select_df_IN, indiana3)

# Subset 4
dists <- fields::rdist(coords_df_IN4, coords_indiana4)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana4$long) - 0.1 & x <= max(coords_indiana4$long) + 0.1 &
                  y >= min(coords_indiana4$lat) - 0.1 & y <= max(coords_indiana4$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana4)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN4, coords_indiana4, select_df_IN, indiana4)

# Subset 5
dists <- fields::rdist(coords_df_IN5, coords_indiana5)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana5$long) - 0.1 & x <= max(coords_indiana5$long) + 0.1 &
                  y >= min(coords_indiana5$lat) - 0.1 & y <= max(coords_indiana5$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana5)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN5, coords_indiana5, select_df_IN, indiana5)

# Subset 6
dists <- fields::rdist(coords_df_IN6, coords_indiana6)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana6$long) - 0.1 & x <= max(coords_indiana6$long) + 0.1 &
                  y >= min(coords_indiana6$lat) - 0.1 & y <= max(coords_indiana6$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana6)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN6, coords_indiana6, select_df_IN, indiana6)

# Subset 7
dists <- fields::rdist(coords_df_IN7, coords_indiana7)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana7$long) - 0.1 & x <= max(coords_indiana7$long) + 0.1 &
                  y >= min(coords_indiana7$lat) - 0.1 & y <= max(coords_indiana7$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana7)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN7, coords_indiana7, select_df_IN, indiana7)

# Subset 8
dists <- fields::rdist(coords_df_IN8, coords_indiana8)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana8$long) - 0.1 & x <= max(coords_indiana8$long) + 0.1 &
                  y >= min(coords_indiana8$lat) - 0.1 & y <= max(coords_indiana8$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana8)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN8, coords_indiana8, select_df_IN, indiana8)

# Subset 9
dists <- fields::rdist(coords_df_IN9, coords_indiana9)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana9$long) - 0.1 & x <= max(coords_indiana9$long) + 0.1 &
                  y >= min(coords_indiana9$lat) - 0.1 & y <= max(coords_indiana9$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana9)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN9, coords_indiana9, select_df_IN, indiana9)

# Subset 10
dists <- fields::rdist(coords_df_IN10, coords_indiana10)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana10$long) - 0.1 & x <= max(coords_indiana10$long) + 0.1 &
                  y >= min(coords_indiana10$lat) - 0.1 & y <= max(coords_indiana10$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana10)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN10, coords_indiana10, select_df_IN, indiana10)

# Subset 11
dists <- fields::rdist(coords_df_IN11, coords_indiana11)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana11$long) - 0.1 & x <= max(coords_indiana11$long) + 0.1 &
                  y >= min(coords_indiana11$lat) - 0.1 & y <= max(coords_indiana11$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana11)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN11, coords_indiana11, select_df_IN, indiana11)

# Subset 12
dists <- fields::rdist(coords_df_IN12, coords_indiana12)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana12$long) - 0.1 & x <= max(coords_indiana12$long) + 0.1 &
                  y >= min(coords_indiana12$lat) - 0.1 & y <= max(coords_indiana12$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana12)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN12, coords_indiana12, select_df_IN, indiana12)

# Subset 13
dists <- fields::rdist(coords_df_IN13, coords_indiana13)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana13$long) - 0.1 & x <= max(coords_indiana13$long) + 0.1 &
                  y >= min(coords_indiana13$lat) - 0.1 & y <= max(coords_indiana13$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana13)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN13, coords_indiana13, select_df_IN, indiana13)

# Subset 14
dists <- fields::rdist(coords_df_IN14, coords_indiana14)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana14$long) - 0.1 & x <= max(coords_indiana14$long) + 0.1 &
                  y >= min(coords_indiana14$lat) - 0.1 & y <= max(coords_indiana14$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana14)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN14, coords_indiana14, select_df_IN, indiana14)

# Subset 15
dists <- fields::rdist(coords_df_IN15, coords_indiana15)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana15$long) - 0.1 & x <= max(coords_indiana15$long) + 0.1 &
                  y >= min(coords_indiana15$lat) - 0.1 & y <= max(coords_indiana15$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana15)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN15, coords_indiana15, select_df_IN, indiana15)

# Subset 16
dists <- fields::rdist(coords_df_IN16, coords_indiana16)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana16$long) - 0.1 & x <= max(coords_indiana16$long) + 0.1 &
                  y >= min(coords_indiana16$lat) - 0.1 & y <= max(coords_indiana16$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana16)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN16, coords_indiana16, select_df_IN, indiana16)

# Subset 17
dists <- fields::rdist(coords_df_IN17, coords_indiana17)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana17$long) - 0.1 & x <= max(coords_indiana17$long) + 0.1 &
                  y >= min(coords_indiana17$lat) - 0.1 & y <= max(coords_indiana17$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana17)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN17, coords_indiana17, select_df_IN, indiana17)

# Subset 18
dists <- fields::rdist(coords_df_IN18, coords_indiana18)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana18$long) - 0.1 & x <= max(coords_indiana18$long) + 0.1 &
                  y >= min(coords_indiana18$lat) - 0.1 & y <= max(coords_indiana18$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana18)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN18, coords_indiana18, select_df_IN, indiana18)

# Subset 19
dists <- fields::rdist(coords_df_IN19, coords_indiana19)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana19$long) - 0.1 & x <= max(coords_indiana19$long) + 0.1 &
                  y >= min(coords_indiana19$lat) - 0.1 & y <= max(coords_indiana19$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana19)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN19, coords_indiana19, select_df_IN, indiana19)

# Subset 20
dists <- fields::rdist(coords_df_IN20, coords_indiana20)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN |>
  dplyr::filter(x >= min(coords_indiana20$long) - 0.1 & x <= max(coords_indiana20$long) + 0.1 &
                  y >= min(coords_indiana20$lat) - 0.1 & y <= max(coords_indiana20$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana20)
IN_flood_pls <- rbind(IN_flood_pls, temp)

rm(closest_points, coords_df_IN20, coords_indiana20, select_df_IN, indiana20)

# Update column names
colnames(IN_flood_pls) <- c('soil_x', 'soil_y', 'geomdesc', 'Floodplain',
                            'state', 'pls_x', 'pls_y',
                           'L1_tree1', 'L1_tree2', 'uniqueID')
# Save IN floodplain Confirmed in same order as indiana nad indiana_ecosystem
IN_flood <- dplyr::select(IN_flood_pls, soil_x, soil_y, Floodplain, pls_x, pls_y)
save(IN_flood, file = 'data/processed/soils/processed_flood_in.RData')

#### MICHIGAN ####
rm(list = ls())

load('data/raw/soils/gssurgo_floodplain_030_700m.RData')

load('data/processed/PLS/upmichigan_process.RData')
load('data/processed/PLS/lowmichigan_process.RData')

# Format lower michigan data
lowmichigan <- lowmichigan |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species') |>
  dplyr::rename(SPP1 = species1,
                SPP2 = species2)

# Format upper michigan data
upmichigan <- upmichigan |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species')

michigan <- rbind(lowmichigan, upmichigan)
michigan_ecosystem <- rbind(lowmichigan_ecosystem,
                            upmichigan_ecosystem)

# Divide pls michigan into 20 sections
michigan1 <- michigan[1:5942,]
michigan2 <- michigan[5943:11885,]
michigan3 <- michigan[11886:17828,]
michigan4 <- michigan[17829:23771,]
michigan5 <- michigan[23772:29715,]
michigan6 <- michigan[29716:35658,]
michigan7 <- michigan[35659:41601,]
michigan8 <- michigan[41602:47543,]
michigan9 <- michigan[47544:53486,]
michigan10 <- michigan[53487:59429,]
michigan11 <- michigan[59430:65372,]
michigan12 <- michigan[65373:71315,]
michigan13 <- michigan[71316:77258,]
michigan14 <- michigan[77259:83201,]
michigan15 <- michigan[83202:89144,]
michigan16 <- michigan[89145:95087,]
michigan17 <- michigan[95088:101030,]
michigan18 <- michigan[101031:106973,]
michigan19 <- michigan[106974:112916,]
michigan20 <- michigan[112917:120554,]

# Select coordinates
coords_michigan1 <- dplyr::select(michigan1, y, x)
coords_michigan2 <- dplyr::select(michigan2, y, x)
coords_michigan3 <- dplyr::select(michigan3, y, x)
coords_michigan4 <- dplyr::select(michigan4, y, x)
coords_michigan5 <- dplyr::select(michigan5, y, x)
coords_michigan6 <- dplyr::select(michigan6, y, x)
coords_michigan7 <- dplyr::select(michigan7, y, x)
coords_michigan8 <- dplyr::select(michigan8, y, x)
coords_michigan9 <- dplyr::select(michigan9, y, x)
coords_michigan10 <- dplyr::select(michigan10, y, x)
coords_michigan11 <- dplyr::select(michigan11, y, x)
coords_michigan12 <- dplyr::select(michigan12, y, x)
coords_michigan13 <- dplyr::select(michigan13, y, x)
coords_michigan14 <- dplyr::select(michigan14, y, x)
coords_michigan15 <- dplyr::select(michigan15, y, x)
coords_michigan16 <- dplyr::select(michigan16, y, x)
coords_michigan17 <- dplyr::select(michigan17, y, x)
coords_michigan18 <- dplyr::select(michigan18, y, x)
coords_michigan19 <- dplyr::select(michigan19, y, x)
coords_michigan20 <- dplyr::select(michigan20, y, x)

# Add column names
colnames(coords_michigan1) <- colnames(coords_michigan2) <- colnames(coords_michigan3) <-
  colnames(coords_michigan4) <- colnames(coords_michigan5) <- colnames(coords_michigan6) <-
  colnames(coords_michigan7) <- colnames(coords_michigan8) <- colnames(coords_michigan9) <-
  colnames(coords_michigan10) <- colnames(coords_michigan11) <- colnames(coords_michigan12) <-
  colnames(coords_michigan13) <- colnames(coords_michigan14) <- colnames(coords_michigan15) <-
  colnames(coords_michigan16) <- colnames(coords_michigan17) <- colnames(coords_michigan18) <-
  colnames(coords_michigan19) <- colnames(coords_michigan20) <- c('lat', 'long')

# Subset the soil data to the lat/long extent of each pls subsection
# Then extract only the lat/long columns
coords_df_MI1 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan1$long) - 0.1 & x <= max(coords_michigan1$long) + 0.1 &
                  y >= min(coords_michigan1$lat) - 0.1 & y <= max(coords_michigan1$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI2 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan2$long) - 0.1 & x <= max(coords_michigan2$long) + 0.1 &
                  y >= min(coords_michigan2$lat) - 0.1 & y <= max(coords_michigan2$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI3 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan3$long) - 0.1 & x <= max(coords_michigan3$long) + 0.1 &
                  y >= min(coords_michigan3$lat) - 0.1 & y <= max(coords_michigan3$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI4 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan4$long) - 0.1 & x <= max(coords_michigan4$long) + 0.1 &
                  y >= min(coords_michigan4$lat) - 0.1 & y <= max(coords_michigan4$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI5 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan5$long) - 0.1 & x <= max(coords_michigan5$long) + 0.1 &
                  y >= min(coords_michigan5$lat) - 0.1 & y <= max(coords_michigan5$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI6 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan6$long) - 0.1 & x <= max(coords_michigan6$long) + 0.1 &
                  y >= min(coords_michigan6$lat) - 0.1 & y <= max(coords_michigan6$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI7 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan7$long) - 0.1 & x <= max(coords_michigan7$long) + 0.1 &
                  y >= min(coords_michigan7$lat) - 0.1 & y <= max(coords_michigan7$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI8 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan8$long) - 0.1 & x <= max(coords_michigan8$long) + 0.1 &
                  y >= min(coords_michigan8$lat) - 0.1 & y <= max(coords_michigan8$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI9 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan9$long) - 0.1 & x <= max(coords_michigan9$long) + 0.1 &
                  y >= min(coords_michigan9$lat) - 0.1 & y <= max(coords_michigan9$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI10 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan10$long) - 0.1 & x <= max(coords_michigan10$long) + 0.1 &
                  y >= min(coords_michigan10$lat) - 0.1 & y <= max(coords_michigan10$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI11 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan11$long) - 0.1 & x <= max(coords_michigan11$long) + 0.1 &
                  y >= min(coords_michigan11$lat) - 0.1 & y <= max(coords_michigan11$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI12 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan12$long) - 0.1 & x <= max(coords_michigan12$long) + 0.1 &
                  y >= min(coords_michigan12$lat) - 0.1 & y <= max(coords_michigan12$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI13 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan13$long) - 0.1 & x <= max(coords_michigan13$long) + 0.1 &
                  y >= min(coords_michigan13$lat) - 0.1 & y <= max(coords_michigan13$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI14 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan14$long) - 0.1 & x <= max(coords_michigan14$long) + 0.1 &
                  y >= min(coords_michigan14$lat) - 0.1 & y <= max(coords_michigan14$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI15 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan15$long) - 0.1 & x <= max(coords_michigan15$long) + 0.1 &
                  y >= min(coords_michigan15$lat) - 0.1 & y <= max(coords_michigan15$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI16 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan16$long) - 0.1 & x <= max(coords_michigan16$long) + 0.1 &
                  y >= min(coords_michigan16$lat) - 0.1 & y <= max(coords_michigan16$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI17 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan17$long) - 0.1 & x <= max(coords_michigan17$long) + 0.1 &
                  y >= min(coords_michigan17$lat) - 0.1 & y <= max(coords_michigan17$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI18 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan18$long) - 0.1 & x <= max(coords_michigan18$long) + 0.1 &
                  y >= min(coords_michigan18$lat) - 0.1 & y <= max(coords_michigan18$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI19 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan19$long) - 0.1 & x <= max(coords_michigan19$long) + 0.1 &
                  y >= min(coords_michigan19$lat) - 0.1 & y <= max(coords_michigan19$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI20 <- df_MI |>
  dplyr::filter(x >= min(coords_michigan20$long) - 0.1 & x <= max(coords_michigan20$long) + 0.1 &
                  y >= min(coords_michigan20$lat) - 0.1 & y <= max(coords_michigan20$lat) + 0.1) |>
  dplyr::select(y, x)

# Find the distance between each pair of points in the pls (1) and soil (2) dataframes
dists <- fields::rdist(coords_df_MI1, coords_michigan1)
# Find the closest soil point to each PLS point
# (should have length = nrow(pls subset))
closest_points <- apply(dists, 2, which.min)
# Immediately delete dists because it's huge
rm(dists)

# Make new dataframe of corresponding soil data to the PLS points
# We'll add to this subsequently
select_df_MI <- df_MI |>
  # Filter as done above to make sure indexing matches
  dplyr::filter(x >= min(coords_michigan1$long) - 0.1 & x <= max(coords_michigan1$long) + 0.1 &
                  y >= min(coords_michigan1$lat) - 0.1 & y <= max(coords_michigan1$lat) + 0.1) |>
  # subset by row numbers given by which.min
  dplyr::slice(closest_points)

# Column bind with PLS data
MI_flood_pls <- cbind(select_df_MI, michigan1)

# Remove unnecessary objects to keep memory usage lower
rm(closest_points, coords_df_MI1, coords_michigan1, select_df_MI, michigan1)

# Repeat all these steps for the remaining 39 subsets

# Subset 2
dists <- fields::rdist(coords_df_MI2, coords_michigan2)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan2$long) - 0.1 & x <= max(coords_michigan2$long) + 0.1 &
                  y >= min(coords_michigan2$lat) - 0.1 & y <= max(coords_michigan2$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan2)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI2, coords_michigan2, select_df_MI, michigan2)

# Subset 3
dists <- fields::rdist(coords_df_MI3, coords_michigan3)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan3$long) - 0.1 & x <= max(coords_michigan3$long) + 0.1 &
                  y >= min(coords_michigan3$lat) - 0.1 & y <= max(coords_michigan3$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan3)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI3, coords_michigan3, select_df_MI, michigan3)

# Subset 4
dists <- fields::rdist(coords_df_MI4, coords_michigan4)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan4$long) - 0.1 & x <= max(coords_michigan4$long) + 0.1 &
                  y >= min(coords_michigan4$lat) - 0.1 & y <= max(coords_michigan4$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan4)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI4, coords_michigan4, select_df_MI, michigan4)

# Subset 5
dists <- fields::rdist(coords_df_MI5, coords_michigan5)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan5$long) - 0.1 & x <= max(coords_michigan5$long) + 0.1 &
                  y >= min(coords_michigan5$lat) - 0.1 & y <= max(coords_michigan5$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan5)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI5, coords_michigan5, select_df_MI, michigan5)

# Subset 6
dists <- fields::rdist(coords_df_MI6, coords_michigan6)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan6$long) - 0.1 & x <= max(coords_michigan6$long) + 0.1 &
                  y >= min(coords_michigan6$lat) - 0.1 & y <= max(coords_michigan6$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan6)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI6, coords_michigan6, select_df_MI, michigan6)

# Subset 7
dists <- fields::rdist(coords_df_MI7, coords_michigan7)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan7$long) - 0.1 & x <= max(coords_michigan7$long) + 0.1 &
                  y >= min(coords_michigan7$lat) - 0.1 & y <= max(coords_michigan7$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan7)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI7, coords_michigan7, select_df_MI, michigan7)

# Subset 8
dists <- fields::rdist(coords_df_MI8, coords_michigan8)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan8$long) - 0.1 & x <= max(coords_michigan8$long) + 0.1 &
                  y >= min(coords_michigan8$lat) - 0.1 & y <= max(coords_michigan8$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan8)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI8, coords_michigan8, select_df_MI, michigan8)

# Subset 9
dists <- fields::rdist(coords_df_MI9, coords_michigan9)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan9$long) - 0.1 & x <= max(coords_michigan9$long) + 0.1 &
                  y >= min(coords_michigan9$lat) - 0.1 & y <= max(coords_michigan9$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan9)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI9, coords_michigan9, select_df_MI, michigan9)

# Subset 10
dists <- fields::rdist(coords_df_MI10, coords_michigan10)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan10$long) - 0.1 & x <= max(coords_michigan10$long) + 0.1 &
                  y >= min(coords_michigan10$lat) - 0.1 & y <= max(coords_michigan10$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan10)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI10, coords_michigan10, select_df_MI, michigan10)

# Subset 11
dists <- fields::rdist(coords_df_MI11, coords_michigan11)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan11$long) - 0.1 & x <= max(coords_michigan11$long) + 0.1 &
                  y >= min(coords_michigan11$lat) - 0.1 & y <= max(coords_michigan11$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan11)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI11, coords_michigan11, select_df_MI, michigan11)

# Subset 12
dists <- fields::rdist(coords_df_MI12, coords_michigan12)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan12$long) - 0.1 & x <= max(coords_michigan12$long) + 0.1 &
                  y >= min(coords_michigan12$lat) - 0.1 & y <= max(coords_michigan12$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan12)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI12, coords_michigan12, select_df_MI, michigan12)

# Subset 13
dists <- fields::rdist(coords_df_MI13, coords_michigan13)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan13$long) - 0.1 & x <= max(coords_michigan13$long) + 0.1 &
                  y >= min(coords_michigan13$lat) - 0.1 & y <= max(coords_michigan13$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan13)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI13, coords_michigan13, select_df_MI, michigan13)

# Subset 14
dists <- fields::rdist(coords_df_MI14, coords_michigan14)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan14$long) - 0.1 & x <= max(coords_michigan14$long) + 0.1 &
                  y >= min(coords_michigan14$lat) - 0.1 & y <= max(coords_michigan14$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan14)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI14, coords_michigan14, select_df_MI, michigan14)

# Subset 15
dists <- fields::rdist(coords_df_MI15, coords_michigan15)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan15$long) - 0.1 & x <= max(coords_michigan15$long) + 0.1 &
                  y >= min(coords_michigan15$lat) - 0.1 & y <= max(coords_michigan15$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan15)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI15, coords_michigan15, select_df_MI, michigan15)

# Subset 16
dists <- fields::rdist(coords_df_MI16, coords_michigan16)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan16$long) - 0.1 & x <= max(coords_michigan16$long) + 0.1 &
                  y >= min(coords_michigan16$lat) - 0.1 & y <= max(coords_michigan16$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan16)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI16, coords_michigan16, select_df_MI, michigan16)

# Subset 17
dists <- fields::rdist(coords_df_MI17, coords_michigan17)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan17$long) - 0.1 & x <= max(coords_michigan17$long) + 0.1 &
                  y >= min(coords_michigan17$lat) - 0.1 & y <= max(coords_michigan17$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan17)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI17, coords_michigan17, select_df_MI, michigan17)

# Subset 18
dists <- fields::rdist(coords_df_MI18, coords_michigan18)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan18$long) - 0.1 & x <= max(coords_michigan18$long) + 0.1 &
                  y >= min(coords_michigan18$lat) - 0.1 & y <= max(coords_michigan18$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan18)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI18, coords_michigan18, select_df_MI, michigan18)

# Subset 19
dists <- fields::rdist(coords_df_MI19, coords_michigan19)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan19$long) - 0.1 & x <= max(coords_michigan19$long) + 0.1 &
                  y >= min(coords_michigan19$lat) - 0.1 & y <= max(coords_michigan19$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan19)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI19, coords_michigan19, select_df_MI, michigan19)

# Subset 20
dists <- fields::rdist(coords_df_MI20, coords_michigan20)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI |>
  dplyr::filter(x >= min(coords_michigan20$long) - 0.1 & x <= max(coords_michigan20$long) + 0.1 &
                  y >= min(coords_michigan20$lat) - 0.1 & y <= max(coords_michigan20$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan20)
MI_flood_pls <- rbind(MI_flood_pls, temp)

rm(closest_points, coords_df_MI20, coords_michigan20, select_df_MI, michigan20)

# Update column names
colnames(MI_flood_pls) <- c('soil_x', 'soil_y', 'geomdesc', 'Floodplain',
                            'state', 'pls_x', 'pls_y',
                           'L1_tree1', 'L1_tree2')

# Save MI floodplain. Confirmed in same order as michigan and michigan_ecosystem
MI_flood <- dplyr::select(MI_flood_pls, soil_x, soil_y, Floodplain, pls_x, pls_y)
save(MI_flood, file = 'data/processed/soils/processed_flood_mi.RData')

#### MINNESOTA ####
rm(list = ls())

load('data/raw/soils/gssurgo_floodplain_030_700m.RData')

load('data/processed/PLS/minnesota_process.RData')

# Format minnesota data
minnesota <- minnesota |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species',
                     values_fn = list) |>
  tidyr::unnest(cols = everything()) |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Divide pls minnesota into 30 sections
minnesota1 <- minnesota[1:4797,]
minnesota2 <- minnesota[4798:9595,]
minnesota3 <- minnesota[9596:14392,]
minnesota4 <- minnesota[14393:19190,]
minnesota5 <- minnesota[19191:23988,]
minnesota6 <- minnesota[23989:28786,]
minnesota7 <- minnesota[28787:33584,]
minnesota8 <- minnesota[33585:38382,]
minnesota9 <- minnesota[38383:43180,]
minnesota10 <- minnesota[43181:47978,]
minnesota11 <- minnesota[47979:52776,]
minnesota12 <- minnesota[52777:57574,]
minnesota13 <- minnesota[57575:62372,]
minnesota14 <- minnesota[62373:67170,]
minnesota15 <- minnesota[67171:71968,]
minnesota16 <- minnesota[71969:76766,]
minnesota17 <- minnesota[76767:81564,]
minnesota18 <- minnesota[81565:86362,]
minnesota19 <- minnesota[86363:91160,]
minnesota20 <- minnesota[91161:95958,]
minnesota21 <- minnesota[95959:100756,]
minnesota22 <- minnesota[100757:105554,]
minnesota23 <- minnesota[105555:110352,]
minnesota24 <- minnesota[110353:115150,]
minnesota25 <- minnesota[115151:119948,]
minnesota26 <- minnesota[119949:124746,]
minnesota27 <- minnesota[124747:129544,]
minnesota28 <- minnesota[129545:134342,]
minnesota29 <- minnesota[134343:139140,]
minnesota30 <- minnesota[139141:143932,]

# Select coordinates
coords_minnesota1 <- dplyr::select(minnesota1, y, x)
coords_minnesota2 <- dplyr::select(minnesota2, y, x)
coords_minnesota3 <- dplyr::select(minnesota3, y, x)
coords_minnesota4 <- dplyr::select(minnesota4, y, x)
coords_minnesota5 <- dplyr::select(minnesota5, y, x)
coords_minnesota6 <- dplyr::select(minnesota6, y, x)
coords_minnesota7 <- dplyr::select(minnesota7, y, x)
coords_minnesota8 <- dplyr::select(minnesota8, y, x)
coords_minnesota9 <- dplyr::select(minnesota9, y, x)
coords_minnesota10 <- dplyr::select(minnesota10, y, x)
coords_minnesota11 <- dplyr::select(minnesota11, y, x)
coords_minnesota12 <- dplyr::select(minnesota12, y, x)
coords_minnesota13 <- dplyr::select(minnesota13, y, x)
coords_minnesota14 <- dplyr::select(minnesota14, y, x)
coords_minnesota15 <- dplyr::select(minnesota15, y, x)
coords_minnesota16 <- dplyr::select(minnesota16, y, x)
coords_minnesota17 <- dplyr::select(minnesota17, y, x)
coords_minnesota18 <- dplyr::select(minnesota18, y, x)
coords_minnesota19 <- dplyr::select(minnesota19, y, x)
coords_minnesota20 <- dplyr::select(minnesota20, y, x)
coords_minnesota21 <- dplyr::select(minnesota21, y, x)
coords_minnesota22 <- dplyr::select(minnesota22, y, x)
coords_minnesota23 <- dplyr::select(minnesota23, y, x)
coords_minnesota24 <- dplyr::select(minnesota24, y, x)
coords_minnesota25 <- dplyr::select(minnesota25, y, x)
coords_minnesota26 <- dplyr::select(minnesota26, y, x)
coords_minnesota27 <- dplyr::select(minnesota27, y, x)
coords_minnesota28 <- dplyr::select(minnesota28, y, x)
coords_minnesota29 <- dplyr::select(minnesota29, y, x)
coords_minnesota30 <- dplyr::select(minnesota30, y, x)

# Add column names
colnames(coords_minnesota1) <- colnames(coords_minnesota2) <- colnames(coords_minnesota3) <-
  colnames(coords_minnesota4) <- colnames(coords_minnesota5) <- colnames(coords_minnesota6) <-
  colnames(coords_minnesota7) <- colnames(coords_minnesota8) <- colnames(coords_minnesota9) <-
  colnames(coords_minnesota10) <- colnames(coords_minnesota11) <- colnames(coords_minnesota12) <-
  colnames(coords_minnesota13) <- colnames(coords_minnesota14) <- colnames(coords_minnesota15) <-
  colnames(coords_minnesota16) <- colnames(coords_minnesota17) <- colnames(coords_minnesota18) <-
  colnames(coords_minnesota19) <- colnames(coords_minnesota20) <- colnames(coords_minnesota21) <-
  colnames(coords_minnesota22) <- colnames(coords_minnesota23) <- colnames(coords_minnesota24) <-
  colnames(coords_minnesota25) <- colnames(coords_minnesota26) <- colnames(coords_minnesota27) <-
  colnames(coords_minnesota28) <- colnames(coords_minnesota29) <- colnames(coords_minnesota30) <-
  c('lat', 'long')

# Subset the soil data to the lat/long extent of each pls subsection
# Then extract only the lat/long columns
coords_df_MN1 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota1$long) - 0.1 & x <= max(coords_minnesota1$long) + 0.1 &
                  y >= min(coords_minnesota1$lat) - 0.1 & y <= max(coords_minnesota1$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN2 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota2$long) - 0.1 & x <= max(coords_minnesota2$long) + 0.1 &
                  y >= min(coords_minnesota2$lat) - 0.1 & y <= max(coords_minnesota2$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN3 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota3$long) - 0.1 & x <= max(coords_minnesota3$long) + 0.1 &
                  y >= min(coords_minnesota3$lat) - 0.1 & y <= max(coords_minnesota3$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN4 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota4$long) - 0.1 & x <= max(coords_minnesota4$long) + 0.1 &
                  y >= min(coords_minnesota4$lat) - 0.1 & y <= max(coords_minnesota4$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN5 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota5$long) - 0.1 & x <= max(coords_minnesota5$long) + 0.1 &
                  y >= min(coords_minnesota5$lat) - 0.1 & y <= max(coords_minnesota5$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN6 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota6$long) - 0.1 & x <= max(coords_minnesota6$long) + 0.1 &
                  y >= min(coords_minnesota6$lat) - 0.1 & y <= max(coords_minnesota6$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN7 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota7$long) - 0.1 & x <= max(coords_minnesota7$long) + 0.1 &
                  y >= min(coords_minnesota7$lat) - 0.1 & y <= max(coords_minnesota7$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN8 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota8$long) - 0.1 & x <= max(coords_minnesota8$long) + 0.1 &
                  y >= min(coords_minnesota8$lat) - 0.1 & y <= max(coords_minnesota8$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN9 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota9$long) - 0.1 & x <= max(coords_minnesota9$long) + 0.1 &
                  y >= min(coords_minnesota9$lat) - 0.1 & y <= max(coords_minnesota9$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN10 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota10$long) - 0.1 & x <= max(coords_minnesota10$long) + 0.1 &
                  y >= min(coords_minnesota10$lat) - 0.1 & y <= max(coords_minnesota10$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN11 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota11$long) - 0.1 & x <= max(coords_minnesota11$long) + 0.1 &
                  y >= min(coords_minnesota11$lat) - 0.1 & y <= max(coords_minnesota11$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN12 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota12$long) - 0.1 & x <= max(coords_minnesota12$long) + 0.1 &
                  y >= min(coords_minnesota12$lat) - 0.1 & y <= max(coords_minnesota12$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN13 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota13$long) - 0.1 & x <= max(coords_minnesota13$long) + 0.1 &
                  y >= min(coords_minnesota13$lat) - 0.1 & y <= max(coords_minnesota13$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN14 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota14$long) - 0.1 & x <= max(coords_minnesota14$long) + 0.1 &
                  y >= min(coords_minnesota14$lat) - 0.1 & y <= max(coords_minnesota14$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN15 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota15$long) - 0.1 & x <= max(coords_minnesota15$long) + 0.1 &
                  y >= min(coords_minnesota15$lat) - 0.1 & y <= max(coords_minnesota15$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN16 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota16$long) - 0.1 & x <= max(coords_minnesota16$long) + 0.1 &
                  y >= min(coords_minnesota16$lat) - 0.1 & y <= max(coords_minnesota16$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN17 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota17$long) - 0.1 & x <= max(coords_minnesota17$long) + 0.1 &
                  y >= min(coords_minnesota17$lat) - 0.1 & y <= max(coords_minnesota17$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN18 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota18$long) - 0.1 & x <= max(coords_minnesota18$long) + 0.1 &
                  y >= min(coords_minnesota18$lat) - 0.1 & y <= max(coords_minnesota18$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN19 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota19$long) - 0.1 & x <= max(coords_minnesota19$long) + 0.1 &
                  y >= min(coords_minnesota19$lat) - 0.1 & y <= max(coords_minnesota19$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN20 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota20$long) - 0.1 & x <= max(coords_minnesota20$long) + 0.1 &
                  y >= min(coords_minnesota20$lat) - 0.1 & y <= max(coords_minnesota20$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN21 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota21$long) - 0.1 & x <= max(coords_minnesota21$long) + 0.1 &
                  y >= min(coords_minnesota21$lat) - 0.1 & y <= max(coords_minnesota21$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN22 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota22$long) - 0.1 & x <= max(coords_minnesota22$long) + 0.1 &
                  y >= min(coords_minnesota22$lat) - 0.1 & y <= max(coords_minnesota22$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN23 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota23$long) - 0.1 & x <= max(coords_minnesota23$long) + 0.1 &
                  y >= min(coords_minnesota23$lat) - 0.1 & y <= max(coords_minnesota23$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN24 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota24$long) - 0.1 & x <= max(coords_minnesota24$long) + 0.1 &
                  y >= min(coords_minnesota24$lat) - 0.1 & y <= max(coords_minnesota24$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN25 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota25$long) - 0.1 & x <= max(coords_minnesota25$long) + 0.1 &
                  y >= min(coords_minnesota25$lat) - 0.1 & y <= max(coords_minnesota25$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN26 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota26$long) - 0.1 & x <= max(coords_minnesota26$long) + 0.1 &
                  y >= min(coords_minnesota26$lat) - 0.1 & y <= max(coords_minnesota26$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN27 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota27$long) - 0.1 & x <= max(coords_minnesota27$long) + 0.1 &
                  y >= min(coords_minnesota27$lat) - 0.1 & y <= max(coords_minnesota27$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN28 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota28$long) - 0.1 & x <= max(coords_minnesota28$long) + 0.1 &
                  y >= min(coords_minnesota28$lat) - 0.1 & y <= max(coords_minnesota28$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN29 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota29$long) - 0.1 & x <= max(coords_minnesota29$long) + 0.1 &
                  y >= min(coords_minnesota29$lat) - 0.1 & y <= max(coords_minnesota29$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN30 <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota30$long) - 0.1 & x <= max(coords_minnesota30$long) + 0.1 &
                  y >= min(coords_minnesota30$lat) - 0.1 & y <= max(coords_minnesota30$lat) + 0.1) |>
  dplyr::select(y, x)

# Find the distance between each pair of points in the pls (1) and soil (2) dataframes
dists <- fields::rdist(coords_df_MN1, coords_minnesota1)
# Find the closest soil point to each PLS point
# (should have length = nrow(pls subset))
closest_points <- apply(dists, 2, which.min)
# Immediately delete dists because it's huge
rm(dists)

# Make new dataframe of corresponding soil data to the PLS points
# We'll add to this subsequently
select_df_MN <- df_MN |>
  # Filter as done above to make sure indexing matches
  dplyr::filter(x >= min(coords_minnesota1$long) - 0.1 & x <= max(coords_minnesota1$long) + 0.1 &
                  y >= min(coords_minnesota1$lat) - 0.1 & y <= max(coords_minnesota1$lat) + 0.1) |>
  # subset by row numbers given by which.min
  dplyr::slice(closest_points)

# Column bind with PLS data
MN_flood_pls <- cbind(select_df_MN, minnesota1)

# Remove unnecessary objects to keep memory usage lower
rm(closest_points, coords_df_MN1, coords_minnesota1, select_df_MN, minnesota1)

# Repeat all these steps for the remaining 29 subsets

# Subset 2
dists <- fields::rdist(coords_df_MN2, coords_minnesota2)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota2$long) - 0.1 & x <= max(coords_minnesota2$long) + 0.1 &
                  y >= min(coords_minnesota2$lat) - 0.1 & y <= max(coords_minnesota2$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota2)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN2, coords_minnesota2, select_df_MN, minnesota2)

# Subset 3
dists <- fields::rdist(coords_df_MN3, coords_minnesota3)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota3$long) - 0.1 & x <= max(coords_minnesota3$long) + 0.1 &
                  y >= min(coords_minnesota3$lat) - 0.1 & y <= max(coords_minnesota3$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota3)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN3, coords_minnesota3, select_df_MN, minnesota3)

# Subset 4
dists <- fields::rdist(coords_df_MN4, coords_minnesota4)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota4$long) - 0.1 & x <= max(coords_minnesota4$long) + 0.1 &
                  y >= min(coords_minnesota4$lat) - 0.1 & y <= max(coords_minnesota4$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota4)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN4, coords_minnesota4, select_df_MN, minnesota4)

# Subset 5
dists <- fields::rdist(coords_df_MN5, coords_minnesota5)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota5$long) - 0.1 & x <= max(coords_minnesota5$long) + 0.1 &
                  y >= min(coords_minnesota5$lat) - 0.1 & y <= max(coords_minnesota5$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota5)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN5, coords_minnesota5, select_df_MN, minnesota5)

# Subset 6
dists <- fields::rdist(coords_df_MN6, coords_minnesota6)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota6$long) - 0.1 & x <= max(coords_minnesota6$long) + 0.1 &
                  y >= min(coords_minnesota6$lat) - 0.1 & y <= max(coords_minnesota6$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota6)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN6, coords_minnesota6, select_df_MN, minnesota6)

# Subset 7
dists <- fields::rdist(coords_df_MN7, coords_minnesota7)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota7$long) - 0.1 & x <= max(coords_minnesota7$long) + 0.1 &
                  y >= min(coords_minnesota7$lat) - 0.1 & y <= max(coords_minnesota7$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota7)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN7, coords_minnesota7, select_df_MN, minnesota7)

# Subset 8
dists <- fields::rdist(coords_df_MN8, coords_minnesota8)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota8$long) - 0.1 & x <= max(coords_minnesota8$long) + 0.1 &
                  y >= min(coords_minnesota8$lat) - 0.1 & y <= max(coords_minnesota8$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota8)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN8, coords_minnesota8, select_df_MN, minnesota8)

# Subset 9
dists <- fields::rdist(coords_df_MN9, coords_minnesota9)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota9$long) - 0.1 & x <= max(coords_minnesota9$long) + 0.1 &
                  y >= min(coords_minnesota9$lat) - 0.1 & y <= max(coords_minnesota9$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota9)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN9, coords_minnesota9, select_df_MN, minnesota9)

# Subset 10
dists <- fields::rdist(coords_df_MN10, coords_minnesota10)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota10$long) - 0.1 & x <= max(coords_minnesota10$long) + 0.1 &
                  y >= min(coords_minnesota10$lat) - 0.1 & y <= max(coords_minnesota10$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota10)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN10, coords_minnesota10, select_df_MN, minnesota10)

# Subset 11
dists <- fields::rdist(coords_df_MN11, coords_minnesota11)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota11$long) - 0.1 & x <= max(coords_minnesota11$long) + 0.1 &
                  y >= min(coords_minnesota11$lat) - 0.1 & y <= max(coords_minnesota11$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota11)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN11, coords_minnesota11, select_df_MN, minnesota11)

# Subset 12
dists <- fields::rdist(coords_df_MN12, coords_minnesota12)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota12$long) - 0.1 & x <= max(coords_minnesota12$long) + 0.1 &
                  y >= min(coords_minnesota12$lat) - 0.1 & y <= max(coords_minnesota12$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota12)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN12, coords_minnesota12, select_df_MN, minnesota12)

# Subset 13
dists <- fields::rdist(coords_df_MN13, coords_minnesota13)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota13$long) - 0.1 & x <= max(coords_minnesota13$long) + 0.1 &
                  y >= min(coords_minnesota13$lat) - 0.1 & y <= max(coords_minnesota13$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota13)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN13, coords_minnesota13, select_df_MN, minnesota13)

# Subset 14
dists <- fields::rdist(coords_df_MN14, coords_minnesota14)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota14$long) - 0.1 & x <= max(coords_minnesota14$long) + 0.1 &
                  y >= min(coords_minnesota14$lat) - 0.1 & y <= max(coords_minnesota14$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota14)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN14, coords_minnesota14, select_df_MN, minnesota14)

# Subset 15
dists <- fields::rdist(coords_df_MN15, coords_minnesota15)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota15$long) - 0.1 & x <= max(coords_minnesota15$long) + 0.1 &
                  y >= min(coords_minnesota15$lat) - 0.1 & y <= max(coords_minnesota15$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota15)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN15, coords_minnesota15, select_df_MN, minnesota15)

# Subset 16
dists <- fields::rdist(coords_df_MN16, coords_minnesota16)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota16$long) - 0.1 & x <= max(coords_minnesota16$long) + 0.1 &
                  y >= min(coords_minnesota16$lat) - 0.1 & y <= max(coords_minnesota16$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota16)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN16, coords_minnesota16, select_df_MN, minnesota16)

# Subset 17
dists <- fields::rdist(coords_df_MN17, coords_minnesota17)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota17$long) - 0.1 & x <= max(coords_minnesota17$long) + 0.1 &
                  y >= min(coords_minnesota17$lat) - 0.1 & y <= max(coords_minnesota17$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota17)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN17, coords_minnesota17, select_df_MN, minnesota17)

# Subset 18
dists <- fields::rdist(coords_df_MN18, coords_minnesota18)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota18$long) - 0.1 & x <= max(coords_minnesota18$long) + 0.1 &
                  y >= min(coords_minnesota18$lat) - 0.1 & y <= max(coords_minnesota18$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota18)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN18, coords_minnesota18, select_df_MN, minnesota18)

# Subset 19
dists <- fields::rdist(coords_df_MN19, coords_minnesota19)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota19$long) - 0.1 & x <= max(coords_minnesota19$long) + 0.1 &
                  y >= min(coords_minnesota19$lat) - 0.1 & y <= max(coords_minnesota19$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota19)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN19, coords_minnesota19, select_df_MN, minnesota19)

# Subset 20
dists <- fields::rdist(coords_df_MN20, coords_minnesota20)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota20$long) - 0.1 & x <= max(coords_minnesota20$long) + 0.1 &
                  y >= min(coords_minnesota20$lat) - 0.1 & y <= max(coords_minnesota20$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota20)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN20, coords_minnesota20, select_df_MN, minnesota20)

# Subset 21
dists <- fields::rdist(coords_df_MN21, coords_minnesota21)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota21$long) - 0.1 & x <= max(coords_minnesota21$long) + 0.1 &
                  y >= min(coords_minnesota21$lat) - 0.1 & y <= max(coords_minnesota21$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota21)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN21, coords_minnesota21, select_df_MN, minnesota21)

# Subset 22
dists <- fields::rdist(coords_df_MN22, coords_minnesota22)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota22$long) - 0.1 & x <= max(coords_minnesota22$long) + 0.1 &
                  y >= min(coords_minnesota22$lat) - 0.1 & y <= max(coords_minnesota22$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota22)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN22, coords_minnesota22, select_df_MN, minnesota22)

# Subset 23
dists <- fields::rdist(coords_df_MN23, coords_minnesota23)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota23$long) - 0.1 & x <= max(coords_minnesota23$long) + 0.1 &
                  y >= min(coords_minnesota23$lat) - 0.1 & y <= max(coords_minnesota23$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota23)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN23, coords_minnesota23, select_df_MN, minnesota23)

# Subset 24
dists <- fields::rdist(coords_df_MN24, coords_minnesota24)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota24$long) - 0.1 & x <= max(coords_minnesota24$long) + 0.1 &
                  y >= min(coords_minnesota24$lat) - 0.1 & y <= max(coords_minnesota24$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota24)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN24, coords_minnesota24, select_df_MN, minnesota24)

# Subset 25
dists <- fields::rdist(coords_df_MN25, coords_minnesota25)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota25$long) - 0.1 & x <= max(coords_minnesota25$long) + 0.1 &
                  y >= min(coords_minnesota25$lat) - 0.1 & y <= max(coords_minnesota25$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota25)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN25, coords_minnesota25, select_df_MN, minnesota25)

# Subset 26
dists <- fields::rdist(coords_df_MN26, coords_minnesota26)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota26$long) - 0.1 & x <= max(coords_minnesota26$long) + 0.1 &
                  y >= min(coords_minnesota26$lat) - 0.1 & y <= max(coords_minnesota26$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota26)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN26, coords_minnesota26, select_df_MN, minnesota26)

# Subset 27
dists <- fields::rdist(coords_df_MN27, coords_minnesota27)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota27$long) - 0.1 & x <= max(coords_minnesota27$long) + 0.1 &
                  y >= min(coords_minnesota27$lat) - 0.1 & y <= max(coords_minnesota27$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota27)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN27, coords_minnesota27, select_df_MN, minnesota27)

# Subset 28
dists <- fields::rdist(coords_df_MN28, coords_minnesota28)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota28$long) - 0.1 & x <= max(coords_minnesota28$long) + 0.1 &
                  y >= min(coords_minnesota28$lat) - 0.1 & y <= max(coords_minnesota28$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota28)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN28, coords_minnesota28, select_df_MN, minnesota28)

# Subset 29
dists <- fields::rdist(coords_df_MN29, coords_minnesota29)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota29$long) - 0.1 & x <= max(coords_minnesota29$long) + 0.1 &
                  y >= min(coords_minnesota29$lat) - 0.1 & y <= max(coords_minnesota29$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota29)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN29, coords_minnesota29, select_df_MN, minnesota29)

# Subset 30
dists <- fields::rdist(coords_df_MN30, coords_minnesota30)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN |>
  dplyr::filter(x >= min(coords_minnesota30$long) - 0.1 & x <= max(coords_minnesota30$long) + 0.1 &
                  y >= min(coords_minnesota30$lat) - 0.1 & y <= max(coords_minnesota30$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota30)
MN_flood_pls <- rbind(MN_flood_pls, temp)

rm(closest_points, coords_df_MN30, coords_minnesota30, select_df_MN, minnesota30)

# Update column names
colnames(MN_flood_pls) <- c('soil_x', 'soil_y', 'geomdesc', 'Floodplain',
                            'state', 'pls_x', 'pls_y',
                           'L1_tree1', 'L1_tree2', 'L1_tree3', 'L1_tree4', 'uniqueID')

# Save MN floodplain. Confirmed in same order as minnesota and minnesota_ecosystem
MN_flood <- dplyr::select(MN_flood_pls, soil_x, soil_y, Floodplain, pls_x, pls_y)
save(MN_flood, file = 'data/processed/soils/processed_flood_mn.RData')

#### WISCONSIN ####
rm(list = ls())

load('data/raw/soils/gssurgo_floodplain_030_700m.RData')

load('data/processed/PLS/wisconsin_process.RData')

# Format wisconsin data
wisconsin <- wisconsin |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species') |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Divide pls wisconsin into 30 sections
wisconsin1 <- wisconsin[1:5554,]
wisconsin2 <- wisconsin[5555:11109,]
wisconsin3 <- wisconsin[11110:16664,]
wisconsin4 <- wisconsin[16665:22219,]
wisconsin5 <- wisconsin[22220:27774,]
wisconsin6 <- wisconsin[27775:33329,]
wisconsin7 <- wisconsin[33330:38884,]
wisconsin8 <- wisconsin[38885:44439,]
wisconsin9 <- wisconsin[44440:49994,]
wisconsin10 <- wisconsin[49995:55549,]
wisconsin11 <- wisconsin[55550:61104,]
wisconsin12 <- wisconsin[61105:66659,]
wisconsin13 <- wisconsin[66660:72214,]
wisconsin14 <- wisconsin[72215:77769,]
wisconsin15 <- wisconsin[77770:83324,]
wisconsin16 <- wisconsin[83325:88879,]
wisconsin17 <- wisconsin[88880:94434,]
wisconsin18 <- wisconsin[94435:99989,]
wisconsin19 <- wisconsin[99990:105544,]
wisconsin20 <- wisconsin[105545:111099,]
wisconsin21 <- wisconsin[111100:116654,]
wisconsin22 <- wisconsin[116655:122209,]
wisconsin23 <- wisconsin[122210:127764,]
wisconsin24 <- wisconsin[127765:133319,]
wisconsin25 <- wisconsin[133320:138874,]
wisconsin26 <- wisconsin[138875:144429,]
wisconsin27 <- wisconsin[144430:149984,]
wisconsin28 <- wisconsin[149985:155539,]
wisconsin29 <- wisconsin[155540:161094,]
wisconsin30 <- wisconsin[161095:166636,]

# Select coordinates
coords_wisconsin1 <- dplyr::select(wisconsin1, y, x)
coords_wisconsin2 <- dplyr::select(wisconsin2, y, x)
coords_wisconsin3 <- dplyr::select(wisconsin3, y, x)
coords_wisconsin4 <- dplyr::select(wisconsin4, y, x)
coords_wisconsin5 <- dplyr::select(wisconsin5, y, x)
coords_wisconsin6 <- dplyr::select(wisconsin6, y, x)
coords_wisconsin7 <- dplyr::select(wisconsin7, y, x)
coords_wisconsin8 <- dplyr::select(wisconsin8, y, x)
coords_wisconsin9 <- dplyr::select(wisconsin9, y, x)
coords_wisconsin10 <- dplyr::select(wisconsin10, y, x)
coords_wisconsin11 <- dplyr::select(wisconsin11, y, x)
coords_wisconsin12 <- dplyr::select(wisconsin12, y, x)
coords_wisconsin13 <- dplyr::select(wisconsin13, y, x)
coords_wisconsin14 <- dplyr::select(wisconsin14, y, x)
coords_wisconsin15 <- dplyr::select(wisconsin15, y, x)
coords_wisconsin16 <- dplyr::select(wisconsin16, y, x)
coords_wisconsin17 <- dplyr::select(wisconsin17, y, x)
coords_wisconsin18 <- dplyr::select(wisconsin18, y, x)
coords_wisconsin19 <- dplyr::select(wisconsin19, y, x)
coords_wisconsin20 <- dplyr::select(wisconsin20, y, x)
coords_wisconsin21 <- dplyr::select(wisconsin21, y, x)
coords_wisconsin22 <- dplyr::select(wisconsin22, y, x)
coords_wisconsin23 <- dplyr::select(wisconsin23, y, x)
coords_wisconsin24 <- dplyr::select(wisconsin24, y, x)
coords_wisconsin25 <- dplyr::select(wisconsin25, y, x)
coords_wisconsin26 <- dplyr::select(wisconsin26, y, x)
coords_wisconsin27 <- dplyr::select(wisconsin27, y, x)
coords_wisconsin28 <- dplyr::select(wisconsin28, y, x)
coords_wisconsin29 <- dplyr::select(wisconsin29, y, x)
coords_wisconsin30 <- dplyr::select(wisconsin30, y, x)

# Add column names
colnames(coords_wisconsin1) <- colnames(coords_wisconsin2) <- colnames(coords_wisconsin3) <-
  colnames(coords_wisconsin4) <- colnames(coords_wisconsin5) <- colnames(coords_wisconsin6) <-
  colnames(coords_wisconsin7) <- colnames(coords_wisconsin8) <- colnames(coords_wisconsin9) <-
  colnames(coords_wisconsin10) <- colnames(coords_wisconsin11) <- colnames(coords_wisconsin12) <-
  colnames(coords_wisconsin13) <- colnames(coords_wisconsin14) <- colnames(coords_wisconsin15) <-
  colnames(coords_wisconsin16) <- colnames(coords_wisconsin17) <- colnames(coords_wisconsin18) <-
  colnames(coords_wisconsin19) <- colnames(coords_wisconsin20) <- colnames(coords_wisconsin21) <-
  colnames(coords_wisconsin22) <- colnames(coords_wisconsin23) <- colnames(coords_wisconsin24) <-
  colnames(coords_wisconsin25) <- colnames(coords_wisconsin26) <- colnames(coords_wisconsin27) <-
  colnames(coords_wisconsin28) <- colnames(coords_wisconsin29) <- colnames(coords_wisconsin30) <-
  c('lat', 'long')

# Subset the soil data to the lat/long extent of each pls subsection
# Then extract only the lat/long columns
coords_df_WI1 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin1$long) - 0.1 & x <= max(coords_wisconsin1$long) + 0.1 &
                  y >= min(coords_wisconsin1$lat) - 0.1 & y <= max(coords_wisconsin1$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI2 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin2$long) - 0.1 & x <= max(coords_wisconsin2$long) + 0.1 &
                  y >= min(coords_wisconsin2$lat) - 0.1 & y <= max(coords_wisconsin2$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI3 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin3$long) - 0.1 & x <= max(coords_wisconsin3$long) + 0.1 &
                  y >= min(coords_wisconsin3$lat) - 0.1 & y <= max(coords_wisconsin3$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI4 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin4$long) - 0.1 & x <= max(coords_wisconsin4$long) + 0.1 &
                  y >= min(coords_wisconsin4$lat) - 0.1 & y <= max(coords_wisconsin4$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI5 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin5$long) - 0.1 & x <= max(coords_wisconsin5$long) + 0.1 &
                  y >= min(coords_wisconsin5$lat) - 0.1 & y <= max(coords_wisconsin5$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI6 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin6$long) - 0.1 & x <= max(coords_wisconsin6$long) + 0.1 &
                  y >= min(coords_wisconsin6$lat) - 0.1 & y <= max(coords_wisconsin6$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI7 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin7$long) - 0.1 & x <= max(coords_wisconsin7$long) + 0.1 &
                  y >= min(coords_wisconsin7$lat) - 0.1 & y <= max(coords_wisconsin7$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI8 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin8$long) - 0.1 & x <= max(coords_wisconsin8$long) + 0.1 &
                  y >= min(coords_wisconsin8$lat) - 0.1 & y <= max(coords_wisconsin8$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI9 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin9$long) - 0.1 & x <= max(coords_wisconsin9$long) + 0.1 &
                  y >= min(coords_wisconsin9$lat) - 0.1 & y <= max(coords_wisconsin9$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI10 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin10$long) - 0.1 & x <= max(coords_wisconsin10$long) + 0.1 &
                  y >= min(coords_wisconsin10$lat) - 0.1 & y <= max(coords_wisconsin10$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI11 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin11$long) - 0.1 & x <= max(coords_wisconsin11$long) + 0.1 &
                  y >= min(coords_wisconsin11$lat) - 0.1 & y <= max(coords_wisconsin11$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI12 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin12$long) - 0.1 & x <= max(coords_wisconsin12$long) + 0.1 &
                  y >= min(coords_wisconsin12$lat) - 0.1 & y <= max(coords_wisconsin12$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI13 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin13$long) - 0.1 & x <= max(coords_wisconsin13$long) + 0.1 &
                  y >= min(coords_wisconsin13$lat) - 0.1 & y <= max(coords_wisconsin13$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI14 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin14$long) - 0.1 & x <= max(coords_wisconsin14$long) + 0.1 &
                  y >= min(coords_wisconsin14$lat) - 0.1 & y <= max(coords_wisconsin14$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI15 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin15$long) - 0.1 & x <= max(coords_wisconsin15$long) + 0.1 &
                  y >= min(coords_wisconsin15$lat) - 0.1 & y <= max(coords_wisconsin15$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI16 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin16$long) - 0.1 & x <= max(coords_wisconsin16$long) + 0.1 &
                  y >= min(coords_wisconsin16$lat) - 0.1 & y <= max(coords_wisconsin16$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI17 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin17$long) - 0.1 & x <= max(coords_wisconsin17$long) + 0.1 &
                  y >= min(coords_wisconsin17$lat) - 0.1 & y <= max(coords_wisconsin17$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI18 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin18$long) - 0.1 & x <= max(coords_wisconsin18$long) + 0.1 &
                  y >= min(coords_wisconsin18$lat) - 0.1 & y <= max(coords_wisconsin18$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI19 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin19$long) - 0.1 & x <= max(coords_wisconsin19$long) + 0.1 &
                  y >= min(coords_wisconsin19$lat) - 0.1 & y <= max(coords_wisconsin19$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI20 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin20$long) - 0.1 & x <= max(coords_wisconsin20$long) + 0.1 &
                  y >= min(coords_wisconsin20$lat) - 0.1 & y <= max(coords_wisconsin20$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI21 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin21$long) - 0.1 & x <= max(coords_wisconsin21$long) + 0.1 &
                  y >= min(coords_wisconsin21$lat) - 0.1 & y <= max(coords_wisconsin21$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI22 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin22$long) - 0.1 & x <= max(coords_wisconsin22$long) + 0.1 &
                  y >= min(coords_wisconsin22$lat) - 0.1 & y <= max(coords_wisconsin22$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI23 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin23$long) - 0.1 & x <= max(coords_wisconsin23$long + 0.1) &
                  y >= min(coords_wisconsin23$lat) - 0.1 & y <= max(coords_wisconsin23$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI24 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin24$long) - 0.1 & x <= max(coords_wisconsin24$long) + 0.1 &
                  y >= min(coords_wisconsin24$lat) - 0.1 & y <= max(coords_wisconsin24$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI25 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin25$long) - 0.1 & x <= max(coords_wisconsin25$long) + 0.1 &
                  y >= min(coords_wisconsin25$lat) - 0.1 & y <= max(coords_wisconsin25$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI26 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin26$long) - 0.1 & x <= max(coords_wisconsin26$long) + 0.1 &
                  y >= min(coords_wisconsin26$lat) - 0.1 & y <= max(coords_wisconsin26$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI27 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin27$long) - 0.1 & x <= max(coords_wisconsin27$long) + 0.1 &
                  y >= min(coords_wisconsin27$lat) - 0.1 & y <= max(coords_wisconsin27$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI28 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin28$long) - 0.1 & x <= max(coords_wisconsin28$long) + 0.1 &
                  y >= min(coords_wisconsin28$lat) - 0.1 & y <= max(coords_wisconsin28$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI29 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin29$long) - 0.1 & x <= max(coords_wisconsin29$long) + 0.1 &
                  y >= min(coords_wisconsin29$lat) - 0.1 & y <= max(coords_wisconsin29$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI30 <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin30$long) - 0.1 & x <= max(coords_wisconsin30$long) + 0.1 &
                  y >= min(coords_wisconsin30$lat) - 0.1 & y <= max(coords_wisconsin30$lat) + 0.1) |>
  dplyr::select(y, x)

# Find the distance between each pair of points in the pls (1) and soil (2) dataframes
dists <- fields::rdist(coords_df_WI1, coords_wisconsin1)
# Find the closest soil point to each PLS point
# (should have length = nrow(pls subset))
closest_points <- apply(dists, 2, which.min)
# Immediately delete dists because it's huge
rm(dists)

# Make new dataframe of corresponding soil data to the PLS points
# We'll add to this subsequently
select_df_WI <- df_WI |>
  # Filter as done above to make sure indexing matches
  dplyr::filter(x >= min(coords_wisconsin1$long) - 0.1 & x <= max(coords_wisconsin1$long) + 0.1 &
                  y >= min(coords_wisconsin1$lat) - 0.1 & y <= max(coords_wisconsin1$lat) + 0.1) |>
  # subset by row numbers given by which.min
  dplyr::slice(closest_points)

# Column bind with PLS data
WI_flood_pls <- cbind(select_df_WI, wisconsin1)

# Remove unnecessary objects to keep memory usage lower
rm(closest_points, coords_df_WI1, coords_wisconsin1, select_df_WI, wisconsin1)

# Repeat all these steps for the remaining 29 subsets

# Subset 2
dists <- fields::rdist(coords_df_WI2, coords_wisconsin2)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin2$long) - 0.1 & x <= max(coords_wisconsin2$long) + 0.1 &
                  y >= min(coords_wisconsin2$lat) - 0.1 & y <= max(coords_wisconsin2$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin2)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI2, coords_wisconsin2, select_df_WI, wisconsin2)

# Subset 3
dists <- fields::rdist(coords_df_WI3, coords_wisconsin3)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin3$long) - 0.1 & x <= max(coords_wisconsin3$long) + 0.1 &
                  y >= min(coords_wisconsin3$lat) - 0.1 & y <= max(coords_wisconsin3$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin3)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI3, coords_wisconsin3, select_df_WI, wisconsin3)

# Subset 4
dists <- fields::rdist(coords_df_WI4, coords_wisconsin4)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin4$long) - 0.1 & x <= max(coords_wisconsin4$long) + 0.1 &
                  y >= min(coords_wisconsin4$lat) - 0.1 & y <= max(coords_wisconsin4$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin4)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI4, coords_wisconsin4, select_df_WI, wisconsin4)

# Subset 5
dists <- fields::rdist(coords_df_WI5, coords_wisconsin5)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin5$long) - 0.1 & x <= max(coords_wisconsin5$long) + 0.1 &
                  y >= min(coords_wisconsin5$lat) - 0.1 & y <= max(coords_wisconsin5$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin5)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI5, coords_wisconsin5, select_df_WI, wisconsin5)

# Subset 6
dists <- fields::rdist(coords_df_WI6, coords_wisconsin6)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin6$long) - 0.1 & x <= max(coords_wisconsin6$long) + 0.1 &
                  y >= min(coords_wisconsin6$lat) - 0.1 & y <= max(coords_wisconsin6$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin6)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI6, coords_wisconsin6, select_df_WI, wisconsin6)

# Subset 7
dists <- fields::rdist(coords_df_WI7, coords_wisconsin7)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin7$long) - 0.1 & x <= max(coords_wisconsin7$long) + 0.1 &
                  y >= min(coords_wisconsin7$lat) - 0.1 & y <= max(coords_wisconsin7$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin7)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI7, coords_wisconsin7, select_df_WI, wisconsin7)

# Subset 8
dists <- fields::rdist(coords_df_WI8, coords_wisconsin8)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin8$long) - 0.1 & x <= max(coords_wisconsin8$long) + 0.1 &
                  y >= min(coords_wisconsin8$lat) - 0.1 & y <= max(coords_wisconsin8$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin8)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI8, coords_wisconsin8, select_df_WI, wisconsin8)

# Subset 9
dists <- fields::rdist(coords_df_WI9, coords_wisconsin9)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin9$long) - 0.1 & x <= max(coords_wisconsin9$long) + 0.1 &
                  y >= min(coords_wisconsin9$lat) - 0.1 & y <= max(coords_wisconsin9$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin9)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI9, coords_wisconsin9, select_df_WI, wisconsin9)

# Subset 10
dists <- fields::rdist(coords_df_WI10, coords_wisconsin10)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin10$long) - 0.1 & x <= max(coords_wisconsin10$long) + 0.1 &
                  y >= min(coords_wisconsin10$lat) - 0.1 & y <= max(coords_wisconsin10$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin10)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI10, coords_wisconsin10, select_df_WI, wisconsin10)

# Subset 11
dists <- fields::rdist(coords_df_WI11, coords_wisconsin11)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin11$long) - 0.1 & x <= max(coords_wisconsin11$long) + 0.1 &
                  y >= min(coords_wisconsin11$lat) - 0.1 & y <= max(coords_wisconsin11$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin11)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI11, coords_wisconsin11, select_df_WI, wisconsin11)

# Subset 12
dists <-fields::rdist(coords_df_WI12, coords_wisconsin12)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin12$long) - 0.1 & x <= max(coords_wisconsin12$long) + 0.1 &
                  y >= min(coords_wisconsin12$lat) - 0.1 & y <= max(coords_wisconsin12$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin12)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI12, coords_wisconsin12, select_df_WI, wisconsin12)

# Subset 13
dists <- fields::rdist(coords_df_WI13, coords_wisconsin13)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin13$long) - 0.1 & x <= max(coords_wisconsin13$long) + 0.1 &
                  y >= min(coords_wisconsin13$lat) - 0.1 & y <= max(coords_wisconsin13$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin13)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI13, coords_wisconsin13, select_df_WI, wisconsin13)

# Subset 14
dists <- fields::rdist(coords_df_WI14, coords_wisconsin14)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin14$long) - 0.1 & x <= max(coords_wisconsin14$long) + 0.1 &
                  y >= min(coords_wisconsin14$lat) - 0.1 & y <= max(coords_wisconsin14$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin14)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI14, coords_wisconsin14, select_df_WI, wisconsin14)

# Subset 15
dists <- fields::rdist(coords_df_WI15, coords_wisconsin15)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin15$long) - 0.1 & x <= max(coords_wisconsin15$long) + 0.1 &
                  y >= min(coords_wisconsin15$lat) - 0.1 & y <= max(coords_wisconsin15$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin15)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI15, coords_wisconsin15, select_df_WI, wisconsin15)

# Subset 16
dists <- fields::rdist(coords_df_WI16, coords_wisconsin16)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin16$long) - 0.1 & x <= max(coords_wisconsin16$long) + 0.1 &
                  y >= min(coords_wisconsin16$lat) - 0.1 & y <= max(coords_wisconsin16$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin16)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI16, coords_wisconsin16, select_df_WI, wisconsin16)

# Subset 17
dists <- fields::rdist(coords_df_WI17, coords_wisconsin17)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin17$long) - 0.1 & x <= max(coords_wisconsin17$long) + 0.1 &
                  y >= min(coords_wisconsin17$lat) - 0.1 & y <= max(coords_wisconsin17$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin17)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI17, coords_wisconsin17, select_df_WI, wisconsin17)

# Subset 18
dists <- fields::rdist(coords_df_WI18, coords_wisconsin18)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin18$long) - 0.1 & x <= max(coords_wisconsin18$long) + 0.1 &
                  y >= min(coords_wisconsin18$lat) - 0.1 & y <= max(coords_wisconsin18$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin18)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI18, coords_wisconsin18, select_df_WI, wisconsin18)

# Subset 19
dists <- fields::rdist(coords_df_WI19, coords_wisconsin19)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin19$long) - 0.1 & x <= max(coords_wisconsin19$long) + 0.1 &
                  y >= min(coords_wisconsin19$lat) - 0.1 & y <= max(coords_wisconsin19$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin19)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI19, coords_wisconsin19, select_df_WI, wisconsin19)

# Subset 20
dists <- fields::rdist(coords_df_WI20, coords_wisconsin20)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin20$long) - 0.1 & x <= max(coords_wisconsin20$long) + 0.1 &
                  y >= min(coords_wisconsin20$lat) - 0.1 & y <= max(coords_wisconsin20$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin20)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI20, coords_wisconsin20, select_df_WI, wisconsin20)

# Subset 21
dists <- fields::rdist(coords_df_WI21, coords_wisconsin21)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin21$long) - 0.1 & x <= max(coords_wisconsin21$long) + 0.1 &
                  y >= min(coords_wisconsin21$lat) - 0.1 & y <= max(coords_wisconsin21$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin21)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI21, coords_wisconsin21, select_df_WI, wisconsin21)

# Subset 22
dists <- fields::rdist(coords_df_WI22, coords_wisconsin22)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin22$long) - 0.1 & x <= max(coords_wisconsin22$long) + 0.1 &
                  y >= min(coords_wisconsin22$lat) - 0.1 & y <= max(coords_wisconsin22$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin22)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI22, coords_wisconsin22, select_df_WI, wisconsin22)

# Subset 23
dists <- fields::rdist(coords_df_WI23, coords_wisconsin23)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin23$long) - 0.1 & x <= max(coords_wisconsin23$long) + 0.1 &
                  y >= min(coords_wisconsin23$lat) - 0.1 & y <= max(coords_wisconsin23$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin23)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI23, coords_wisconsin23, select_df_WI, wisconsin23)

# Subset 24
dists <- fields::rdist(coords_df_WI24, coords_wisconsin24)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin24$long) - 0.1 & x <= max(coords_wisconsin24$long) + 0.1 &
                  y >= min(coords_wisconsin24$lat) - 0.1 & y <= max(coords_wisconsin24$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin24)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI24, coords_wisconsin24, select_df_WI, wisconsin24)

# Subset 25
dists <- fields::rdist(coords_df_WI25, coords_wisconsin25)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin25$long) - 0.1 & x <= max(coords_wisconsin25$long) + 0.1 &
                  y >= min(coords_wisconsin25$lat) - 0.1 & y <= max(coords_wisconsin25$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin25)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI25, coords_wisconsin25, select_df_WI, wisconsin25)

# Subset 26
dists <- fields::rdist(coords_df_WI26, coords_wisconsin26)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin26$long) - 0.1 & x <= max(coords_wisconsin26$long) + 0.1 &
                  y >= min(coords_wisconsin26$lat) - 0.1 & y <= max(coords_wisconsin26$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin26)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI26, coords_wisconsin26, select_df_WI, wisconsin26)

# Subset 27
dists <- fields::rdist(coords_df_WI27, coords_wisconsin27)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin27$long) - 0.1 & x <= max(coords_wisconsin27$long) + 0.1 &
                  y >= min(coords_wisconsin27$lat) - 0.1 & y <= max(coords_wisconsin27$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin27)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI27, coords_wisconsin27, select_df_WI, wisconsin27)

# Subset 28
dists <- fields::rdist(coords_df_WI28, coords_wisconsin28)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin28$long) - 0.1 & x <= max(coords_wisconsin28$long) + 0.1 &
                  y >= min(coords_wisconsin28$lat) - 0.1 & y <= max(coords_wisconsin28$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin28)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI28, coords_wisconsin28, select_df_WI, wisconsin28)

# Subset 29
dists <- fields::rdist(coords_df_WI29, coords_wisconsin29)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin29$long) - 0.1 & x <= max(coords_wisconsin29$long) + 0.1 &
                  y >= min(coords_wisconsin29$lat) - 0.1 & y <= max(coords_wisconsin29$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin29)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI29, coords_wisconsin29, select_df_WI, wisconsin29)

# Subset 30
dists <- fields::rdist(coords_df_WI30, coords_wisconsin30)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI |>
  dplyr::filter(x >= min(coords_wisconsin30$long) - 0.1 & x <= max(coords_wisconsin30$long) + 0.1 &
                  y >= min(coords_wisconsin30$lat) - 0.1 & y <= max(coords_wisconsin30$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin30)
WI_flood_pls <- rbind(WI_flood_pls, temp)

rm(closest_points, coords_df_WI30, coords_wisconsin30, select_df_WI, wisconsin30)

# Update column names
colnames(WI_flood_pls) <- c('soil_x', 'soil_y', 'geomdesc', 'Floodplain',
                            'state', 'pls_x', 'pls_y', 
                           'L1_tree1', 'L1_tree2', 'L1_tree3', 'L1_tree4', 'uniqueID')

# Save WI floodplain. Confirmed in same order as wisconsin and wisconsin_ecosystem
WI_flood <- dplyr::select(WI_flood_pls, soil_x, soil_y, Floodplain, pls_x, pls_y)
save(WI_flood, file = 'data/processed/soils/processed_flood_wi.RData')

#### CHECKS ####

# Check to make sure we're not too far off of the 1:1 line with any of our coordinate matching

rm(list = ls())

load('data/processed/soils/processed_flood_il.RData')

IL_flood |>
  ggplot2::ggplot(ggplot2::aes(x = pls_x, y = soil_x)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()
IL_flood |>
  ggplot2::ggplot(ggplot2::aes(x = pls_y, y = soil_y)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

IL_flood |>
  dplyr::mutate(deviance_x = abs(pls_x - soil_x),
                deviance_y = abs(pls_y - soil_y)) |>
  dplyr::summarize(mean_deviance_x = mean(deviance_x),
                   mean_deviance_y = mean(deviance_y),
                   max_deviance_x = max(deviance_x),
                   max_deviance_y = max(deviance_y),
                   med_deviance_x = median(deviance_x),
                   med_deviance_y = median(deviance_y))

load('data/processed/soils/processed_flood_in.RData')

IN_flood |>
  ggplot2::ggplot(ggplot2::aes(x = pls_x, y = soil_x)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

IN_flood |>
  ggplot2::ggplot(ggplot2::aes(x = pls_y, y = soil_y)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

IN_flood |>
  dplyr::mutate(deviance_x = abs(pls_x - soil_x),
                deviance_y = abs(pls_y - soil_y)) |>
  dplyr::summarize(mean_deviance_x = mean(deviance_x),
                   mean_deviance_y = mean(deviance_y),
                   max_deviance_x = max(deviance_x),
                   max_deviance_y = max(deviance_y),
                   med_deviance_x = median(deviance_x),
                   med_deviance_y = median(deviance_y))

load('data/processed/soils/processed_flood_mi.RData')

MI_flood |>
  ggplot2::ggplot(ggplot2::aes(x = pls_x, y = soil_x)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

MI_flood |>
  ggplot2::ggplot(ggplot2::aes(x = pls_y, y = soil_y)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

MI_flood |>
  dplyr::mutate(deviance_x = abs(pls_x - soil_x),
                deviance_y = abs(pls_y - soil_y)) |>
  dplyr::summarize(mean_deviance_x = mean(deviance_x),
                   mean_deviance_y = mean(deviance_y),
                   max_deviance_x = max(deviance_x),
                   max_deviance_y = max(deviance_y),
                   med_deviance_x = median(deviance_x),
                   med_deviance_y = median(deviance_y))

load('data/processed/soils/processed_flood_mn.RData')

MN_flood |>
  ggplot2::ggplot(ggplot2::aes(x = pls_x, y = soil_x)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

MN_flood |>
  ggplot2::ggplot(ggplot2::aes(x = pls_y, y = soil_y)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

MN_flood |>
  dplyr::mutate(deviance_x = abs(pls_x - soil_x),
                deviance_y = abs(pls_y - soil_y)) |>
  dplyr::summarize(mean_deviance_x = mean(deviance_x),
                   mean_deviance_y = mean(deviance_y),
                   max_deviance_x = max(deviance_x),
                   max_deviance_y = max(deviance_y),
                   med_deviance_x = median(deviance_x),
                   med_deviance_y = median(deviance_y))

load('data/processed/soils/processed_flood_wi.RData')

WI_flood |>
  ggplot2::ggplot(ggplot2::aes(x = pls_x, y = soil_x)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

WI_flood |>
  ggplot2::ggplot(ggplot2::aes(x = pls_y, y = soil_y)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

WI_flood |>
  dplyr::mutate(deviance_x = abs(pls_x - soil_x),
                deviance_y = abs(pls_y - soil_y)) |>
  dplyr::summarize(mean_deviance_x = mean(deviance_x),
                   mean_deviance_y = mean(deviance_y),
                   max_deviance_x = max(deviance_x),
                   max_deviance_y = max(deviance_y),
                   med_deviance_x = median(deviance_x),
                   med_deviance_y = median(deviance_y))

# Plot high deviance
states <- sf::st_as_sf(maps::map('state', region = 'illinois',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

IL_flood |>
  dplyr::mutate(deviance_x = abs(pls_x - soil_x),
                deviance_y = abs(pls_y - soil_y),
                flag = 'none',
                flag = dplyr::if_else(deviance_x > 0.02, 'x', flag),
                flag = dplyr::if_else(deviance_y > 0.02, 'y', flag),
                flag = dplyr::if_else(deviance_x > 0.02 &
                                        deviance_y > 0.02, 'both', flag)) |>
  dplyr::filter(flag != 'none') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = flag)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

states <- sf::st_as_sf(maps::map('state', region = 'indiana',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

IN_flood |>
  dplyr::mutate(deviance_x = abs(pls_x - soil_x),
                deviance_y = abs(pls_y - soil_y),
                flag = 'none',
                flag = dplyr::if_else(deviance_x > 0.02, 'x', flag),
                flag = dplyr::if_else(deviance_y > 0.02, 'y', flag),
                flag = dplyr::if_else(deviance_x > 0.02 &
                                        deviance_y > 0.02, 'both', flag)) |>
  dplyr::filter(flag != 'none') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, pls_y, color = flag)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

states <- sf::st_as_sf(maps::map('state', region = 'michigan',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

MI_flood |>
  dplyr::mutate(deviance_x = abs(pls_x - soil_x),
                deviance_y = abs(pls_y - soil_y),
                flag = 'none',
                flag = dplyr::if_else(deviance_x > 0.02, 'x', flag),
                flag = dplyr::if_else(deviance_y > 0.02, 'y', flag),
                flag = dplyr::if_else(deviance_x > 0.02 &
                                        deviance_y > 0.02, 'both', flag)) |>
  dplyr::filter(flag != 'none') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, pls_y, color = flag)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

states <- sf::st_as_sf(maps::map('state', region = 'minnesota',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

MN_flood |>
  dplyr::mutate(deviance_x = abs(pls_x - soil_x),
                deviance_y = abs(pls_y - soil_y),
                flag = 'none',
                flag = dplyr::if_else(deviance_x > 0.02, 'x', flag),
                flag = dplyr::if_else(deviance_y > 0.02, 'y', flag),
                flag = dplyr::if_else(deviance_x > 0.02 &
                                        deviance_y > 0.02, 'both', flag)) |>
  dplyr::filter(flag != 'none') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, pls_y, color = flag)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

states <- sf::st_as_sf(maps::map('state', region = 'wisconsin',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

WI_flood |>
  dplyr::mutate(deviance_x = abs(pls_x - soil_x),
                deviance_y = abs(pls_y - soil_y),
                flag = 'none',
                flag = dplyr::if_else(deviance_x > 0.02, 'x', flag),
                flag = dplyr::if_else(deviance_y > 0.02, 'y', flag),
                flag = dplyr::if_else(deviance_x > 0.02 &
                                        deviance_y > 0.02, 'both', flag)) |>
  dplyr::filter(flag != 'none') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, pls_y, color = flag)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

# Plot distribution of soil variables
states <- sf::st_as_sf(maps::map('state', region = c('indiana', 'illinois', 'michigan',
                                                     'minnesota', 'wisconsin'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

flood <- rbind(IL_flood, IN_flood, MI_flood, MN_flood, WI_flood)

flood |>
  dplyr::filter(Floodplain == 'Yes') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y), alpha = 0.7, shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

