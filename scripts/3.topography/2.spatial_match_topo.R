## Matching topography and PLS spatially

rm(list = ls())

#### ILLINOIS ####
load('data/raw/topography/il_topo.RData')

load('data/processed/PLS/illinois_format.RData')

# Remove negative points because we know land in illinois is not below sea level
il_topo <- dplyr::filter(il_topo, elevation >= 0)

# Format illinois data
illinois <- illinois |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species') |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Divide pls illinois into 20 sections
illinois1 <- illinois[1:2641,]
illinois2 <- illinois[2642:5283,]
illinois3 <- illinois[5284:7925,]
illinois4 <- illinois[7926:10567,]
illinois5 <- illinois[10568:13209,]
illinois6 <- illinois[13210:15851,]
illinois7 <- illinois[15852:18493,]
illinois8 <- illinois[18494:21135,]
illinois9 <- illinois[21136:23777,]
illinois10 <- illinois[23778:26419,]
illinois11 <- illinois[26420:29061,]
illinois12 <- illinois[29062:31703,]
illinois13 <- illinois[31704:34345,]
illinois14 <- illinois[34346:36987,]
illinois15 <- illinois[36988:39629,]
illinois16 <- illinois[39630:42271,]
illinois17 <- illinois[42272:44913,]
illinois18 <- illinois[44914:47555,]
illinois19 <- illinois[47556:50197,]
illinois20 <- illinois[50198:52839,]
illinois21 <- illinois[52840:55481,]
illinois22 <- illinois[55482:58123,]
illinois23 <- illinois[58124:60765,]
illinois24 <- illinois[60766:63407,]
illinois25 <- illinois[63408:66049,]
illinois26 <- illinois[66050:68691,]
illinois27 <- illinois[68692:71333,]
illinois28 <- illinois[71334:73975,]
illinois29 <- illinois[73976:76617,]
illinois30 <- illinois[76618:79232,]

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
coords_illinois21 <- dplyr::select(illinois21, y, x)
coords_illinois22 <- dplyr::select(illinois22, y, x)
coords_illinois23 <- dplyr::select(illinois23, y, x)
coords_illinois24 <- dplyr::select(illinois24, y, x)
coords_illinois25 <- dplyr::select(illinois25, y, x)
coords_illinois26 <- dplyr::select(illinois26, y, x)
coords_illinois27 <- dplyr::select(illinois27, y, x)
coords_illinois28 <- dplyr::select(illinois28, y, x)
coords_illinois29 <- dplyr::select(illinois29, y, x)
coords_illinois30 <- dplyr::select(illinois30, y, x)

# Add column names
colnames(coords_illinois1) <- colnames(coords_illinois2) <- colnames(coords_illinois3) <-
  colnames(coords_illinois4) <- colnames(coords_illinois5) <- colnames(coords_illinois6) <-
  colnames(coords_illinois7) <- colnames(coords_illinois8) <- colnames(coords_illinois9) <- 
  colnames(coords_illinois10) <- colnames(coords_illinois11) <- colnames(coords_illinois12) <-
  colnames(coords_illinois13) <- colnames(coords_illinois14) <- colnames(coords_illinois15) <-
  colnames(coords_illinois16) <- colnames(coords_illinois17) <- colnames(coords_illinois18) <-
  colnames(coords_illinois19) <- colnames(coords_illinois20) <- colnames(coords_illinois21) <- 
  colnames(coords_illinois22) <- colnames(coords_illinois23) <- colnames(coords_illinois24) <-
  colnames(coords_illinois25) <- colnames(coords_illinois26) <- colnames(coords_illinois27) <-
  colnames(coords_illinois28) <- colnames(coords_illinois29) <- colnames(coords_illinois30) <-
  c('lat', 'long')

# Subset the topography data to the lat/long extent of each pls subsection
coords_df_IL1 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois1$long) - 0.1 & x <= max(coords_illinois1$long) + 0.1 &
                  y >= min(coords_illinois1$lat) - 0.1 & y <= max(coords_illinois1$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL2 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois2$long) - 0.1 & x <= max(coords_illinois2$long) + 0.1 &
                  y >= min(coords_illinois2$lat) - 0.1 & y <= max(coords_illinois2$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL3 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois3$long) - 0.1 & x <= max(coords_illinois3$long) + 0.1 &
                  y >= min(coords_illinois3$lat) - 0.1 & y <= max(coords_illinois3$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL4 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois4$long) - 0.1 & x <= max(coords_illinois4$long) + 0.1 &
                  y >= min(coords_illinois4$lat) - 0.1 & y <= max(coords_illinois4$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL5 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois5$long) - 0.1 & x <= max(coords_illinois5$long) + 0.1 &
                  y >= min(coords_illinois5$lat) - 0.1 & y <= max(coords_illinois5$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL6 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois6$long) - 0.1 & x <= max(coords_illinois6$long) + 0.1 &
                  y >= min(coords_illinois6$lat) - 0.1 & y <= max(coords_illinois6$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL7 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois7$long) - 0.1 & x <= max(coords_illinois7$long) + 0.1 &
                  y >= min(coords_illinois7$lat) - 0.1 & y <= max(coords_illinois7$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL8 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois8$long) - 0.1 & x <= max(coords_illinois8$long) + 0.1 &
                  y >= min(coords_illinois8$lat) - 0.1 & y <= max(coords_illinois8$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL9 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois9$long) - 0.1 & x <= max(coords_illinois9$long) + 0.1 &
                  y >= min(coords_illinois9$lat) - 0.1 & y <= max(coords_illinois9$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL10 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois10$long) - 0.1 & x <= max(coords_illinois10$long) + 0.1 &
                  y >= min(coords_illinois10$lat) - 0.1 & y <= max(coords_illinois10$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL11 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois11$long) - 0.1 & x <= max(coords_illinois11$long) + 0.1 &
                  y >= min(coords_illinois11$lat) - 0.1 & y <= max(coords_illinois11$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL12 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois12$long) - 0.1 & x <= max(coords_illinois12$long) + 0.1 &
                  y >= min(coords_illinois12$lat) - 0.1 & y <= max(coords_illinois12$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL13 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois13$long) - 0.1 & x <= max(coords_illinois13$long) + 0.1 &
                  y >= min(coords_illinois13$lat) - 0.1 & y <= max(coords_illinois13$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL14 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois14$long) - 0.1 & x <= max(coords_illinois14$long) + 0.1 &
                  y >= min(coords_illinois14$lat) - 0.1 & y <= max(coords_illinois14$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL15 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois15$long) - 0.1 & x <= max(coords_illinois15$long) + 0.1 &
                  y >= min(coords_illinois15$lat) - 0.1 & y <= max(coords_illinois15$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL16 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois16$long) - 0.1 & x <= max(coords_illinois16$long) + 0.1 &
                  y >= min(coords_illinois16$lat) - 0.1 & y <= max(coords_illinois16$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL17 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois17$long) - 0.1 & x <= max(coords_illinois17$long) + 0.1 &
                  y >= min(coords_illinois17$lat) - 0.1 & y <= max(coords_illinois17$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL18 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois18$long) - 0.1 & x <= max(coords_illinois18$long) + 0.1 &
                  y >= min(coords_illinois18$lat) - 0.1 & y <= max(coords_illinois18$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL19 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois19$long) - 0.1 & x <= max(coords_illinois19$long) + 0.1 &
                  y >= min(coords_illinois19$lat) - 0.1 & y <= max(coords_illinois19$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL20 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois20$long) - 0.1 & x <= max(coords_illinois20$long) + 0.1 &
                  y >= min(coords_illinois20$lat) - 0.1 & y <= max(coords_illinois20$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL21 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois21$long) - 0.1 & x <= max(coords_illinois21$long) + 0.1 &
                  y >= min(coords_illinois21$lat) - 0.1 & y <= max(coords_illinois21$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL22 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois22$long) - 0.1 & x <= max(coords_illinois22$long) + 0.1 &
                  y >= min(coords_illinois22$lat) - 0.1 & y <= max(coords_illinois22$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL23 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois23$long) - 0.1 & x <= max(coords_illinois23$long) + 0.1 &
                  y >= min(coords_illinois23$lat) - 0.1 & y <= max(coords_illinois23$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL24 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois24$long) - 0.1 & x <= max(coords_illinois24$long) + 0.1 &
                  y >= min(coords_illinois24$lat) - 0.1 & y <= max(coords_illinois24$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL25 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois25$long) - 0.1 & x <= max(coords_illinois25$long) + 0.1 &
                  y >= min(coords_illinois25$lat) - 0.1 & y <= max(coords_illinois25$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL26 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois26$long) - 0.1 & x <= max(coords_illinois26$long) + 0.1 &
                  y >= min(coords_illinois26$lat) - 0.1 & y <= max(coords_illinois26$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL27 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois27$long) - 0.1 & x <= max(coords_illinois27$long) + 0.1 &
                  y >= min(coords_illinois27$lat) - 0.1 & y <= max(coords_illinois27$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL28 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois28$long) - 0.1 & x <= max(coords_illinois28$long) + 0.1 &
                  y >= min(coords_illinois28$lat) - 0.1 & y <= max(coords_illinois28$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL29 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois29$long) - 0.1 & x <= max(coords_illinois29$long) + 0.1 &
                  y >= min(coords_illinois29$lat) - 0.1 & y <= max(coords_illinois29$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL30 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois30$long) - 0.1 & x <= max(coords_illinois30$long) + 0.1 &
                  y >= min(coords_illinois30$lat) - 0.1 & y <= max(coords_illinois30$lat) + 0.1) |>
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
select_df_IL <- il_topo |>
  # Filter as done above to make sure indexing matches
  dplyr::filter(x >= min(coords_illinois1$long) - 0.1 & x <= max(coords_illinois1$long) + 0.1 &
                  y >= min(coords_illinois1$lat) - 0.1 & y <= max(coords_illinois1$lat) + 0.1) |>
  # subset by row numbers given by which.min
  dplyr::slice(closest_points)

# Column bind with PLS data
IL_topo_pls <- cbind(select_df_IL, illinois1)

# Remove unnecessary objects to keep memory usage lower
rm(closest_points, coords_df_IL1, coords_illinois1, select_df_IL, illinois1)

# Repeat all these steps for the remaining 29 subsets

# Subset 2
dists <- fields::rdist(coords_df_IL2, coords_illinois2)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois2$long) - 0.1 & x <= max(coords_illinois2$long) + 0.1 &
                  y >= min(coords_illinois2$lat) - 0.1 & y <= max(coords_illinois2$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois2)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL2, coords_illinois2, select_df_IL, illinois2)

# Subset 3
dists <- fields::rdist(coords_df_IL3, coords_illinois3)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois3$long) - 0.1 & x <= max(coords_illinois3$long) + 0.1 &
                  y >= min(coords_illinois3$lat) - 0.1 & y <= max(coords_illinois3$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois3)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL3, coords_illinois3, select_df_IL, illinois3)

# Subset 4
dists <- fields::rdist(coords_df_IL4, coords_illinois4)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois4$long) - 0.1 & x <= max(coords_illinois4$long) + 0.1 &
                  y >= min(coords_illinois4$lat) - 0.1 & y <= max(coords_illinois4$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois4)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL4, coords_illinois4, select_df_IL, illinois4)

# Subset 5
dists <- fields::rdist(coords_df_IL5, coords_illinois5)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois5$long) - 0.1 & x <= max(coords_illinois5$long) + 0.1 &
                  y >= min(coords_illinois5$lat) - 0.1 & y <= max(coords_illinois5$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois5)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL5, coords_illinois5, select_df_IL, illinois5)

# Subset 6
dists <- fields::rdist(coords_df_IL6, coords_illinois6)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois6$long) - 0.1 & x <= max(coords_illinois6$long) + 0.1 &
                  y >= min(coords_illinois6$lat) - 0.1 & y <= max(coords_illinois6$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois6)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL6, coords_illinois6, select_df_IL, illinois6)

# Subset 7
dists <- fields::rdist(coords_df_IL7, coords_illinois7)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois7$long) - 0.1 & x <= max(coords_illinois7$long) + 0.1 &
                  y >= min(coords_illinois7$lat) - 0.1 & y <= max(coords_illinois7$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois7)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL7, coords_illinois7, select_df_IL, illinois7)

# Subset 8
dists <- fields::rdist(coords_df_IL8, coords_illinois8)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois8$long) - 0.1 & x <= max(coords_illinois8$long) + 0.1 &
                  y >= min(coords_illinois8$lat) - 0.1 & y <= max(coords_illinois8$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois8)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL8, coords_illinois8, select_df_IL, illinois8)

# Subset 9
dists <- fields::rdist(coords_df_IL9, coords_illinois9)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois9$long) - 0.1 & x <= max(coords_illinois9$long) + 0.1 &
                  y >= min(coords_illinois9$lat) - 0.1 & y <= max(coords_illinois9$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois9)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL9, coords_illinois9, select_df_IL, illinois9)

# Subset 10
dists <- fields::rdist(coords_df_IL10, coords_illinois10)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois10$long) - 0.1 & x <= max(coords_illinois10$long) + 0.1 &
                  y >= min(coords_illinois10$lat) - 0.1 & y <= max(coords_illinois10$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois10)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL10, coords_illinois10, select_df_IL, illinois10)

# Subset 11
dists <- fields::rdist(coords_df_IL11, coords_illinois11)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois11$long) - 0.1 & x <= max(coords_illinois11$long) + 0.1 &
                  y >= min(coords_illinois11$lat) - 0.1 & y <= max(coords_illinois11$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois11)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL11, coords_illinois11, select_df_IL, illinois11)

# Subset 12
dists <- fields::rdist(coords_df_IL12, coords_illinois12)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois12$long) - 0.1 & x <= max(coords_illinois12$long) + 0.1 &
                  y >= min(coords_illinois12$lat) - 0.1 & y <= max(coords_illinois12$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois12)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL12, coords_illinois12, select_df_IL, illinois12)

# Subset 13
dists <- fields::rdist(coords_df_IL13, coords_illinois13)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois13$long) - 0.1 & x <= max(coords_illinois13$long) + 0.1 &
                  y >= min(coords_illinois13$lat) - 0.1 & y <= max(coords_illinois13$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois13)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL13, coords_illinois13, select_df_IL, illinois13)

# Subset 14
dists <- fields::rdist(coords_df_IL14, coords_illinois14)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois14$long) - 0.1 & x <= max(coords_illinois14$long) + 0.1 &
                  y >= min(coords_illinois14$lat) - 0.1 & y <= max(coords_illinois14$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois14)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL14, coords_illinois14, select_df_IL, illinois14)

# Subset 15
dists <- fields::rdist(coords_df_IL15, coords_illinois15)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois15$long) - 0.1 & x <= max(coords_illinois15$long) + 0.1 &
                  y >= min(coords_illinois15$lat) - 0.1 & y <= max(coords_illinois15$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois15)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL15, coords_illinois15, select_df_IL, illinois15)

# Subset 16
dists <- fields::rdist(coords_df_IL16, coords_illinois16)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois16$long) - 0.1 & x <= max(coords_illinois16$long) + 0.1 &
                  y >= min(coords_illinois16$lat) - 0.1 & y <= max(coords_illinois16$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois16)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL16, coords_illinois16, select_df_IL, illinois16)

# Subset 17
dists <- fields::rdist(coords_df_IL17, coords_illinois17)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois17$long) - 0.1 & x <= max(coords_illinois17$long) + 0.1 &
                  y >= min(coords_illinois17$lat) - 0.1 & y <= max(coords_illinois17$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois17)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL17, coords_illinois17, select_df_IL, illinois17)

# Subset 18
dists <- fields::rdist(coords_df_IL18, coords_illinois18)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois18$long) - 0.1 & x <= max(coords_illinois18$long) + 0.1 &
                  y >= min(coords_illinois18$lat) - 0.1 & y <= max(coords_illinois18$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois18)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL18, coords_illinois18, select_df_IL, illinois18)

# Subset 19
dists <- fields::rdist(coords_df_IL19, coords_illinois19)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois19$long) - 0.1 & x <= max(coords_illinois19$long) + 0.1 &
                  y >= min(coords_illinois19$lat) - 0.1 & y <= max(coords_illinois19$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois19)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL19, coords_illinois19, select_df_IL, illinois19)

# Subset 20
dists <- fields::rdist(coords_df_IL20, coords_illinois20)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois20$long) - 0.1 & x <= max(coords_illinois20$long) + 0.1 &
                  y >= min(coords_illinois20$lat) - 0.1 & y <= max(coords_illinois20$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois20)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL20, coords_illinois20, select_df_IL, illinois20)

# Subset 21
dists <- fields::rdist(coords_df_IL21, coords_illinois21)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois21$long) - 0.1 & x <= max(coords_illinois21$long) + 0.1 &
                  y >= min(coords_illinois21$lat) - 0.1 & y <= max(coords_illinois21$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois21)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL21, coords_illinois21, select_df_IL, illinois21)

# Subset 22
dists <- fields::rdist(coords_df_IL22, coords_illinois22)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois22$long) - 0.1 & x <= max(coords_illinois22$long) + 0.1 &
                  y >= min(coords_illinois22$lat) - 0.1 & y <= max(coords_illinois22$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois22)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL22, coords_illinois22, select_df_IL, illinois22)

# Subset 23
dists <- fields::rdist(coords_df_IL23, coords_illinois23)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois23$long) - 0.1 & x <= max(coords_illinois23$long) + 0.1 &
                  y >= min(coords_illinois23$lat) - 0.1 & y <= max(coords_illinois23$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois23)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL23, coords_illinois23, select_df_IL, illinois23)

# Subset 24
dists <- fields::rdist(coords_df_IL24, coords_illinois24)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois24$long) - 0.1 & x <= max(coords_illinois24$long) + 0.1 &
                  y >= min(coords_illinois24$lat) - 0.1 & y <= max(coords_illinois24$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois24)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL24, coords_illinois24, select_df_IL, illinois24)

# Subset 25
dists <- fields::rdist(coords_df_IL25, coords_illinois25)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois25$long) - 0.1 & x <= max(coords_illinois25$long) + 0.1 &
                  y >= min(coords_illinois25$lat) - 0.1 & y <= max(coords_illinois25$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois25)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL25, coords_illinois25, select_df_IL, illinois25)

# Subset 26
dists <- fields::rdist(coords_df_IL26, coords_illinois26)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois26$long) - 0.1 & x <= max(coords_illinois26$long) + 0.1 &
                  y >= min(coords_illinois26$lat) - 0.1 & y <= max(coords_illinois26$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois26)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL26, coords_illinois26, select_df_IL, illinois26)

# Subset 27
dists <- fields::rdist(coords_df_IL27, coords_illinois27)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois27$long) - 0.1 & x <= max(coords_illinois27$long) + 0.1 &
                  y >= min(coords_illinois27$lat) - 0.1 & y <= max(coords_illinois27$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <-cbind(select_df_IL, illinois27)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL27, coords_illinois27, select_df_IL, illinois27)

# Subset 28
dists <- fields::rdist(coords_df_IL28, coords_illinois28)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois28$long) - 0.1 & x <= max(coords_illinois28$long) + 0.1 &
                  y >= min(coords_illinois28$lat) - 0.1 & y <= max(coords_illinois28$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois28)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL28, coords_illinois28, select_df_IL, illinois28)

# Subset 29
dists <- fields::rdist(coords_df_IL29, coords_illinois29)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois29$long) - 0.1 & x <= max(coords_illinois29$long) + 0.1 &
                  y >= min(coords_illinois29$lat) - 0.1 & y <= max(coords_illinois29$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois29)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL29, coords_illinois29, select_df_IL, illinois29)

# Subset 30
dists <- fields::rdist(coords_df_IL30, coords_illinois30)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois30$long) - 0.1 & x <= max(coords_illinois30$long) + 0.1 &
                  y >= min(coords_illinois30$lat) - 0.1 & y <= max(coords_illinois30$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois30)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL30, coords_illinois30, select_df_IL, illinois30)

# Update column names
colnames(IL_topo_pls) <- c('topo_x', 'topo_y', 'elevation', 'slope', 
                           'aspect', 'state', 'pls_x', 'pls_y', 
                           'L1_tree1', 'L1_tree2', 'L1_tree3', 'L1_tree4', 
                           'uniqueID')

# Save IL topography. Confirmed in same order as illinois and illinois ecosystem
IL_topo <- dplyr::select(IL_topo_pls, topo_x, topo_y, elevation, slope, aspect, pls_x, pls_y)
save(IL_topo, file = 'data/processed/topography/processed_topo_il.RData')

#### INDIANA ####
rm(list = ls())

load('data/raw/topography/in_topo.RData')

load('data/processed/PLS/indiana_format.RData')

# Remove negative points because we know land in indiana is not below sea level
in_topo <- dplyr::filter(in_topo, elevation >= 0)

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
coords_df_IN1 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana1$long) - 0.1 & x <= max(coords_indiana1$long) + 0.1 &
                  y >= min(coords_indiana1$lat) - 0.1 & y <= max(coords_indiana1$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN2 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana2$long) - 0.1 & x <= max(coords_indiana2$long) + 0.1 &
                  y >= min(coords_indiana2$lat) - 0.1 & y <= max(coords_indiana2$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN3 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana3$long) - 0.1 & x <= max(coords_indiana3$long) + 0.1 &
                  y >= min(coords_indiana3$lat) - 0.1 & y <= max(coords_indiana3$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN4 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana4$long) - 0.1 & x <= max(coords_indiana4$long) + 0.1 &
                  y >= min(coords_indiana4$lat) - 0.1 & y <= max(coords_indiana4$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN5 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana5$long) - 0.1 & x <= max(coords_indiana5$long) + 0.1 &
                  y >= min(coords_indiana5$lat) - 0.1 & y <= max(coords_indiana5$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN6 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana6$long) - 0.1 & x <= max(coords_indiana6$long) + 0.1 &
                  y >= min(coords_indiana6$lat) - 0.1 & y <= max(coords_indiana6$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN7 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana7$long) - 0.1 & x <= max(coords_indiana7$long) + 0.1 &
                  y >= min(coords_indiana7$lat) - 0.1 & y <= max(coords_indiana7$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN8 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana8$long) - 0.1 & x <= max(coords_indiana8$long) + 0.1 &
                  y >= min(coords_indiana8$lat) - 0.1 & y <= max(coords_indiana8$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN9 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana9$long) - 0.1 & x <= max(coords_indiana9$long) + 0.1 &
                  y >= min(coords_indiana9$lat) - 0.1 & y <= max(coords_indiana9$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN10 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana10$long) - 0.1 & x <= max(coords_indiana10$long) + 0.1 &
                  y >= min(coords_indiana10$lat) - 0.1 & y <= max(coords_indiana10$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN11 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana11$long) - 0.1 & x <= max(coords_indiana11$long) + 0.1 &
                  y >= min(coords_indiana11$lat) - 0.1 & y <= max(coords_indiana11$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN12 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana12$long) - 0.1 & x <= max(coords_indiana12$long) + 0.1 &
                  y >= min(coords_indiana12$lat) - 0.1 & y <= max(coords_indiana12$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN13 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana13$long) - 0.1 & x <= max(coords_indiana13$long) + 0.1 &
                  y >= min(coords_indiana13$lat) - 0.1 & y <= max(coords_indiana13$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN14 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana14$long) - 0.1 & x <= max(coords_indiana14$long) + 0.1 &
                  y >= min(coords_indiana14$lat) - 0.1 & y <= max(coords_indiana14$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN15 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana15$long) - 0.1 & x <= max(coords_indiana15$long) + 0.1 &
                  y >= min(coords_indiana15$lat) - 0.1 & y <= max(coords_indiana15$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN16 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana16$long) - 0.1 & x <= max(coords_indiana16$long) + 0.1 &
                  y >= min(coords_indiana16$lat) - 0.1 & y <= max(coords_indiana16$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN17 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana17$long) - 0.1 & x <= max(coords_indiana17$long) + 0.1 &
                  y >= min(coords_indiana17$lat) - 0.1 & y <= max(coords_indiana17$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN18 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana18$long) - 0.1 & x <= max(coords_indiana18$long) + 0.1 &
                  y >= min(coords_indiana18$lat) - 0.1 & y <= max(coords_indiana18$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN19 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana19$long) - 0.1 & x <= max(coords_indiana19$long) + 0.1 &
                  y >= min(coords_indiana19$lat) - 0.1 & y <= max(coords_indiana19$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN20 <- in_topo |>
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
select_df_IN <- in_topo |>
  # Filter as done above to make sure indexing matches
  dplyr::filter(x >= min(coords_indiana1$long) - 0.1 & x <= max(coords_indiana1$long) + 0.1 &
                  y >= min(coords_indiana1$lat) - 0.1 & y <= max(coords_indiana1$lat) + 0.1) |>
  # subset by row numbers given by which.min
  dplyr::slice(closest_points)

# Column bind with PLS data
IN_topo_pls <- cbind(select_df_IN, indiana1)

# Remove unnecessary objects to keep memory usage lower
rm(closest_points, coords_df_IN1, coords_indiana1, select_df_IN, indiana1)

# Repeat all these steps for the remaining 19 subsets

# Subset 2
dists <- fields::rdist(coords_df_IN2, coords_indiana2)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana2$long) - 0.1 & x <= max(coords_indiana2$long) + 0.1 &
                  y >= min(coords_indiana2$lat) - 0.1 & y <= max(coords_indiana2$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana2)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN2, coords_indiana2, select_df_IN, indiana2)

# Subset 3
dists <- fields::rdist(coords_df_IN3, coords_indiana3)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana3$long) - 0.1 & x <= max(coords_indiana3$long) + 0.1 &
                  y >= min(coords_indiana3$lat) - 0.1 & y <= max(coords_indiana3$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana3)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN3, coords_indiana3, select_df_IN, indiana3)

# Subset 4
dists <- fields::rdist(coords_df_IN4, coords_indiana4)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana4$long) - 0.1 & x <= max(coords_indiana4$long) + 0.1 &
                  y >= min(coords_indiana4$lat) - 0.1 & y <= max(coords_indiana4$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana4)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN4, coords_indiana4, select_df_IN, indiana4)

# Subset 5
dists <- fields::rdist(coords_df_IN5, coords_indiana5)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana5$long) - 0.1 & x <= max(coords_indiana5$long) + 0.1 &
                  y >= min(coords_indiana5$lat) - 0.1 & y <= max(coords_indiana5$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana5)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN5, coords_indiana5, select_df_IN, indiana5)

# Subset 6
dists <- fields::rdist(coords_df_IN6, coords_indiana6)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana6$long) - 0.1 & x <= max(coords_indiana6$long) + 0.1 &
                  y >= min(coords_indiana6$lat) - 0.1 & y <= max(coords_indiana6$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana6)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN6, coords_indiana6, select_df_IN, indiana6)

# Subset 7
dists <- fields::rdist(coords_df_IN7, coords_indiana7)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana7$long) - 0.1 & x <= max(coords_indiana7$long) + 0.1 &
                  y >= min(coords_indiana7$lat) - 0.1 & y <= max(coords_indiana7$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana7)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN7, coords_indiana7, select_df_IN, indiana7)

# Subset 8
dists <- fields::rdist(coords_df_IN8, coords_indiana8)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana8$long) - 0.1 & x <= max(coords_indiana8$long) + 0.1 &
                  y >= min(coords_indiana8$lat) - 0.1 & y <= max(coords_indiana8$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana8)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN8, coords_indiana8, select_df_IN, indiana8)

# Subset 9
dists <- fields::rdist(coords_df_IN9, coords_indiana9)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana9$long) - 0.1 & x <= max(coords_indiana9$long) + 0.1 &
                  y >= min(coords_indiana9$lat) - 0.1 & y <= max(coords_indiana9$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana9)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN9, coords_indiana9, select_df_IN, indiana9)

# Subset 10
dists <- fields::rdist(coords_df_IN10, coords_indiana10)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana10$long) - 0.1 & x <= max(coords_indiana10$long) + 0.1 &
                  y >= min(coords_indiana10$lat) - 0.1 & y <= max(coords_indiana10$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana10)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN10, coords_indiana10, select_df_IN, indiana10)

# Subset 11
dists <- fields::rdist(coords_df_IN11, coords_indiana11)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana11$long) - 0.1 & x <= max(coords_indiana11$long) + 0.1 &
                  y >= min(coords_indiana11$lat) - 0.1 & y <= max(coords_indiana11$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana11)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN11, coords_indiana11, select_df_IN, indiana11)

# Subset 12
dists <- fields::rdist(coords_df_IN12, coords_indiana12)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana12$long) - 0.1 & x <= max(coords_indiana12$long) + 0.1 &
                  y >= min(coords_indiana12$lat) - 0.1 & y <= max(coords_indiana12$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana12)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN12, coords_indiana12, select_df_IN, indiana12)

# Subset 13
dists <- fields::rdist(coords_df_IN13, coords_indiana13)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana13$long) - 0.1 & x <= max(coords_indiana13$long) + 0.1 &
                  y >= min(coords_indiana13$lat) - 0.1 & y <= max(coords_indiana13$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana13)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN13, coords_indiana13, select_df_IN, indiana13)

# Subset 14
dists <- fields::rdist(coords_df_IN14, coords_indiana14)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana14$long) - 0.1 & x <= max(coords_indiana14$long) + 0.1 &
                  y >= min(coords_indiana14$lat) - 0.1 & y <= max(coords_indiana14$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana14)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN14, coords_indiana14, select_df_IN, indiana14)

# Subset 15
dists <- fields::rdist(coords_df_IN15, coords_indiana15)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana15$long) - 0.1 & x <= max(coords_indiana15$long) + 0.1 &
                  y >= min(coords_indiana15$lat) - 0.1 & y <= max(coords_indiana15$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana15)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN15, coords_indiana15, select_df_IN, indiana15)

# Subset 16
dists <- fields::rdist(coords_df_IN16, coords_indiana16)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana16$long) - 0.1 & x <= max(coords_indiana16$long) + 0.1 &
                  y >= min(coords_indiana16$lat) - 0.1 & y <= max(coords_indiana16$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana16)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN16, coords_indiana16, select_df_IN, indiana16)

# Subset 17
dists <- fields::rdist(coords_df_IN17, coords_indiana17)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana17$long) - 0.1 & x <= max(coords_indiana17$long) + 0.1 &
                  y >= min(coords_indiana17$lat) - 0.1 & y <= max(coords_indiana17$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana17)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN17, coords_indiana17, select_df_IN, indiana17)

# Subset 18
dists <- fields::rdist(coords_df_IN18, coords_indiana18)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana18$long) - 0.1 & x <= max(coords_indiana18$long) + 0.1 &
                  y >= min(coords_indiana18$lat) - 0.1 & y <= max(coords_indiana18$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana18)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN18, coords_indiana18, select_df_IN, indiana18)

# Subset 19
dists <- fields::rdist(coords_df_IN19, coords_indiana19)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana19$long) - 0.1 & x <= max(coords_indiana19$long) + 0.1 &
                  y >= min(coords_indiana19$lat) - 0.1 & y <= max(coords_indiana19$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana19)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN19, coords_indiana19, select_df_IN, indiana19)

# Subset 20
dists <- fields::rdist(coords_df_IN20, coords_indiana20)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana20$long) - 0.1 & x <= max(coords_indiana20$long) + 0.1 &
                  y >= min(coords_indiana20$lat) - 0.1 & y <= max(coords_indiana20$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana20)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN20, coords_indiana20, select_df_IN, indiana20)

# Update column names
colnames(IN_topo_pls) <- c('topo_x', 'topo_y', 'elevation', 'slope',
                           'aspect', 'state', 'pls_x', 'pls_y',
                           'L1_tree1', 'L1_tree2', 'uniqueID')

# Save IN topography. Confirmed in same order as indiana and indiana_ecosystem
IN_topo <- dplyr::select(IN_topo_pls, topo_x, topo_y, elevation, slope, aspect, pls_x, pls_y)
save(IN_topo, file = 'data/processed/topography/processed_topo_in.RData')

#### MICHIGAN ####
rm(list = ls())

load('data/raw/topography/mi_topo.RData')

load('data/processed/PLS/lowmichigan_process.RData')
load('data/processed/PLS/upmichigan_process.RData')

# Remove negative points because we know land in michigan is not below sea level
mi_topo <- dplyr::filter(mi_topo, elevation >= 0)

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

# Divide pls michigan into 30 sections
michigan1 <- michigan[1:4000,]
michigan2 <- michigan[4001:8000,]
michigan3 <- michigan[8001:12000,]
michigan4 <- michigan[12001:16000,]
michigan5 <- michigan[16001:20000,]
michigan6 <- michigan[20001:24000,]
michigan7 <- michigan[24001:28500,]
michigan8 <- michigan[28501:31000,]
michigan9 <- michigan[31001:36000,]
michigan10 <- michigan[36001:40000,]
michigan11 <- michigan[40001:44000,]
michigan12 <- michigan[44001:48000,]
michigan13 <- michigan[48001:52000,]
michigan14 <- michigan[52001:56000,]
michigan15 <- michigan[56001:60000,]
michigan16 <- michigan[60001:64000,]
michigan17 <- michigan[64001:68000,]
michigan18 <- michigan[68001:72000,]
michigan19 <- michigan[72001:75500,]
michigan20 <- michigan[75501:80000,]
michigan21 <- michigan[80001:84000,]
michigan22 <- michigan[84001:88000,]
michigan23 <- michigan[88001:92000,]
michigan24 <- michigan[92001:96000,]
michigan25 <- michigan[96001:100000,]
michigan26 <- michigan[100001:104000,]
michigan27 <- michigan[104001:108000,]
michigan28 <- michigan[108001:112000,]
michigan29 <- michigan[112001:116000,]
michigan30 <- michigan[116001:120134,]

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
coords_michigan21 <- dplyr::select(michigan21, y, x)
coords_michigan22 <- dplyr::select(michigan22, y, x)
coords_michigan23 <- dplyr::select(michigan23, y, x)
coords_michigan24 <- dplyr::select(michigan24, y, x)
coords_michigan25 <- dplyr::select(michigan25, y, x)
coords_michigan26 <- dplyr::select(michigan26, y, x)
coords_michigan27 <- dplyr::select(michigan27, y, x)
coords_michigan28 <- dplyr::select(michigan28, y, x)
coords_michigan29 <- dplyr::select(michigan29, y, x)
coords_michigan30 <- dplyr::select(michigan30, y, x)

# Add column names
colnames(coords_michigan1) <- colnames(coords_michigan2) <- colnames(coords_michigan3) <-
  colnames(coords_michigan4) <- colnames(coords_michigan5) <- colnames(coords_michigan6) <-
  colnames(coords_michigan7) <- colnames(coords_michigan8) <- colnames(coords_michigan9) <-
  colnames(coords_michigan10) <- colnames(coords_michigan11) <- colnames(coords_michigan12) <-
  colnames(coords_michigan13) <- colnames(coords_michigan14) <- colnames(coords_michigan15) <-
  colnames(coords_michigan16) <- colnames(coords_michigan17) <- colnames(coords_michigan18) <-
  colnames(coords_michigan19) <- colnames(coords_michigan20) <- colnames(coords_michigan21) <-
  colnames(coords_michigan22) <- colnames(coords_michigan23) <- colnames(coords_michigan24) <-
  colnames(coords_michigan25) <- colnames(coords_michigan26) <- colnames(coords_michigan27) <-
  colnames(coords_michigan28) <- colnames(coords_michigan29) <- colnames(coords_michigan30) <-
  c('lat', 'long')

# Subset the soil data to the lat/long extent of each pls subsection
# Then extract only the lat/long columns
coords_df_MI1 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan1$long) - 0.1 & x <= max(coords_michigan1$long) + 0.1 &
                  y >= min(coords_michigan1$lat) - 0.1 & y <= max(coords_michigan1$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI2 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan2$long) - 0.1 & x <= max(coords_michigan2$long) + 0.1 &
                  y >= min(coords_michigan2$lat) - 0.1 & y <= max(coords_michigan2$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI3 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan3$long) - 0.1 & x <= max(coords_michigan3$long) + 0.1 &
                  y >= min(coords_michigan3$lat) - 0.1 & y <= max(coords_michigan3$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI4 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan4$long) - 0.1 & x <= max(coords_michigan4$long) + 0.1 &
                  y >= min(coords_michigan4$lat) - 0.1 & y <= max(coords_michigan4$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI5 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan5$long) - 0.1 & x <= max(coords_michigan5$long) + 0.1 &
                  y >= min(coords_michigan5$lat) - 0.1 & y <= max(coords_michigan5$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI6 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan6$long) - 0.1 & x <= max(coords_michigan6$long) + 0.1 &
                  y >= min(coords_michigan6$lat) - 0.1 & y <= max(coords_michigan6$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI7 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan7$long) - 0.1 & x <= max(coords_michigan7$long) + 0.1 &
                  y >= min(coords_michigan7$lat) - 0.1 & y <= max(coords_michigan7$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI8 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan8$long) - 0.1 & x <= max(coords_michigan8$long) + 0.1 &
                  y >= min(coords_michigan8$lat) - 0.1 & y <= max(coords_michigan8$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI9 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan9$long) - 0.1 & x <= max(coords_michigan9$long) + 0.1 &
                  y >= min(coords_michigan9$lat) - 0.1 & y <= max(coords_michigan9$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI10 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan10$long) - 0.1 & x <= max(coords_michigan10$long) + 0.1 &
                  y >= min(coords_michigan10$lat) - 0.1 & y <= max(coords_michigan10$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI11 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan11$long) - 0.1 & x <= max(coords_michigan11$long) + 0.1 &
                  y >= min(coords_michigan11$lat) - 0.1 & y <= max(coords_michigan11$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI12 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan12$long) - 0.1 & x <= max(coords_michigan12$long) + 0.1 &
                  y >= min(coords_michigan12$lat) - 0.1 & y <= max(coords_michigan12$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI13 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan13$long) - 0.1 & x <= max(coords_michigan13$long) + 0.1 &
                  y >= min(coords_michigan13$lat) - 0.1 & y <= max(coords_michigan13$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI14 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan14$long) - 0.1 & x <= max(coords_michigan14$long) + 0.1 &
                  y >= min(coords_michigan14$lat) - 0.1 & y <= max(coords_michigan14$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI15 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan15$long) - 0.1 & x <= max(coords_michigan15$long) + 0.1 &
                  y >= min(coords_michigan15$lat) - 0.1 & y <= max(coords_michigan15$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI16 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan16$long) - 0.1 & x <= max(coords_michigan16$long) + 0.1 &
                  y >= min(coords_michigan16$lat) - 0.1 & y <= max(coords_michigan16$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI17 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan17$long) - 0.1 & x <= max(coords_michigan17$long) + 0.1 &
                  y >= min(coords_michigan17$lat) - 0.1 & y <= max(coords_michigan17$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI18 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan18$long) - 0.1 & x <= max(coords_michigan18$long) + 0.1 &
                  y >= min(coords_michigan18$lat) - 0.1 & y <= max(coords_michigan18$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI19 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan19$long) - 0.1 & x <= max(coords_michigan19$long) + 0.1 &
                  y >= min(coords_michigan19$lat) - 0.1 & y <= max(coords_michigan19$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI20 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan20$long) - 0.1 & x <= max(coords_michigan20$long) + 0.1 &
                  y >= min(coords_michigan20$lat) - 0.1 & y <= max(coords_michigan20$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI21 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan21$long) - 0.1 & x <= max(coords_michigan21$long) + 0.1 &
                  y >= min(coords_michigan21$lat) - 0.1 & y <= max(coords_michigan21$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI22 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan22$long) - 0.1 & x <= max(coords_michigan22$long) + 0.1 &
                  y >= min(coords_michigan22$lat) - 0.1 & y <= max(coords_michigan22$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI23 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan23$long) - 0.1 & x <= max(coords_michigan23$long) + 0.1 &
                  y >= min(coords_michigan23$lat) - 0.1 & y <= max(coords_michigan23$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI24 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan24$long) - 0.1 & x <= max(coords_michigan24$long) + 0.1 &
                  y >= min(coords_michigan24$lat) - 0.1 & y <= max(coords_michigan24$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI25 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan25$long) - 0.1 & x <= max(coords_michigan25$long) + 0.1 &
                  y >= min(coords_michigan25$lat) - 0.1 & y <= max(coords_michigan25$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI26 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan26$long) - 0.1 & x <= max(coords_michigan26$long) + 0.1 &
                  y >= min(coords_michigan26$lat) - 0.1 & y <= max(coords_michigan26$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI27 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan27$long) - 0.1 & x <= max(coords_michigan27$long) + 0.1 &
                  y >= min(coords_michigan27$lat) - 0.1 & y <= max(coords_michigan27$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI28 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan28$long) - 0.1 & x <= max(coords_michigan28$long) + 0.1 &
                  y >= min(coords_michigan28$lat) - 0.1 & y <= max(coords_michigan28$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI29 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan29$long) - 0.1 & x <= max(coords_michigan29$long) + 0.1 &
                  y >= min(coords_michigan29$lat) - 0.1 & y <= max(coords_michigan29$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI30 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan30$long) - 0.1 & x <= max(coords_michigan30$long) + 0.1 &
                  y >= min(coords_michigan30$lat) - 0.1 & y <= max(coords_michigan30$lat) + 0.1) |>
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
select_df_MI <- mi_topo |>
  # Filter as done above to make sure indexing matches
  dplyr::filter(x >= min(coords_michigan1$long) - 0.1 & x <= max(coords_michigan1$long) + 0.1 &
                  y >= min(coords_michigan1$lat) - 0.1 & y <= max(coords_michigan1$lat) + 0.1) |>
  # subset by row numbers given by which.min
  dplyr::slice(closest_points)

# Column bind with PLS data
MI_topo_pls <- cbind(select_df_MI, michigan1)

# Remove unnecessary objects to keep memory usage lower
rm(closest_points, coords_df_MI1, coords_michigan1, select_df_MI, michigan1)

# Repeat all these steps for the remaining 39 subsets

# Subset 2
dists <- fields::rdist(coords_df_MI2, coords_michigan2)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan2$long) - 0.1 & x <= max(coords_michigan2$long) + 0.1 &
                  y >= min(coords_michigan2$lat) - 0.1 & y <= max(coords_michigan2$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan2)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI2, coords_michigan2, select_df_MI, michigan2)

# Subset 3
dists <- fields::rdist(coords_df_MI3, coords_michigan3)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan3$long) - 0.1 & x <= max(coords_michigan3$long) + 0.1 &
                  y >= min(coords_michigan3$lat) - 0.1 & y <= max(coords_michigan3$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan3)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI3, coords_michigan3, select_df_MI, michigan3)

# Subset 4
dists <- fields::rdist(coords_df_MI4, coords_michigan4)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan4$long) - 0.1 & x <= max(coords_michigan4$long) + 0.1 &
                  y >= min(coords_michigan4$lat) - 0.1 & y <= max(coords_michigan4$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan4)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI4, coords_michigan4, select_df_MI, michigan4)

# Subset 5
dists <- fields::rdist(coords_df_MI5, coords_michigan5)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan5$long) - 0.1 & x <= max(coords_michigan5$long) + 0.1 &
                  y >= min(coords_michigan5$lat) - 0.1 & y <= max(coords_michigan5$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan5)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI5, coords_michigan5, select_df_MI, michigan5)

# Subset 6
dists <- fields::rdist(coords_df_MI6, coords_michigan6)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan6$long) - 0.1 & x <= max(coords_michigan6$long) + 0.1 &
                  y >= min(coords_michigan6$lat) - 0.1 & y <= max(coords_michigan6$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan6)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI6, coords_michigan6, select_df_MI, michigan6)

# Subset 7
dists <- fields::rdist(coords_df_MI7, coords_michigan7)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan7$long) - 0.1 & x <= max(coords_michigan7$long) + 0.1 &
                  y >= min(coords_michigan7$lat) - 0.1 & y <= max(coords_michigan7$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan7)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI7, coords_michigan7, select_df_MI, michigan7)

# Subset 8
dists <- fields::rdist(coords_df_MI8, coords_michigan8)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan8$long) - 0.1 & x <= max(coords_michigan8$long) + 0.1 &
                  y >= min(coords_michigan8$lat) - 0.1 & y <= max(coords_michigan8$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan8)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI8, coords_michigan8, select_df_MI, michigan8)

# Subset 9
dists <- fields::rdist(coords_df_MI9, coords_michigan9)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan9$long) - 0.1 & x <= max(coords_michigan9$long) + 0.1 &
                  y >= min(coords_michigan9$lat) - 0.1 & y <= max(coords_michigan9$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan9)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI9, coords_michigan9, select_df_MI, michigan9)

# Subset 10
dists <- fields::rdist(coords_df_MI10, coords_michigan10)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan10$long) - 0.1 & x <= max(coords_michigan10$long) + 0.1 &
                  y >= min(coords_michigan10$lat) - 0.1 & y <= max(coords_michigan10$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan10)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI10, coords_michigan10, select_df_MI, michigan10)

# Subset 11
dists <- fields::rdist(coords_df_MI11, coords_michigan11)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan11$long) - 0.1 & x <= max(coords_michigan11$long) + 0.1 &
                  y >= min(coords_michigan11$lat) - 0.1 & y <= max(coords_michigan11$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan11)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI11, coords_michigan11, select_df_MI, michigan11)

# Subset 12
dists <- fields::rdist(coords_df_MI12, coords_michigan12)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan12$long) - 0.1 & x <= max(coords_michigan12$long) + 0.1 &
                  y >= min(coords_michigan12$lat) - 0.1 & y <= max(coords_michigan12$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan12)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI12, coords_michigan12, select_df_MI, michigan12)

# Subset 13
dists <- fields::rdist(coords_df_MI13, coords_michigan13)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan13$long) - 0.1 & x <= max(coords_michigan13$long) + 0.1 &
                  y >= min(coords_michigan13$lat) - 0.1 & y <= max(coords_michigan13$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan13)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI13, coords_michigan13, select_df_MI, michigan13)

# Subset 14
dists <- fields::rdist(coords_df_MI14, coords_michigan14)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan14$long) - 0.1 & x <= max(coords_michigan14$long) + 0.1 &
                  y >= min(coords_michigan14$lat) - 0.1 & y <= max(coords_michigan14$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan14)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI14, coords_michigan14, select_df_MI, michigan14)

# Subset 15
dists <- fields::rdist(coords_df_MI15, coords_michigan15)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan15$long) - 0.1 & x <= max(coords_michigan15$long) + 0.1 &
                  y >= min(coords_michigan15$lat) - 0.1 & y <= max(coords_michigan15$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan15)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI15, coords_michigan15, select_df_MI, michigan15)

# Subset 16
dists <- fields::rdist(coords_df_MI16, coords_michigan16)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan16$long) - 0.1 & x <= max(coords_michigan16$long) + 0.1 &
                  y >= min(coords_michigan16$lat) - 0.1 & y <= max(coords_michigan16$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan16)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI16, coords_michigan16, select_df_MI, michigan16)

# Subset 17
dists <- fields::rdist(coords_df_MI17, coords_michigan17)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan17$long) - 0.1 & x <= max(coords_michigan17$long) + 0.1 &
                  y >= min(coords_michigan17$lat) - 0.1 & y <= max(coords_michigan17$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan17)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI17, coords_michigan17, select_df_MI, michigan17)

# Subset 18
dists <- fields::rdist(coords_df_MI18, coords_michigan18)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan18$long) - 0.1 & x <= max(coords_michigan18$long) + 0.1 &
                  y >= min(coords_michigan18$lat) - 0.1 & y <= max(coords_michigan18$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan18)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI18, coords_michigan18, select_df_MI, michigan18)

# Subset 19
dists <- fields::rdist(coords_df_MI19, coords_michigan19)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan19$long) - 0.1 & x <= max(coords_michigan19$long) + 0.1 &
                  y >= min(coords_michigan19$lat) - 0.1 & y <= max(coords_michigan19$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan19)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI19, coords_michigan19, select_df_MI, michigan19)

# Subset 20
dists <- fields::rdist(coords_df_MI20, coords_michigan20)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan20$long) - 0.1 & x <= max(coords_michigan20$long) + 0.1 &
                  y >= min(coords_michigan20$lat) - 0.1 & y <= max(coords_michigan20$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan20)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI20, coords_michigan20, select_df_MI, michigan20)

# Subset 21
dists <- fields::rdist(coords_df_MI21, coords_michigan21)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan21$long) - 0.1 & x <= max(coords_michigan21$long) + 0.1 &
                  y >= min(coords_michigan21$lat) - 0.1 & y <= max(coords_michigan21$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan21)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI21, coords_michigan21, select_df_MI, michigan21)

# Subset 22
dists <- fields::rdist(coords_df_MI22, coords_michigan22)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan22$long) - 0.1 & x <= max(coords_michigan22$long) + 0.1 &
                  y >= min(coords_michigan22$lat) - 0.1 & y <= max(coords_michigan22$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan22)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI22, coords_michigan22, select_df_MI, michigan22)

# Subset 23
dists <- fields::rdist(coords_df_MI23, coords_michigan23)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan23$long) - 0.1 & x <= max(coords_michigan23$long) + 0.1 &
                  y >= min(coords_michigan23$lat) - 0.1 & y <= max(coords_michigan23$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan23)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI23, coords_michigan23, select_df_MI, michigan23)

# Subset 24
dists <- fields::rdist(coords_df_MI24, coords_michigan24)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan24$long) - 0.1 & x <= max(coords_michigan24$long) + 0.1 &
                  y >= min(coords_michigan24$lat) - 0.1 & y <= max(coords_michigan24$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan24)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI24, coords_michigan24, select_df_MI, michigan24)

# Subset 25
dists <- fields::rdist(coords_df_MI25, coords_michigan25)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan25$long) - 0.1 & x <= max(coords_michigan25$long) + 0.1 &
                  y >= min(coords_michigan25$lat) - 0.1 & y <= max(coords_michigan25$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan25)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI25, coords_michigan25, select_df_MI, michigan25)

# Subset 26
dists <- fields::rdist(coords_df_MI26, coords_michigan26)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan26$long) - 0.1 & x <= max(coords_michigan26$long) + 0.1 &
                  y >= min(coords_michigan26$lat) - 0.1 & y <= max(coords_michigan26$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan26)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI26, coords_michigan26, select_df_MI, michigan26)

# Subset 27
dists <- fields::rdist(coords_df_MI27, coords_michigan27)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan27$long) - 0.1 & x <= max(coords_michigan27$long) + 0.1 &
                  y >= min(coords_michigan27$lat) - 0.1 & y <= max(coords_michigan27$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan27)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI27, coords_michigan27, select_df_MI, michigan27)

# Subset 28
dists <- fields::rdist(coords_df_MI28, coords_michigan28)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan28$long) - 0.1 & x <= max(coords_michigan28$long) + 0.1 &
                  y >= min(coords_michigan28$lat) - 0.1 & y <= max(coords_michigan28$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan28)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI28, coords_michigan28, select_df_MI, michigan28)

# Subset 29
dists <- fields::rdist(coords_df_MI29, coords_michigan29)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan29$long) - 0.1 & x <= max(coords_michigan29$long) + 0.1 &
                  y >= min(coords_michigan29$lat) - 0.1 & y <= max(coords_michigan29$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan29)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI29, coords_michigan29, select_df_MI, michigan29)

# Subset 30
dists <- fields::rdist(coords_df_MI30, coords_michigan30)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan30$long) - 0.1 & x <= max(coords_michigan30$long) + 0.1 &
                  y >= min(coords_michigan30$lat) - 0.1 & y <= max(coords_michigan30$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan30)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI30, coords_michigan30, select_df_MI, michigan30)

# Update column names
colnames(MI_topo_pls) <- c('topo_x', 'topo_y', 'elevation', 'slope',
                           'aspect', 'state', 'pls_x', 'pls_y',
                           'L1_tree1', 'L1_tree2')

# Save MI topography. Confirmed in same order as michigan and michigan_ecosystem
MI_topo <- dplyr::select(MI_topo_pls, topo_x, topo_y, elevation, slope, aspect, pls_x, pls_y)
save(MI_topo, file = 'data/processed/topography/processed_topo_mi.RData')

#### MINNESOTA ####
rm(list = ls())

load('data/raw/topography/mn_topo.RData')

load('data/processed/PLS/minnesota_process.RData')

# Remove negative points because we know land in michigan is not below sea level
mn_topo <- dplyr::filter(mn_topo, elevation >= 0)

# Format minnesota data
minnesota <- minnesota |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species') |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Divide pls minnesota into 70 sections
minnesota1 <- minnesota[1:3598,]
minnesota2 <- minnesota[3599:7197,]
minnesota3 <- minnesota[7198:10796,]
minnesota4 <- minnesota[10797:14395,]
minnesota5 <- minnesota[14396:17994,]
minnesota6 <- minnesota[17995:21593,]
minnesota7 <- minnesota[21594:25592,]
minnesota8 <- minnesota[25593:29791,]
minnesota9 <- minnesota[29792:32390,]
minnesota10 <- minnesota[32391:35989,]
minnesota11 <- minnesota[35990:39588,]
minnesota12 <- minnesota[39589:43187,]
minnesota13 <- minnesota[43188:46786,]
minnesota14 <- minnesota[46787:50385,]
minnesota15 <- minnesota[50386:53984,]
minnesota16 <- minnesota[53985:57583,]
minnesota17 <- minnesota[57584:61182,]
minnesota18 <- minnesota[61183:64781,]
minnesota19 <- minnesota[64782:68380,]
minnesota20 <- minnesota[68381:71979,]
minnesota21 <- minnesota[71980:75578,]
minnesota22 <- minnesota[75579:79177,]
minnesota23 <- minnesota[79178:82776,]
minnesota24 <- minnesota[82777:86375,]
minnesota25 <- minnesota[86376:90474,]
minnesota26 <- minnesota[90475:93573,]
minnesota27 <- minnesota[93574:97372,]
minnesota28 <- minnesota[97373:100771,]
minnesota29 <- minnesota[100772:104670,]
minnesota30 <- minnesota[104671:108069,]
minnesota31 <- minnesota[108070:111568,]
minnesota32 <- minnesota[111569:115167,]
minnesota33 <- minnesota[115168:118366,]
minnesota34 <- minnesota[118367:122365,]
minnesota35 <- minnesota[122366:125964,]
minnesota36 <- minnesota[125965:129563,]
minnesota37 <- minnesota[129564:133162,]
minnesota38 <- minnesota[133163:136761,]
minnesota39 <- minnesota[136762:140360,]
minnesota40 <- minnesota[140361:143932,]
minnesota41 <- minnesota[143933:147531,]
minnesota42 <- minnesota[147532:151630,]
minnesota43 <- minnesota[151631:154729,]
minnesota44 <- minnesota[154730:158328,]
minnesota45 <- minnesota[158329:161927,]
minnesota46 <- minnesota[161928:165526,]
minnesota47 <- minnesota[165527:169125,]
minnesota48 <- minnesota[169126:172724,]
minnesota49 <- minnesota[172725:176323,]
minnesota50 <- minnesota[176324:179922,]
minnesota51 <- minnesota[179923:183521,]
minnesota52 <- minnesota[183522:187118,]
minnesota53 <- minnesota[187119:190717,]
minnesota54 <- minnesota[190718:194316,]
minnesota55 <- minnesota[194317:197915,]
minnesota56 <- minnesota[197916:201514,]
minnesota57 <- minnesota[201515:205113,]
minnesota58 <- minnesota[205114:208712,]
minnesota59 <- minnesota[208713:212411,]
minnesota60 <- minnesota[212412:216010,]
minnesota61 <- minnesota[216011:219609,]
minnesota62 <- minnesota[219610:223208,]
minnesota63 <- minnesota[223209:226807,]
minnesota64 <- minnesota[226808:230406,]
minnesota65 <- minnesota[230407:234005,]
minnesota66 <- minnesota[234006:237604,]
minnesota67 <- minnesota[237605:241203,]
minnesota68 <- minnesota[241204:244802,]
minnesota69 <- minnesota[244803:246189,]

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
coords_minnesota31 <- dplyr::select(minnesota31, y, x)
coords_minnesota32 <- dplyr::select(minnesota32, y, x)
coords_minnesota33 <- dplyr::select(minnesota33, y, x)
coords_minnesota34 <- dplyr::select(minnesota34, y, x)
coords_minnesota35 <- dplyr::select(minnesota35, y, x)
coords_minnesota36 <- dplyr::select(minnesota36, y, x)
coords_minnesota37 <- dplyr::select(minnesota37, y, x)
coords_minnesota38 <- dplyr::select(minnesota38, y, x)
coords_minnesota39 <- dplyr::select(minnesota39, y, x)
coords_minnesota40 <- dplyr::select(minnesota40, y, x)
coords_minnesota41 <- dplyr::select(minnesota41, y, x)
coords_minnesota42 <- dplyr::select(minnesota42, y, x)
coords_minnesota43 <- dplyr::select(minnesota43, y, x)
coords_minnesota44 <- dplyr::select(minnesota44, y, x)
coords_minnesota45 <- dplyr::select(minnesota45, y, x)
coords_minnesota46 <- dplyr::select(minnesota46, y, x)
coords_minnesota47 <- dplyr::select(minnesota47, y, x)
coords_minnesota48 <- dplyr::select(minnesota48, y, x)
coords_minnesota49 <- dplyr::select(minnesota49, y, x)
coords_minnesota50 <- dplyr::select(minnesota50, y, x)
coords_minnesota51 <- dplyr::select(minnesota51, y, x)
coords_minnesota52 <- dplyr::select(minnesota52, y, x)
coords_minnesota53 <- dplyr::select(minnesota53, y, x)
coords_minnesota54 <- dplyr::select(minnesota54, y, x)
coords_minnesota55 <- dplyr::select(minnesota55, y, x)
coords_minnesota56 <- dplyr::select(minnesota56, y, x)
coords_minnesota57 <- dplyr::select(minnesota57, y, x)
coords_minnesota58 <- dplyr::select(minnesota58, y, x)
coords_minnesota59 <- dplyr::select(minnesota59, y, x)
coords_minnesota60 <- dplyr::select(minnesota60, y, x)
coords_minnesota61 <- dplyr::select(minnesota61, y, x)
coords_minnesota62 <- dplyr::select(minnesota62, y, x)
coords_minnesota63 <- dplyr::select(minnesota63, y, x)
coords_minnesota64 <- dplyr::select(minnesota64, y, x)
coords_minnesota65 <- dplyr::select(minnesota65, y, x)
coords_minnesota66 <- dplyr::select(minnesota66, y, x)
coords_minnesota67 <- dplyr::select(minnesota67, y, x)
coords_minnesota68 <- dplyr::select(minnesota68, y, x)
coords_minnesota69 <- dplyr::select(minnesota69, y, x)

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
  colnames(coords_minnesota31) <- colnames(coords_minnesota32) <- colnames(coords_minnesota33) <-
  colnames(coords_minnesota34) <- colnames(coords_minnesota35) <- colnames(coords_minnesota36) <-
  colnames(coords_minnesota37) <- colnames(coords_minnesota38) <- colnames(coords_minnesota39) <-
  colnames(coords_minnesota40) <- colnames(coords_minnesota41) <- colnames(coords_minnesota42) <-
  colnames(coords_minnesota43) <- colnames(coords_minnesota44) <- colnames(coords_minnesota45) <-
  colnames(coords_minnesota46) <- colnames(coords_minnesota47) <- colnames(coords_minnesota48) <-
  colnames(coords_minnesota49) <- colnames(coords_minnesota50) <- colnames(coords_minnesota51) <-
  colnames(coords_minnesota52) <- colnames(coords_minnesota53) <- colnames(coords_minnesota54) <-
  colnames(coords_minnesota55) <- colnames(coords_minnesota56) <- colnames(coords_minnesota57) <-
  colnames(coords_minnesota58) <- colnames(coords_minnesota59) <- colnames(coords_minnesota60) <-
  colnames(coords_minnesota61) <- colnames(coords_minnesota62) <- colnames(coords_minnesota63) <-
  colnames(coords_minnesota64) <- colnames(coords_minnesota65) <- colnames(coords_minnesota66) <-
  colnames(coords_minnesota67) <- colnames(coords_minnesota68) <- colnames(coords_minnesota69) <-
  c('lat', 'long')

# Subset the soil data to the lat/long extent of each pls subsection
# Then extract only the lat/long columns
coords_df_MN1 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota1$long) - 0.1 & x <= max(coords_minnesota1$long) + 0.1 &
                  y >= min(coords_minnesota1$lat) - 0.1 & y <= max(coords_minnesota1$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN2 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota2$long) - 0.1 & x <= max(coords_minnesota2$long) + 0.1 &
                  y >= min(coords_minnesota2$lat) - 0.1 & y <= max(coords_minnesota2$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN3 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota3$long) - 0.1 & x <= max(coords_minnesota3$long) + 0.1 &
                  y >= min(coords_minnesota3$lat) - 0.1 & y <= max(coords_minnesota3$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN4 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota4$long) - 0.1 & x <= max(coords_minnesota4$long) + 0.1 &
                  y >= min(coords_minnesota4$lat) - 0.1 & y <= max(coords_minnesota4$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN5 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota5$long) - 0.1 & x <= max(coords_minnesota5$long) + 0.1 &
                  y >= min(coords_minnesota5$lat) - 0.1 & y <= max(coords_minnesota5$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN6 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota6$long) - 0.1 & x <= max(coords_minnesota6$long) + 0.1 &
                  y >= min(coords_minnesota6$lat) - 0.1 & y <= max(coords_minnesota6$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN7 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota7$long) - 0.1 & x <= max(coords_minnesota7$long) + 0.1 &
                  y >= min(coords_minnesota7$lat) - 0.1 & y <= max(coords_minnesota7$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN8 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota8$long) - 0.1 & x <= max(coords_minnesota8$long) + 0.1 &
                  y >= min(coords_minnesota8$lat) - 0.1 & y <= max(coords_minnesota8$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN9 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota9$long) - 0.1 & x <= max(coords_minnesota9$long) + 0.1 &
                  y >= min(coords_minnesota9$lat) - 0.1 & y <= max(coords_minnesota9$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN10 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota10$long) - 0.1 & x <= max(coords_minnesota10$long) + 0.1 &
                  y >= min(coords_minnesota10$lat) - 0.1 & y <= max(coords_minnesota10$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN11 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota11$long) - 0.1 & x <= max(coords_minnesota11$long) + 0.1 &
                  y >= min(coords_minnesota11$lat) - 0.1 & y <= max(coords_minnesota11$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN12 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota12$long) - 0.1 & x <= max(coords_minnesota12$long) + 0.1 &
                  y >= min(coords_minnesota12$lat) - 0.1 & y <= max(coords_minnesota12$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN13 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota13$long) - 0.1 & x <= max(coords_minnesota13$long) + 0.1 &
                  y >= min(coords_minnesota13$lat) - 0.1 & y <= max(coords_minnesota13$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN14 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota14$long) - 0.1 & x <= max(coords_minnesota14$long) + 0.1 &
                  y >= min(coords_minnesota14$lat) - 0.1 & y <= max(coords_minnesota14$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN15 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota15$long) - 0.1 & x <= max(coords_minnesota15$long) + 0.1 &
                  y >= min(coords_minnesota15$lat) - 0.1 & y <= max(coords_minnesota15$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN16 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota16$long) - 0.1 & x <= max(coords_minnesota16$long) + 0.1 &
                  y >= min(coords_minnesota16$lat) - 0.1 & y <= max(coords_minnesota16$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN17 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota17$long) - 0.1 & x <= max(coords_minnesota17$long) + 0.1 &
                  y >= min(coords_minnesota17$lat) - 0.1 & y <= max(coords_minnesota17$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN18 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota18$long) - 0.1 & x <= max(coords_minnesota18$long) + 0.1 &
                  y >= min(coords_minnesota18$lat) - 0.1 & y <= max(coords_minnesota18$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN19 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota19$long) - 0.1 & x <= max(coords_minnesota19$long) + 0.1 &
                  y >= min(coords_minnesota19$lat) - 0.1 & y <= max(coords_minnesota19$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN20 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota20$long) - 0.1 & x <= max(coords_minnesota20$long) + 0.1 &
                  y >= min(coords_minnesota20$lat) - 0.1 & y <= max(coords_minnesota20$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN21 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota21$long) - 0.1 & x <= max(coords_minnesota21$long) + 0.1 &
                  y >= min(coords_minnesota21$lat) - 0.1 & y <= max(coords_minnesota21$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN22 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota22$long) - 0.1 & x <= max(coords_minnesota22$long) + 0.1 &
                  y >= min(coords_minnesota22$lat) - 0.1 & y <= max(coords_minnesota22$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN23 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota23$long) - 0.1 & x <= max(coords_minnesota23$long) + 0.1 &
                  y >= min(coords_minnesota23$lat) - 0.1 & y <= max(coords_minnesota23$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN24 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota24$long) - 0.1 & x <= max(coords_minnesota24$long) + 0.1 &
                  y >= min(coords_minnesota24$lat) - 0.1 & y <= max(coords_minnesota24$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN25 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota25$long) - 0.1 & x <= max(coords_minnesota25$long) + 0.1 &
                  y >= min(coords_minnesota25$lat) - 0.1 & y <= max(coords_minnesota25$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN26 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota26$long) - 0.1 & x <= max(coords_minnesota26$long) + 0.1 &
                  y >= min(coords_minnesota26$lat) - 0.1 & y <= max(coords_minnesota26$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN27 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota27$long) - 0.1 & x <= max(coords_minnesota27$long) + 0.1 &
                  y >= min(coords_minnesota27$lat) - 0.1 & y <= max(coords_minnesota27$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN28 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota28$long) - 0.1 & x <= max(coords_minnesota28$long) + 0.1 &
                  y >= min(coords_minnesota28$lat) - 0.1 & y <= max(coords_minnesota28$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN29 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota29$long) - 0.1 & x <= max(coords_minnesota29$long) + 0.1 &
                  y >= min(coords_minnesota29$lat) - 0.1 & y <= max(coords_minnesota29$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN30 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota30$long) - 0.1 & x <= max(coords_minnesota30$long) + 0.1 &
                  y >= min(coords_minnesota30$lat) - 0.1 & y <= max(coords_minnesota30$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN31 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota31$long) - 0.1 & x <= max(coords_minnesota31$long) + 0.1 &
                  y >= min(coords_minnesota31$lat) - 0.1 & y <= max(coords_minnesota31$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN32 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota32$long) - 0.1 & x <= max(coords_minnesota32$long) + 0.1 &
                  y >= min(coords_minnesota32$lat) - 0.1 & y <= max(coords_minnesota32$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN33 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota33$long) - 0.1 & x <= max(coords_minnesota33$long) + 0.1 &
                  y >= min(coords_minnesota33$lat) - 0.1 & y <= max(coords_minnesota33$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN34 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota34$long) - 0.1 & x <= max(coords_minnesota34$long) + 0.1 &
                  y >= min(coords_minnesota34$lat) - 0.1 & y <= max(coords_minnesota34$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN35 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota35$long) - 0.1 & x <= max(coords_minnesota35$long) + 0.1 &
                  y >= min(coords_minnesota35$lat) - 0.1 & y <= max(coords_minnesota35$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN36 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota36$long) - 0.1 & x <= max(coords_minnesota36$long) + 0.1 &
                  y >= min(coords_minnesota36$lat) - 0.1 & y <= max(coords_minnesota36$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN37 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota37$long) - 0.1 & x <= max(coords_minnesota37$long) + 0.1 &
                  y >= min(coords_minnesota37$lat) - 0.1 & y <= max(coords_minnesota37$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN38 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota38$long) - 0.1 & x <= max(coords_minnesota38$long) + 0.1 &
                  y >= min(coords_minnesota38$lat) - 0.1 & y <= max(coords_minnesota38$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN39 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota39$long) - 0.1 & x <= max(coords_minnesota39$long) + 0.1 &
                  y >= min(coords_minnesota39$lat) - 0.1 & y <= max(coords_minnesota39$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN40 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota40$long) - 0.1 & x <= max(coords_minnesota40$long) + 0.1 &
                  y >= min(coords_minnesota40$lat) - 0.1 & y <= max(coords_minnesota40$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN41 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota41$long) - 0.1 & x <= max(coords_minnesota41$long) + 0.1 &
                  y >= min(coords_minnesota41$lat) - 0.1 & y <= max(coords_minnesota41$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN42 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota42$long) - 0.1 & x <= max(coords_minnesota42$long) + 0.1 &
                  y >= min(coords_minnesota42$lat) - 0.1 & y <= max(coords_minnesota42$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN43 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota43$long) - 0.1 & x <= max(coords_minnesota43$long) + 0.1 &
                  y >= min(coords_minnesota43$lat) - 0.1 & y <= max(coords_minnesota43$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN44 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota44$long) - 0.1 & x <= max(coords_minnesota44$long) + 0.1 &
                  y >= min(coords_minnesota44$lat) - 0.1 & y <= max(coords_minnesota44$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN45 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota45$long) - 0.1 & x <= max(coords_minnesota45$long) + 0.1 &
                  y >= min(coords_minnesota45$lat) - 0.1 & y <= max(coords_minnesota45$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN46 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota46$long) - 0.1 & x <= max(coords_minnesota46$long) + 0.1 &
                  y >= min(coords_minnesota46$lat) - 0.1 & y <= max(coords_minnesota46$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN47 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota47$long) - 0.1 & x <= max(coords_minnesota47$long) + 0.1 &
                  y >= min(coords_minnesota47$lat) - 0.1 & y <= max(coords_minnesota47$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN48 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota48$long) - 0.1 & x <= max(coords_minnesota48$long) + 0.1 &
                  y >= min(coords_minnesota48$lat) - 0.1 & y <= max(coords_minnesota48$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN49 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota49$long) - 0.1 & x <= max(coords_minnesota49$long) + 0.1 &
                  y >= min(coords_minnesota49$lat) - 0.1 & y <= max(coords_minnesota49$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN50 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota50$long) - 0.1 & x <= max(coords_minnesota50$long) + 0.1 &
                  y >= min(coords_minnesota50$lat) - 0.1 & y <= max(coords_minnesota50$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN51 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota51$long) - 0.1 & x <= max(coords_minnesota51$long) + 0.1 &
                  y >= min(coords_minnesota51$lat) - 0.1 & y <= max(coords_minnesota51$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN52 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota52$long) - 0.1 & x <= max(coords_minnesota52$long) + 0.1 &
                  y >= min(coords_minnesota52$lat) - 0.1 & y <= max(coords_minnesota52$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN53 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota53$long) - 0.1 & x <= max(coords_minnesota53$long) + 0.1 &
                  y >= min(coords_minnesota53$lat) - 0.1 & y <= max(coords_minnesota53$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN54 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota54$long) - 0.1 & x <= max(coords_minnesota54$long) + 0.1 &
                  y >= min(coords_minnesota54$lat) - 0.1 & y <= max(coords_minnesota54$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN55 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota55$long) - 0.1 & x <= max(coords_minnesota55$long) + 0.1 &
                  y >= min(coords_minnesota55$lat) - 0.1 & y <= max(coords_minnesota55$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN56 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota56$long) - 0.1 & x <= max(coords_minnesota56$long) + 0.1 &
                  y >= min(coords_minnesota56$lat) - 0.1 & y <= max(coords_minnesota56$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN57 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota57$long) - 0.1 & x <= max(coords_minnesota57$long) + 0.1 &
                  y >= min(coords_minnesota57$lat) - 0.1 & y <= max(coords_minnesota57$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN58 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota58$long) - 0.1 & x <= max(coords_minnesota58$long) + 0.1 &
                  y >= min(coords_minnesota58$lat) - 0.1 & y <= max(coords_minnesota58$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN59 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota59$long) - 0.1 & x <= max(coords_minnesota59$long) + 0.1 &
                  y >= min(coords_minnesota59$lat) - 0.1 & y <= max(coords_minnesota59$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN60 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota60$long) - 0.1 & x <= max(coords_minnesota60$long) + 0.1 &
                  y >= min(coords_minnesota60$lat) - 0.1 & y <= max(coords_minnesota60$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN61 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota61$long) - 0.1 & x <= max(coords_minnesota61$long) + 0.1 &
                  y >= min(coords_minnesota61$lat) - 0.1 & y <= max(coords_minnesota61$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN62 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota62$long) - 0.1 & x <= max(coords_minnesota62$long) + 0.1 &
                  y >= min(coords_minnesota62$lat) - 0.1 & y <= max(coords_minnesota62$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN63 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota63$long) - 0.1 & x <= max(coords_minnesota63$long) + 0.1 &
                  y >= min(coords_minnesota63$lat) - 0.1 & y <= max(coords_minnesota63$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN64 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota64$long) - 0.1 & x <= max(coords_minnesota64$long) + 0.1 &
                  y >= min(coords_minnesota64$lat) - 0.1 & y <= max(coords_minnesota64$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN65 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota65$long) - 0.1 & x <= max(coords_minnesota65$long) + 0.1 &
                  y >= min(coords_minnesota65$lat) - 0.1 & y <= max(coords_minnesota65$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN66 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota66$long) - 0.1 & x <= max(coords_minnesota66$long) + 0.1 &
                  y >= min(coords_minnesota66$lat) - 0.1 & y <= max(coords_minnesota66$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN67 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota67$long) - 0.1 & x <= max(coords_minnesota67$long) + 0.1 &
                  y >= min(coords_minnesota67$lat) - 0.1 & y <= max(coords_minnesota67$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN68 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota68$long) - 0.1 & x <= max(coords_minnesota68$long) + 0.1 &
                  y >= min(coords_minnesota68$lat) - 0.1 & y <= max(coords_minnesota68$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MN69 <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota69$long) - 0.1 & x <= max(coords_minnesota69$long) + 0.1 &
                  y >= min(coords_minnesota69$lat) - 0.1 & y <= max(coords_minnesota69$lat) + 0.1) |>
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
select_df_MN <- mn_topo |>
  # Filter as done above to make sure indexing matches
  dplyr::filter(x >= min(coords_minnesota1$long) - 0.1 & x <= max(coords_minnesota1$long) + 0.1 &
                  y >= min(coords_minnesota1$lat) - 0.1 & y <= max(coords_minnesota1$lat) + 0.1) |>
  # subset by row numbers given by which.min
  dplyr::slice(closest_points)

# Column bind with PLS data
MN_topo_pls <- cbind(select_df_MN, minnesota1)

# Remove unnecessary objects to keep memory usage lower
rm(closest_points, coords_df_MN1, coords_minnesota1, select_df_MN, minnesota1)

# Repeat all these steps for the remaining 29 subsets

# Subset 2
dists <- fields::rdist(coords_df_MN2, coords_minnesota2)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota2$long) - 0.1 & x <= max(coords_minnesota2$long) + 0.1 &
                  y >= min(coords_minnesota2$lat) - 0.1 & y <= max(coords_minnesota2$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota2)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN2, coords_minnesota2, select_df_MN, minnesota2)

# Subset 3
dists <- fields::rdist(coords_df_MN3, coords_minnesota3)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota3$long) - 0.1 & x <= max(coords_minnesota3$long) + 0.1 &
                  y >= min(coords_minnesota3$lat) - 0.1 & y <= max(coords_minnesota3$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota3)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN3, coords_minnesota3, select_df_MN, minnesota3)

# Subset 4
dists <- fields::rdist(coords_df_MN4, coords_minnesota4)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota4$long) - 0.1 & x <= max(coords_minnesota4$long) + 0.1 &
                  y >= min(coords_minnesota4$lat) - 0.1 & y <= max(coords_minnesota4$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota4)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN4, coords_minnesota4, select_df_MN, minnesota4)

# Subset 5
dists <- fields::rdist(coords_df_MN5, coords_minnesota5)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota5$long) - 0.1 & x <= max(coords_minnesota5$long) + 0.1 &
                  y >= min(coords_minnesota5$lat) - 0.1 & y <= max(coords_minnesota5$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota5)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN5, coords_minnesota5, select_df_MN, minnesota5)

# Subset 6
dists <- fields::rdist(coords_df_MN6, coords_minnesota6)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota6$long) - 0.1 & x <= max(coords_minnesota6$long) + 0.1 &
                  y >= min(coords_minnesota6$lat) - 0.1 & y <= max(coords_minnesota6$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota6)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN6, coords_minnesota6, select_df_MN, minnesota6)

# Subset 7
dists <- fields::rdist(coords_df_MN7, coords_minnesota7)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota7$long) - 0.1 & x <= max(coords_minnesota7$long) + 0.1 &
                  y >= min(coords_minnesota7$lat) - 0.1 & y <= max(coords_minnesota7$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota7)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN7, coords_minnesota7, select_df_MN, minnesota7)

# Subset 8
dists <- fields::rdist(coords_df_MN8, coords_minnesota8)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota8$long) - 0.1 & x <= max(coords_minnesota8$long) + 0.1 &
                  y >= min(coords_minnesota8$lat) - 0.1 & y <= max(coords_minnesota8$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota8)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN8, coords_minnesota8, select_df_MN, minnesota8)

# Subset 9
dists <- fields::rdist(coords_df_MN9, coords_minnesota9)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota9$long) - 0.1 & x <= max(coords_minnesota9$long) + 0.1 &
                  y >= min(coords_minnesota9$lat) - 0.1 & y <= max(coords_minnesota9$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota9)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN9, coords_minnesota9, select_df_MN, minnesota9)

# Subset 10
dists <- fields::rdist(coords_df_MN10, coords_minnesota10)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota10$long) - 0.1 & x <= max(coords_minnesota10$long) + 0.1 &
                  y >= min(coords_minnesota10$lat) - 0.1 & y <= max(coords_minnesota10$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota10)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN10, coords_minnesota10, select_df_MN, minnesota10)

# Subset 11
dists <- fields::rdist(coords_df_MN11, coords_minnesota11)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota11$long) - 0.1 & x <= max(coords_minnesota11$long) + 0.1 &
                  y >= min(coords_minnesota11$lat) - 0.1 & y <= max(coords_minnesota11$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota11)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN11, coords_minnesota11, select_df_MN, minnesota11)

# Subset 12
dists <- fields::rdist(coords_df_MN12, coords_minnesota12)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota12$long) - 0.1 & x <= max(coords_minnesota12$long) + 0.1 &
                  y >= min(coords_minnesota12$lat) - 0.1 & y <= max(coords_minnesota12$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota12)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN12, coords_minnesota12, select_df_MN, minnesota12)

# Subset 13
dists <- fields::rdist(coords_df_MN13, coords_minnesota13)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota13$long) - 0.1 & x <= max(coords_minnesota13$long) + 0.1 &
                  y >= min(coords_minnesota13$lat) - 0.1 & y <= max(coords_minnesota13$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota13)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN13, coords_minnesota13, select_df_MN, minnesota13)

# Subset 14
dists <- fields::rdist(coords_df_MN14, coords_minnesota14)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota14$long) - 0.1 & x <= max(coords_minnesota14$long) + 0.1 &
                  y >= min(coords_minnesota14$lat) - 0.1 & y <= max(coords_minnesota14$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota14)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN14, coords_minnesota14, select_df_MN, minnesota14)

# Subset 15
dists <- fields::rdist(coords_df_MN15, coords_minnesota15)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota15$long) - 0.1 & x <= max(coords_minnesota15$long) + 0.1 &
                  y >= min(coords_minnesota15$lat) - 0.1 & y <= max(coords_minnesota15$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota15)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN15, coords_minnesota15, select_df_MN, minnesota15)

# Subset 16
dists <- fields::rdist(coords_df_MN16, coords_minnesota16)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota16$long) - 0.1 & x <= max(coords_minnesota16$long) + 0.1 &
                  y >= min(coords_minnesota16$lat) - 0.1 & y <= max(coords_minnesota16$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota16)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN16, coords_minnesota16, select_df_MN, minnesota16)

# Subset 17
dists <- fields::rdist(coords_df_MN17, coords_minnesota17)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota17$long) - 0.1 & x <= max(coords_minnesota17$long) + 0.1 &
                  y >= min(coords_minnesota17$lat) - 0.1 & y <= max(coords_minnesota17$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota17)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN17, coords_minnesota17, select_df_MN, minnesota17)

# Subset 18
dists <- fields::rdist(coords_df_MN18, coords_minnesota18)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota18$long) - 0.1 & x <= max(coords_minnesota18$long) + 0.1 &
                  y >= min(coords_minnesota18$lat) - 0.1 & y <= max(coords_minnesota18$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota18)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN18, coords_minnesota18, select_df_MN, minnesota18)

# Subset 19
dists <- fields::rdist(coords_df_MN19, coords_minnesota19)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota19$long) - 0.1 & x <= max(coords_minnesota19$long) + 0.1 &
                  y >= min(coords_minnesota19$lat) - 0.1 & y <= max(coords_minnesota19$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota19)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN19, coords_minnesota19, select_df_MN, minnesota19)

# Subset 20
dists <- fields::rdist(coords_df_MN20, coords_minnesota20)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota20$long) - 0.1 & x <= max(coords_minnesota20$long) + 0.1 &
                  y >= min(coords_minnesota20$lat) - 0.1 & y <= max(coords_minnesota20$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota20)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN20, coords_minnesota20, select_df_MN, minnesota20)

# Subset 21
dists <- fields::rdist(coords_df_MN21, coords_minnesota21)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota21$long) - 0.1 & x <= max(coords_minnesota21$long) + 0.1 &
                  y >= min(coords_minnesota21$lat) - 0.1 & y <= max(coords_minnesota21$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota21)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN21, coords_minnesota21, select_df_MN, minnesota21)

# Subset 22
dists <- fields::rdist(coords_df_MN22, coords_minnesota22)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota22$long) - 0.1 & x <= max(coords_minnesota22$long) + 0.1 &
                  y >= min(coords_minnesota22$lat) - 0.1 & y <= max(coords_minnesota22$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota22)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN22, coords_minnesota22, select_df_MN, minnesota22)

# Subset 23
dists <- fields::rdist(coords_df_MN23, coords_minnesota23)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota23$long) - 0.1 & x <= max(coords_minnesota23$long) + 0.1 &
                  y >= min(coords_minnesota23$lat) - 0.1 & y <= max(coords_minnesota23$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota23)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN23, coords_minnesota23, select_df_MN, minnesota23)

# Subset 24
dists <- fields::rdist(coords_df_MN24, coords_minnesota24)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota24$long) - 0.1 & x <= max(coords_minnesota24$long) + 0.1 &
                  y >= min(coords_minnesota24$lat) - 0.1 & y <= max(coords_minnesota24$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota24)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN24, coords_minnesota24, select_df_MN, minnesota24)

# Subset 25
dists <- fields::rdist(coords_df_MN25, coords_minnesota25)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota25$long) - 0.1 & x <= max(coords_minnesota25$long) + 0.1 &
                  y >= min(coords_minnesota25$lat) - 0.1 & y <= max(coords_minnesota25$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota25)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN25, coords_minnesota25, select_df_MN, minnesota25)

# Subset 26
dists <- fields::rdist(coords_df_MN26, coords_minnesota26)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota26$long) - 0.1 & x <= max(coords_minnesota26$long) + 0.1 &
                  y >= min(coords_minnesota26$lat) - 0.1 & y <= max(coords_minnesota26$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota26)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN26, coords_minnesota26, select_df_MN, minnesota26)

# Subset 27
dists <- fields::rdist(coords_df_MN27, coords_minnesota27)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota27$long) - 0.1 & x <= max(coords_minnesota27$long) + 0.1 &
                  y >= min(coords_minnesota27$lat) - 0.1 & y <= max(coords_minnesota27$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota27)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN27, coords_minnesota27, select_df_MN, minnesota27)

# Subset 28
dists <- fields::rdist(coords_df_MN28, coords_minnesota28)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota28$long) - 0.1 & x <= max(coords_minnesota28$long) + 0.1 &
                  y >= min(coords_minnesota28$lat) - 0.1 & y <= max(coords_minnesota28$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota28)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN28, coords_minnesota28, select_df_MN, minnesota28)

# Subset 29
dists <- fields::rdist(coords_df_MN29, coords_minnesota29)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota29$long) - 0.1 & x <= max(coords_minnesota29$long) + 0.1 &
                  y >= min(coords_minnesota29$lat) - 0.1 & y <= max(coords_minnesota29$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota29)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN29, coords_minnesota29, select_df_MN, minnesota29)

# Subset 30
dists <- fields::rdist(coords_df_MN30, coords_minnesota30)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota30$long) - 0.1 & x <= max(coords_minnesota30$long) + 0.1 &
                  y >= min(coords_minnesota30$lat) - 0.1 & y <= max(coords_minnesota30$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota30)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN30, coords_minnesota30, select_df_MN, minnesota30)

# Subset 31
dists <- fields::rdist(coords_df_MN31, coords_minnesota31)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota31$long) - 0.1 & x <= max(coords_minnesota31$long) + 0.1 &
                  y >= min(coords_minnesota31$lat) - 0.1 & y <= max(coords_minnesota31$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota31)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN31, coords_minnesota31, select_df_MN, minnesota31)

# Subset 32
dists <- fields::rdist(coords_df_MN32, coords_minnesota32)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota32$long) - 0.1 & x <= max(coords_minnesota32$long) + 0.1 &
                  y >= min(coords_minnesota32$lat) - 0.1 & y <= max(coords_minnesota32$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota32)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN32, coords_minnesota32, select_df_MN, minnesota32)

# Subset 33
dists <- fields::rdist(coords_df_MN33, coords_minnesota33)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota33$long) - 0.1 & x <= max(coords_minnesota33$long) + 0.1 &
                  y >= min(coords_minnesota33$lat) - 0.1 & y <= max(coords_minnesota33$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota33)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN33, coords_minnesota33, select_df_MN, minnesota33)

# Subset 34
dists <- fields::rdist(coords_df_MN34, coords_minnesota34)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota34$long) - 0.1 & x <= max(coords_minnesota34$long) + 0.1 &
                  y >= min(coords_minnesota34$lat) - 0.1 & y <= max(coords_minnesota34$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota34)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN34, coords_minnesota34, select_df_MN, minnesota34)

# Subset 35
dists <- fields::rdist(coords_df_MN35, coords_minnesota35)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota35$long) - 0.1 & x <= max(coords_minnesota35$long) + 0.1 &
                  y >= min(coords_minnesota35$lat) - 0.1 & y <= max(coords_minnesota35$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota35)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN35, coords_minnesota35, select_df_MN, minnesota35)

# Subset 36
dists <- fields::rdist(coords_df_MN36, coords_minnesota36)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota36$long) - 0.1 & x <= max(coords_minnesota36$long) + 0.1 &
                  y >= min(coords_minnesota36$lat) - 0.1 & y <= max(coords_minnesota36$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota36)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN36, coords_minnesota36, select_df_MN, minnesota36)

# Subset 37
dists <- fields::rdist(coords_df_MN37, coords_minnesota37)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota37$long) - 0.1 & x <= max(coords_minnesota37$long) + 0.1 &
                  y >= min(coords_minnesota37$lat) - 0.1 & y <= max(coords_minnesota37$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota37)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN37, coords_minnesota37, select_df_MN, minnesota37)

# Subset 38
dists <- fields::rdist(coords_df_MN38, coords_minnesota38)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota38$long) - 0.1 & x <= max(coords_minnesota38$long) + 0.1 &
                  y >= min(coords_minnesota38$lat) - 0.1 & y <= max(coords_minnesota38$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota38)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN38, coords_minnesota38, select_df_MN, minnesota38)

# Subset 39
dists <- fields::rdist(coords_df_MN39, coords_minnesota39)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota39$long) - 0.1 & x <= max(coords_minnesota39$long) + 0.1 &
                  y >= min(coords_minnesota39$lat) - 0.1 & y <= max(coords_minnesota39$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota39)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN39, coords_minnesota39, select_df_MN, minnesota39)

# Subset 40
dists <- fields::rdist(coords_df_MN40, coords_minnesota40)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota40$long) - 0.1 & x <= max(coords_minnesota40$long) + 0.1 &
                  y >= min(coords_minnesota40$lat) - 0.1 & y <= max(coords_minnesota40$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota40)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN40, coords_minnesota40, select_df_MN, minnesota40)

# Subset 41
dists <- fields::rdist(coords_df_MN41, coords_minnesota41)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota41$long) - 0.1 & x <= max(coords_minnesota41$long) + 0.1 &
                  y >= min(coords_minnesota41$lat) - 0.1 & y <= max(coords_minnesota41$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota41)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN41, coords_minnesota41, select_df_MN, minnesota41)

# Subset 42
dists <- fields::rdist(coords_df_MN42, coords_minnesota42)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota42$long) - 0.1 & x <= max(coords_minnesota42$long) + 0.1 &
                  y >= min(coords_minnesota42$lat) - 0.1 & y <= max(coords_minnesota42$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota42)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN42, coords_minnesota42, select_df_MN, minnesota42)

# Subset 43
dists <- fields::rdist(coords_df_MN43, coords_minnesota43)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota43$long) - 0.1 & x <= max(coords_minnesota43$long) + 0.1 &
                  y >= min(coords_minnesota43$lat) - 0.1 & y <= max(coords_minnesota43$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota43)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN43, coords_minnesota43, select_df_MN, minnesota43)

# Subset 44
dists <- fields::rdist(coords_df_MN44, coords_minnesota44)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota44$long) - 0.1 & x <= max(coords_minnesota44$long) + 0.1 &
                  y >= min(coords_minnesota44$lat) - 0.1 & y <= max(coords_minnesota44$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota44)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN44, coords_minnesota44, select_df_MN, minnesota44)

# Subset 45
dists <- fields::rdist(coords_df_MN45, coords_minnesota45)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota45$long) - 0.1 & x <= max(coords_minnesota45$long) + 0.1 &
                  y >= min(coords_minnesota45$lat) - 0.1 & y <= max(coords_minnesota45$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota45)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN45, coords_minnesota45, select_df_MN, minnesota45)

# Subset 46
dists <- fields::rdist(coords_df_MN46, coords_minnesota46)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota46$long) - 0.1 & x <= max(coords_minnesota46$long) + 0.1 &
                  y >= min(coords_minnesota46$lat) - 0.1 & y <= max(coords_minnesota46$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota46)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN46, coords_minnesota46, select_df_MN, minnesota46)

# Subset 47
dists <- fields::rdist(coords_df_MN47, coords_minnesota47)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota47$long) - 0.1 & x <= max(coords_minnesota47$long) + 0.1 &
                  y >= min(coords_minnesota47$lat) - 0.1 & y <= max(coords_minnesota47$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota47)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN47, coords_minnesota47, select_df_MN, minnesota47)

# Subset 48
dists <- fields::rdist(coords_df_MN48, coords_minnesota48)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota48$long) - 0.1 & x <= max(coords_minnesota48$long) + 0.1 &
                  y >= min(coords_minnesota48$lat) - 0.1 & y <= max(coords_minnesota48$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota48)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN48, coords_minnesota48, select_df_MN, minnesota48)

# Subset 49
dists <- fields::rdist(coords_df_MN49, coords_minnesota49)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota49$long) - 0.1 & x <= max(coords_minnesota49$long) + 0.1 &
                  y >= min(coords_minnesota49$lat) - 0.1 & y <= max(coords_minnesota49$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota49)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN49, coords_minnesota49, select_df_MN, minnesota49)

# Subset 50
dists <- fields::rdist(coords_df_MN50, coords_minnesota50)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota50$long) - 0.1 & x <= max(coords_minnesota50$long) + 0.1 &
                  y >= min(coords_minnesota50$lat) - 0.1 & y <= max(coords_minnesota50$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota50)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN50, coords_minnesota50, select_df_MN, minnesota50)

# Subset 51
dists <- fields::rdist(coords_df_MN51, coords_minnesota51)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota51$long) - 0.1 & x <= max(coords_minnesota51$long) + 0.1 &
                  y >= min(coords_minnesota51$lat) - 0.1 & y <= max(coords_minnesota51$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota51)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN51, coords_minnesota51, select_df_MN, minnesota51)

# Subset 52
dists <- fields::rdist(coords_df_MN52, coords_minnesota52)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota52$long) - 0.1 & x <= max(coords_minnesota52$long) + 0.1 &
                  y >= min(coords_minnesota52$lat) - 0.1 & y <= max(coords_minnesota52$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota52)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN52, coords_minnesota52, select_df_MN, minnesota52)

# Subset 53
dists <- fields::rdist(coords_df_MN53, coords_minnesota53)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota53$long) - 0.1 & x <= max(coords_minnesota53$long) + 0.1 &
                  y >= min(coords_minnesota53$lat) - 0.1 & y <= max(coords_minnesota53$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota53)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN53, coords_minnesota53, select_df_MN, minnesota53)

# Subset 54
dists <- fields::rdist(coords_df_MN54, coords_minnesota54)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota54$long) - 0.1 & x <= max(coords_minnesota54$long) + 0.1 &
                  y >= min(coords_minnesota54$lat) - 0.1 & y <= max(coords_minnesota54$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota54)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN54, coords_minnesota54, select_df_MN, minnesota54)

# Subset 55
dists <- fields::rdist(coords_df_MN55, coords_minnesota55)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota55$long) - 0.1 & x <= max(coords_minnesota55$long) + 0.1 &
                  y >= min(coords_minnesota55$lat) - 0.1 & y <= max(coords_minnesota55$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota55)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN55, coords_minnesota55, select_df_MN, minnesota55)

# Subset 56
dists <- fields::rdist(coords_df_MN56, coords_minnesota56)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota56$long) - 0.1 & x <= max(coords_minnesota56$long) + 0.1 &
                  y >= min(coords_minnesota56$lat) - 0.1 & y <= max(coords_minnesota56$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota56)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN56, coords_minnesota56, select_df_MN, minnesota56)

# Subset 57
dists <- fields::rdist(coords_df_MN57, coords_minnesota57)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota57$long) - 0.1 & x <= max(coords_minnesota57$long) + 0.1 &
                  y >= min(coords_minnesota57$lat) - 0.1 & y <= max(coords_minnesota57$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota57)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN57, coords_minnesota57, select_df_MN, minnesota57)

# Subset 58
dists <- fields::rdist(coords_df_MN58, coords_minnesota58)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota58$long) - 0.1 & x <= max(coords_minnesota58$long) + 0.1 &
                  y >= min(coords_minnesota58$lat) - 0.1 & y <= max(coords_minnesota58$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota58)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN58, coords_minnesota58, select_df_MN, minnesota58)

# Subset 59
dists <- fields::rdist(coords_df_MN59, coords_minnesota59)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota59$long) - 0.1 & x <= max(coords_minnesota59$long) + 0.1 &
                  y >= min(coords_minnesota59$lat) - 0.1 & y <= max(coords_minnesota59$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota59)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN59, coords_minnesota59, select_df_MN, minnesota59)

# Subset 60
dists <- fields::rdist(coords_df_MN60, coords_minnesota60)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota60$long) - 0.1 & x <= max(coords_minnesota60$long) + 0.1 &
                  y >= min(coords_minnesota60$lat) - 0.1 & y <= max(coords_minnesota60$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota60)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN60, coords_minnesota60, select_df_MN, minnesota60)

# Subset 61
dists <- fields::rdist(coords_df_MN61, coords_minnesota61)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota61$long) - 0.1 & x <= max(coords_minnesota61$long) + 0.1 &
                  y >= min(coords_minnesota61$lat) - 0.1 & y <= max(coords_minnesota61$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota61)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN61, coords_minnesota61, select_df_MN, minnesota61)

# Subset 62
dists <- fields::rdist(coords_df_MN62, coords_minnesota62)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota62$long) - 0.1 & x <= max(coords_minnesota62$long) + 0.1 &
                  y >= min(coords_minnesota62$lat) - 0.1 & y <= max(coords_minnesota62$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota62)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN62, coords_minnesota62, select_df_MN, minnesota62)

# Subset 63
dists <- fields::rdist(coords_df_MN63, coords_minnesota63)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota63$long) - 0.1 & x <= max(coords_minnesota63$long) + 0.1 &
                  y >= min(coords_minnesota63$lat) - 0.1 & y <= max(coords_minnesota63$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota63)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN63, coords_minnesota63, select_df_MN, minnesota63)

# Subset 64
dists <- fields::rdist(coords_df_MN64, coords_minnesota64)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota64$long) - 0.1 & x <= max(coords_minnesota64$long) + 0.1 &
                  y >= min(coords_minnesota64$lat) - 0.1 & y <= max(coords_minnesota64$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota64)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN64, coords_minnesota64, select_df_MN, minnesota64)

# Subset 65
dists <- fields::rdist(coords_df_MN65, coords_minnesota65)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota65$long) - 0.1 & x <= max(coords_minnesota65$long) + 0.1 &
                  y >= min(coords_minnesota65$lat) - 0.1 & y <= max(coords_minnesota65$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota65)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN65, coords_minnesota65, select_df_MN, minnesota65)

# Subset 66
dists <- fields::rdist(coords_df_MN66, coords_minnesota66)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota66$long) - 0.1 & x <= max(coords_minnesota66$long) + 0.1 &
                  y >= min(coords_minnesota66$lat) - 0.1 & y <= max(coords_minnesota66$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota66)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN66, coords_minnesota66, select_df_MN, minnesota66)

# Subset 67
dists <- fields::rdist(coords_df_MN67, coords_minnesota67)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota67$long) - 0.1 & x <= max(coords_minnesota67$long) + 0.1 &
                  y >= min(coords_minnesota67$lat) - 0.1 & y <= max(coords_minnesota67$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota67)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN67, coords_minnesota67, select_df_MN, minnesota67)

# Subset 68
dists <- fields::rdist(coords_df_MN68, coords_minnesota68)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota68$long) - 0.1 & x <= max(coords_minnesota68$long) + 0.1 &
                  y >= min(coords_minnesota68$lat) - 0.1 & y <= max(coords_minnesota68$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota68)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN68, coords_minnesota68, select_df_MN, minnesota68)

# Subset 69
dists <- fields::rdist(coords_df_MN69, coords_minnesota69)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- mn_topo |>
  dplyr::filter(x >= min(coords_minnesota69$long) - 0.1 & x <= max(coords_minnesota69$long) + 0.1 &
                  y >= min(coords_minnesota69$lat) - 0.1 & y <= max(coords_minnesota69$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MN, minnesota69)
MN_topo_pls <- rbind(MN_topo_pls, temp)

rm(closest_points, coords_df_MN69, coords_minnesota69, select_df_MN, minnesota69)

# Update column names
colnames(MN_topo_pls) <- c('topo_x', 'topo_y', 'elevation', 'slope',
                           'aspect', 'state', 'pls_x', 'pls_y',
                           'L1_tree1', 'L1_tree2', 'L1_tree3', 'L1_tree4', 
                           'uniqueID')

# Save MN soil. Confirmed in same order as minnesota and minnesota_ecosystem
MN_topo <- dplyr::select(MN_topo_pls, topo_x, topo_y, elevation, slope, aspect, pls_x, pls_y)
save(MN_topo, file = 'data/processed/topography/processed_topo_mn.RData')

#### WISCONSIN ####
rm(list = ls())

load('data/raw/topography/wi_topo.RData')

load('data/processed/PLS/wisconsin_process.RData')

# Remove negative points because we know land in wisconsin is not below sea level
wi_topo <- dplyr::filter(wi_topo, elevation >= 0)

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
wisconsin30 <- wisconsin[161095:166617,]

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
coords_df_WI1 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin1$long) - 0.1 & x <= max(coords_wisconsin1$long) + 0.1 &
                  y >= min(coords_wisconsin1$lat) - 0.1 & y <= max(coords_wisconsin1$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI2 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin2$long) - 0.1 & x <= max(coords_wisconsin2$long) + 0.1 &
                  y >= min(coords_wisconsin2$lat) - 0.1 & y <= max(coords_wisconsin2$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI3 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin3$long) - 0.1 & x <= max(coords_wisconsin3$long) + 0.1 &
                  y >= min(coords_wisconsin3$lat) - 0.1 & y <= max(coords_wisconsin3$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI4 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin4$long) - 0.1 & x <= max(coords_wisconsin4$long) + 0.1 &
                  y >= min(coords_wisconsin4$lat) - 0.1 & y <= max(coords_wisconsin4$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI5 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin5$long) - 0.1 & x <= max(coords_wisconsin5$long) + 0.1 &
                  y >= min(coords_wisconsin5$lat) - 0.1 & y <= max(coords_wisconsin5$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI6 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin6$long) - 0.1 & x <= max(coords_wisconsin6$long) + 0.1 &
                  y >= min(coords_wisconsin6$lat) - 0.1 & y <= max(coords_wisconsin6$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI7 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin7$long) - 0.1 & x <= max(coords_wisconsin7$long) + 0.1 &
                  y >= min(coords_wisconsin7$lat) - 0.1 & y <= max(coords_wisconsin7$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI8 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin8$long) - 0.1 & x <= max(coords_wisconsin8$long) + 0.1 &
                  y >= min(coords_wisconsin8$lat) - 0.1 & y <= max(coords_wisconsin8$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI9 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin9$long) - 0.1 & x <= max(coords_wisconsin9$long) + 0.1 &
                  y >= min(coords_wisconsin9$lat) - 0.1 & y <= max(coords_wisconsin9$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI10 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin10$long) - 0.1 & x <= max(coords_wisconsin10$long) + 0.1 &
                  y >= min(coords_wisconsin10$lat) - 0.1 & y <= max(coords_wisconsin10$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI11 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin11$long) - 0.1 & x <= max(coords_wisconsin11$long) + 0.1 &
                  y >= min(coords_wisconsin11$lat) - 0.1 & y <= max(coords_wisconsin11$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI12 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin12$long) - 0.1 & x <= max(coords_wisconsin12$long) + 0.1 &
                  y >= min(coords_wisconsin12$lat) - 0.1 & y <= max(coords_wisconsin12$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI13 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin13$long) - 0.1 & x <= max(coords_wisconsin13$long) + 0.1 &
                  y >= min(coords_wisconsin13$lat) - 0.1 & y <= max(coords_wisconsin13$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI14 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin14$long) - 0.1 & x <= max(coords_wisconsin14$long) + 0.1 &
                  y >= min(coords_wisconsin14$lat) - 0.1 & y <= max(coords_wisconsin14$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI15 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin15$long) - 0.1 & x <= max(coords_wisconsin15$long) + 0.1 &
                  y >= min(coords_wisconsin15$lat) - 0.1 & y <= max(coords_wisconsin15$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI16 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin16$long) - 0.1 & x <= max(coords_wisconsin16$long) + 0.1 &
                  y >= min(coords_wisconsin16$lat) - 0.1 & y <= max(coords_wisconsin16$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI17 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin17$long) - 0.1 & x <= max(coords_wisconsin17$long) + 0.1 &
                  y >= min(coords_wisconsin17$lat) - 0.1 & y <= max(coords_wisconsin17$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI18 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin18$long) - 0.1 & x <= max(coords_wisconsin18$long) + 0.1 &
                  y >= min(coords_wisconsin18$lat) - 0.1 & y <= max(coords_wisconsin18$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI19 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin19$long) - 0.1 & x <= max(coords_wisconsin19$long) + 0.1 &
                  y >= min(coords_wisconsin19$lat) - 0.1 & y <= max(coords_wisconsin19$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI20 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin20$long) - 0.1 & x <= max(coords_wisconsin20$long) + 0.1 &
                  y >= min(coords_wisconsin20$lat) - 0.1 & y <= max(coords_wisconsin20$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI21 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin21$long) - 0.1 & x <= max(coords_wisconsin21$long) + 0.1 &
                  y >= min(coords_wisconsin21$lat) - 0.1 & y <= max(coords_wisconsin21$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI22 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin22$long) - 0.1 & x <= max(coords_wisconsin22$long) + 0.1 &
                  y >= min(coords_wisconsin22$lat) - 0.1 & y <= max(coords_wisconsin22$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI23 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin23$long) - 0.1 & x <= max(coords_wisconsin23$long + 0.1) &
                  y >= min(coords_wisconsin23$lat) - 0.1 & y <= max(coords_wisconsin23$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI24 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin24$long) - 0.1 & x <= max(coords_wisconsin24$long) + 0.1 &
                  y >= min(coords_wisconsin24$lat) - 0.1 & y <= max(coords_wisconsin24$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI25 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin25$long) - 0.1 & x <= max(coords_wisconsin25$long) + 0.1 &
                  y >= min(coords_wisconsin25$lat) - 0.1 & y <= max(coords_wisconsin25$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI26 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin26$long) - 0.1 & x <= max(coords_wisconsin26$long) + 0.1 &
                  y >= min(coords_wisconsin26$lat) - 0.1 & y <= max(coords_wisconsin26$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI27 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin27$long) - 0.1 & x <= max(coords_wisconsin27$long) + 0.1 &
                  y >= min(coords_wisconsin27$lat) - 0.1 & y <= max(coords_wisconsin27$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI28 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin28$long) - 0.1 & x <= max(coords_wisconsin28$long) + 0.1 &
                  y >= min(coords_wisconsin28$lat) - 0.1 & y <= max(coords_wisconsin28$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI29 <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin29$long) - 0.1 & x <= max(coords_wisconsin29$long) + 0.1 &
                  y >= min(coords_wisconsin29$lat) - 0.1 & y <= max(coords_wisconsin29$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_WI30 <- wi_topo |>
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
select_df_WI <- wi_topo |>
  # Filter as done above to make sure indexing matches
  dplyr::filter(x >= min(coords_wisconsin1$long) - 0.1 & x <= max(coords_wisconsin1$long) + 0.1 &
                  y >= min(coords_wisconsin1$lat) - 0.1 & y <= max(coords_wisconsin1$lat) + 0.1) |>
  # subset by row numbers given by which.min
  dplyr::slice(closest_points)

# Column bind with PLS data
WI_topo_pls <- cbind(select_df_WI, wisconsin1)

# Remove unnecessary objects to keep memory usage lower
rm(closest_points, coords_df_WI1, coords_wisconsin1, select_df_WI, wisconsin1)

# Repeat all these steps for the remaining 29 subsets

# Subset 2
dists <- fields::rdist(coords_df_WI2, coords_wisconsin2)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin2$long) - 0.1 & x <= max(coords_wisconsin2$long) + 0.1 &
                  y >= min(coords_wisconsin2$lat) - 0.1 & y <= max(coords_wisconsin2$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin2)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI2, coords_wisconsin2, select_df_WI, wisconsin2)

# Subset 3
dists <- fields::rdist(coords_df_WI3, coords_wisconsin3)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin3$long) - 0.1 & x <= max(coords_wisconsin3$long) + 0.1 &
                  y >= min(coords_wisconsin3$lat) - 0.1 & y <= max(coords_wisconsin3$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin3)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI3, coords_wisconsin3, select_df_WI, wisconsin3)

# Subset 4
dists <- fields::rdist(coords_df_WI4, coords_wisconsin4)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin4$long) - 0.1 & x <= max(coords_wisconsin4$long) + 0.1 &
                  y >= min(coords_wisconsin4$lat) - 0.1 & y <= max(coords_wisconsin4$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin4)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI4, coords_wisconsin4, select_df_WI, wisconsin4)

# Subset 5
dists <- fields::rdist(coords_df_WI5, coords_wisconsin5)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin5$long) - 0.1 & x <= max(coords_wisconsin5$long) + 0.1 &
                  y >= min(coords_wisconsin5$lat) - 0.1 & y <= max(coords_wisconsin5$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin5)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI5, coords_wisconsin5, select_df_WI, wisconsin5)

# Subset 6
dists <- fields::rdist(coords_df_WI6, coords_wisconsin6)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin6$long) - 0.1 & x <= max(coords_wisconsin6$long) + 0.1 &
                  y >= min(coords_wisconsin6$lat) - 0.1 & y <= max(coords_wisconsin6$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin6)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI6, coords_wisconsin6, select_df_WI, wisconsin6)

# Subset 7
dists <- fields::rdist(coords_df_WI7, coords_wisconsin7)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin7$long) - 0.1 & x <= max(coords_wisconsin7$long) + 0.1 &
                  y >= min(coords_wisconsin7$lat) - 0.1 & y <= max(coords_wisconsin7$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin7)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI7, coords_wisconsin7, select_df_WI, wisconsin7)

# Subset 8
dists <- fields::rdist(coords_df_WI8, coords_wisconsin8)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin8$long) - 0.1 & x <= max(coords_wisconsin8$long) + 0.1 &
                  y >= min(coords_wisconsin8$lat) - 0.1 & y <= max(coords_wisconsin8$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin8)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI8, coords_wisconsin8, select_df_WI, wisconsin8)

# Subset 9
dists <- fields::rdist(coords_df_WI9, coords_wisconsin9)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin9$long) - 0.1 & x <= max(coords_wisconsin9$long) + 0.1 &
                  y >= min(coords_wisconsin9$lat) - 0.1 & y <= max(coords_wisconsin9$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin9)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI9, coords_wisconsin9, select_df_WI, wisconsin9)

# Subset 10
dists <- fields::rdist(coords_df_WI10, coords_wisconsin10)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin10$long) - 0.1 & x <= max(coords_wisconsin10$long) + 0.1 &
                  y >= min(coords_wisconsin10$lat) - 0.1 & y <= max(coords_wisconsin10$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin10)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI10, coords_wisconsin10, select_df_WI, wisconsin10)

# Subset 11
dists <- fields::rdist(coords_df_WI11, coords_wisconsin11)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin11$long) - 0.1 & x <= max(coords_wisconsin11$long) + 0.1 &
                  y >= min(coords_wisconsin11$lat) - 0.1 & y <= max(coords_wisconsin11$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin11)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI11, coords_wisconsin11, select_df_WI, wisconsin11)

# Subset 12
dists <-fields::rdist(coords_df_WI12, coords_wisconsin12)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin12$long) - 0.1 & x <= max(coords_wisconsin12$long) + 0.1 &
                  y >= min(coords_wisconsin12$lat) - 0.1 & y <= max(coords_wisconsin12$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin12)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI12, coords_wisconsin12, select_df_WI, wisconsin12)

# Subset 13
dists <- fields::rdist(coords_df_WI13, coords_wisconsin13)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin13$long) - 0.1 & x <= max(coords_wisconsin13$long) + 0.1 &
                  y >= min(coords_wisconsin13$lat) - 0.1 & y <= max(coords_wisconsin13$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin13)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI13, coords_wisconsin13, select_df_WI, wisconsin13)

# Subset 14
dists <- fields::rdist(coords_df_WI14, coords_wisconsin14)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin14$long) - 0.1 & x <= max(coords_wisconsin14$long) + 0.1 &
                  y >= min(coords_wisconsin14$lat) - 0.1 & y <= max(coords_wisconsin14$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin14)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI14, coords_wisconsin14, select_df_WI, wisconsin14)

# Subset 15
dists <- fields::rdist(coords_df_WI15, coords_wisconsin15)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin15$long) - 0.1 & x <= max(coords_wisconsin15$long) + 0.1 &
                  y >= min(coords_wisconsin15$lat) - 0.1 & y <= max(coords_wisconsin15$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin15)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI15, coords_wisconsin15, select_df_WI, wisconsin15)

# Subset 16
dists <- fields::rdist(coords_df_WI16, coords_wisconsin16)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin16$long) - 0.1 & x <= max(coords_wisconsin16$long) + 0.1 &
                  y >= min(coords_wisconsin16$lat) - 0.1 & y <= max(coords_wisconsin16$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin16)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI16, coords_wisconsin16, select_df_WI, wisconsin16)

# Subset 17
dists <- fields::rdist(coords_df_WI17, coords_wisconsin17)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin17$long) - 0.1 & x <= max(coords_wisconsin17$long) + 0.1 &
                  y >= min(coords_wisconsin17$lat) - 0.1 & y <= max(coords_wisconsin17$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin17)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI17, coords_wisconsin17, select_df_WI, wisconsin17)

# Subset 18
dists <- fields::rdist(coords_df_WI18, coords_wisconsin18)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin18$long) - 0.1 & x <= max(coords_wisconsin18$long) + 0.1 &
                  y >= min(coords_wisconsin18$lat) - 0.1 & y <= max(coords_wisconsin18$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin18)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI18, coords_wisconsin18, select_df_WI, wisconsin18)

# Subset 19
dists <- fields::rdist(coords_df_WI19, coords_wisconsin19)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin19$long) - 0.1 & x <= max(coords_wisconsin19$long) + 0.1 &
                  y >= min(coords_wisconsin19$lat) - 0.1 & y <= max(coords_wisconsin19$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin19)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI19, coords_wisconsin19, select_df_WI, wisconsin19)

# Subset 20
dists <- fields::rdist(coords_df_WI20, coords_wisconsin20)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin20$long) - 0.1 & x <= max(coords_wisconsin20$long) + 0.1 &
                  y >= min(coords_wisconsin20$lat) - 0.1 & y <= max(coords_wisconsin20$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin20)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI20, coords_wisconsin20, select_df_WI, wisconsin20)

# Subset 21
dists <- fields::rdist(coords_df_WI21, coords_wisconsin21)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin21$long) - 0.1 & x <= max(coords_wisconsin21$long) + 0.1 &
                  y >= min(coords_wisconsin21$lat) - 0.1 & y <= max(coords_wisconsin21$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin21)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI21, coords_wisconsin21, select_df_WI, wisconsin21)

# Subset 22
dists <- fields::rdist(coords_df_WI22, coords_wisconsin22)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin22$long) - 0.1 & x <= max(coords_wisconsin22$long) + 0.1 &
                  y >= min(coords_wisconsin22$lat) - 0.1 & y <= max(coords_wisconsin22$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin22)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI22, coords_wisconsin22, select_df_WI, wisconsin22)

# Subset 23
dists <- fields::rdist(coords_df_WI23, coords_wisconsin23)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin23$long) - 0.1 & x <= max(coords_wisconsin23$long) + 0.1 &
                  y >= min(coords_wisconsin23$lat) - 0.1 & y <= max(coords_wisconsin23$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin23)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI23, coords_wisconsin23, select_df_WI, wisconsin23)

# Subset 24
dists <- fields::rdist(coords_df_WI24, coords_wisconsin24)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin24$long) - 0.1 & x <= max(coords_wisconsin24$long) + 0.1 &
                  y >= min(coords_wisconsin24$lat) - 0.1 & y <= max(coords_wisconsin24$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin24)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI24, coords_wisconsin24, select_df_WI, wisconsin24)

# Subset 25
dists <- fields::rdist(coords_df_WI25, coords_wisconsin25)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin25$long) - 0.1 & x <= max(coords_wisconsin25$long) + 0.1 &
                  y >= min(coords_wisconsin25$lat) - 0.1 & y <= max(coords_wisconsin25$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin25)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI25, coords_wisconsin25, select_df_WI, wisconsin25)

# Subset 26
dists <- fields::rdist(coords_df_WI26, coords_wisconsin26)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin26$long) - 0.1 & x <= max(coords_wisconsin26$long) + 0.1 &
                  y >= min(coords_wisconsin26$lat) - 0.1 & y <= max(coords_wisconsin26$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin26)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI26, coords_wisconsin26, select_df_WI, wisconsin26)

# Subset 27
dists <- fields::rdist(coords_df_WI27, coords_wisconsin27)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin27$long) - 0.1 & x <= max(coords_wisconsin27$long) + 0.1 &
                  y >= min(coords_wisconsin27$lat) - 0.1 & y <= max(coords_wisconsin27$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin27)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI27, coords_wisconsin27, select_df_WI, wisconsin27)

# Subset 28
dists <- fields::rdist(coords_df_WI28, coords_wisconsin28)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin28$long) - 0.1 & x <= max(coords_wisconsin28$long) + 0.1 &
                  y >= min(coords_wisconsin28$lat) - 0.1 & y <= max(coords_wisconsin28$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin28)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI28, coords_wisconsin28, select_df_WI, wisconsin28)

# Subset 29
dists <- fields::rdist(coords_df_WI29, coords_wisconsin29)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin29$long) - 0.1 & x <= max(coords_wisconsin29$long) + 0.1 &
                  y >= min(coords_wisconsin29$lat) - 0.1 & y <= max(coords_wisconsin29$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin29)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI29, coords_wisconsin29, select_df_WI, wisconsin29)

# Subset 30
dists <- fields::rdist(coords_df_WI30, coords_wisconsin30)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- wi_topo |>
  dplyr::filter(x >= min(coords_wisconsin30$long) - 0.1 & x <= max(coords_wisconsin30$long) + 0.1 &
                  y >= min(coords_wisconsin30$lat) - 0.1 & y <= max(coords_wisconsin30$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_WI, wisconsin30)
WI_topo_pls <- rbind(WI_topo_pls, temp)

rm(closest_points, coords_df_WI30, coords_wisconsin30, select_df_WI, wisconsin30)

# Update column names
colnames(WI_topo_pls) <- c('topo_x', 'topo_y', 'elevation', 'slope', 
                           'aspect', 'state', 'pls_x', 'pls_y', 
                           'L1_tree1', 'L1_tree2', 'L1_tree3', 'L1_tree4', 
                           'uniqueID')

# Save WI topo Confirmed in same order as wisconsin and wisconsin_ecosystem
WI_topo <- dplyr::select(WI_topo_pls, topo_x, topo_y, elevation, slope, aspect, pls_x, pls_y)
save(WI_topo, file = 'data/processed/topography/processed_topo_wi.RData')

#### CHECKS ####

# Check to make sure we're not too far off of the 1:1 line with any of our coordinate matching

rm(list = ls())

load('data/processed/topography/processed_topo_il.RData')

IL_topo |>
  ggplot2::ggplot(ggplot2::aes(x = pls_x, y = topo_x)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()
IL_topo |>
  ggplot2::ggplot(ggplot2::aes(x = pls_y, y = topo_y)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

IL_topo |>
  dplyr::mutate(deviance_x = abs(pls_x - topo_x),
                deviance_y = abs(pls_y - topo_y)) |>
  dplyr::summarize(mean_deviance_x = mean(deviance_x),
                   mean_deviance_y = mean(deviance_y),
                   max_deviance_x = max(deviance_x),
                   max_deviance_y = max(deviance_y),
                   med_deviance_x = median(deviance_x),
                   med_deviance_y = median(deviance_y))

load('data/processed/topography/processed_topo_in.RData')

IN_topo |>
  ggplot2::ggplot(ggplot2::aes(x = pls_x, y = topo_x)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

IN_topo |>
  ggplot2::ggplot(ggplot2::aes(x = pls_y, y = topo_y)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

IN_topo |>
  dplyr::mutate(deviance_x = abs(pls_x - topo_x),
                deviance_y = abs(pls_y - topo_y)) |>
  dplyr::summarize(mean_deviance_x = mean(deviance_x),
                   mean_deviance_y = mean(deviance_y),
                   max_deviance_x = max(deviance_x),
                   max_deviance_y = max(deviance_y),
                   med_deviance_x = median(deviance_x),
                   med_deviance_y = median(deviance_y))

load('data/processed/topography/processed_topo_mi.RData')

MI_topo |>
  ggplot2::ggplot(ggplot2::aes(x = pls_x, y = topo_x)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

MI_topo |>
  ggplot2::ggplot(ggplot2::aes(x = pls_y, y = topo_y)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

MI_topo |>
  dplyr::mutate(deviance_x = abs(pls_x - topo_x),
                deviance_y = abs(pls_y - topo_y)) |>
  dplyr::summarize(mean_deviance_x = mean(deviance_x),
                   mean_deviance_y = mean(deviance_y),
                   max_deviance_x = max(deviance_x),
                   max_deviance_y = max(deviance_y),
                   med_deviance_x = median(deviance_x),
                   med_deviance_y = median(deviance_y))

load('data/processed/topography/processed_topo_mn.RData')

MN_topo |>
  ggplot2::ggplot(ggplot2::aes(x = pls_x, y = topo_x)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

MN_topo |>
  ggplot2::ggplot(ggplot2::aes(x = pls_y, y = topo_y)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

MN_topo |>
  dplyr::mutate(deviance_x = abs(pls_x - topo_x),
                deviance_y = abs(pls_y - topo_y)) |>
  dplyr::summarize(mean_deviance_x = mean(deviance_x),
                   mean_deviance_y = mean(deviance_y),
                   max_deviance_x = max(deviance_x),
                   max_deviance_y = max(deviance_y),
                   med_deviance_x = median(deviance_x),
                   med_deviance_y = median(deviance_y))

load('data/processed/topography/processed_topo_wi.RData')

WI_topo |>
  ggplot2::ggplot(ggplot2::aes(x = pls_x, y = topo_x)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

WI_topo |>
  ggplot2::ggplot(ggplot2::aes(x = pls_y, y = topo_y)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

WI_topo |>
  dplyr::mutate(deviance_x = abs(pls_x - topo_x),
                deviance_y = abs(pls_y - topo_y)) |>
  dplyr::summarize(mean_deviance_x = mean(deviance_x),
                   mean_deviance_y = mean(deviance_y),
                   max_deviance_x = max(deviance_x),
                   max_deviance_y = max(deviance_y),
                   med_deviance_x = median(deviance_x),
                   med_deviance_y = median(deviance_y))

# Plot distribution of soil variables
states <- sf::st_as_sf(maps::map('state', region = c('indiana', 'illinois', 'michigan',
                                                     'minnesota', 'wisconsin'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

topo <- rbind(IL_topo, IN_topo, MI_topo, MN_topo, WI_topo)

topo |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = elevation), alpha = 0.7, shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

topo |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = slope), alpha = 0.7, shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

topo |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = aspect), alpha = 0.7, shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

