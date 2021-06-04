#Instal packages
install.packages("dplyr")
install.packages("purrr")
library(purrr)

# Read dbf file in shapefile
#df <- foreign::read.dbf("C://01_Projects/NWFSC_misc/Britta/HUC_12_SkagitWater_boundary_correct/HUC_12_SkagitWater_boundary_correct.dbf")
df <- read.csv ("D:/R/SkagitWaterSupply_Demand/data/input.csv")
df$HUC12 <- as.character(df$HUC12)

# Get list of HUCs
all_hucs <- unique(df$HUC12)



# Return contributing sheds for a given HUC
find_contrib_HUCs <- function(HUC_code){
  df[df$TOHUC == HUC_code, 'HUC12']
}


# Iteratively search for contributing HUCs
#find_contrib_area <- function(df, HUC_code){
find_contrib_vol <- function(df, HUC_code){
  x <- HUC_code
  y <- x

  while (length(y) > 0) {
    y <- unlist(map(y, find_contrib_HUCs))
    x <- c(x, y)
  }

  data.frame(HUC12 = HUC_code,
             #Total_area = sum(df[df$HUC12 %in% x, 'AREASQKM']),
             Total_Accum_Vol = sum(df[df$HUC12 %in% x, 'Total_Med_Vol']),
             contrib_HUCs_count = length(x),
             contrib_HUCs = paste(x, collapse = '-')
  )
}


#df_out <- purrr::map_dfr(all_hucs, ~find_contrib_area(df, .x))
df_out <- purrr::map_dfr(all_hucs, ~find_contrib_vol(df, .x))


write.csv(df_out,'D:/R/SkagitWaterSupply_Demand/data/output/HUC_12_contrib_Volumes.csv')

