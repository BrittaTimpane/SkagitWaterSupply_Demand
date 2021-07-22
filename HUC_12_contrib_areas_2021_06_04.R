#Install packages
library(purrr)

# Read dbf file in shapefile

df <- read.csv ("data/input/Flow_Accumulation_28.03.2021.csv")
df$HUC12 <- as.character(df$HUC12)

# Get list of HUCs
all_hucs <- unique(df$HUC12)


# Replace data with data from dams
huc_w_dams <- c('171100050708', '171100051007')
df[df$HUC12 %in% huc_w_dams, 'Total_Med_Vol'] <- c(3091988, 1906057)


# Return contributing sheds for a given HUC
find_contrib_HUCs <- function(HUC_code){

  if(HUC_code %in% huc_w_dams){
    NULL
  } else {
    df[df$TOHUC == HUC_code, 'HUC12']
  }

}



# Iteratively search for contributing HUCs
find_contrib_vol <- function(df, HUC_code){
  x <- HUC_code
  y <- x

  while (length(y) > 0) {
    y <- unlist(map(y, find_contrib_HUCs))
    x <- c(x, y)
  }

  data.frame(HUC12 = HUC_code,
             Total_Accum_Vol = sum(df[df$HUC12 %in% x, 'Total_Med_Vol']),
             contrib_HUCs_count = length(x),
             contrib_HUCs = paste(x, collapse = '-')
  )
}


df_out <- purrr::map_dfr(all_hucs, ~find_contrib_vol(df, .x))


write.csv(df_out,'data/output/HUC_12_contrib_Volumes_21.07.2021.csv')
