######################################################################################################
library(gdalUtilities)
library(raster)
library(tidyverse)
library(sf)
library(lubridate)
library(here)

use.heath <- "y"
#paste0("BF", YEAR1, "-", str_replace(NUMBER, "_", ""))
dates <- read.csv(here::here("inputs",  "clean_dates_edited.csv")) 
colnames(dates)[1] <- "BURNID"
dates<-   mutate(dates, BURNID = str_replace(BURNID, "_", ""), mapText = "") %>%
  rename(burnName = NAME) %>%
  mutate(pageNumber = paste0(BURNID, "_", burnName)) %>%
  dplyr::select(BURNID, pageNumber, mapText, burnName)

dates$pageNumber <- str_replace_all(dates$pageNumber, " ", "")
dates$pageNumber <- str_replace_all(dates$pageNumber, "\\(", "-")
dates$pageNumber <- str_replace_all(dates$pageNumber, "\\)", "")
dates$pageNumber <- str_replace_all(dates$pageNumber, "\\/", "-")
dates$pageNumber <- str_replace_all(dates$pageNumber, ",", "")

dir.create(here("maps"), showWarnings = FALSE)

mdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\sevSentinel\\xModels\\"
shp <- st_read(paste0(mdir, "Template_AFED\\Template_AFED.shp"))[0,]
burnt.shp <- shp #dplyr::select(shp, -BURNID)

all.tifs <- NA
df.stat <- data.frame()
i <- 1
# for (i in 1:length(region)){
#   foldr <- here(region[i])
  fold2 <- list.dirs(here(), recursive = FALSE)
  if (use.heath == "y"){
    fold2 <- fold2[str_detect(fold2, "/v2")]
    vs <- sort(ymd(str_split_fixed(fold2, "/v", 2)[,2]))
  }else{
    fold2 <- fold2[str_detect(fold2, "/nh2")]
    vs <- sort(ymd(str_split_fixed(fold2, "/nh", 2)[,2]))
  }
  fold2 <- fold2[str_detect(fold2, as.character(vs[length(vs)]))]
  

  tifs <- as.data.frame(list.files(paste0(fold2, "/severity_geoTifs"), pattern = "tif$", full.names = TRUE))
  colnames(tifs) <- "file"
  tifs <- tifs %>% mutate(BURNID = str_split_fixed(str_split_fixed(file, "BurnSeverity_", 2)[,2], "_", 2)[,1]) 
  

 
  #csvs <- list.files(paste0(fold2, "/severity_stats"), pattern = "csv$", full.names = TRUE)
  df.stat <- read.csv(paste0(fold2, "/severity_stats/summaryStats.csv"))
 
  #df.stat <- bind_rows(df.stat, csv)
  shp.name <- list.files(paste0(fold2, "/treatment_area"), pattern = ".shp$", full.names = TRUE) 
  shpi <- st_read(shp.name)%>%
    st_transform(crs = st_crs(shp))
  shp <- rbind(shp, shpi)

plot(shp[,2])
plot(burnt.shp[,2])

shpj <- shp %>% dplyr::select(-date) %>%
  left_join(df.stat, by = "BURNID") %>%
  left_join(dates, by = "BURNID" ) 

df.txt <- shpj %>% st_drop_geometry() %>%
  dplyr::select(BURNID, burnName, pageNumber, mapText)

#shp.m <- st_read(list.files(here("multiSeason", "treatment_area"), full.names = TRUE, pattern = "shp$") )

shpf <- shpj %>% dplyr::select(BURNID, X1, X2, X3, X4, X5, X6, date) %>%
  #filter((BURNID %in% shp.m$BURNID) == FALSE) %>%
  #rbind(st_transform(shp.m, crs = crs(shp))) %>%
  left_join(df.txt, by = "BURNID")

folder.name <- str_split_fixed(here(), "sevSentinel/", 2)[,2]
#shpf$imLast <- NA

csvs <- list.files(here("tifs"), pattern = "imgUsed.csv", recursive = TRUE, full.names = TRUE)

imgUsed <- read.csv(csvs[1]) %>%
  dplyr::select(file, date, type)
if(length(csvs) >1){
for(i in 2:length(csvs)){
  imgUsed.i <- read.csv(csvs[i]) %>%
    dplyr::select(file, date, type)
  imgUsed <- bind_rows(imgUsed, imgUsed.i)
}
}
imgUsed <- imgUsed %>% mutate(BURNID = str_split_fixed(file, "_", 4)[,1]) %>%
  group_by(BURNID) %>%
  summarise(date = max(ymd(date))) %>%
  rename(imLast = date)

shpf <- left_join(shpf, imgUsed, by = "BURNID")
#for (i in 1:length(region)){
  # rgb.f <- list.dirs(here("all_rgbs"), recursive = FALSE)[-1]
  # rgb.f <- rgb.f[str_detect(rgb.f, "_InvestigateFurther") == FALSE]
  # rgb.f <- rgb.f[str_detect(rgb.f, "_NoFire") == FALSE]
  # if (length(rgb.f)!=0){
  #   for (j in 1:length(rgb.f)){
  #     pngs <- list.files(rgb.f[j], pattern = "png$", recursive = TRUE)
  #     pngs <- str_replace(pngs, "remove/", "")
  #     mxdate <- max(ymd(str_split_fixed(pngs, "_", 4)[,2]))+1
  #     id <- unique(str_split_fixed(pngs, "_", 4)[,1])
  #     shpf$imLast[which(shpf$BURNID == id)] <- as.character(mxdate)
  #   }
  # }
#}
dir.create(here("burntArea"), showWarnings = FALSE)
st_write(burnt.shp, here("burntArea", paste0(folder.name, "_burntArea_", Sys.Date(), ".shp")), append=FALSE)

shpf$date <- ymd(shpf$date)
shpf <- st_make_valid(shpf)
shpf$Hectares <- round(as.numeric(st_area(shpf))/10000, 0)
st_write(shpf, here("maps", paste0(folder.name, "_mapOutput_", Sys.Date(), ".shp")), append=FALSE)

#all.tifs <- na.omit(tifs)

## check projection

rst.tmp <- raster(tifs$file[1])
for(i in 1:length(tifs$file)){
  rst.i <- raster(tifs$file[i]) %>%
    projectRaster(crs = crs(rst.tmp), method = "ngb")
  writeRaster(rst.i, tifs$file[i], overwrite=TRUE)
}

tif.vrt <-gdalbuildvrt(gdalfile = tifs$file, output.vrt = here("maps", "output.vrt"))

ouput.rst<-raster::raster(tif.vrt)
if (use.heath == "y"){
 writeRaster(ouput.rst, here("maps", paste0(folder.name, "_mosaic_", Sys.Date(), ".tif")), format='GTiff', overwrite=TRUE)
  }else{
  writeRaster(ouput.rst, here("maps", paste0(folder.name, "_noHeath_", Sys.Date(), ".tif")), format='GTiff', overwrite=TRUE)
}
