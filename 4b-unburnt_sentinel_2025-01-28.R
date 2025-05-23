# updated: 3/2/2023
# buffer calculation corrected

library(raster)
library(tidyverse)
library(sf)
library(lubridate)
library(fasterize)
library(lwgeom)
library(doParallel)
library(here)

v <- paste0("v", Sys.Date())
#v <- "v2023-02-20"

lshp <- list.files(here::here("inputs", "shpByBurn"), pattern = "shp$", full.names = TRUE)
shp <- st_read(lshp[1], stringsAsFactors = FALSE, quiet = TRUE)

shpx <- st_read("M:\\Zdrive\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\sevSentinel\\xIndex\\DBCA_FireHistory_2017-2023_Id.shp", quiet = TRUE) %>%
   st_transform("+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs") %>%
  rename(BURNID = id)

dates <- read.csv(here("inputs", "clean_dates_edited.csv")) %>%
  rename(start = date, end = date_end)

shp.tmp <- st_read("Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\sevSentinel\\xModels\\Template_AFED\\Template_AFED.shp")
shp.tmp[1,1] <- NA

tlist <- as.data.frame(list.files(here("dNBR"), pattern = ".tiff$"))
colnames(tlist) <- "tif"
tlist <- mutate(tlist, Burn.Id = str_split_fixed(tif, "_", 2)[,1])

plist <- str_split_fixed(list.dirs(here("all_rgbs"), 
                                   full.names = FALSE, recursive = FALSE), "_", 2)[,2] 

burns <- tlist$Burn.Id[tlist$Burn.Id %in% plist]
#burns <- "BWD-2017-52569681"

rst.per <-raster("M:\\Zdrive\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\vegMask\\perenialVeg\\rem_Woody_veg_2020.tif")

i <- 2


#Define how many cores (memory is limiting factor here)
UseCores <- 10
#Register CoreCluster
cl <- makeCluster(UseCores)
registerDoParallel(cl)
foreach(i = 1:length(burns)) %dopar% {
  library(raster)
  library(tidyverse)
  library(sf)
  library(lubridate)
  library(fasterize)
  library(lwgeom)
  library(here)
  
  t.burn <- filter(tlist, Burn.Id == burns[i])
  
  burn.shp <- st_read(here::here("inputs", "shpByBurn", paste0(burns[i], "_boundry.shp")), 
                      quiet = TRUE)%>%
    st_make_valid()
  plot(burn.shp[,1])
  yr <- year(burn.shp$date)
  
  burn.shpx <- filter(shpx, FIH_YEAR1 == yr | FIH_YEAR1 == yr-1 | FIH_YEAR1 == yr+1)
  burn.shpx <- filter(burn.shpx, BURNID != burns[i]) %>%
    dplyr::select(BURNID)
  burn.shpx <- rbind(burn.shpx, st_transform(dplyr::select(burn.shp, BURNID), crs(burn.shpx) )) %>%
    st_cast("MULTIPOLYGON")
  
  if (dir.exists(here("notVeg"))){
    noVeg.shps.list <- list.files(here("notVeg"), pattern = "shp$", full.names = TRUE)
    noVeg.shps <- st_read(noVeg.shps.list[1])
    plot(noVeg.shps[,1])
    if(("BURNID" %in% colnames(noVeg.shps))==FALSE){
      noVeg.shps$BURNID <- burns[i]
    }else{
      noVeg.shpsF <- filter(noVeg.shps, BURNID == burns[i])
      if(nrow(noVeg.shpsF) != 0){
        noVeg.shps <- filter(noVeg.shps, BURNID == burns[i])
      }else(
        noVeg.shps <- filter(noVeg.shps, is.na(BURNID))
      )
    }
    
    #plot(noVeg.shps[,1])
    burn.shpx <- rbind(burn.shpx, st_transform(dplyr::select(noVeg.shps, BURNID), crs(burn.shpx) )) %>%
      st_cast("MULTIPOLYGON")
  }
  
  burn.shpx$n <- 2
  shp.buf <- st_buffer(burn.shp, dist= 240)
  
  dnbr <- raster(here("dNBR", t.burn$tif[1])) %>%
    mask(mask = shp.buf) %>%
    crop(shp.buf)
  plot(dnbr)
  
  unburnt.rst <- fasterize(burn.shpx, dnbr, field = "n")
  unburnt.rst[is.na(unburnt.rst)] <- 1
  unburnt.rst[unburnt.rst == 2] <- NA
  plot(unburnt.rst)#was burn.rst

  dnbr.ub <- dnbr * unburnt.rst
  
 
  
  
  
  
  
  
  
  
  
  
  
  per.i <- crop(rst.per, st_buffer(st_transform(burn.shp, crs(rst.per)), 240))
  per.i <- projectRaster(per.i, dnbr, method = "ngb")

  
  dnbr.ub <- dnbr.ub * per.i
  plot(dnbr.ub)
 
  burn.rst.buf <- dnbr.ub
  burn.rst.buf[is.na(burn.rst.buf)==FALSE] <- 1
  
  buff.pix.predict <- ((sum(as.numeric(st_area(st_buffer(burn.shp, 240)))) - sum(as.numeric(st_area(burn.shp))))/10000) * 25
  
  if(nrow(freq(burn.rst.buf))==1){
    buff.pix.actual <- 1
  }else{
    buff.pix.actual <- as.numeric(freq(burn.rst.buf)[1,2])
  }
  
  # check if there is more than 5% of the burn perimeter that was used
  if((buff.pix.actual/buff.pix.predict)>0.01){
    q <- quantile(dnbr.ub, probs = seq(0, 1, 0.10))
    threshold <- q[10]
    buf.dif <- q[10]
    if (threshold > 0.05){
      threshold <- 0.05
    }
    unburnt <- dnbr 
    unburnt[unburnt>threshold] <- NA
    unburnt[unburnt<=threshold] <- 1
    cat("buffer difference for", burns[i], " = ", buf.dif, "\n")
  }else{
    unburnt <- dnbr 
    threshold <- 0.05
    buf.dif <- 0.05
    unburnt[unburnt>threshold] <- NA
    unburnt[unburnt<=threshold] <- 1
    cat("unburnt threshold for", burns[i], " = 0.05\n")
  }
  
  ub.stats <- data.frame(BURNID = as.character(), threshold = as.numeric())[1, ]
  ub.stats$BURNID[1] <- burns[i]
  ub.stats$threshold[1] <- buf.dif
  saveRDS(ub.stats, here("tmp", paste0("ub_", burns[i])))
  
  dir.create(here("bufferStats"), showWarnings = FALSE)
  dir.create(here("bufferStats", "figures"), showWarnings = FALSE)
  png(here::here("bufferStats", "figures", paste0(burns[i], ".png")), 
      width = 550, height = 550)
  
  plot(dnbr.ub, main = paste0(burns[i], ": ", round(buf.dif, digits = 2)))
  
  dev.off()
  
  
   unburnt <- mask(unburnt, mask = burn.shp)
  #plot(unburnt)
  
  dir.create(here::here(v), showWarnings = FALSE)
  dir.create(here::here(v, "actual_burnt"), showWarnings = FALSE)
  
  writeRaster(unburnt, here(v, "actual_burnt",paste0(burns[i], "_unburnt.tif") ), overwrite=TRUE)
  if(T){
    burnt <- dnbr
    #plot(burnt)
    burnt[burnt>=threshold] <- 1
    burnt[burnt<threshold] <- NA
    burnt <- mask(burnt, mask = burn.shp)
    
    burnt.ply <- st_as_sf(rasterToPolygons(burnt, dissolve = TRUE))[,-1] %>%
      st_transform(crs = crs(shp.tmp))
    burnt.ply <- cbind(burnt.ply, st_drop_geometry(shp.tmp))
    
    burnt.ply$NUMBER <- paste0(str_sub(burns[i], end = 3), "_", str_sub(burns[i], start = 4))
    burnt.ply$DISTRICT <- str_sub(burns[i], end = 3)
    burnt.ply$DATE1 <- as.Date(parse_date_time(dates$start[which(dates$BURNID == burns[i])], 
                                               c("ymd", "dmy")))
    burnt.ply$CAPT_METH <- "RS10"
    burnt.ply$AUTHOR <- "automated"
    burnt.ply$Hectares <- round(as.numeric(st_area(burnt.ply))/10000, 2)
    burnt.ply$Perimeter <- as.numeric(st_perimeter(st_transform(burnt.ply, crs = crs(shp)))/1000)
    burnt.ply$YEAR1 <- year(burnt.ply$DATE1)
    burnt.ply$POLY_TYPE <- "Actual Burnt"
    burnt.ply$Master_Key <- burn.shp$id[1]
    
    burnt.ply$SEASON1 <- case_when(yday(burnt.ply$DATE1[1]) <= 79 ~ "SU", 
                                   yday(burnt.ply$DATE1[1]) <= 171 ~ "AU",
                                   yday(burnt.ply$DATE1[1]) <= 263 ~ "WI", 
                                   TRUE ~ "SP")
    
    burnt.ply$FIRE_SEASO <- case_when(month(burnt.ply$DATE1[1]) <= 6 ~ 
                                        paste0(year(burnt.ply$DATE1[1])-1, "/", 
                                               year(burnt.ply$DATE1[1])), 
                                      TRUE ~ paste0(year(burnt.ply$DATE1[1]), "/", 
                                                    year(burnt.ply$DATE1[1])+1))
    
    st_write(burnt.ply, here(v, "actual_burnt",paste0(burns[i], "_burnt_area.shp") ), 
             append=FALSE, quiet = TRUE)
  }
  
   
  #cat(i, "of", length(burns), "\n")
}
stopCluster(cl)


ub <- list.files(here("tmp"), pattern = "ub", full.names = TRUE)
ub.stats <- lapply(ub, readRDS) %>% bind_rows()
ub.stats <- filter(ub.stats, BURNID %in% burns)
ggplot(ub.stats, aes(BURNID, threshold))+
  geom_col()+
  geom_hline(yintercept=0.15, linetype="dashed", color = "red")+
  geom_text(aes(label = round(threshold, 2)), hjust = -0.1)+
  coord_flip()+
  ylim(0, 0.35)+
  theme_bw()
d <- list.files(here("bufferStats"), pattern = "unburnt_dif")
ggsave(here("bufferStats", paste0("unburnt_dif_",length(d)+1 , ".jpg")))

