library(raster)
library(tidyverse)
library(sf)
library(lubridate)
library(here)
library(doParallel)

lshp <- list.files(here::here("inputs", "shpByBurn"), pattern = "shp$", full.names = TRUE)
#shp <- st_read(lshp[1], stringsAsFactors = FALSE, quiet = TRUE)

burns.f <- list.dirs(here("all_rgbs"), recursive = FALSE, full.names = FALSE)
burns.f <- burns.f[str_detect(burns.f, "rgb_")]
burns <- str_split_fixed(burns.f, "_", 2)[,2]

dates <- read.csv(here("inputs", "clean_dates_edited.csv")) %>%
  rename(start = date, end = date_end)

#burns <- "BWD-2017-52569681"

i <- 6
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
  library(here)
  
  ply <- st_read(here::here("inputs", "shpByBurn", paste0(burns[i], "_boundry.shp")), 
                 quiet = TRUE)
  ply <- st_buffer(ply, dist = 240)
  
  date <- filter(dates, BURNID == burns[i])
  
  #fstart <- ymd("2021-09-15")
  fstart <- parse_date_time(date$start[1], c("ymd", "dmy"))
  fend <- parse_date_time(date$end[1], c("ymd", "dmy"))

  plist <- as.data.frame(list.files(here("all_rgbs", paste0("rgb_",burns[i])), "png"))
  colnames(plist)[1] <- "file" 

  if(nrow(plist)!=0){
  
    write.csv(plist, here("all_rgbs", paste0("rgb_",burns[i]),"cleanDates.csv"))
    plist <- mutate(plist, date = ymd(str_split_fixed(file, "_", 3)[,2]))
    date.list <- plist$date
  
    tlist <- as.data.frame(list.files(here("tifs", burns[i])))
    colnames(tlist)[1] <- "file" 
    tlist <- tlist %>% mutate(date = ymd(str_split_fixed(file, "_", 3)[,2])) %>%
      filter(date %in% date.list)
  
  #pre image nbr
  preDates <- filter(tlist, date < fstart)
  if (nrow(preDates)==0){
    preIm <- raster(here("tifs", burns[i], tlist$file[1])) 
    cat(burns[i], "has no pre burn image\n")
  }else{
    preDate <- preDates[nrow(preDates),]
    preIm <- raster(here("tifs", burns[i], preDate$file[1]))
    preDate$type <- "pre"
    imUsed <- preDate
  }
  plot(preIm)
  
  #post image nbr
  postDates <- filter(tlist, date > fstart & date <= fend)
  if (nrow(postDates) == 0){
    postDates <- filter(tlist, date > fstart)
    postDate1 <- postDates[1,]
  }
  
  
  if (nrow(postDates) != 0){
    
    #im <- paste0(here("tifs", burns[i], postDate$file[1]))
    ims <- stack(here("tifs", burns[i], postDates$file))
    #plot(ims)
    #postIm20 <- mask(crop(ims, ply), mask = ply)
  
    postNBRmin <- calc(ims, min)
  
    plot(postNBRmin)
 
    dNBRmax <- preIm - postNBRmin
    plot(dNBRmax, main = burns[i])

    names(dNBRmax) <- "Index"

    dir.create(here::here("dNBR"), showWarnings = FALSE)
    writeRaster(dNBRmax, here("dNBR", paste0(burns[i],"_dNBR.tiff")), overwrite=TRUE)  
    #plot(stk)
    postDates$type <- "post"
    imUsed <- bind_rows(imUsed, postDates) 
    write.csv(imUsed, here( "tifs", burns[i], "imgUsed.csv"))
  }else{
    cat(burns[i], "has no post burn image\n")
  }
}
}

stopCluster(cl)