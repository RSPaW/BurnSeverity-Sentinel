  # Load libraries
  library(sf) # to handle shp files
  library(tidyverse) # data manipulation
  library(lubridate) # date manipulation
  library(here) # reference data locations
  library(lwgeom)
  library(raster)
  
  mga50 <- "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs"
  
  # set the number of days to be included in the analysis. Fires along the south coast
  # may need more days to capture pre and post fire images.
  # including more days just means more time ding the cloud qa
  pre.days <- 200 # number of days prior to start date included
  post.days <- 100  # number of days following to end date included
  
  histpath <- "M:\\Zdrive\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\sevSentinel"
  
  # inputs 
  shp.name <- "PHS2024dec.shp" # shp name
  
  # read in shp
  shp.select <- st_read(here::here("fireSelection", shp.name), stringsAsFactors = FALSE) %>% 
    st_make_valid() # check and correct geometry
  
  # if the selection shp was provided by "Fire Systems Support"
  if ("FIRE_SEASO" %in% colnames(shp.select) == TRUE){
    shp <- shp.select %>% rename(OBJECTID = OBJECTID_1, FIH_FIRE_S = FIRE_SEASO, FIH_YEAR1 = YEAR1, FIH_SEASON = SEASON1, 
                  FIH_DISTRI = DISTRICT, FIH_HIST_D = HIST_DISTR, FIH_NUMBER = NUMBER, FIH_FIRE_T = FIRE_TYPE, 
                  FIH_DATE1 = DATE1, FIH_CAUSE = CAUSE, FIH_IGNIT_ = IGNIT_TYPE, FIH_DETECT = DETECT_FDI, 
                  FIH_CAPT_M = CAPT_METH, FIH_AUTHOR = AUTHOR, FIH_POLY_T = POLY_TYPE_, FIH_COMMEN = COMMENT, 
                  FIH_NAME = NAME, FIH_BURN_P = BURN_PURP, FIH_MASTER = Master_Key, FIH_PERIME = Perimeter, 
                  FIH_HECTAR = Hectares) %>%
      mutate(FIH_NUMBER = str_replace(FIH_NUMBER, "_", "")) %>%
      mutate(id = case_when(FIH_FIRE_T == "BF" & nchar(FIH_NUMBER) == 6 ~ 
                                     paste0("BF", FIH_YEAR1, "-", FIH_NUMBER),
                                   TRUE ~ FIH_NUMBER))
    
    st_write(shp, here::here("fireSelection", shp.name), append=FALSE)
  }else{
    shp <- shp.select
  }
  
  colnames(shp)[which(colnames(shp) == "id")] <- "BURNID"
  shp$BURNID <- as.character(shp$BURNID)

  if (length(unique(shp$BURNID)) - nrow(shp) != 0){
    cat("the selection has duplicate ids!!!")
  }
  
  ####################################
  # check plys done
  foldsdir <- list.dirs(histpath, recursive = FALSE, full.names = FALSE)
  folds <- list.dirs(histpath, recursive = FALSE)[stringr::str_starts(foldsdir, "[[:digit:]]")]
  
  ifold <- unlist(str_split(here(), "/"))
  ifold <- ifold[length(ifold)]
  folds <- folds[str_detect(folds, ifold)==FALSE]
  i <- 6
  idsDone <- NA
  
  
  for(i in seq_along(folds)){
    foldi <- list.dirs(folds[i], recursive = FALSE)
    if(sum(str_detect(foldi,  "all_rgbs"))==1){
      foldi <- folds[i]
    }
    foldi <- foldi[str_detect(foldi,  "_SYNCAPP")==FALSE]
    foldi
    j <- 2
    for (j in seq_along(foldi)){
      shpn <- list.files(paste0(foldi[j], "/inputs"), pattern = ".shp$",
                         full.names = TRUE)
      if(length(shpn != 0)){
        burn_ids <- sf::st_read(shpn[1], quiet = TRUE) %>%
          dplyr::pull(BURNID)
        idsDone <- c(idsDone, burn_ids)
      }
    }
  }
  
  op.done <- read.csv("M:\\Zdrive\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\Operational\\xIndex\\DoneIds_2023-07-21.csv")
  idsDone <- c(idsDone, op.done$id)
  
  shp.done <- filter(shp, BURNID %in% idsDone)
  shp <- filter(shp, (BURNID %in% idsDone)==FALSE)
  
  ### If the shapefile does not have records, the code will highlight that shp is empty
  if (nrow(shp) == 0) {
    print("Error: shp is empty.")
  }else{
    

    # check and correct date format
    shp$date <- as.Date(parse_date_time(shp$FIH_DATE1, c("ymd", "dmy")))
    # calculate start and end dates
    shp <- shp %>% mutate(date_end = date + 60, im_strt = date - pre.days, im_end = date + post.days)
    
    shp.n <- dplyr::select(shp, BURNID, FIH_NAME, date, date_end, im_strt, im_end) %>%
      na.omit()
    
    # create directory for inputs
    dir.create(here::here("inputs"), showWarnings = FALSE)
    # reproject to albers equal
    shp.alb <- st_transform(shp.n, crs = "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
    # check if polys look sensible
    plot(shp.alb[,1])
    #save out shp
    st_write(shp.alb, here::here(paste0("inputs\\clean_", 
                                        str_sub(shp.name, end = -5), "_", Sys.Date(), "_alb.shp")), delete_dsn=TRUE)
 
      write_csv(st_drop_geometry(shp.n), here("inputs", "clean_dates.csv"))
      write_csv(st_drop_geometry(shp.n), here("inputs", "clean_dates_edited.csv"))

    
    # create directory for inputs
    dir.create(here::here("inputs", "shpByBurn"), showWarnings = FALSE)
    i <- 1
    for (i in 1:nrow(shp.alb)){
      ply <- shp.alb[i,]
      st_write(ply, here::here("inputs", "shpByBurn", paste0(ply$BURNID[1], "_boundry.shp")), 
               delete_dsn=FALSE, showWarnings = FALSE, quiet = TRUE)
    }
    
  }
  
  
