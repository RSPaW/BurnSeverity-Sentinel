library(filesstrings)
library(sf)
library(here)
library(tidyverse)
library(lubridate)
library(parsedate)

unlink(here("tmp"), recursive = TRUE)

tarfile <- here("s2_rgb_nbr (6).tar.gz")
untar(tarfile, exdir = here("tmp"))

plst <- list.files(here("tmp\\home\\jovyan\\fireSeverity\\rgb_nbr"), pattern = ".png$" )
tlst <- list.files(here("tmp\\home\\jovyan\\fireSeverity\\rgb_nbr"), pattern = ".tif$" )

dates <- read_csv(here("inputs", "clean_dates_edited.csv"))


burns <- unique(str_split_fixed(plst, "_", 3)[,1])
#burns <- "BWD096"

dir.create(here("all_rgbs"), showWarnings = FALSE)
dir.create(here("tifs\\"), showWarnings = FALSE)
dir.create(here("all_rgbs\\_InvestigateFurther"), showWarnings = FALSE)
dir.create(here("all_rgbs\\_NoFire"), showWarnings = FALSE)

i <- 1
pi.all <- data.frame()
for (i in 1:length(burns)){
  burn.i <- burns[i]
  date.i <- filter(dates, BURNID == burns[i])
  pi <- as.data.frame(plst[str_detect(plst, paste0(burn.i, "_"))])
  colnames(pi) <- "file"
  pi <- pi %>% mutate(date = ymd(str_split_fixed(file, "_", 3)[,2])) %>%
    mutate(type = case_when(date < parse_date_time(date.i$date[1], c("ymd", "dmy")) ~ "pre", 
                            TRUE ~ "post")) %>%
    mutate(new.name = paste0(str_sub(file, end = -5), "_", type, ".png"), 
           BURNID = burns[i])
  pi.all <- bind_rows(pi, pi.all)
  file.rename(here("tmp\\home\\jovyan\\fireSeverity\\rgb_nbr", pi$file), 
              here("tmp\\home\\jovyan\\fireSeverity\\rgb_nbr", pi$new.name)) 
  
  fname <- here("all_rgbs", paste0("rgb_", burn.i))
  dir.create(fname, showWarnings = FALSE)
  dir.create(paste0(fname,"\\remove"), showWarnings = FALSE)
  file.move(here("tmp\\home\\jovyan\\fireSeverity\\rgb_nbr", pi$new.name), paste0(fname))
  
  ti <- tlst[str_detect(tlst, paste0(burn.i, "_"))]
  fname <- here("tifs", burn.i)
  dir.create(fname, showWarnings = FALSE)
  file.move(here("tmp\\home\\jovyan\\fireSeverity\\rgb_nbr", ti), paste0(fname))
} 

pi.sum <- pi.all %>% group_by(BURNID,type)  %>%
  summarise(n = n()) 

pi.sum$type <- factor(pi.sum$type, levels = c("post", "pre"))
ggplot(pi.sum, aes(BURNID, n, fill = type, label = n) ) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  coord_flip()+
  labs(title = "Number of Sentinel scenes per burn", y = "Number", 
       caption = Sys.Date())+
  theme_bw()
ggsave(here("scene_numbers_pre_cloud_qa.png"))
# now delete all cloudy .png images
#################################################