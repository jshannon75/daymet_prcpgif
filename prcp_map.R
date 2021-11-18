library(tidyverse)
library(sf)
library(tmap)
library(extrafont) #I use Segoe UI below

month_lookup<-read_csv("month_lookup.csv",lazy=FALSE)

daymet<-read_csv("daymet_data_us48.csv")

states<-st_read("USstates_48.gpkg")

cty<-st_read("cty_points.gpkg") %>%
  st_transform(5070)

#Adjust bounding box for title
bbox_new <- st_bbox(states)# current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
#bbox_new[3] <- bbox_new[3] + (xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.1 * yrange) # ymax - top

daymet_map<-function(ym_sel){
  daymet_sel<-daymet %>%
    filter(ym==ym_sel)
  daymet_sel_sf <- cty %>%
    inner_join(daymet_sel)
  
  year_sel<-daymet_sel$year
  month_sel<-daymet_sel$month_num
  title_sel<-paste("Monthly precipitation, ",daymet_sel$month," ",
               daymet_sel$year,sep="")

  map<-tm_shape(daymet_sel_sf,bbox=bbox_new) +
    tm_dots("prcp",size="prcp",palette="Blues",style="jenks",
                border.alpha=0)+
    tm_layout(legend.show=FALSE,
              title=title_sel,
              title.position=c("center","top"),
              frame=FALSE,
              fontfamily="Segoe UI")+
    tm_credits("Data: Daymet | Map by @jerry_shannon")+
    tm_shape(states)+tm_borders()
  
  file_title<-paste("maps/",year_sel,"_",month_sel,".png",sep="")
  tmap_save(map,file_title)
}

#daymet_map("2008_Mar") #Test for one file

years<-data.frame(months_sel=unique(daymet$ym)) 

map(years$months_sel,daymet_map)

#GIF
library(gifski)

map_list<-paste("maps/",list.files(path="maps"),sep="")

gifski(map_list,"prcp_map.gif",delay=0.5,loop = TRUE,
       width=2790,height=1580)
