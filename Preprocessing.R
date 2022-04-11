#Christina De Cesaris
#File contains:
# - preprocessin()g of raw data
# - charts which illustrate the preprocessing 


library(MASS)
library(tidyverse)
library(kableExtra)
library(gridExtra)
#dataset

mush = read.csv("primary_data.csv", sep=";",na.strings=c(""," ","NA",character(0)))
head(mush)
percent_na = data.frame(colMeans(is.na(mush[c(4:23)])))
percent_na = cbind(rownames(percent_na),percent_na*100)
rownames(percent_na)=NULL

percent_na%>%kable(col.names = c("Variable","Percent Missing"))%>% kable_classic_2(full_width =F,lightable_options   =c("striped","bordered") )%>% 
  column_spec (1:2,
               border_left = T, 
               border_right = T) %>%
  row_spec(c(3,6,7,11,12,14,15,18), bold = T, color = "black", background = "gold")



str(mush)


mush1 = mush[,-c(6,8,9,10,14,15,17,18,21)]
head(mush1,3)%>%kable%>% kable_classic(full_width =F,lightable_options   =c("striped","bordered") )%>% 
  column_spec (1:14,
               border_left = T, 
               border_right = T)

colMeans(is.na(mush1))
mush1$class = as.factor(ifelse(mush1$class=="p",1,0))#inediple=1



mush1 = as.data.frame(sapply(mush1,
                             function(x) gsub("\\[|\\]", "", x)))
mush1 = as.data.frame(sapply(mush1,
                             function(x) gsub(" ","", x)))

#STEM.WIDTH, STEM.HEIGHT, CAP.DIAMETER

minmax=function(x){
  result=c()
  for (ele in x){
    result= rbind(result, cbind(min(as.numeric(unlist(strsplit(ele,",")))),
                                max(as.numeric(unlist(strsplit(ele,","))))))
    
  }
  return(result)
}
mush1=mush1 %>%
  mutate(
    cap.diameter_max = minmax(mush1$cap.diameter)[,2],
    stem.height_max = minmax(mush1$stem.height)[,2],
    stem.width_max = minmax(mush1$stem.width)[,2],
    cap.diameter_min = minmax(mush1$cap.diameter)[,1],
    stem.height_min = minmax(mush1$stem.height)[,1],
    stem.width_min = minmax(mush1$stem.width)[,1]
  )

# SEASON

mush1=mush1 %>%
  mutate(
    season_a = str_extract(season, "a"),
    season_s = str_extract(season, "s"),
    season_u = str_extract(season, "u"),
    season_w = str_extract(season, "w")
  )

mush1$season_a=ifelse(is.na(mush1$season_a),0,1)
mush1$season_s=ifelse(is.na(mush1$season_s),0,1)
mush1$season_u=ifelse(is.na(mush1$season_u),0,1)
mush1$season_w=ifelse(is.na(mush1$season_w),0,1)

# CAP.SHAPE

mush1=mush1 %>%
  mutate(
    cap.shape_b = str_extract(cap.shape, "b"),
    cap.shape_c = str_extract(cap.shape, "c"),
    cap.shape_x = str_extract(cap.shape, "x"),
    cap.shape_f = str_extract(cap.shape, "f"),
    cap.shape_s = str_extract(cap.shape, "s"),
    cap.shape_p = str_extract(cap.shape, "p"),
    cap.shape_o = str_extract(cap.shape, "o"),
  )


mush1$cap.shape_b = ifelse(is.na(mush1$cap.shape_b),0,1)
mush1$cap.shape_c = ifelse(is.na(mush1$cap.shape_c),0,1)
mush1$cap.shape_x = ifelse(is.na(mush1$cap.shape_x),0,1)
mush1$cap.shape_f = ifelse(is.na(mush1$cap.shape_f),0,1)
mush1$cap.shape_s = ifelse(is.na(mush1$cap.shape_s),0,1)
mush1$cap.shape_p = ifelse(is.na(mush1$cap.shape_p),0,1)
mush1$cap.shape_o = ifelse(is.na(mush1$cap.shape_o),0,1)

#HAS.RING

mush1$has.ring = ifelse((mush1$has.ring)=="t",1,0)

#CAP.COLOR

mush1=mush1 %>%
  mutate(
    cap.color_n = str_extract(cap.color, "n"),
    cap.color_b = str_extract(cap.color, "b"),
    cap.color_g = str_extract(cap.color, "g"),
    cap.color_r = str_extract(cap.color, "r"),
    cap.color_p = str_extract(cap.color, "p"),
    cap.color_u = str_extract(cap.color, "u"),
    cap.color_e = str_extract(cap.color, "e"),
    cap.color_w = str_extract(cap.color, "w"),
    cap.color_y = str_extract(cap.color, "y"),
    cap.color_l = str_extract(cap.color, "l"),
    cap.color_o = str_extract(cap.color, "o"),
    cap.color_k = str_extract(cap.color, "k")
  )

mush1$cap.color_n = ifelse(is.na(mush1$cap.color_n),0,1)
mush1$cap.color_b = ifelse(is.na(mush1$cap.color_b),0,1)
mush1$cap.color_g = ifelse(is.na(mush1$cap.color_g),0,1)
mush1$cap.color_r = ifelse(is.na(mush1$cap.color_r),0,1)
mush1$cap.color_p = ifelse(is.na(mush1$cap.color_p),0,1)
mush1$cap.color_u = ifelse(is.na(mush1$cap.color_u),0,1)
mush1$cap.color_e = ifelse(is.na(mush1$cap.color_e),0,1)
mush1$cap.color_w = ifelse(is.na(mush1$cap.color_w),0,1)
mush1$cap.color_y = ifelse(is.na(mush1$cap.color_y),0,1)
mush1$cap.color_l = ifelse(is.na(mush1$cap.color_l),0,1)
mush1$cap.color_o = ifelse(is.na(mush1$cap.color_o),0,1)
mush1$cap.color_k = ifelse(is.na(mush1$cap.color_k),0,1)



# HABITAT


mush1=mush1 %>%
  mutate(
    habitat_g = str_extract(habitat, "g"),
    habitat_l = str_extract(habitat, "l"),
    habitat_m = str_extract(habitat, "m"),
    habitat_p = str_extract(habitat, "p"),
    habitat_h = str_extract(habitat, "h"),
    habitat_u = str_extract(habitat, "u"),
    habitat_w = str_extract(habitat, "w"),
    habitat_d = str_extract(habitat, "d")
  )

mush1$habitat_g = ifelse(is.na(mush1$habitat_g),0,1)
mush1$habitat_l = ifelse(is.na(mush1$habitat_l),0,1)
mush1$habitat_m = ifelse(is.na(mush1$habitat_m),0,1)
mush1$habitat_p = ifelse(is.na(mush1$habitat_p),0,1)
mush1$habitat_h = ifelse(is.na(mush1$habitat_h),0,1)
mush1$habitat_u = ifelse(is.na(mush1$habitat_u),0,1)
mush1$habitat_w = ifelse(is.na(mush1$habitat_w),0,1)
mush1$habitat_d = ifelse(is.na(mush1$habitat_d),0,1)


# STEM.COLOR
mush1=mush1 %>%
  mutate(
    stem.color_n = str_extract(stem.color, "n"),
    stem.color_b = str_extract(stem.color, "b"),
    stem.color_g = str_extract(stem.color, "g"),
    stem.color_r = str_extract(stem.color, "r"),
    stem.color_p = str_extract(stem.color, "p"),
    stem.color_u = str_extract(stem.color, "u"),
    stem.color_e = str_extract(stem.color, "e"),
    stem.color_w = str_extract(stem.color, "w"),
    stem.color_y = str_extract(stem.color, "y"),
    stem.color_l = str_extract(stem.color, "l"),
    stem.color_o = str_extract(stem.color, "o"),
    stem.color_k = str_extract(stem.color, "k")
  )

mush1$stem.color_n = ifelse(is.na(mush1$stem.color_n),0,1)
mush1$stem.color_b = ifelse(is.na(mush1$stem.color_b),0,1)
mush1$stem.color_g = ifelse(is.na(mush1$stem.color_g),0,1)
mush1$stem.color_r = ifelse(is.na(mush1$stem.color_r),0,1)
mush1$stem.color_p = ifelse(is.na(mush1$stem.color_p),0,1)
mush1$stem.color_u = ifelse(is.na(mush1$stem.color_u),0,1)
mush1$stem.color_e = ifelse(is.na(mush1$stem.color_e),0,1)
mush1$stem.color_w = ifelse(is.na(mush1$stem.color_w),0,1)
mush1$stem.color_y = ifelse(is.na(mush1$stem.color_y),0,1)
mush1$stem.color_l = ifelse(is.na(mush1$stem.color_l),0,1)
mush1$stem.color_o = ifelse(is.na(mush1$stem.color_o),0,1)
mush1$stem.color_k = ifelse(is.na(mush1$stem.color_k),0,1)


# GILL.COLOR

mush1=mush1 %>%
  mutate(
    gill.color_n = str_extract(gill.color, "n"),
    gill.color_b = str_extract(gill.color, "b"),
    gill.color_g = str_extract(gill.color, "g"),
    gill.color_r = str_extract(gill.color, "r"),
    gill.color_p = str_extract(gill.color, "p"),
    gill.color_u = str_extract(gill.color, "u"),
    gill.color_e = str_extract(gill.color, "e"),
    gill.color_w = str_extract(gill.color, "w"),
    gill.color_y = str_extract(gill.color, "y"),
    gill.color_l = str_extract(gill.color, "l"),
    gill.color_o = str_extract(gill.color, "o"),
    gill.color_k = str_extract(gill.color, "k")
  )

mush1$gill.color_n = ifelse(is.na(mush1$gill.color_n),0,1)
mush1$gill.color_b = ifelse(is.na(mush1$gill.color_b),0,1)
mush1$gill.color_g = ifelse(is.na(mush1$gill.color_g),0,1)
mush1$gill.color_r = ifelse(is.na(mush1$gill.color_r),0,1)
mush1$gill.color_p = ifelse(is.na(mush1$gill.color_p),0,1)
mush1$gill.color_u = ifelse(is.na(mush1$gill.color_u),0,1)
mush1$gill.color_e = ifelse(is.na(mush1$gill.color_e),0,1)
mush1$gill.color_w = ifelse(is.na(mush1$gill.color_w),0,1)
mush1$gill.color_y = ifelse(is.na(mush1$gill.color_y),0,1)
mush1$gill.color_l = ifelse(is.na(mush1$gill.color_l),0,1)
mush1$gill.color_o = ifelse(is.na(mush1$gill.color_o),0,1)
mush1$gill.color_k = ifelse(is.na(mush1$gill.color_k),0,1)

# RING.TYPE
mush1=mush1 %>%
  mutate(
    ring.type_c = str_extract(ring.type, "c"),
    ring.type_e = str_extract(ring.type, "e"),
    ring.type_r = str_extract(ring.type, "r"),
    ring.type_g = str_extract(ring.type, "g"),
    ring.type_l = str_extract(ring.type, "l"),
    ring.type_p = str_extract(ring.type, "p"),
    ring.type_s = str_extract(ring.type, "s"),
    ring.type_z = str_extract(ring.type, "z"),
    ring.type_y = str_extract(ring.type, "y"),
    ring.type_m = str_extract(ring.type, "m"),
    ring.type_f = str_extract(ring.type, "f"),
    ring.type_z = str_extract(ring.type, "z")
  )

mush1$ring.type_c = ifelse(is.na(mush1$ring.type_c),0,1)
mush1$ring.type_e = ifelse(is.na(mush1$ring.type_e),0,1)
mush1$ring.type_r = ifelse(is.na(mush1$ring.type_r),0,1)
mush1$ring.type_g = ifelse(is.na(mush1$ring.type_g),0,1)
mush1$ring.type_l = ifelse(is.na(mush1$ring.type_l),0,1)
mush1$ring.type_p = ifelse(is.na(mush1$ring.type_p),0,1)
mush1$ring.type_s = ifelse(is.na(mush1$ring.type_s),0,1)
mush1$ring.type_z = ifelse(is.na(mush1$ring.type_z),0,1)
mush1$ring.type_y = ifelse(is.na(mush1$ring.type_y),0,1)
mush1$ring.type_m = ifelse(is.na(mush1$ring.type_m),0,1)
mush1$ring.type_f = ifelse(is.na(mush1$ring.type_f),0,1)





mush2 = mush1[c(3,11,15:86)]


head(mush2,3)%>%
  kable%>% 
  kable_classic(full_width =F,lightable_options   =c("striped","bordered") )%>% 
  column_spec (1:50,
               border_left = T, 
               border_right = T)

#convert units so they are on the same scale as other measured
#values 
mush2$stem.width_max=mush2$stem.width_max/10
mush2$stem.width_min=mush2$stem.width_min/10

