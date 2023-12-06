#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################

library(raster)
library(rasterVis)
library(ggplot2)
library(sf)
library(tidyverse)
library(readxl)
library(rgeos)
library(rgdal)
library(dplyr)
library(rgeos)
library(sp)
library("xlsx")
library(terra)
setwd("/set working directory/Projection") 

#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################

##load rasterstacks
current<-stack("/set working directory/Current/Current_EU.tif")                 #Rasterstack with current P balance flows for target area calculation
BAU<-stack("BAU_EU.tif")                                                        #BAU, business as usual  
CC<-stack("CC_EU.tif")                                                          #CC, increased use of cover crops
CC_nfix<-stack("CC_nfix_EU.tif")                                                #CC_nfix, increase use of cover crops that fix nitrogen
lowPmin<-stack("lowPmin_EU.tif")                                                #lowPmin, reduced mineral P input (-40%)
lowPorg<-stack("lowPorg_EU.tif")                                                #lowPorg, reduced organic P input (-40%)
Pav_Ols_EU<-(current[[12]]*10*0.62) - 1.98                                      #current available P pool (Olsen extractable) to calculate target areas for agricultural management scenario #kg/ha    
bd<-stack("Bulk_density.tif")                                                   #bulk density of soils to convert P pool to mass-based concentration to calculate target areas  for agricultural management scenario  #t/m3
Perod_EU<-current[[15]]/current[[14]] * current[[13]] * 1000                    #current annual Net P erosion to calculate target areas for agricultural management scenario #kg/ha/year 

## Net P erosion
#2020-2029
Perod_bau_2030 <- (BAU[[10]]/BAU[[9]]) * BAU[[8]] * 1000                        #kg/ha/year 
Perod_cc_2030 <-  (CC[[10]]/CC[[9]]) * CC[[8]] * 1000                           #kg/ha/year 
Perod_cc_nfix_2030 <-  (CC_nfix[[10]]/CC_nfix[[9]]) * CC_nfix[[8]] * 1000       #kg/ha/year 
Perod_lowPmin_2030 <-  (lowPmin[[10]]/lowPmin[[9]]) * lowPmin[[8]] * 1000       #kg/ha/year 
Perod_lowPorg_2030 <-  (lowPorg[[10]]/lowPorg[[9]]) * lowPorg[[8]] * 1000       #kg/ha/year 
#2040-2049
Perod_bau_2050 <- (BAU[[20]]/BAU[[19]]) * BAU[[18]] * 1000                      #kg/ha/year 
Perod_cc_2050 <-  (CC[[20]]/CC[[19]]) * CC[[18]] * 1000                         #kg/ha/year 
Perod_cc_nfix_2050 <-  (CC_nfix[[20]]/CC_nfix[[19]]) * CC_nfix[[18]] * 1000     #kg/ha/year 
Perod_lowPmin_2050 <-  (lowPmin[[20]]/lowPmin[[19]]) * lowPmin[[18]] * 1000     #kg/ha/year 
Perod_lowPorg_2050 <-  (lowPorg[[20]]/lowPorg[[19]]) * lowPorg[[18]] * 1000     #kg/ha/year 

cellStats(Perod_bau_2030, mean, na.rm=TRUE)
cellStats(Perod_cc_2030, mean, na.rm=TRUE)
cellStats(Perod_cc_nfix_2030, mean, na.rm=TRUE)
cellStats(Perod_lowPmin_2030, mean, na.rm=TRUE)
cellStats(Perod_lowPorg_2030, mean, na.rm=TRUE)

cellStats(Perod_bau_2050, mean, na.rm=TRUE)
cellStats(Perod_cc_2050, mean, na.rm=TRUE)
cellStats(Perod_cc_nfix_2050, mean, na.rm=TRUE)
cellStats(Perod_lowPmin_2050, mean, na.rm=TRUE)
cellStats(Perod_lowPorg_2050, mean, na.rm=TRUE)

## P inputs
#2020-2029
cellStats(BAU[[1]], mean, na.rm=TRUE)                                           #kg/ha/year #average mineral P input  
cellStats(BAU[[2]], mean, na.rm=TRUE)                                           #kg/ha/year #average organic P input   
cellStats(lowPmin[[1]], mean, na.rm=TRUE)                                       #kg/ha/year #average mineral P input  
cellStats(lowPmin[[2]], mean, na.rm=TRUE)                                       #kg/ha/year #average organic P input  
cellStats(lowPorg[[1]], mean, na.rm=TRUE)                                       #kg/ha/year #average mineral P input  
cellStats(lowPorg[[2]], mean, na.rm=TRUE)                                       #kg/ha/year #average organic P input  

#2040-2049 
cellStats(BAU[[11]], mean, na.rm=TRUE)                                          #kg/ha/year #average mineral P input  
cellStats(BAU[[12]], mean, na.rm=TRUE)                                          #kg/ha/year #average organic P input  
cellStats(lowPmin[[11]], mean, na.rm=TRUE)                                      #kg/ha/year #average mineral P input  
cellStats(lowPmin[[12]], mean, na.rm=TRUE)                                      #kg/ha/year #average organic P input  
cellStats(lowPorg[[11]], mean, na.rm=TRUE)                                      #kg/ha/year #average mineral P input  
cellStats(lowPorg[[12]], mean, na.rm=TRUE)                                      #kg/ha/year #average organic P input  

# P budget (major fluxes)
#2020-2029
Psur_bau_2030<-BAU[[1]]+BAU[[2]]-BAU[[3]]-max(BAU[[4]],BAU[[5]]) - Perod_bau_2030                                     #P balance #kg/ha/year
Psur_cc_2030<-CC[[1]] +CC[[2]]-CC[[3]]-max(CC[[4]],CC[[5]]) - Perod_cc_2030                                           #P balance #kg/ha/year
Psur_cc_nfix_2030<-CC_nfix[[1]] +CC_nfix[[2]]-CC_nfix[[3]]-max(CC_nfix[[4]],CC_nfix[[5]]) - Perod_cc_nfix_2030        #P balance #kg/ha/year
Psur_lowPmin_2030<-lowPmin[[1]] +lowPmin[[2]]-lowPmin[[3]]-max(lowPmin[[4]],lowPmin[[5]]) - Perod_lowPmin_2030        #P balance #kg/ha/year
Psur_lowPorg_2030<-lowPorg[[1]] +lowPorg[[2]]-lowPorg[[3]]-max(lowPorg[[4]],lowPorg[[5]]) - Perod_lowPorg_2030        #P balance #kg/ha/year

cellStats(Psur_bau_2030, mean, na.rm=TRUE)
cellStats(Psur_cc_2030, mean, na.rm=TRUE)
cellStats(Psur_cc_nfix_2030, mean, na.rm=TRUE)
cellStats(Psur_lowPorg_2030, mean, na.rm=TRUE)
cellStats(Psur_lowPmin_2030, mean, na.rm=TRUE)

#2040-2049 
Psur_bau_2050<-BAU[[11]]+BAU[[12]]-BAU[[13]]-max(BAU[[14]],BAU[[15]]) - Perod_bau_2050                                #P balance #kg/ha/year
Psur_cc_2050 <-CC[[11]] +CC[[12]]-CC[[13]]-max(CC[[14]],CC[[15]]) - Perod_cc_2050                                     #P balance #kg/ha/year
Psur_cc_nfix_2050 <-CC_nfix[[11]] +CC_nfix[[12]]-CC_nfix[[13]]-max(CC_nfix[[14]],CC_nfix[[15]]) - Perod_cc_nfix_2050  #P balance #kg/ha/year
Psur_lowPmin_2050<-lowPmin[[11]] +lowPmin[[12]]-lowPmin[[13]]-max(lowPmin[[14]],lowPmin[[15]]) - Perod_lowPmin_2050   #P balance #kg/ha/year
Psur_lowPorg_2050<-lowPorg[[11]] +lowPorg[[12]]-lowPorg[[13]]-max(lowPorg[[14]],lowPorg[[15]]) - Perod_lowPorg_2050   #P balance #kg/ha/year

cellStats(Psur_bau_2050, mean, na.rm=TRUE)
cellStats(Psur_cc_2050, mean, na.rm=TRUE)
cellStats(Psur_cc_nfix_2050, mean, na.rm=TRUE)
cellStats(Psur_lowPorg_2050, mean, na.rm=TRUE)
cellStats(Psur_lowPmin_2050, mean, na.rm=TRUE)
 
#P export via crop harvest and crop residue removal
#2020-2029
Pexport_BAU<-BAU[[3]]+max(BAU[[4]],BAU[[5]])                                    #kg/ha/year
Pexport_CC<-CC[[3]]+max(CC[[4]],CC[[5]])                                        #kg/ha/year
Pexport_CC_nfix<-CC_nfix[[3]]+max(CC_nfix[[4]],CC_nfix[[5]])                    #kg/ha/year
Pexport_lowPmin<-lowPmin[[3]]+max(lowPmin[[4]],lowPmin[[5]])                    #kg/ha/year
Pexport_lowPorg<-lowPorg[[3]]+max(lowPorg[[4]],lowPorg[[5]])                    #kg/ha/year

cellStats(Pexport_BAU, mean, na.rm=TRUE)
cellStats(Pexport_CC, mean, na.rm=TRUE)
cellStats(Pexport_CC_nfix, mean, na.rm=TRUE)
cellStats(Pexport_lowPorg, mean, na.rm=TRUE)
cellStats(Pexport_lowPmin, mean, na.rm=TRUE)

#2040-2049 
Pexport_BAU_2050<-BAU[[13]]+max(BAU[[14]],BAU[[15]])                            #kg/ha/year
Pexport_CC_2050<-CC[[13]]+max(CC[[14]],CC[[15]])                                #kg/ha/year
Pexport_CC_nfix_2050<-CC_nfix[[13]]+max(CC_nfix[[14]],CC_nfix[[15]])            #kg/ha/year
Pexport_lowPmin_2050<-lowPmin[[13]]+max(lowPmin[[14]],lowPmin[[15]])            #kg/ha/year
Pexport_lowPorg_2050<-lowPorg[[13]]+max(lowPorg[[14]],lowPorg[[15]])            #kg/ha/year

cellStats(Pexport_BAU_2050, mean, na.rm=TRUE)
cellStats(Pexport_CC_2050, mean, na.rm=TRUE)
cellStats(Pexport_CC_nfix_2050, mean, na.rm=TRUE)
cellStats(Pexport_lowPorg_2050, mean, na.rm=TRUE)
cellStats(Pexport_lowPmin_2050, mean, na.rm=TRUE)

#################################################################################################################################################
###############################################################    Scenario's   #################################################################
#################################################################################################################################################
#### Target Areas
Pav_clipped_na <- Pav_Ols_EU_2

Pav_clipped_na <- Pav_Ols_EU
Pav_clipped_na[Pav_clipped_na < 0] <-NA
bd_resample <- resample(bd, Pav_clipped_na, method = "bilinear")                #use aggregate
Pav_MgKg<- Pav_clipped_na*0.3*bd_resample

##areas to use as mask
Pav_suff<- Pav_MgKg>22                                                          #areas with sufficient available P (Pav>22mg/kg) for all crops (Steinfurth et al., 2022)
Pav_suff[Pav_suff< 0.5] <-NA

leach<- Pav_MgKg>40                                                             #areas with critical available P level (Pav>22mg/kg) above which the risk of P leach starts (Bai et al., 2013)
leach[leach< 0.5] <-NA

Peros_max<- Perod_EU>1.0                                                        #areas that experience high P erosion
Peros_max[Peros_max < 0.5] <-NA

cellStats(Pav_suff, stat="sum", na.rm=TRUE)                                     #100 ha
cellStats(leach, stat="sum", na.rm=TRUE)                                        #100 ha
cellStats(Peros_max, stat="sum", na.rm=TRUE)                                    #100 ha

###############################################################
###############################################################
### 1.Target area: Sufficient available P
##1.budget #2030
Psur_lowPmin_cropped <- crop(Psur_lowPmin_2030, Pav_suff)                         # crop to cutline extend
Psur_lowPmin_masked <- mask(Psur_lowPmin_cropped, Pav_suff)                       # mask values outside of polygon
Psur_lowPmin_Pav_suff = merge(Psur_lowPmin_masked, Psur_bau_2030)                 # merge raster layers (areas with scenario management with areas experiencing BAU management)

Psur_lowPorg_cropped <- crop(Psur_lowPorg_2030, Pav_suff)     
Psur_lowPorg_masked <- mask(Psur_lowPorg_cropped, Pav_suff) 
Psur_lowPorg_Pav_suff = merge(Psur_lowPorg_masked, Psur_bau_2030) 

Psur_cc_cropped <- crop(Psur_cc_2030, Pav_suff)     
Psur_cc_masked <- mask(Psur_cc_cropped, Pav_suff) 
Psur_cc_Pav_suff = merge(Psur_cc_masked, Psur_bau_2030) 

Psur_ccnfix_cropped <- crop(Psur_cc_nfix_2030, Pav_suff)     
Psur_ccnfix_masked <- mask(Psur_ccnfix_cropped, Pav_suff) 
Psur_ccnfix_Pav_suff = merge(Psur_ccnfix_masked, Psur_bau_2030) 

cellStats(Psur_lowPmin_Pav_suff, mean, na.rm=TRUE)
cellStats(Psur_lowPorg_Pav_suff, mean, na.rm=TRUE)
cellStats(Psur_cc_Pav_suff, mean, na.rm=TRUE)
cellStats(Psur_ccnfix_Pav_suff, mean, na.rm=TRUE)

## 1. export #2030
Pexport_lowPmin_cropped <- crop(Pexport_lowPmin, Pav_suff)     
Pexport_lowPmin_masked <- mask(Pexport_lowPmin_cropped, Pav_suff) 
Pexport_lowPmin_Pav_suff = merge(Pexport_lowPmin_masked, Pexport_BAU) 

Pexport_lowPorg_cropped <- crop(Pexport_lowPorg, Pav_suff)     
Pexport_lowPorg_masked <- mask(Pexport_lowPorg_cropped, Pav_suff) 
Pexport_lowPorg_Pav_suff = merge(Pexport_lowPorg_masked, Pexport_BAU)  

Pexport_cc_cropped <- crop(Pexport_CC, Pav_suff)     
Pexport_cc_masked <- mask(Pexport_cc_cropped, Pav_suff) 
Pexport_cc_Pav_suff = merge(Pexport_cc_masked, Pexport_BAU) 

Pexport_ccnfix_cropped <- crop(Pexport_CC_nfix, Pav_suff)     
Pexport_ccnfix_masked <- mask(Pexport_ccnfix_cropped, Pav_suff) 
Pexport_ccnfix_Pav_suff = merge(Pexport_ccnfix_masked, Pexport_BAU) 

cellStats(Pexport_lowPmin_Pav_suff, mean, na.rm=TRUE)
cellStats(Pexport_lowPorg_Pav_suff, mean, na.rm=TRUE)
cellStats(Pexport_cc_Pav_suff, mean, na.rm=TRUE)
cellStats(Pexport_ccnfix_Pav_suff, mean, na.rm=TRUE)

## 1. input #2030
#lowPmin
Pin_lowPmin_cropped <- crop(lowPmin[[1]], Pav_suff)     
Pin_lowPmin_masked <- mask(Pin_lowPmin_cropped, Pav_suff) 
Pin_lowPmin_Pav_suff_1 = merge(Pin_lowPmin_masked, BAU[[1]]) 

Pin_lowPmin_cropped <- crop(lowPmin[[2]], Pav_suff)     
Pin_lowPmin_masked <- mask(Pin_lowPmin_cropped, Pav_suff) 
Pin_lowPmin_Pav_suff_2 = merge(Pin_lowPmin_masked, BAU[[2]]) 

#lowPorg
Pin_lowPorg_cropped <- crop(lowPorg[[1]], Pav_suff)     
Pin_lowPorg_masked <- mask(Pin_lowPorg_cropped, Pav_suff) 
Pin_lowPorg_Pav_suff_1 = merge(Pin_lowPorg_masked, BAU[[1]]) 

Pin_lowPorg_cropped <- crop(lowPorg[[2]], Pav_suff)     
Pin_lowPorg_masked <- mask(Pin_lowPorg_cropped, Pav_suff) 
Pin_lowPorg_Pav_suff_2 = merge(Pin_lowPorg_masked, BAU[[2]]) 

#CC
Pin_CC_cropped <- crop(CC[[2]], Pav_suff)     
Pin_CC_masked <- mask(Pin_CC_cropped, Pav_suff) 
Pin_CC_Pav_suff_2 = merge(Pin_CC_masked, BAU[[2]]) 

Pin_CC_cropped <- crop(CC[[1]], Pav_suff)     
Pin_CC_masked <- mask(Pin_CC_cropped, Pav_suff) 
Pin_CC_Pav_suff_1 = merge(Pin_CC_masked, BAU[[1]]) 

#CCnfix
Pin_CCnfix_cropped <- crop(CC_nfix[[2]], Pav_suff)     
Pin_CCnfix_masked <- mask(Pin_CCnfix_cropped, Pav_suff) 
Pin_CCnfix_Pav_suff_2 = merge(Pin_CCnfix_masked, BAU[[2]]) 

Pin_CCnfix_cropped <- crop(CC_nfix[[1]], Pav_suff)     
Pin_CCnfix_masked <- mask(Pin_CCnfix_cropped, Pav_suff) 
Pin_CCnfix_Pav_suff_1 = merge(Pin_CCnfix_masked, BAU[[1]]) 

cellStats(Pin_lowPmin_Pav_suff_1, mean, na.rm=TRUE)
cellStats(Pin_lowPmin_Pav_suff_2, mean, na.rm=TRUE)
cellStats(Pin_lowPorg_Pav_suff_1, mean, na.rm=TRUE)
cellStats(Pin_lowPorg_Pav_suff_2, mean, na.rm=TRUE)
cellStats(Pin_CC_Pav_suff_1, mean, na.rm=TRUE)
cellStats(Pin_CC_Pav_suff_2, mean, na.rm=TRUE)
cellStats(Pin_CCnfix_Pav_suff_1, mean, na.rm=TRUE)
cellStats(Pin_CCnfix_Pav_suff_2, mean, na.rm=TRUE)

## 1. erosion #2030
Perod_lowPmin_cropped <- crop(Perod_lowPmin_2030, Pav_suff)     
Perod_lowPmin_masked <- mask(Perod_lowPmin_cropped, Pav_suff) 
Perod_lowPmin_Pav_suff = merge(Perod_lowPmin_masked, Perod_bau_2030) 

Perod_lowPorg_cropped <- crop(Perod_lowPorg_2030, Pav_suff)     
Perod_lowPorg_masked <- mask(Perod_lowPorg_cropped, Pav_suff) 
Perod_lowPorg_Pav_suff = merge(Perod_lowPorg_masked, Perod_bau_2030) 

Perod_cc_cropped <- crop(Perod_cc_2030, Pav_suff)     
Perod_cc_masked <- mask(Perod_cc_cropped, Pav_suff) 
Perod_cc_Pav_suff = merge(Perod_cc_masked, Perod_bau_2030) 

Perod_ccnfix_cropped <- crop(Perod_cc_nfix_2030, Pav_suff)     
Perod_ccnfix_masked <- mask(Perod_ccnfix_cropped, Pav_suff) 
Perod_ccnfix_Pav_suff = merge(Perod_ccnfix_masked, Perod_bau_2030) 

cellStats(Perod_lowPmin_Pav_suff, mean, na.rm=TRUE)
cellStats(Perod_lowPorg_Pav_suff, mean, na.rm=TRUE)
cellStats(Perod_cc_Pav_suff, mean, na.rm=TRUE)
cellStats(Perod_ccnfix_Pav_suff, mean, na.rm=TRUE)

###############################################################
###############################################################
### 2. Target area: available P levels above risk for leaching
##2.budget  #2030
Psur_lowPmin_cropped <- crop(Psur_lowPmin_2030, leach)     
Psur_lowPmin_masked <- mask(Psur_lowPmin_cropped, leach) 
Psur_lowPmin_leach = merge(Psur_lowPmin_masked, Psur_bau_2030) 

Psur_lowPorg_cropped <- crop(Psur_lowPorg_2030, leach)     
Psur_lowPorg_masked <- mask(Psur_lowPorg_cropped, leach) 
Psur_lowPorg_leach = merge(Psur_lowPorg_masked, Psur_bau_2030) 

Psur_cc_cropped <- crop(Psur_cc_2030, leach)     
Psur_cc_masked <- mask(Psur_cc_cropped, leach) 
Psur_cc_leach = merge(Psur_cc_masked, Psur_bau_2030) 

Psur_ccnfix_cropped <- crop(Psur_cc_nfix_2030, leach)     
Psur_ccnfix_masked <- mask(Psur_ccnfix_cropped, leach) 
Psur_ccnfix_leach = merge(Psur_ccnfix_masked, Psur_bau_2030) 

cellStats(Psur_lowPmin_leach, mean, na.rm=TRUE)
cellStats(Psur_lowPorg_leach, mean, na.rm=TRUE)
cellStats(Psur_cc_leach, mean, na.rm=TRUE)
cellStats(Psur_ccnfix_leach, mean, na.rm=TRUE)

## 2. export #2030
Pexport_lowPmin_cropped <- crop(Pexport_lowPmin, leach)     
Pexport_lowPmin_masked <- mask(Pexport_lowPmin_cropped, leach) 
Pexport_lowPmin_leach = merge(Pexport_lowPmin_masked, Pexport_BAU) 

Pexport_lowPorg_cropped <- crop(Pexport_lowPorg, leach)     
Pexport_lowPorg_masked <- mask(Pexport_lowPorg_cropped, leach) 
Pexport_lowPorg_leach = merge(Pexport_lowPorg_masked, Pexport_BAU) 

Pexport_cc_cropped <- crop(Pexport_CC, leach)     
Pexport_cc_masked <- mask(Pexport_cc_cropped, leach) 
Pexport_cc_leach = merge(Pexport_cc_masked, Pexport_BAU) 

Pexport_ccnfix_cropped <- crop(Pexport_CC_nfix, leach)     
Pexport_ccnfix_masked <- mask(Pexport_ccnfix_cropped, leach) 
Pexport_ccnfix_leach = merge(Pexport_ccnfix_masked, Pexport_BAU) 

cellStats(Pexport_lowPmin_leach, mean, na.rm=TRUE)
cellStats(Pexport_lowPorg_leach, mean, na.rm=TRUE)
cellStats(Pexport_cc_leach, mean, na.rm=TRUE)
cellStats(Pexport_ccnfix_leach, mean, na.rm=TRUE)

## 2. input #2030
#lowPmin
Pin_lowPmin_cropped <- crop(lowPmin[[1]], leach)     
Pin_lowPmin_masked <- mask(Pin_lowPmin_cropped, leach) 
Pin_lowPmin_leach_1 = merge(Pin_lowPmin_masked, BAU[[1]]) 

Pin_lowPmin_cropped <- crop(lowPmin[[2]], leach)     
Pin_lowPmin_masked <- mask(Pin_lowPmin_cropped, leach) 
Pin_lowPmin_leach_2 = merge(Pin_lowPmin_masked, BAU[[2]]) 

#lowPorg
Pin_lowPorg_cropped <- crop(lowPorg[[1]], leach)     
Pin_lowPorg_masked <- mask(Pin_lowPorg_cropped, leach) 
Pin_lowPorg_leach_1 = merge(Pin_lowPorg_masked, BAU[[1]]) 

Pin_lowPorg_cropped <- crop(lowPorg[[2]], leach)     
Pin_lowPorg_masked <- mask(Pin_lowPorg_cropped, leach) 
Pin_lowPorg_leach_2 = merge(Pin_lowPorg_masked, BAU[[2]]) 

#CC
Pin_CC_cropped <- crop(CC[[2]], leach)     
Pin_CC_masked <- mask(Pin_CC_cropped, leach) 
Pin_CC_leach_2 = merge(Pin_CC_masked, BAU[[2]]) 

Pin_CC_cropped <- crop(CC[[1]], leach)     
Pin_CC_masked <- mask(Pin_CC_cropped, leach) 
Pin_CC_leach_1 = merge(Pin_CC_masked, BAU[[1]]) 

#CCnfix
Pin_CCnfix_cropped <- crop(CC_nfix[[2]], leach)     
Pin_CCnfix_masked <- mask(Pin_CCnfix_cropped, leach) 
Pin_CCnfix_leach_2 = merge(Pin_CCnfix_masked, BAU[[2]]) 

Pin_CCnfix_cropped <- crop(CC_nfix[[1]], leach)     
Pin_CCnfix_masked <- mask(Pin_CCnfix_cropped, leach) 
Pin_CCnfix_leach_1 = merge(Pin_CCnfix_masked, BAU[[1]]) 

cellStats(Pin_lowPmin_leach_1, mean, na.rm=TRUE)
cellStats(Pin_lowPmin_leach_2, mean, na.rm=TRUE)
cellStats(Pin_lowPorg_leach_1, mean, na.rm=TRUE)
cellStats(Pin_lowPorg_leach_2, mean, na.rm=TRUE)
cellStats(Pin_CC_leach_1, mean, na.rm=TRUE)
cellStats(Pin_CC_leach_2, mean, na.rm=TRUE)
cellStats(Pin_CCnfix_leach_1, mean, na.rm=TRUE)
cellStats(Pin_CCnfix_leach_2, mean, na.rm=TRUE)

## 2. erosion #2030
Perod_lowPmin_cropped <- crop(Perod_lowPmin_2030, leach)     
Perod_lowPmin_masked <- mask(Perod_lowPmin_cropped, leach) 
Perod_lowPmin_leach = merge(Perod_lowPmin_masked, Perod_bau_2030) 

Perod_lowPorg_cropped <- crop(Perod_lowPorg_2030, leach)     
Perod_lowPorg_masked <- mask(Perod_lowPorg_cropped, leach) 
Perod_lowPorg_leach = merge(Perod_lowPorg_masked, Perod_bau_2030) 

Perod_cc_cropped <- crop(Perod_cc_2030, leach)     
Perod_cc_masked <- mask(Perod_cc_cropped, leach) 
Perod_cc_leach = merge(Perod_cc_masked, Perod_bau_2030) 

Perod_ccnfix_cropped <- crop(Perod_cc_nfix_2030, leach)     
Perod_ccnfix_masked <- mask(Perod_ccnfix_cropped, leach) 
Perod_ccnfix_leach = merge(Perod_ccnfix_masked, Perod_bau_2030) 

cellStats(Perod_lowPmin_leach, mean, na.rm=TRUE)
cellStats(Perod_lowPorg_leach, mean, na.rm=TRUE)
cellStats(Perod_cc_leach, mean, na.rm=TRUE)
cellStats(Perod_ccnfix_leach, mean, na.rm=TRUE)

###############################################################
###############################################################
### 3. Target area: high P erosion
##3.budget #2030
Psur_lowPmin_cropped <- crop(Psur_lowPmin_2030, Peros_max)     
Psur_lowPmin_masked <- mask(Psur_lowPmin_cropped, Peros_max) 
Psur_lowPmin_Peros_max = merge(Psur_lowPmin_masked, Psur_bau_2030) 

Psur_lowPorg_cropped <- crop(Psur_lowPorg_2030, Peros_max)     
Psur_lowPorg_masked <- mask(Psur_lowPorg_cropped, Peros_max) 
Psur_lowPorg_Peros_max = merge(Psur_lowPorg_masked, Psur_bau_2030) 

Psur_cc_cropped <- crop(Psur_cc_2030, Peros_max)     
Psur_cc_masked <- mask(Psur_cc_cropped, Peros_max) 
Psur_cc_Peros_max = merge(Psur_cc_masked, Psur_bau_2030) 

Psur_ccnfix_cropped <- crop(Psur_cc_nfix_2030, Peros_max)     
Psur_ccnfix_masked <- mask(Psur_ccnfix_cropped, Peros_max) 
Psur_ccnfix_Peros_max = merge(Psur_ccnfix_masked, Psur_bau_2030) 

cellStats(Psur_lowPmin_Peros_max, mean, na.rm=TRUE)
cellStats(Psur_lowPorg_Peros_max, mean, na.rm=TRUE)
cellStats(Psur_cc_Peros_max, mean, na.rm=TRUE)
cellStats(Psur_ccnfix_Peros_max, mean, na.rm=TRUE)

## 3. export #2030
Pexport_lowPmin_cropped <- crop(Pexport_lowPmin, Peros_max)     
Pexport_lowPmin_masked <- mask(Pexport_lowPmin_cropped, Peros_max) 
Pexport_lowPmin_Peros_max = merge(Pexport_lowPmin_masked, Pexport_BAU) 

Pexport_lowPorg_cropped <- crop(Pexport_lowPorg, Peros_max)     
Pexport_lowPorg_masked <- mask(Pexport_lowPorg_cropped, Peros_max) 
Pexport_lowPorg_Peros_max = merge(Pexport_lowPorg_masked, Pexport_BAU) 

Pexport_cc_cropped <- crop(Pexport_CC, Peros_max)     
Pexport_cc_masked <- mask(Pexport_cc_cropped, Peros_max) 
Pexport_cc_Peros_max = merge(Pexport_cc_masked, Pexport_BAU) 

Pexport_ccnfix_cropped <- crop(Pexport_CC_nfix, Peros_max)     
Pexport_ccnfix_masked <- mask(Pexport_ccnfix_cropped, Peros_max) 
Pexport_ccnfix_Peros_max = merge(Pexport_ccnfix_masked, Pexport_BAU) 

cellStats(Pexport_lowPmin_Peros_max, mean, na.rm=TRUE)
cellStats(Pexport_lowPorg_Peros_max, mean, na.rm=TRUE)
cellStats(Pexport_cc_Peros_max, mean, na.rm=TRUE)
cellStats(Pexport_ccnfix_Peros_max, mean, na.rm=TRUE)

## 3. input #2030
#lowPmin
Pin_lowPmin_cropped <- crop(lowPmin[[1]], Peros_max)     
Pin_lowPmin_masked <- mask(Pin_lowPmin_cropped, Peros_max) 
Pin_lowPmin_Peros_max_1 = merge(Pin_lowPmin_masked, BAU[[1]]) 

Pin_lowPmin_cropped <- crop(lowPmin[[2]], Peros_max)     
Pin_lowPmin_masked <- mask(Pin_lowPmin_cropped, Peros_max) 
Pin_lowPmin_Peros_max_2 = merge(Pin_lowPmin_masked, BAU[[2]]) 

#lowPorg
Pin_lowPorg_cropped <- crop(lowPorg[[1]], Peros_max)     
Pin_lowPorg_masked <- mask(Pin_lowPorg_cropped, Peros_max) 
Pin_lowPorg_Peros_max_1 = merge(Pin_lowPorg_masked, BAU[[1]]) 

Pin_lowPorg_cropped <- crop(lowPorg[[2]], Peros_max)     
Pin_lowPorg_masked <- mask(Pin_lowPorg_cropped, Peros_max) 
Pin_lowPorg_Peros_max_2 = merge(Pin_lowPorg_masked, BAU[[2]]) 

#CC
Pin_CC_cropped <- crop(CC[[2]], Peros_max)     
Pin_CC_masked <- mask(Pin_CC_cropped, Peros_max) 
Pin_CC_Peros_max_2 = merge(Pin_CC_masked, BAU[[2]]) 

Pin_CC_cropped <- crop(CC[[1]], Peros_max)     
Pin_CC_masked <- mask(Pin_CC_cropped, Peros_max) 
Pin_CC_Peros_max_1 = merge(Pin_CC_masked, BAU[[1]]) 

#CCnfix
Pin_CCnfix_cropped <- crop(CC_nfix[[2]], Peros_max)     
Pin_CCnfix_masked <- mask(Pin_CCnfix_cropped, Peros_max) 
Pin_CCnfix_Peros_max_2 = merge(Pin_CCnfix_masked, BAU[[2]]) 

Pin_CCnfix_cropped <- crop(CC_nfix[[1]], Peros_max)     
Pin_CCnfix_masked <- mask(Pin_CCnfix_cropped, Peros_max) 
Pin_CCnfix_Peros_max_1 = merge(Pin_CCnfix_masked, BAU[[1]]) 

cellStats(Pin_lowPmin_Peros_max_1, mean, na.rm=TRUE)
cellStats(Pin_lowPmin_Peros_max_2, mean, na.rm=TRUE)
cellStats(Pin_lowPorg_Peros_max_1, mean, na.rm=TRUE)
cellStats(Pin_lowPorg_Peros_max_2, mean, na.rm=TRUE)
cellStats(Pin_CC_Peros_max_1, mean, na.rm=TRUE)
cellStats(Pin_CC_Peros_max_2, mean, na.rm=TRUE)
cellStats(Pin_CCnfix_Peros_max_1, mean, na.rm=TRUE)
cellStats(Pin_CCnfix_Peros_max_2, mean, na.rm=TRUE)

## 3. erosion #2030
Perod_lowPmin_cropped <- crop(Perod_lowPmin_2030, Peros_max)     
Perod_lowPmin_masked <- mask(Perod_lowPmin_cropped, Peros_max) 
Perod_lowPmin_Peros_max = merge(Perod_lowPmin_masked, Perod_bau_2030) 

Perod_lowPorg_cropped <- crop(Perod_lowPorg_2030, Peros_max)     
Perod_lowPorg_masked <- mask(Perod_lowPorg_cropped, Peros_max) 
Perod_lowPorg_Peros_max = merge(Perod_lowPorg_masked, Perod_bau_2030) 

Perod_cc_cropped <- crop(Perod_cc_2030, Peros_max)     
Perod_cc_masked <- mask(Perod_cc_cropped, Peros_max) 
Perod_cc_Peros_max = merge(Perod_cc_masked, Perod_bau_2030) 

Perod_ccnfix_cropped <- crop(Perod_cc_nfix_2030, Peros_max)     
Perod_ccnfix_masked <- mask(Perod_ccnfix_cropped, Peros_max) 
Perod_ccnfix_Peros_max = merge(Perod_ccnfix_masked, Perod_bau_2030) 

cellStats(Perod_lowPmin_Peros_max, mean, na.rm=TRUE)
cellStats(Perod_lowPorg_Peros_max, mean, na.rm=TRUE)
cellStats(Perod_cc_Peros_max, mean, na.rm=TRUE)
cellStats(Perod_ccnfix_Peros_max, mean, na.rm=TRUE)

#################################################################################################################################################
###################################################################   2050   #################################################################### 
#################################################################################################################################################
### 1.Target area: Sufficient available P
##1.budget #2050
Psur_lowPmin_cropped <- crop(Psur_lowPmin_2050, Pav_suff)     
Psur_lowPmin_masked <- mask(Psur_lowPmin_cropped, Pav_suff) 
Psur_lowPmin_Pav_suff = merge(Psur_lowPmin_masked, Psur_bau_2050) 

Psur_lowPorg_cropped <- crop(Psur_lowPorg_2050, Pav_suff)     
Psur_lowPorg_masked <- mask(Psur_lowPorg_cropped, Pav_suff) 
Psur_lowPorg_Pav_suff = merge(Psur_lowPorg_masked, Psur_bau_2050) 

Psur_cc_cropped <- crop(Psur_cc_2050, Pav_suff)     
Psur_cc_masked <- mask(Psur_cc_cropped, Pav_suff) 
Psur_cc_Pav_suff = merge(Psur_cc_masked, Psur_bau_2050) 

Psur_ccnfix_cropped <- crop(Psur_cc_nfix_2050, Pav_suff)     
Psur_ccnfix_masked <- mask(Psur_ccnfix_cropped, Pav_suff) 
Psur_ccnfix_Pav_suff = merge(Psur_ccnfix_masked, Psur_bau_2050) 

cellStats(Psur_lowPmin_Pav_suff, mean, na.rm=TRUE)
cellStats(Psur_lowPorg_Pav_suff, mean, na.rm=TRUE)
cellStats(Psur_cc_Pav_suff, mean, na.rm=TRUE)
cellStats(Psur_ccnfix_Pav_suff, mean, na.rm=TRUE)

## 1. export #2050
Pexport_lowPmin_cropped <- crop(Pexport_lowPmin_2050, Pav_suff)     
Pexport_lowPmin_masked <- mask(Pexport_lowPmin_cropped, Pav_suff) 
Pexport_lowPmin_Pav_suff = merge(Pexport_lowPmin_masked, Pexport_BAU_2050) 

Pexport_lowPorg_cropped <- crop(Pexport_lowPorg_2050, Pav_suff)     
Pexport_lowPorg_masked <- mask(Pexport_lowPorg_cropped, Pav_suff) 
Pexport_lowPorg_Pav_suff = merge(Pexport_lowPorg_masked, Pexport_BAU_2050) 

Pexport_cc_cropped <- crop(Pexport_CC_2050, Pav_suff)     
Pexport_cc_masked <- mask(Pexport_cc_cropped, Pav_suff) 
Pexport_cc_Pav_suff = merge(Pexport_cc_masked, Pexport_BAU_2050) 

Pexport_ccnfix_cropped <- crop(Pexport_CC_nfix_2050, Pav_suff)     
Pexport_ccnfix_masked <- mask(Pexport_ccnfix_cropped, Pav_suff) 
Pexport_ccnfix_Pav_suff = merge(Pexport_ccnfix_masked, Pexport_BAU_2050) 

cellStats(Pexport_lowPmin_Pav_suff, mean, na.rm=TRUE)
cellStats(Pexport_lowPorg_Pav_suff, mean, na.rm=TRUE)
cellStats(Pexport_cc_Pav_suff, mean, na.rm=TRUE)
cellStats(Pexport_ccnfix_Pav_suff, mean, na.rm=TRUE)

## 1. input #2050
#lowPmin
Pin_lowPmin_cropped <- crop(lowPmin[[11]], Pav_suff)     
Pin_lowPmin_masked <- mask(Pin_lowPmin_cropped, Pav_suff) 
Pin_lowPmin_Pav_suff_1 = merge(Pin_lowPmin_masked, BAU[[11]]) 

Pin_lowPmin_cropped <- crop(lowPmin[[12]], Pav_suff)     
Pin_lowPmin_masked <- mask(Pin_lowPmin_cropped, Pav_suff) 
Pin_lowPmin_Pav_suff_2 = merge(Pin_lowPmin_masked, BAU[[12]]) 

#lowPorg
Pin_lowPorg_cropped <- crop(lowPorg[[11]], Pav_suff)     
Pin_lowPorg_masked <- mask(Pin_lowPorg_cropped, Pav_suff) 
Pin_lowPorg_Pav_suff_1 = merge(Pin_lowPorg_masked, BAU[[11]]) 

Pin_lowPorg_cropped <- crop(lowPorg[[12]], Pav_suff)     
Pin_lowPorg_masked <- mask(Pin_lowPorg_cropped, Pav_suff) 
Pin_lowPorg_Pav_suff_2 = merge(Pin_lowPorg_masked, BAU[[12]]) 

#CC
Pin_CC_cropped <- crop(CC[[12]], Pav_suff)     
Pin_CC_masked <- mask(Pin_CC_cropped, Pav_suff) 
Pin_CC_Pav_suff_2 = merge(Pin_CC_masked, BAU[[12]]) 

Pin_CC_cropped <- crop(CC[[11]], Pav_suff)     
Pin_CC_masked <- mask(Pin_CC_cropped, Pav_suff) 
Pin_CC_Pav_suff_1 = merge(Pin_CC_masked, BAU[[11]]) 

#CCnfix
Pin_CCnfix_cropped <- crop(CC_nfix[[12]], Pav_suff)     
Pin_CCnfix_masked <- mask(Pin_CCnfix_cropped, Pav_suff) 
Pin_CCnfix_Pav_suff_2 = merge(Pin_CCnfix_masked, BAU[[12]]) 

Pin_CCnfix_cropped <- crop(CC_nfix[[11]], Pav_suff)     
Pin_CCnfix_masked <- mask(Pin_CCnfix_cropped, Pav_suff) 
Pin_CCnfix_Pav_suff_1 = merge(Pin_CCnfix_masked, BAU[[11]]) 

cellStats(Pin_lowPmin_Pav_suff_1, mean, na.rm=TRUE)
cellStats(Pin_lowPorg_Pav_suff_1, mean, na.rm=TRUE)
cellStats(Pin_lowPorg_Pav_suff_2, mean, na.rm=TRUE)
cellStats(Pin_lowPmin_Pav_suff_2, mean, na.rm=TRUE)
cellStats(Pin_CC_Pav_suff_1, mean, na.rm=TRUE)
cellStats(Pin_CC_Pav_suff_2, mean, na.rm=TRUE)
cellStats(Pin_CCnfix_Pav_suff_1, mean, na.rm=TRUE)
cellStats(Pin_CCnfix_Pav_suff_2, mean, na.rm=TRUE)

## 1. erosion #2050
Perod_lowPmin_cropped <- crop(Perod_lowPmin_2050, Pav_suff)     
Perod_lowPmin_masked <- mask(Perod_lowPmin_cropped, Pav_suff) 
Perod_lowPmin_Pav_suff = merge(Perod_lowPmin_masked, Perod_bau_2050) 

Perod_lowPorg_cropped <- crop(Perod_lowPorg_2050, Pav_suff)     
Perod_lowPorg_masked <- mask(Perod_lowPorg_cropped, Pav_suff) 
Perod_lowPorg_Pav_suff = merge(Perod_lowPorg_masked, Perod_bau_2050) 

Perod_cc_cropped <- crop(Perod_cc_2050, Pav_suff)     
Perod_cc_masked <- mask(Perod_cc_cropped, Pav_suff) 
Perod_cc_Pav_suff = merge(Perod_cc_masked, Perod_bau_2050) 

Perod_ccnfix_cropped <- crop(Perod_cc_nfix_2050, Pav_suff)     
Perod_ccnfix_masked <- mask(Perod_ccnfix_cropped, Pav_suff) 
Perod_ccnfix_Pav_suff = merge(Perod_ccnfix_masked, Perod_bau_2050) 

cellStats(Perod_lowPmin_Pav_suff, mean, na.rm=TRUE)
cellStats(Perod_lowPorg_Pav_suff, mean, na.rm=TRUE)
cellStats(Perod_cc_Pav_suff, mean, na.rm=TRUE)
cellStats(Perod_ccnfix_Pav_suff, mean, na.rm=TRUE)

###############################################################
###############################################################
### 2. Target area: available P levels above risk for leaching
##2.budget #2050
Psur_lowPmin_cropped <- crop(Psur_lowPmin_2050, leach)     
Psur_lowPmin_masked <- mask(Psur_lowPmin_cropped, leach) 
Psur_lowPmin_leach = merge(Psur_lowPmin_masked, Psur_bau_2050) 

Psur_lowPorg_cropped <- crop(Psur_lowPorg_2050, leach)     
Psur_lowPorg_masked <- mask(Psur_lowPorg_cropped, leach) 
Psur_lowPorg_leach = merge(Psur_lowPorg_masked, Psur_bau_2050) 

Psur_cc_cropped <- crop(Psur_cc_2050, leach)     
Psur_cc_masked <- mask(Psur_cc_cropped, leach) 
Psur_cc_leach = merge(Psur_cc_masked, Psur_bau_2050) 

Psur_ccnfix_cropped <- crop(Psur_cc_nfix_2050, leach)     
Psur_ccnfix_masked <- mask(Psur_ccnfix_cropped, leach) 
Psur_ccnfix_leach = merge(Psur_ccnfix_masked, Psur_bau_2050) 

cellStats(Psur_lowPmin_leach, mean, na.rm=TRUE)
cellStats(Psur_lowPorg_leach, mean, na.rm=TRUE)
cellStats(Psur_cc_leach, mean, na.rm=TRUE)
cellStats(Psur_ccnfix_leach, mean, na.rm=TRUE)

##2. export #2050
Pexport_lowPmin_cropped <- crop(Pexport_lowPmin_2050, leach)     
Pexport_lowPmin_masked <- mask(Pexport_lowPmin_cropped, leach) 
Pexport_lowPmin_leach = merge(Pexport_lowPmin_masked, Pexport_BAU_2050) 

Pexport_lowPorg_cropped <- crop(Pexport_lowPorg_2050, leach)     
Pexport_lowPorg_masked <- mask(Pexport_lowPorg_cropped, leach) 
Pexport_lowPorg_leach = merge(Pexport_lowPorg_masked, Pexport_BAU_2050) 

Pexport_cc_cropped <- crop(Pexport_CC_2050, leach)     
Pexport_cc_masked <- mask(Pexport_cc_cropped, leach) 
Pexport_cc_leach = merge(Pexport_cc_masked, Pexport_BAU_2050) 

Pexport_ccnfix_cropped <- crop(Pexport_CC_nfix_2050, leach)     
Pexport_ccnfix_masked <- mask(Pexport_ccnfix_cropped, leach) 
Pexport_ccnfix_leach = merge(Pexport_ccnfix_masked, Pexport_BAU_2050) 

cellStats(Pexport_lowPmin_leach, mean, na.rm=TRUE)
cellStats(Pexport_lowPorg_leach, mean, na.rm=TRUE)
cellStats(Pexport_cc_leach, mean, na.rm=TRUE)
cellStats(Pexport_ccnfix_leach, mean, na.rm=TRUE)

##2. input #2050
#lowPmin
Pin_lowPmin_cropped <- crop(lowPmin[[11]], leach)     
Pin_lowPmin_masked <- mask(Pin_lowPmin_cropped, leach) 
Pin_lowPmin_leach_1 = merge(Pin_lowPmin_masked, BAU[[11]]) 
Pin_lowPmin_cropped <- crop(lowPmin[[12]], leach)     
Pin_lowPmin_masked <- mask(Pin_lowPmin_cropped, leach) 
Pin_lowPmin_leach_2 = merge(Pin_lowPmin_masked, BAU[[12]]) 

#lowPorg
Pin_lowPorg_cropped <- crop(lowPorg[[11]], leach)     
Pin_lowPorg_masked <- mask(Pin_lowPorg_cropped, leach) 
Pin_lowPorg_leach_1 = merge(Pin_lowPorg_masked, BAU[[11]]) 
Pin_lowPorg_cropped <- crop(lowPorg[[12]], leach)     
Pin_lowPorg_masked <- mask(Pin_lowPorg_cropped, leach) 
Pin_lowPorg_leach_2 = merge(Pin_lowPorg_masked, BAU[[12]]) 

#CC
Pin_CC_cropped <- crop(CC[[12]], leach)     
Pin_CC_masked <- mask(Pin_CC_cropped, leach) 
Pin_CC_leach_2 = merge(Pin_CC_masked, BAU[[12]]) 
Pin_CC_cropped <- crop(CC[[11]], leach)     
Pin_CC_masked <- mask(Pin_CC_cropped, leach) 
Pin_CC_leach_1 = merge(Pin_CC_masked, BAU[[11]]) 

#CCnfix
Pin_CCnfix_cropped <- crop(CC_nfix[[12]], leach)     
Pin_CCnfix_masked <- mask(Pin_CCnfix_cropped, leach) 
Pin_CCnfix_leach_2 = merge(Pin_CCnfix_masked, BAU[[12]]) 
Pin_CCnfix_cropped <- crop(CC_nfix[[11]], leach)     
Pin_CCnfix_masked <- mask(Pin_CCnfix_cropped, leach) 
Pin_CCnfix_leach_1 = merge(Pin_CCnfix_masked, BAU[[11]]) 

cellStats(Pin_lowPmin_leach_1, mean, na.rm=TRUE)
cellStats(Pin_lowPorg_leach_1, mean, na.rm=TRUE)
cellStats(Pin_lowPorg_leach_2, mean, na.rm=TRUE)
cellStats(Pin_lowPmin_leach_2, mean, na.rm=TRUE)
cellStats(Pin_CC_leach_1, mean, na.rm=TRUE)
cellStats(Pin_CC_leach_2, mean, na.rm=TRUE)
cellStats(Pin_CCnfix_leach_1, mean, na.rm=TRUE)
cellStats(Pin_CCnfix_leach_2, mean, na.rm=TRUE)

##2. erosion #2050
Perod_lowPmin_cropped <- crop(Perod_lowPmin_2050, leach)     
Perod_lowPmin_masked <- mask(Perod_lowPmin_cropped, leach) 
Perod_lowPmin_leach = merge(Perod_lowPmin_masked, Perod_bau_2050) 

Perod_lowPorg_cropped <- crop(Perod_lowPorg_2050, leach)     
Perod_lowPorg_masked <- mask(Perod_lowPorg_cropped, leach) 
Perod_lowPorg_leach = merge(Perod_lowPorg_masked, Perod_bau_2050) 

Perod_cc_cropped <- crop(Perod_cc_2050, leach)     
Perod_cc_masked <- mask(Perod_cc_cropped, leach) 
Perod_cc_leach = merge(Perod_cc_masked, Perod_bau_2050) 

Perod_ccnfix_cropped <- crop(Perod_cc_nfix_2050, leach)     
Perod_ccnfix_masked <- mask(Perod_ccnfix_cropped, leach) 
Perod_ccnfix_leach = merge(Perod_ccnfix_masked, Perod_bau_2050) 

cellStats(Perod_lowPmin_leach, mean, na.rm=TRUE)
cellStats(Perod_lowPorg_leach, mean, na.rm=TRUE)
cellStats(Perod_cc_leach, mean, na.rm=TRUE)
cellStats(Perod_ccnfix_leach, mean, na.rm=TRUE)

###############################################################
###############################################################
### 3. Target area: high P erosion
##3.budget #2050
Psur_lowPmin_cropped <- crop(Psur_lowPmin_2050, Peros_max)     
Psur_lowPmin_masked <- mask(Psur_lowPmin_cropped, Peros_max) 
Psur_lowPmin_Peros_max = merge(Psur_lowPmin_masked, Psur_bau_2050) 

Psur_lowPorg_cropped <- crop(Psur_lowPorg_2050, Peros_max)     
Psur_lowPorg_masked <- mask(Psur_lowPorg_cropped, Peros_max) 
Psur_lowPorg_Peros_max = merge(Psur_lowPorg_masked, Psur_bau_2050) 

Psur_cc_cropped <- crop(Psur_cc_2050, Peros_max)     
Psur_cc_masked <- mask(Psur_cc_cropped, Peros_max) 
Psur_cc_Peros_max = merge(Psur_cc_masked, Psur_bau_2050) 

Psur_ccnfix_cropped <- crop(Psur_cc_nfix_2050, Peros_max)     
Psur_ccnfix_masked <- mask(Psur_ccnfix_cropped, Peros_max) 
Psur_ccnfix_Peros_max = merge(Psur_ccnfix_masked, Psur_bau_2050) 

cellStats(Psur_lowPmin_Peros_max, mean, na.rm=TRUE)
cellStats(Psur_lowPorg_Peros_max, mean, na.rm=TRUE)
cellStats(Psur_cc_Peros_max, mean, na.rm=TRUE)
cellStats(Psur_ccnfix_Peros_max, mean, na.rm=TRUE)

##3. export #2050
Pexport_lowPmin_cropped <- crop(Pexport_lowPmin_2050, Peros_max)     
Pexport_lowPmin_masked <- mask(Pexport_lowPmin_cropped, Peros_max) 
Pexport_lowPmin_Peros_max = merge(Pexport_lowPmin_masked, Pexport_BAU_2050) 

Pexport_lowPorg_cropped <- crop(Pexport_lowPorg_2050, Peros_max)     
Pexport_lowPorg_masked <- mask(Pexport_lowPorg_cropped, Peros_max) 
Pexport_lowPorg_Peros_max = merge(Pexport_lowPorg_masked, Pexport_BAU_2050) 

Pexport_cc_cropped <- crop(Pexport_CC_2050, Peros_max)     
Pexport_cc_masked <- mask(Pexport_cc_cropped, Peros_max) 
Pexport_cc_Peros_max = merge(Pexport_cc_masked, Pexport_BAU_2050) 

Pexport_ccnfix_cropped <- crop(Pexport_CC_nfix_2050, Peros_max)     
Pexport_ccnfix_masked <- mask(Pexport_ccnfix_cropped, Peros_max) 
Pexport_ccnfix_Peros_max = merge(Pexport_ccnfix_masked, Pexport_BAU_2050) 

cellStats(Pexport_lowPmin_Peros_max, mean, na.rm=TRUE)
cellStats(Pexport_lowPorg_Peros_max, mean, na.rm=TRUE)
cellStats(Pexport_cc_Peros_max, mean, na.rm=TRUE)
cellStats(Pexport_ccnfix_Peros_max, mean, na.rm=TRUE)

##3. input #2050
#lowPmin
Pin_lowPmin_cropped <- crop(lowPmin[[11]], Pav_suff)     
Pin_lowPmin_masked <- mask(Pin_lowPmin_cropped, Pav_suff) 
Pin_lowPmin_Pav_suff_1 = merge(Pin_lowPmin_masked, BAU[[11]]) 
Pin_lowPmin_cropped <- crop(lowPmin[[12]], Pav_suff)     
Pin_lowPmin_masked <- mask(Pin_lowPmin_cropped, Pav_suff) 
Pin_lowPmin_Pav_suff_2 = merge(Pin_lowPmin_masked, BAU[[12]]) 

#lowPorg
Pin_lowPorg_cropped <- crop(lowPorg[[11]], Pav_suff)     
Pin_lowPorg_masked <- mask(Pin_lowPorg_cropped, Pav_suff) 
Pin_lowPorg_Pav_suff_1 = merge(Pin_lowPorg_masked, BAU[[11]]) 
Pin_lowPorg_cropped <- crop(lowPorg[[12]], Pav_suff)     
Pin_lowPorg_masked <- mask(Pin_lowPorg_cropped, Pav_suff) 
Pin_lowPorg_Pav_suff_2 = merge(Pin_lowPorg_masked, BAU[[12]]) 

#CC
Pin_CC_cropped <- crop(CC[[12]], Peros_max)     
Pin_CC_masked <- mask(Pin_CC_cropped, Peros_max) 
Pin_CC_Peros_max_2 = merge(Pin_CC_masked, BAU[[12]]) 
Pin_CC_cropped <- crop(CC[[11]], Peros_max)     
Pin_CC_masked <- mask(Pin_CC_cropped, Peros_max) 
Pin_CC_Peros_max_1 = merge(Pin_CC_masked, BAU[[11]]) 

#CCnfix
Pin_CCnfix_cropped <- crop(CC_nfix[[12]], Peros_max)     
Pin_CCnfix_masked <- mask(Pin_CCnfix_cropped, Peros_max) 
Pin_CCnfix_Peros_max_2 = merge(Pin_CCnfix_masked, BAU[[12]]) 
Pin_CCnfix_cropped <- crop(CC_nfix[[11]], Peros_max)     
Pin_CCnfix_masked <- mask(Pin_CCnfix_cropped, Peros_max) 
Pin_CCnfix_Peros_max_1 = merge(Pin_CCnfix_masked, BAU[[11]]) 

cellStats(Pin_lowPmin_Peros_max_1, mean, na.rm=TRUE)
cellStats(Pin_lowPmin_Peros_max_2, mean, na.rm=TRUE)
cellStats(Pin_lowPorg_Peros_max_1, mean, na.rm=TRUE)
cellStats(Pin_lowPorg_Peros_max_2, mean, na.rm=TRUE)
cellStats(Pin_CC_Peros_max_1, mean, na.rm=TRUE)
cellStats(Pin_CC_Peros_max_2, mean, na.rm=TRUE)
cellStats(Pin_CCnfix_Peros_max_1, mean, na.rm=TRUE)
cellStats(Pin_CCnfix_Peros_max_2, mean, na.rm=TRUE)

##3. erosion #2050
Perod_lowPmin_cropped <- crop(Perod_lowPmin_2050, Peros_max)     
Perod_lowPmin_masked <- mask(Perod_lowPmin_cropped, Peros_max) 
Perod_lowPmin_Peros_max = merge(Perod_lowPmin_masked, Perod_bau_2050) 

Perod_lowPorg_cropped <- crop(Perod_lowPorg_2050, Peros_max)     
Perod_lowPorg_masked <- mask(Perod_lowPorg_cropped, Peros_max) 
Perod_lowPorg_Peros_max = merge(Perod_lowPorg_masked, Perod_bau_2050) 

Perod_cc_cropped <- crop(Perod_cc_2050, Peros_max)     
Perod_cc_masked <- mask(Perod_cc_cropped, Peros_max) 
Perod_cc_Peros_max = merge(Perod_cc_masked, Perod_bau_2050) 

Perod_ccnfix_cropped <- crop(Perod_cc_nfix_2050, Peros_max)     
Perod_ccnfix_masked <- mask(Perod_ccnfix_cropped, Peros_max) 
Perod_ccnfix_Peros_max = merge(Perod_ccnfix_masked, Perod_bau_2050) 

cellStats(Perod_lowPmin_Peros_max, mean, na.rm=TRUE)
cellStats(Perod_lowPorg_Peros_max, mean, na.rm=TRUE)
cellStats(Perod_cc_Peros_max, mean, na.rm=TRUE)
cellStats(Perod_ccnfix_Peros_max, mean, na.rm=TRUE)
#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
####### How did the target area's change over time (from 2010-2019 to 2040-2049) depending on the agricultural management?

#### Agricultural Management: BAU
Pav_BAU_2050_na <- Pav_BAU_2050
Pav_BAU_2050_na[Pav_BAU_2050_na < 0] <-NA
Pav_MgKg_2050<- Pav_BAU_2050_na*0.3*bd_resample
Pav_suff_2050<- Pav_MgKg_2050>22 
leach_2050<- Pav_MgKg_2050>40 
Peros_2050<- Perod_bau_2050>1.0  
Peros_2050[Peros_2050< 0.1]<-NA

cellStats(Pav_suff_2050, stat="sum", na.rm=TRUE)
cellStats(leach_2050, stat="sum", na.rm=TRUE)
cellStats(Peros_2050, stat="sum", na.rm=TRUE)

#### Agricultural Management: CC
Pav_CC_2050_na <- Pav_CC_2050
Pav_CC_2050_na[Pav_CC_2050_na < 0] <-NA
Pav_MgKg_2050<- Pav_CC_2050_na*0.3*bd_resample
Pav_suff_2050<- Pav_MgKg_2050>22 
leach_2050<- Pav_MgKg_2050>40 
Peros_2050<- Perod_cc_2050>1.0  
Peros_2050[Peros_2050< 0.1]<-NA

cellStats(Pav_suff_2050, stat="sum", na.rm=TRUE)
cellStats(leach_2050, stat="sum", na.rm=TRUE)
cellStats(Peros_2050, stat="sum", na.rm=TRUE)

#### Agricultural Management: CC_nfix
Pav_CC_nfix_2050_na <- Pav_CC_nfix_2050
Pav_CC_nfix_2050_na[Pav_CC_nfix_2050_na < 0] <-NA
Pav_MgKg_2050<- Pav_CC_nfix_2050_na*0.3*bd_resample
Pav_suff_2050<- Pav_MgKg_2050>22 
leach_2050<- Pav_MgKg_2050>40 
Peros_2050<- Perod_cc_nfix_2050>1.0  
Peros_2050[Peros_2050< 0.1]<-NA

cellStats(Peros_2050, stat="sum", na.rm=TRUE)
cellStats(leach_2050, stat="sum", na.rm=TRUE)
cellStats(Pav_suff_2050, stat="sum", na.rm=TRUE)

#### Agricultural Management: lowPmin
Pav_lowPmin_2050_na <- Pav_lowPmin_2050
Pav_lowPmin_2050_na[Pav_lowPmin_2050_na < 0] <-NA
Pav_MgKg_2050<- Pav_lowPmin_2050_na*0.3*bd_resample
Pav_suff_2050<- Pav_MgKg_2050>22 
leach_2050<- Pav_MgKg_2050>40 
Peros_2050<- Perod_lowPmin_2050>1.0  
Peros_2050[Peros_2050< 0.1]<-NA

cellStats(Pav_suff_2050, stat="sum", na.rm=TRUE)
cellStats(leach_2050, stat="sum", na.rm=TRUE)
cellStats(Peros_2050, stat="sum", na.rm=TRUE)

#### Agricultural Management: lowPorg
Pav_lowPorg_2050_na <- Pav_lowPorg_2050
Pav_lowPorg_2050_na[Pav_lowPorg_2050_na < 0] <-NA
Pav_MgKg_2050<- Pav_lowPorg_2050_na*0.3*bd_resample
Pav_suff_2050<- Pav_MgKg_2050>22 
leach_2050<- Pav_MgKg_2050>40 
Peros_2050<- Perod_lowPorg_2050>1.0  
Peros_2050[Peros_2050< 0.1]<-NA

cellStats(Pav_suff_2050, stat="sum", na.rm=TRUE)
cellStats(leach_2050, stat="sum", na.rm=TRUE)
cellStats(Peros_2050, stat="sum", na.rm=TRUE)

#################################################################################################################################################
#################################################################################################################################################

### Which agricultural management practice scenario resulted in the lowest erosion?
EU_erosion_2030 <- stack(Perod_bau_2030, Perod_lowPmin_Pav_suff, Perod_lowPorg_Pav_suff, Perod_cc_Pav_suff, Perod_ccnfix_Pav_suff)
EU_erosion_2050 <- stack(Perod_bau_2050, Perod_lowPmin_Pav_suff, Perod_lowPorg_Pav_suff, Perod_cc_Pav_suff, Perod_ccnfix_Pav_suff)

EU_erosion_2030_min <- which.min(EU_erosion_2030)
EU_erosion_2050_min <- which.min(EU_erosion_2050)

writeRaster(EU_erosion_2030_min, filename="EU_erosion_2030_min.tif", format="GTiff"  ,overwrite=T)
writeRaster(EU_erosion_2050_min, filename="EU_erosion_2050_min.tif", format="GTiff"  ,overwrite=T)

### Which agricultural management practice scenario resulted in the lowest P balance?
EU_budget_2030 <- stack(Psur_bau_2030, Psur_lowPmin_Pav_suff, Psur_lowPorg_Pav_suff, Psur_cc_Pav_suff, Psur_ccnfix_Pav_suff)
EU_budget_2050 <- stack(Psur_bau_2050, Psur_lowPmin_Pav_suff, Psur_lowPorg_Pav_suff, Psur_cc_Pav_suff, Psur_ccnfix_Pav_suff)

EU_budget_2030_min <- which.min(EU_budget_2030)
EU_budget_2050_min <- which.min(EU_budget_2050)

writeRaster(EU_budget_2030_min, filename="EU_budget_2030_min.tif", format="GTiff"  ,overwrite=T)
writeRaster(EU_budget_2050_min, filename="EU_budget_2050_min.tif", format="GTiff"  ,overwrite=T)

### Which agricultural management practice scenario resulted in the highest P export?
EU_export_2030 <- stack(Pexport_BAU, Pexport_lowPmin_Pav_suff, Pexport_lowPorg_Pav_suff, Pexport_cc_Pav_suff, Pexport_ccnfix_Pav_suff)
EU_export_2050 <- stack(Pexport_BAU_2050, Pexport_lowPmin_Pav_suff, Pexport_lowPorg_Pav_suff, Pexport_cc_Pav_suff, Pexport_ccnfix_Pav_suff)

EU_export_2030_max <- which.max(EU_export_2030)
EU_export_2050_max <- which.max(EU_export_2050)

writeRaster(EU_export_2030_max, filename="EU_export_2030_max.tif", format="GTiff"  ,overwrite=T)
writeRaster(EU_export_2050_max, filename="EU_export_2050_max.tif", format="GTiff"  ,overwrite=T)

#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
