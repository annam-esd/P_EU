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
library(sp)
library("xlsx")
library(terra)
setwd("/set working directory")

##################################################################################################################################################
############################################################    current P budget        ##########################################################
##################################################################################################################################################
current<-stack("Current_EU.tif")                                                #Rasterstack with current P balance flows (average of 2010-2019)
Pbal_EU<-stack("QGIS_files/Pbudget.tif")                                        #raster with current P balance 

Pin_EU <-current[[1]]+current[[2]]                                              #kg/ha/year #current annual P input mineral + organic # Figure S1
Pin_min_EU <-current[[1]]                                                       #kg/ha/year #current annual P input mineral # Figure 2.a
Pin_org_EU <-current[[2]]                                                       #kg/ha/year #current annual P input organic (manure)
Pweather_EU <-current[[8]]                                                      #kg/ha/year #current annual P input through weathering of parent rock 
Perod_EU <- current[[15]]/current[[14]] * current[[13]] * 1000                  #kg/ha/year #current annual Net P erosion 
Pexport_EU <-current[[5]]+max(current[[6]],current[[7]])                        #kg/ha/year #current annual P export via grain harvest and residue removal
Pleach_EU <-current[[9]]                                                        #kg/ha/year #current annual P organic leaching
Pav_res_EU <-current[[12]]*10                                                   #kg/ha      #current available P pool (resin extractable (Model output)) 
Pav_Ols_EU <-(current[[12]]*10*0.62) - 1.98                                     #kg/ha      #current available P pool (Olsen extractable) 
Ptot_EU <-current[[15]]*10                                                      #kg/ha      #current total P pool  

##average EU
cellStats(Pin_EU, mean, na.rm=TRUE)                                             
cellStats(Pin_min_EU, mean, na.rm=TRUE)                                         
cellStats(Pin_org_EU, mean, na.rm=TRUE)                                         
cellStats(Pweather_EU, mean, na.rm=TRUE)                                        
cellStats(Pexport_EU, mean, na.rm=TRUE)                                         
cellStats(Pleach_EU, mean, na.rm=TRUE)                                          
cellStats(Perod_EU, mean, na.rm=TRUE)                                           
cellStats(Pav_Ols_EU, mean, na.rm=TRUE)                                         
cellStats(Ptot_EU, mean, na.rm=TRUE)                                            
cellStats(Pbal_EU, mean, na.rm=TRUE)    

hist(as.vector(Pbal_EU),seq(-50,50,by=0.8), ylim= c(0,150000), xlab = "P[kg/ha] ", ylab = "", main = "")  

#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
