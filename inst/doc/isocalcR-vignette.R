## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 2.25
)

## ----setup--------------------------------------------------------------------
library(isocalcR)
library(tidyr)
library(dplyr)
library(ggplot2)


## ----CO2data------------------------------------------------------------------
data(CO2data) #Load CO2data into your environment

head(CO2data, 10) #View initial CO2data observations

tail(CO2data, 10) #View most recent CO2data observations


## ----piru13C------------------------------------------------------------------
data(piru13C)
head(piru13C)


## ----example------------------------------------------------------------------
library(isocalcR) #Load the package

#Calculate iWUE from leaf organic material with a δ13C signature of -27 ‰ for the year 2015, 
#300 meters above sea level at 25°C.
d13C.to.iWUE(d13C.plant = -27, 
             year = 2015, 
             elevation = 300, 
             temp = 25) 

#Use custom.calc to calculate iWUE from the same leaf sample as above.
custom.calc(d13C.plant = -27,
            d13C.atm = -8.44,
            outvar = "iWUE",
            Ca = 399.62,
            elevation = 300,
            temp = 25)

#Calculate the ratio of leaf intercellular to atmospheric CO2 (Ci/Ca) using the simple 
#formulation for leaf and wood. Internally updates apparent fractionation by Rubisco, b, 
#according to Cernusak and Ubierna 2022.
d13C.to.CiCa(d13C.plant = -27,
             year = 2015,
             elevation = 300,
             temp = 25,
             tissue = "leaf")

d13C.to.CiCa(d13C.plant = -27,
             year = 2015,
             elevation = 300,
             temp = 25,
             tissue = "wood")

#Calculate iWUE using the "simple", "photorespiration", and "mesophyll" formulations.
d13C.to.iWUE(d13C.plant = -28,
             year = 2015,
             elevation = 300,
             temp = 15,
             method = "simple")

d13C.to.iWUE(d13C.plant = -28,
             year = 2015,
             elevation = 300,
             temp = 15,
             method = "photorespiration")

d13C.to.iWUE(d13C.plant = -28,
             year = 2015,
             elevation = 300,
             temp = 15,
             method = "mesophyll")




#Calculate iWUE from tree ring (wholewood) d13C from Mathias and Thomas (2018) 
#using previously loaded piru13C data

#First drop years where there are no data
piru13C <- piru13C %>% 
  drop_na() 

#Calculate iWUE for each case using 'mapply'
piru13C$iWUE_simple <- mapply(d13C.to.iWUE, #Call the function
                              d13C.plant = piru13C$wood.d13C, #Assign the plant d13C value
                              year = piru13C$Year, #Assign the year to match atmospheric CO2 and atmospheric d13CO2
                              elevation = piru13C$Elevation_m, #Assign the elevation
                              temp = piru13C$MGT_C, #Assign the temperature 
                              method = "simple", #Specify the method
                              tissue = "wood") #Specify which tissue the sample is from

piru13C$iWUE_photorespiration <- mapply(d13C.to.iWUE, #Call the function
                                        d13C.plant = piru13C$wood.d13C, #Assign the plant d13C value
                                        year = piru13C$Year, #Assign the year to match atmospheric CO2 and atmospheric d13CO2
                                        elevation = piru13C$Elevation_m, #Specify elevation
                                        temp = piru13C$MGT_C, #Specify the temperature during tissue formation
                                        method = "photorespiration", #Specify the iWUE calculation formulation
                                        frac = piru13C$frac) #Specify any post-photosynthetic fractionations. In this case 2 permille to account for leaf to wood.

piru13C$iWUE_mesophyll <- mapply(d13C.to.iWUE, #Call the function
                                        d13C.plant = piru13C$wood.d13C, #Assign the plant d13C value
                                        year = piru13C$Year, #Assign the year to match atmospheric CO2 and atmospheric d13CO2
                                        elevation = piru13C$Elevation_m, #Specify elevation
                                        temp = piru13C$MGT_C, #Specify the temperature during tissue formation
                                        method = "mesophyll", #Specify the iWUE calculation formulation
                                        frac = piru13C$frac) #Specify any post-photosynthetic fractionations. In this case 2 permille to account for leaf to wood.

#Create dataframe for visualizing differences in computed iWUE among the three formulations
piru13C_long <- piru13C %>%
  select(Year, Site, iWUE_simple, iWUE_photorespiration, iWUE_mesophyll) %>% #Select only columns of interest
  rename(Simple = iWUE_simple,
         Photorespiration = iWUE_photorespiration,
         Mesophyll = iWUE_mesophyll) %>% 
  pivot_longer(names_to = "Formulation", values_to = "iWUE", -c(Year, Site))

#Visually examine differences in iWUE based on the formulation used for each study location
ggplot(data = piru13C_long, aes(x = Year, y = iWUE, color = Formulation)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(group = Formulation), color = "gray30") +
  theme_classic() +
  facet_wrap(~Site) +
  ylab(expression("iWUE (µmol mol"^{-1}*")")) 


