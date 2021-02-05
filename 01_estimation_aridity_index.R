# ESTIMATION ARIDITY INDEX
# Author: Ing. Gutierrez Lope Leonardo Flavio

setwd("//data/")
library(tidyverse)
library(raster)

# SEC 1: PARAMETERS ARIDEZ ----------------------------------------
PCP_list <- list.files(path='PPT/', pattern='.tif', full.names=FALSE)
PCP_stack <- raster::stack(paste0("PPT/", PCP_list))

EVP_list <- list.files(path='ETP/', pattern='.tif', full.names=FALSE)
EVP_stack <- raster::stack(paste0("ETP/", EVP_list))

#DEFICIT AND EXCESO HIDRICO MENSUAL
DHM  <- EVP_stack-PCP_stack
EHM  <- PCP_stack-EVP_stack

#PRECIPITATION TOTAL
PCPA <- calc(PCP_stack, sum)
#EVP TOTAL 
EVPA <- calc(EVP_stack, sum)
#DEFICIT HIDRICO
DHA <- calc(DHM, sum)
#EXCESO HIDRICO
EHA <- calc(EHM, sum)

#ARIDITY INDEX 0.05
reclas_ia <- matrix(c(-Inf, 0.0499, 100, 0.0499, Inf, 0.001),ncol = 3,byrow = TRUE)
IA <- reclassify(PCPA/EVPA, reclas_ia)

#PRECIPITATION: 2500 < P < 2500
reclas_pcp <- matrix(c(-Inf, 2499.99, 0.5, 2499.99, Inf, 0.25),ncol = 3,byrow = TRUE)
PCP_clas <- reclassify(PCPA, reclas_pcp)

#MONTHS DRY
reclas_ms <- matrix(c(-Inf, 0.499, 1,0.499, Inf, 0),ncol = 3,byrow = TRUE)

MSECOS <- PCP_stack/EVP_stack
MSECOS <- reclassify(MSECOS, reclas_ms)
MSECOS <- calc(MSECOS, sum)

# SEC 2: ARIDITY REGIME --------------------------------------------------
PARAM_ARIDEZ <- IA+PCP_clas+MSECOS
reclas_reg_ari <- matrix(c(0, 0.3, 1,     #Hiper Hídrico
                   0.3, 0.99, 2,   #Hídrico
                   0.99, 2.99, 3,  #Hiper Húmedo 
                   2.99, 4.99, 4,  #Húmedo
                   4.99, 6.99, 5,  #Subhúmedo
                   6.99, 8.99, 6,  #Semiárido
                   8.99, 10.99, 7, #Árido
                   10.99, 100, 8,  #Hiper Árido
                   100, Inf, 9),   #Xérico
                 ncol = 3,byrow = TRUE)
ARIDEZ_REG <- reclassify(PARAM_ARIDEZ, reclas_reg_ari)

# ESTATISTICS RA
ARIDEZ_REG_df   <- as.data.frame(ARIDEZ_REG, xy = TRUE)
ARIDEZ_REG_df   <- na.omit(ARIDEZ_REG_df)
breaks <- c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5)
ARIDEZ_REG_df$clasd <- cut(ARIDEZ_REG_df$layer,
                            breaks = breaks,
                            right = FALSE,
                            labels=c("Hiper Hídrico", "Hídrico", "Hiper Húmedo",'Húmedo',"Subhúmedo","Semiárido",'Árido','Hiper Árido','Xérico'))
ARIDEZ_REG_df2 <- mutate(count(ARIDEZ_REG_df, clasd),perc = n / nrow(ARIDEZ_REG_df))

# SEC 3: ARIDITY INDEX --------------------------------------------------
reclas_ind_ari <- matrix(c(-Inf, 0.05, 1,  #Hiper-árido
                    0.05, 0.20, 2,  #Árido
                    0.20, 0.50, 3,  #Semi-árido
                    0.50, 0.65, 4,  #Sub-húmedo seco
                    0.65, 1, 5,     #subhumedo humedo
                    1, Inf, 6),     #Humedo
                  ncol = 3,byrow = TRUE)
ARIDEZ_IND <- reclassify(PCPA/EVPA, reclas_ind_ari)

# ESTATISTICS IA
ARIDEZ_IND_df   <- as.data.frame(ARIDEZ_IND, xy = TRUE)
ARIDEZ_IND_df   <- na.omit(ARIDEZ_IND_df)
breaks <- c(0.5,1.5,2.5,3.5,4.5,5.5,6.5)
ARIDEZ_IND_df$clasd <- cut(ARIDEZ_IND_df$layer,
                           breaks = breaks,
                           right = FALSE,
                           labels=c("Hiperárido",'Árido', "Semiárido", 'Subhúmedo seco', "Subhúmedo húmedo",'Húmedo'))
ARIDEZ_IND_df2 <- mutate(count(ARIDEZ_IND_df, clasd),perc = n / nrow(ARIDEZ_IND_df))
