
# library -----------------------------------------------------------------
options(max.print = 100000)
library(ggplot2)
library(tidyverse)
library(stringr)
library(scales)
library(janitor)
library(lubridate)
library(ggpubr)
library(ggpmisc)
library(dplyr)
library(ggthemes)
library(knitr)
library(xlsx)
library(readxl)
library(tibble)
library(nlme)
library(lme4)
library(broom)
library(xlsx)
require(mgcv)
library(multcomp)
library(piecewiseSEM); library(MuMIn)
library(broom.mixed)
require(mgcv)
library(extrafont)

#list packages
(.packages())

# # data import: ----------------------------------------------------------

setwd()
list.files()
getwd()



# Load dataset ------------------------------------------------------------
SOC_Hoh<- read.xlsx(file="Cperc_outliers_allplots_Hohenschulen.xlsx", sheetIndex = 1)

SOC_Hoh<-SOC_Hoh %>% clean_names() %>%  as_tibble

# Prepare data -----------------------------------------------------------------------


# First, calculate SOC stocks
SOC_Hoh <- SOC_Hoh %>% 
 mutate(bd_2020 = case_when(
 tiefe == "0_30" ~ 1.61, 
 tiefe == "30_60" ~ 1.65, 
 tiefe == "60_90" ~ 1.7)) %>% 
 mutate(cstock_tha_2020 = c_in_tm_mean * bd_2020*30) %>% 
 filter(block != "3") %>% 
 filter(block != "4") %>%  # Use if you accept removing block 3 and 4 from the dataset
 dplyr::select(-outlier) %>% 
 na.exclude()


# Summary: to add this info into the paper -------------------------------
c_perc_trt <- SOC_Hoh %>% 
  group_by(variante, duengung, tiefe) %>% 
  summarise(mean_c_per =mean(c_in_tm_mean, na.rm=TRUE))

c_perc_2014_report <-  SOC_Hoh %>% 
  filter(jahr== 2014) %>% 
  group_by(tiefe) %>% 
  summarise(mean_C_per =mean(c_in_tm_mean, na.rm=TRUE))

cn_ratio_2014_report <-  SOC_Hoh %>% 
  filter(jahr== 2014) %>% 
  group_by(tiefe) %>% 
  summarise(CN_ratio =mean(cn_mean, na.rm=TRUE))


count_parz_trt <- SOC_Hoh_without_outliers %>%  
  group_by(variante,duengung, tiefe, jahr) %>% 
  mutate(total_n = n()) %>% 
  ungroup() %>% 
  dplyr::select(variante,duengung, tiefe, jahr, total_n) 


count_parz_trt2 <- SOC_Hoh_without_outliers %>%  
  group_by(variante,duengung,  jahr, tiefe) %>% 
  mutate(total_n = n()) %>% 
  ungroup() %>% 
  dplyr::select(variante,duengung, jahr, tiefe, total_n) 



#Rename dataset before working
SOC_Hoh_without_outliers <- SOC_Hoh 

#Create new factor variables
SOC_Hoh_without_outliers$datacomb <- as.factor(paste(SOC_Hoh_without_outliers$variante,SOC_Hoh_without_outliers$duengung, sep = "_"))
SOC_Hoh_without_outliers$datacomb2 <- as.factor(paste(SOC_Hoh_without_outliers$variante,SOC_Hoh_without_outliers$duengung,SOC_Hoh_without_outliers$tiefe, sep="_"))
SOC_Hoh_without_outliers$YEAR<- as.factor(SOC_Hoh_without_outliers$akk_year)
SOC_Hoh_without_outliers$vordungduengung <- as.factor(paste(SOC_Hoh_without_outliers$vordung, SOC_Hoh_without_outliers$duengung, sep = "_"))

#Define factor variables
SOC_Hoh_without_outliers <- SOC_Hoh_without_outliers %>% 
  mutate(across(c(datacomb, datacomb2, tiefe, parz, block, vornutzung, vordung, YEAR), as.factor))


# Statistical model: takes about 20min to completion -------------------------------------------------------

mod0 <-  lme(cstock_tha_2020 ~ datacomb2*akk_year, 
             random=~1|block/vornutzung/variante/vordungduengung/parz/YEAR,
             weights=varIdent(form=~1|datacomb2), data=SOC_Hoh_without_outliers,
             control=list(maxIter=300,msMaxIter=300,niterEM=300,msMaxEval=300,opt="nlminb")
)

mod1 <- update(mod0, . ~ 0 + datacomb2 + datacomb2:akk_year)

mod1b <- update(mod0, . ~ tiefe*duengung*variante*akk_year);
anovamod1b <- anova(mod1b, type="marginal");  
rsquareMod1b<- MuMIn::r.squaredGLMM(mod1b) 


AIC(mod0,mod1,mod1b)


#Interpretation:
# the slopes differ between variante and tiefe with time
# equivalent to mod1




# Model estimates --------------------------------------------------------


mod_est<- update(mod0, . ~ 0 + variante:tiefe + variante:tiefe:akk_year)
summary(mod_est)
syst_estimates <- mod_est %>% broom.mixed::tidy(conf.int=TRUE)

fixed_ef <- summary(mod_est)$tTable[,1] #fixed-effect parameter estimates
fixed_se <-summary(mod_est)$tTable[,2] #fixed-effect parameter standard errors
fix_se_combined <- bind_cols(fixed_ef, fixed_se)

fixed_lwr_uppr <- intervals(mod4_est,which="fixed"); View(fixed_lwr_uppr[["fixed"]])





# fitted values: Takes about 16h to completion-----------------------------------------------------------
plot_est_mod4 <- mod_est %>% broom.mixed::augment() 
plot_fitted_mod4<- plot_est_mod4 

#Already ran: prepared
plot_fitted_values <- read.table(file="Z:\\Documents\\Long-term field data carbon\\Hohenschulen\\R folder - SOC analysis\\From Mario_SOC\\plot_est_hoh_mod4.txt", header=TRUE, sep="\t", dec=".")



head_info <- plot_fitted_mod4 %>% 
  dplyr::select("parz" ,    "block",  "jahr",    "akk_year",    
                "tiefe",  "datacomb" ,    "datacomb2",    ".fitted",   ".resid",     ".fixed")

head(head_info)
head(pred)




save(SOC_Hoh, SOC_Hoh_without_outliers,  
     mod0, mod1, mod1b, anovamod1b, rsquareMod1b,
     mod_est, syst_estimates, plot_fitted_values, 
     file="Hoh SOC output.Rdata")




