#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')


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
library(purrr)
library(broom)
library(xlsx)
require(mgcv)
library(multcomp)
library(rsq)
library(broom.mixed)
require(mgcv)
library(extrafont)
library(MuMIn)



(.packages())

# # data import: ----------------------------------------------------------


list.files()
getwd()

setwd()




# After outlier detection technique ---------------------------------------

SOC.Hohen_initial<- read.xlsx(file="Cperc_outliers_allplots_Hohenschulen.xlsx", sheetIndex = 1)




# Use this one if you want to work with the original dataset with outliers included (same as Christof file)----

SOC.Hohen<- SOC.Hohen_initial %>% mutate(BD_2020 = case_when(
 tiefe == "0_30" ~ 1.61, tiefe == "30_60" ~ 1.65, tiefe == "60_90" ~ 1.7)) %>% 
 mutate(Cstock_tha_BDfix_2020 = C_in_TM_mean*BD_2020*30) %>% 
 filter(block != "3") %>% filter(block != "4") %>%           # Use if you accept removing block 3 and 4 from the dataset
 dplyr::select(-outlier) %>% 
 na.exclude()





# Variable created from old dataset to new one -----------------------------------------------------------------------


SOC_Hoh_without_outliers <- SOC.Hohen 



# CN ratio and Cstock 2014 and 2020  -----------------------------------------------------------------------


#2014

CN_2014 <- SOC_Hoh_without_outliers %>% filter(jahr =="2014") %>%  group_by(variante, duengung,   tiefe) %>%  #variante, duengung |vornutzung, vordung,
 summarise(initial_CN = mean(CN_mean), sd_CN = sd(CN_mean))
#write.table(x= SOC_Hoh_without_outliers_CN, file="CN_2014_premanag.txt", col.names = TRUE, sep="\t", dec=".")


Cstock_2014 <- SOC_Hoh_without_outliers %>% filter(jahr =="2014") %>%  group_by(variante, duengung,  tiefe) %>% 
 summarise(initial_Cstock = mean(Cstock_tha_BDfix_2020), sd_Cstock = sd(Cstock_tha_BDfix_2020)) 


CN_Cstock2014 <- SOC_Hoh_without_outliers  %>% filter(jahr =="2014") %>% group_by(block, parz, jahr, tiefe, variante, duengung, vornutzung, vordung) %>% 
 summarise(initial_CN = mean(CN_mean), initial_Cstock = mean(Cstock_tha_BDfix_2020)) %>% ungroup() #%>% filter(tiefe=="30_60") 


#2020

CN_2020 <- SOC_Hoh_without_outliers %>% filter(jahr =="2020") %>%  group_by(variante, duengung,   tiefe) %>%  #variante, duengung |vornutzung, vordung,
 summarise(initial_CN = mean(CN_mean), sd_CN = sd(CN_mean))
#write.table(x= SOC_Hoh_without_outliers_CN, file="CN_2014_premanag.txt", col.names = TRUE, sep="\t", dec=".")


Cstock_2020 <- SOC_Hoh_without_outliers %>% filter(jahr =="2020") %>%  group_by(variante, duengung,  tiefe) %>% 
 summarise(initial_Cstock = mean(Cstock_tha_BDfix_2020), sd_Cstock = sd(Cstock_tha_BDfix_2020)) 


CN_Cstock2020 <- SOC_Hoh_without_outliers  %>% filter(jahr =="2020") %>% group_by(block, parz, jahr, tiefe, variante, duengung, vornutzung, vordung) %>% 
 summarise(final_CN = mean(CN_mean), final_Cstock = mean(Cstock_tha_BDfix_2020)) %>% ungroup() #%>% filter(tiefe=="30_60") 



# Get all slopes by plot and treatment of Cstock and CN ratio -----------------------------------------------------------------------


#Cstock
Reg_tiefe_parz_Cstock<- SOC_Hoh_without_outliers %>%
  group_by(parz,tiefe, variante, duengung, vornutzung, vordung) %>% 
 do(tidy(lm(Cstock_tha_BDfix_2020~akk_year,data=.))) %>% filter(term == "akk_year")


Reg_tiefe_parz2_Cstock<- SOC_Hoh_without_outliers %>% 
  group_by(parz,tiefe, variante, duengung, vornutzung, vordung) %>% 
 do(tidy(lm(Cstock_tha_BDfix_2020~akk_year,data=.))) %>% 
  filter(term == "akk_year") %>% 
 group_by(tiefe, variante, duengung, vornutzung, vordung) %>% 
 summarise(mean_rate = mean(estimate))

Reg_tiefe_parz3_Cstock<- SOC_Hoh_without_outliers %>% 
  group_by(parz,tiefe, variante, duengung, vornutzung, vordung) %>% 
 do(tidy(lm(Cstock_tha_BDfix_2020~akk_year,data=.))) %>% 
  filter(term == "akk_year") %>% 
 group_by(parz, tiefe, variante, duengung, vornutzung, vordung) %>% 
 summarise(mean_rate = mean(estimate))


#data storage
#Reg_tiefe_parz3 <- write.table(x= Reg_tiefe_parz3, file="Reg_tiefe_parz3.txt",col.names = TRUE, sep="\t", dec=".")


# To analyze CN and Cstock as covariate; effect on slopes -----------------------------------------------------------------------


Reg_tiefe_parz_Cstock <- Reg_tiefe_parz_Cstock 

#2014
new_dataset_CN_Cstock <-  left_join(Reg_tiefe_parz_Cstock, CN_Cstock2014, 
                                    by= c("parz", "tiefe", "variante", "duengung", 
                                          "vornutzung", "vordung")) 

#2020
new_dataset_CN_Cstock <-  left_join(new_dataset_CN_Cstock, CN_Cstock2020,  
                                    by= c("parz", "tiefe", "variante", "duengung", 
                                          "vornutzung", "vordung", "block")) #%>%




#After AP correction: USE
Name ="Hoh_Cinput_27.09.2021_2015-2020.xlsx" 
Cinput_col<- read.xlsx(file=Name, sheetIndex = 2)


str(Cinput_col)
Cinput_col$parz <- as.numeric(Cinput_col$parz);
Cinput_col$block <- as.numeric(Cinput_col$block)
levels(Cinput_col$variante) <- c("GC", "NT_GR", "NT_CM", "CT_CM")
Cinput_col<- clean_names(Cinput_col); colnames(Cinput_col)

Cinput_col_clean <-Cinput_col %>% dplyr::select(parz, block, variante, duengung, total_harvest_tha, stubbles_cinput_tha, roots_cinput_tha,   rhizodepo_c,   soil_cinput_tha,   soil_cinput_tha_re)%>%
 mutate_if(is.numeric, round, 2) %>% filter(parz <=72)


#Continue under for statistical analysis



#  Prepare for paper: yield and C input by system:USE -------------------------------------------------------------------------
#Same as "C inputs from harvest Hohenschulen - 2015 - 2020.R"


Yield_Cinput ="Hoh_Cinput_27.09.2021_2015-2020.xlsx" 
Yield_Cinput_report<- read.xlsx(file=Yield_Cinput, sheetIndex = 1)

Yield_Cinput_final_report <- Yield_Cinput_report %>% 
  filter(parz <=72) %>% 
  group_by(variante, duengung) %>%  
  summarise(
    harvest_tha_mean = mean(as.numeric(total.harvest.tha), na.rm = TRUE),
    harvest_tha_sd = sd(as.numeric(total.harvest.tha), na.rm = TRUE),
    stubbles_cinput_tha_mean = mean(as.numeric(Stubbles.Cinput.tha), na.rm= TRUE),
    stubbles_cinput_tha_sd = sd(as.numeric(Stubbles.Cinput.tha), na.rm= TRUE),
    roots_cinput.tha  = mean(as.numeric(Roots.Cinput.tha), na.rm=TRUE),
    roots_cinput.tha_sd  = sd(as.numeric(Roots.Cinput.tha), na.rm=TRUE),
    rhizodepo_c_mean = mean(as.numeric(Rhizodepo_C), na.rm=TRUE),
    rhizodepo_c_sd = sd(as.numeric(Rhizodepo_C), na.rm=TRUE),
    root_re_mean = mean(as.numeric(Root_RE_mean), na.rm=TRUE),
    root_re_sd = sd(as.numeric(Root_RE_mean), na.rm=TRUE),
    soil_cinput_tha_mean = mean(as.numeric(Soil.Cinput.tha), na.rm=TRUE),
    soil_cinput_tha_sd = sd(as.numeric(Soil.Cinput.tha), na.rm=TRUE),
    soil_cinput_tha_re_mean = mean(as.numeric(Soil.Cinput.tha.RE), na.rm=TRUE),
    soil_cinput_tha_re_sd = sd(as.numeric(Soil.Cinput.tha.RE), na.rm=TRUE)) %>%
  ungroup() %>% mutate_if(is.numeric, round,1)

  
#Save it where the previous report is.
getwd(); setwd("Z:Hohenschulen_Cinput_2021-09-27_after AP_Use")

write.xlsx(x=Yield_Cinput_final_report, 
           file= "Yield Cinput-finalreport-after Christof-20-01-2022.xlsx", 
           col.names=TRUE,
           append=FALSE)



#####Continue below for statistics




# Dataset joining : ---------------------------------------------------------


##Be careful in choosing between 0-30 and whole profile



#0-30cm
new_dataset_CN_Cstock_30cm <- new_dataset_CN_Cstock %>%  filter(tiefe =="0_30") 

SOC_Cinput_rate<-left_join(new_dataset_CN_Cstock_30cm, Cinput_col_clean, 
                           by= c("parz", "variante", "duengung", "block")) 


#Whole profile
#new_dataset_CN_Cstock_wholeprofile <- new_dataset_CN_Cstock 
#SOC_Cinput_rate<-left_join(new_dataset_CN_Cstock_wholeprofile, Cinput_col_clean, by= c("parz", "variante", "duengung", "block")) 


#transformations

SOC_Cinput_rate <- as_tibble(SOC_Cinput_rate)
SOC_Cinput_rate$variante <-as.factor(SOC_Cinput_rate$variante)


levels(SOC_Cinput_rate$variante) <- c("CT-CM", "GC", "NT-CM", "NT-GR")
levels(SOC_Cinput_rate$duengung) 




# Using the experimental model -----------------------------------------------------------------------

SOC_Cinput_rate2 <- SOC_Cinput_rate %>% filter(variante !="CT-CM") 



SOC_Cinput_rate2$datacomb <- as.factor(paste(SOC_Cinput_rate2$variante,SOC_Cinput_rate2$duengung, sep = "_"))
#SOC_Cinput_rate2$datacomb2 <- as.factor(paste(SOC_Cinput_rate2$variante,SOC_Cinput_rate2$duengung,SOC_Cinput_rate2$tiefe, sep="_"))
SOC_Cinput_rate2$tiefe <- as.factor(SOC_Cinput_rate2$tiefe)
SOC_Cinput_rate2$parz <- as.factor(SOC_Cinput_rate2$parz)
SOC_Cinput_rate2$block <- as.factor(SOC_Cinput_rate2$block)
SOC_Cinput_rate2$vornutzung <- as.factor(SOC_Cinput_rate2$vornutzung)
SOC_Cinput_rate2$vordung <- as.factor(SOC_Cinput_rate2$vordung)
#SOC_Cinput_rate2$YEAR<- as.factor(SOC_Cinput_rate2$akk_year)

SOC_Cinput_rate2$vordungduengung <- as.factor(paste(SOC_Cinput_rate2$vordung, SOC_Cinput_rate2$duengung, sep = "_"))

# SOC_Cinput_rate2 <- as.data.frame(SOC_Cinput_rate2)
colnames(SOC_Cinput_rate2)

SOC_Cinput_rate2 <- SOC_Cinput_rate2 %>% na.omit()



# Model -----------------------------------------------------------------------

mod0 <-  lme(estimate ~ datacomb*soil_cinput_tha_re,   
             random=~1|block/vornutzung/variante/vordungduengung/parz, #/YEAR
             weights=varIdent(form=~1|datacomb), data=SOC_Cinput_rate2,
             control=list(maxIter=300,msMaxIter=300,niterEM=300,msMaxEval=300,opt="optim")
)
mod1 <- update(mod0, . ~ 0 + datacomb + datacomb:soil_cinput_tha_re); anova(mod1, type="marginal")

mod1b <- update(mod0, . ~ duengung*variante*soil_cinput_tha_re) ; anova(mod1b, type="marginal") ; MuMIn::r.squaredGLMM(mod1b)

AIC(mod0,mod1,mod1b)



mod2 <- update(mod1b, . ~ . -duengung:variante:soil_cinput_tha_re); anova(mod2, type="marginal"); MuMIn::r.squaredGLMM(mod2)
anova(mod1b,mod2)

mod3 <- update(mod2, . ~ . -variante:soil_cinput_tha_re); anova(mod3, type="marginal");  MuMIn::r.squaredGLMM(mod3)
anova(mod1b,mod3)

mod4 <- update(mod3, . ~ . -duengung:soil_cinput_tha_re); anova(mod4,  type="marginal");  MuMIn::r.squaredGLMM(mod4)

mod5 <- update(mod4, . ~ . -duengung:variante); anova(mod5,  type="marginal");  MuMIn::r.squaredGLMM(mod5)

mod6 <- update(mod5, . ~ . -variante); anova(mod6,  type="marginal") ;  MuMIn::r.squaredGLMM(mod6)


AIC(mod1b,mod2, mod3,mod4,mod5,mod6); MuMIn::AICc(mod1b,mod2, mod3,mod4,mod5,mod6)



#Best model is mod1b,
#It reveals there are significant main effects by the N rates only and no interaction with  annual soil C inputs
# Resulting in Two intercepts and a common slope

#Same result is obtained for Mod6 and Mod04b





# Choose models mod0b3, mod0b4 or and mod0b_linear --------------



#https://stats.stackexchange.com/questions/3944/explain-model-adjustment-in-plain-english

#common slope but separate intercepts for N levels

mod0b3 <- update(mod0, . ~ duengung*soil_cinput_tha_re); 
anova(mod0b3,  type="marginal"); MuMIn::r.squaredGLMM(mod0b3)
broom.mixed::tidy(mod0b3, conf.int=TRUE)


anova.lme(mod1b,mod0b3); MuMIn::AICc(mod1b,mod0b3)


mod0b4 <- update(mod0, . ~ duengung + soil_cinput_tha_re); 
anova(mod0b4,  type="marginal"); MuMIn::r.squaredGLMM(mod0b4)
broom.mixed::tidy(mod0b4, conf.int=TRUE)

anova.lme(mod0b3,mod0b4); MuMIn::AICc(mod0b3,mod0b4); 



mod0b_linear <- update(mod0, . ~ soil_cinput_tha_re); 
anova(mod0b_linear ,  type="marginal"); MuMIn::r.squaredGLMM(mod0b_linear )
broom.mixed::tidy(mod0b_linear, conf.int=TRUE)

anova(mod1b,mod0b3, mod0b_linear)



#tested but not valid
mod0bx <- update(mod0, . ~ variante + duengung); 
anova(mod0bx,  type="marginal"); MuMIn::r.squaredGLMM(mod0bx)
broom.mixed::tidy(mod0bx, conf.int=TRUE)

mod0by<- update(mod0, . ~ variante + soil_cinput_tha_re); 
anova(mod0by,  type="marginal");MuMIn::r.squaredGLMM(mod0by)
broom.mixed::tidy(mod0bx, conf.int=TRUE)

#replace variante or duengung
mod0bz<- update(mod0, . ~ variante); 
anova(mod0bz,  type="marginal");MuMIn::r.squaredGLMM(mod0bz)
broom.mixed::tidy(mod0bx, conf.int=TRUE)

MuMIn::AICc(mod1b, mod0bx, mod0bz); anova.lme(mod1b, mod0bx, mod0bz)

#quadratic
mod0b_quadratic <- update(mod0, . ~  soil_cinput_tha_re + I(soil_cinput_tha_re^2)); 
anova(mod0b_quadratic,  type="marginal"); MuMIn::r.squaredGLMM(mod0b_quadratic)

summary(mod0b_quadratic)
broom.mixed::tidy(mod0b_quadratic, conf.int=TRUE)


anova(mod0b_linear, mod0b_quadratic)




# Estimates ---------------------------------------------------------



#full model
mod1b_fitted <- mod1b %>% broom.mixed::augment(conf.int=TRUE)

# Like linear: duengung*soil_cinput_tha_re
mod0b3_fitted <- mod0b3 %>% broom.mixed::augment(conf.int=TRUE)

#linear using soil C inputs only
mod0b_linear_fitted <- mod0b_linear %>%  broom.mixed::augment()

#common slope but separate intercepts for N levels
mod0b4_fitted <- update(mod0, . ~ duengung + duengung:soil_cinput_tha_re); 
anova(mod0b4,  type="marginal"); MuMIn::r.squaredGLMM(mod0b4)
mod0b4_fitted <- broom.mixed::augment(mod0b4, conf.int=TRUE)


#Between Variante + Duengung
mod0bx_fitted <-broom.mixed::augment(mod0bx, conf.int=TRUE)



#Quadratic
mod0b_quadratic_fitted <- mod0b_quadratic %>% broom.mixed::augment()




#model comparisons: better mod0b4

anova(mod0b3, mod0b4, mod0b_linear)





# Residual analysis -------------------------------------------------------

#residual analysis mod1b

expected <- fitted(mod1b)
residuals <- resid(mod1b)

windows(10,4); par(mfrow=c(1,3))
boxplot(residuals, main="residual boxplot")
plot(x=expected, y=residuals, xlab="expected values", ylab="residuals", main="residual plot")
abline(h=0)

plot(mod1b, which=2)

# or:
windows(10,5); par(mfrow=c(1,2))
plot(mod0b4, which=1:2)



#residual analysis mod0b3

expected <- fitted(mod0b3)
residuals <- resid(mod0b3)

windows(10,4); par(mfrow=c(1,3))
boxplot(residuals, main="residual boxplot")
plot(x=expected, y=residuals, xlab="expected values", ylab="residuals", main="residual plot")
abline(h=0)

plot(mmod0b3, which=2)

# or:
windows(10,5); par(mfrow=c(1,2))
plot(mod0b4, which=1:2)



#residual analysis mod0b4

expected <- fitted(mod0b4)
residuals <- resid(mod0b4)

windows(10,4); par(mfrow=c(1,3))
boxplot(residuals, main="residual boxplot")
plot(x=expected, y=residuals, xlab="expected values", ylab="residuals", main="residual plot")
abline(h=0)
plot(mmod0b4, which=2)

# or:
windows(10,5); par(mfrow=c(1,2))
plot(mod0b4, which=1:2)



#residual analysis linear model

expected <- fitted(mod0b_linear)
residuals <- resid(mod0b_linear)

windows(10,4); par(mfrow=c(1,3))
boxplot(residuals, main="residual boxplot")
plot(x=expected, y=residuals, xlab="expected values", ylab="residuals", main="residual plot")
abline(h=0)

plot(mod0b_linear, which=2)

# or:
windows(10,5); par(mfrow=c(1,2))
plot(mod0b_linear, which=1:2)


# residual analysis quadratic model


expected <- fitted(mod0b_quadratic)
residuals <- resid(mod0b_quadratic)

windows(10,4); par(mfrow=c(1,3))
boxplot(residuals, main="residual boxplot")
plot(x=expected, y=residuals, xlab="expected values", ylab="residuals", main="residual plot")
abline(h=0)
plot(mod0b_quadratic, which=2)

# or:
windows(10,5); par(mfrow=c(1,2))
plot(mod0b_quadratic, which=1:2)




# plot fitted values ------------------------------------------------------

#mod0b_quadratic_fitted_plot <- mod0b_quadratic_fitted %>% 
mod0b_quadratic_fitted_plot <- mod0b4_fitted %>% group_by(duengung) %>% 
summarise(estimate = mean(estimate, na.rm=TRUE), 
          estimate_fitted = mean(.fitted, na.rm=TRUE),
          estimate_mean_fixed  = mean(.fixed, na.rm=TRUE)) %>% ungroup()



#Choose model, also modify below
my.formula.linear <- y ~ x
my.formula.quadratic <- y ~ poly(x, 2, raw = TRUE)



#This will reorder the levels of fruchtff to the desire plot order
mod0b3_fitted$variante <- factor(mod0b3_fitted$variante, 
                                 levels = c("GC", "NT-GR", "NT-CM")) 

mod0b3_fitted <-as_tibble(mod0b3_fitted)
attributes(mod0b3_fitted)


# Choose mod0b3 --------------------------------------------


x <- ggplot(data = mod0b3_fitted, aes(x =soil_cinput_tha_re, y =.fixed)) 


figure <- x + 
 
 geom_point(aes(y=estimate, col=variante, shape=duengung)) + #Option 1
 # geom_point(aes(y=.fitted, col=variante)) +                #Option 2
 
 geom_smooth(aes(y =.fixed), method= "lm", formula = my.formula.linear, se=FALSE, size=1) +

 geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.5, alpha=1) +
 geom_vline(xintercept =6.45, linetype="dashed", color = "red", size=0.5, alpha=1) +
 annotate("text", x = 7.4, y = 0.05, label = "75 Mg SOC per ha") + # x = 7.7
  
 stat_poly_eq(formula = my.formula.linear, aes(label = paste(after_stat(eq.label), after_stat(n.label), sep = '*","~~~')), parse = TRUE, size=5, family= "Helvetica") +
  #stat_poly_eq(formula = my.formula.quadratic,  aes(label = paste(stat(eq.label), sep = "~~~")),  parse = TRUE, size=5) +  #, stat(rr.label) not needed
 
 scale_color_colorblind() +
  
 theme_bw(base_size=14, base_family = "Helvetica") + 
 theme(
  text=element_text(family="Helvetica"),
  axis.line= element_line(colour= "black", size= 0.5), #size= 0.5 equalize this with panel border and strip background
  axis.text = element_text(color = "black"), 
  #axis.title= element_text(color = "black", face="bold"), #, size = 12
  panel.grid= element_blank(),
  panel.background= element_blank(),
  # panel.border= element_blank(), #size=0.5
  panel.border= element_rect(fill=NULL, colour="black", size= 0.5), 
  strip.background= element_rect(fill="White", color="black",size=0.5), 
  strip.text= element_text(colour="black", face="bold"),  
  strip.text.y= element_text(angle = 0),
  legend.position = "bottom",
  legend.text= element_text(colour="black", face="bold"), 
  legend.title = element_text(colour="black", face = "bold")) + 
 
  labs(col= "Cropping System", shape = "N rate", x = bquote('Annual soil C input rate ('*'Mg' ~ ha^-1*')'), y= bquote('Annual SOC change ( '*'Mg' ~ ha^-1*')'))   #Option 1
 #labs(col= "Cropping System", x = bquote('Annual soil C input rate ('*'Mg' ~ ha^-1*')'), y= bquote('Annual SOC change ( '*'Mg' ~ ha^-1*')'))                     #Option 2
 
dev.new(); figure ; 
dev.size(units = "cm")

setwd("")

ggsave(filename="MSC with common slope mixed model2-Linear-USE-opt2-Helvetica2-after Mario feb 2022.tiff", dpi=1000,  units= "cm", compression ="lzw") 
# A4 width:16 size






# Duengung + soil_C_inputs_re --------------------------------------------

x <- ggplot(data = mod0b4_fitted, aes(x =soil_cinput_tha_re, y =.fixed, col=duengung, shape=duengung, linetype=duengung)) # shape=variante, col=duengung))

figure <- x + 
 
 geom_point(aes(y=.fitted), alpha = 1) +
 
 geom_smooth(aes(y =.fixed), method= "lm", formula = my.formula.linear) +
 #geom_smooth(aes(y =.fixed), method= "lm", formula = my.formula.quadratic) +
 
 geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.5, alpha=1) +
 geom_vline(xintercept =5.0, linetype="dashed", color = "red", size=0.5, alpha=1) +
 geom_vline(xintercept =7.3, linetype="dashed", color = "red", size=0.5, alpha=1) +
 
 stat_poly_eq(formula = my.formula.linear,  aes(label = paste(stat(eq.label), sep = "~~~")),  parse = TRUE, size=5) +  #, stat(rr.label) not needed
 #stat_poly_eq(formula = my.formula.quadratic,  aes(label = paste(stat(eq.label), sep = "~~~")),  parse = TRUE, size=5) +  #, stat(rr.label) not needed

 theme_bw(base_size=14, base_family = "Times New Roman") + 
 theme(
  text=element_text(family="Times New Roman"),
  axis.line= element_line(colour= "black", size= 0.5), #size= 0.5 equalize this with panel border and strip background
  axis.text = element_text(color = "black"), 
  #axis.title= element_text(color = "black", face="bold"), #, size = 12
  panel.grid= element_blank(),
  panel.background= element_blank(),
  # panel.border= element_blank(), #size=0.5
  panel.border= element_rect(fill=NULL, colour="black", size= 0.5), 
  strip.background= element_rect(fill="White", color="black",size=0.5), 
  strip.text= element_text(colour="black", face="bold"),  
  strip.text.y= element_text(angle = 0),
  legend.position = "bottom",
  legend.text= element_text(colour="black", face="bold"), 
  legend.title = element_text(colour="black", face = "bold")) + 
 labs(shape= "N rate", col="N rate", linetype="N rate", x = bquote('Annual soil C input rate ('*'Mg' ~ ha^-1*')'), y= bquote('Annual SOC change ( '*'Mg' ~ ha^-1*')'))

dev.new(); figure


setwd()
ggsave(filename="MSC main effect N rate with common slope Hoh-06-10-2021-mixed model2.tiff", dpi=1000,  units= "cm", compression ="lzw") # A4 width:16 size


# Variante + Duengung: not valid yet -----------------------------------------------------


y <- ggplot(data = mod1b, aes(y =.fixed, fill=duengung)) # shape=variante, col=duengung))

figure <- y + 
 
 geom_boxplot() + 

 theme_bw(base_size=14, base_family = "Times New Roman") + 
 theme(
  text=element_text(family="Times New Roman"),
  axis.line= element_line(colour= "black", size= 0.5), #size= 0.5 equalize this with panel border and strip background
  axis.text = element_text(color = "black"), 
  #axis.title= element_text(color = "black", face="bold"), #, size = 12
  panel.grid= element_blank(),
  panel.background= element_blank(),
  # panel.border= element_blank(), #size=0.5
  panel.border= element_rect(fill=NULL, colour="black", size= 0.5), 
  strip.background= element_rect(fill="White", color="black",size=0.5), 
  strip.text= element_text(colour="black", face="bold"),  
  strip.text.y= element_text(angle = 0),
  legend.position = "bottom",
  legend.text= element_text(colour="black", face="bold"), 
  legend.title = element_text(colour="black", face = "bold")) + 
 labs(fill="N rate",  x = bquote('Annual soil C input rate ('*'Mg' ~ ha^-1*')'), y= bquote('Annual SOC change ( '*'Mg' ~ ha^-1*')'))

dev.new(); figure


setwd()
ggsave(filename="MSC main effect N rate with common slope Hoh-06-10-2021-mixed model2.tiff", dpi=1000,  units= "cm", compression ="lzw") # A4 width:16 size






# # - Trash  ----------------------------------------------------


# Output 2: including CT_CM-----------------------------------------------------------------------

#Using individual datapoints (ungroup)


Output2<- SOC_Cinput_rate %>%
 na.omit() %>% 
 filter(variante !="CT-CM") %>% 
 rename(deltaSOC=estimate) %>% 
 do(tidy(lm(deltaSOC~soil_cinput_tha_re,data=.)))


#same like
SOC_Cinput_rate2 <- SOC_Cinput_rate %>% filter(variante !="CT-CM") 



methods(class = "lm")


########
"Linear"
########
Output2mod <- lm(estimate~soil_cinput_tha_re, 
                 #weights =, soil_cinput_tha_re,
                 na.action = na.omit,
                 data=SOC_Cinput_rate2)


plot(Output2mod) 

tidy(Output2mod , conf.int=TRUE)
glance(Output2mod)
summary(Output2mod)




###########
"Quadratic" 
###########
Output2mod2 <- lm(estimate~soil_cinput_tha_re + I(soil_cinput_tha_re^2), data=SOC_Cinput_rate2)


#plot (Output2mod2)
tidy(Output2mod2, conf.int=TRUE)
glance(Output2mod2)
summary(Output2mod2)


D(expression(a + b*X + c*X^2), "X")
D(expression(-2.87 + 0.838*X + -0.0534*X^2), "X")



AIC(Output2mod,Output2mod2)
anova(Output2mod,Output2mod2)

# Output 3: ---------------------------------------------------------------------



#Using averaged values

Output3<- SOC_Cinput_rate %>%   
 na.omit()  %>% 
 filter(variante !="CT-CM") %>% 
 group_by(variante, duengung) %>%
 summarise(Cinputs_noRE = mean(soil_cinput_tha),  Cinputs_RE=mean(soil_cinput_tha_re), deltaSOC=mean(estimate)) %>%  
 ungroup() %>% 
 do(tidy(lm(deltaSOC~Cinputs_RE,data=.))) 


SOC_Cinput_rate3 <-  SOC_Cinput_rate %>%   
 na.omit()  %>%  filter(variante !="CT-CM") %>% 
 group_by(variante, duengung) %>% 
 summarise(soil_cinput_tha=mean(soil_cinput_tha), soil_cinput_tha_re=mean(soil_cinput_tha_re), estimate=mean(estimate)) %>%  ungroup()

Output3mod <- lm(estimate~soil_cinput_tha_re, data=SOC_Cinput_rate3)
Output3mod2 <- lm(estimate~soil_cinput_tha_re + I(soil_cinput_tha_re^2), data=SOC_Cinput_rate3)


########
"Linear"
########

tidy(Output3mod , conf.int=TRUE)
glance(Output3mod)
summary(Output3mod)


###########
"Quadratic" 
###########

tidy(Output3mod2, conf.int=TRUE)
glance(Output3mod2)
summary(Output3mod2)

AIC(Output3mod,Output3mod2)



# Figure for 0 -30 cm ------------------------------------------------------------------
#Represents individual data regression line


#Use both to produce the figures with individual and averaged points.
SOC_Cinput_rate_figure  <- SOC_Cinput_rate %>%   
 na.omit()  %>% filter(variante !="CT-CM")



group_rate<- SOC_Cinput_rate %>%   
 na.omit()  %>% 
 filter(variante !="CT-CM") %>% 
 group_by(variante, duengung) %>%
 summarise(soil_cinput_tha = mean(soil_cinput_tha),  soil_cinput_tha_re=mean(soil_cinput_tha_re), estimate=mean(estimate)) %>%  
 ungroup()



#Choose model, also modify below
my.formula.linear <- y ~ x
my.formula.quadratic <- y ~ poly(x, 2, raw = TRUE)


#choose individual 
x <- ggplot(data = SOC_Cinput_rate_figure, aes(x =soil_cinput_tha_re, y =estimate))
#or grouped data
x <- ggplot(data = group_rate, aes(x =soil_cinput_tha_re, y =estimate))  



figure <- x + 
 
 geom_point(aes(shape=variante, col=duengung), alpha = 0.4) +
 geom_point(data = group_rate, aes(shape=variante, col=duengung), size=3) +
 geom_smooth(method= "lm", formula = my.formula.quadratic) +
 geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.5, alpha=1) +
 geom_vline(xintercept =5.05, linetype="dashed", color = "red", size=0.5, alpha=1) +
 stat_poly_eq(formula = my.formula.quadratic,  aes(label = paste(stat(eq.label), stat(rr.label), sep = "~~~")),  parse = TRUE, size=5) +  
 theme_bw(base_size=14, base_family = "Times New Roman") + #base_size=14 #, 
 theme(
  text=element_text(family="Times New Roman"),
  axis.line= element_line(colour= "black", size= 0.5), #size= 0.5 equalize this with panel border and strip background
  axis.text = element_text(color = "black"), 
  #axis.title= element_text(color = "black", face="bold"), #, size = 12
  panel.grid= element_blank(),
  panel.background= element_blank(),
  # panel.border= element_blank(), #size=0.5
  panel.border= element_rect(fill=NULL, colour="black", size= 0.5), 
  strip.background= element_rect(fill="White", color="black",size=0.5), 
  strip.text= element_text(colour="black", face="bold"),  
  strip.text.y= element_text(angle = 0),
  legend.position = "bottom",
  legend.text= element_text(colour="black", face="bold"), 
  legend.title = element_text(colour="black", face = "bold")) + 
 labs(shape= "Cropping system", col="N-rate", x = bquote('Annual soil C input rate ('*'Mg' ~ ha^-1*')'), y= bquote('Annual SOC change ( '*'Mg' ~ ha^-1*')'))

dev.new(); figure



setwd("Z:\\Documents\\Long-term field data carbon\\Hohenschulen\\R folder - SOC analysis\\From Mario_SOC")


ggsave(filename="Z:\\Documents\\Long-term field data carbon\\Hohenschulen\\R folder - SOC analysis\\From Mario_SOC\\MSC plots\\MSC linear2- Hoh-updated-28-09-2021-wguides3.tiff", dpi=1000,  units= "cm", compression ="lzw") # A4 width:16 size



shell.exec("Regression estimates of soil C inputs vs Annual SOC change rate.xlsx")




# Using non-linear models:not working yet-----------------------------------------------------------------------

test <- SOC_Cinput_rate %>% dplyr::select(estimate, soil_cinput_tha_re)




"Starting values"
shell.exec("Z:\\Documents\\Long-term field data carbon\\Hohenschulen\\R folder - SOC analysis\\From Mario_SOC\\Asymptote parameters.R")
#   b0    b1   lrc 
#-3.30  4.85 -1.65 
#(a= -3.30, b= 4.85, c= -1.65)

#manually: (a= 0, b= 5, c= 0.52)




"Michaelis-menten asymptotic exponential" #Error singular gradient
#parameters: a/b
mod1a <- nls(estimate~(a*soil_cinput_tha_re)/(1+b*soil_cinput_tha_re),
             #start=list(a= -3.30, b= 4.85),
             start=list(a= -1.8, b= 4.85),
             data = test,
             trace = TRUE)
summary(mod1a)


a=0 
b=-1.6
mod1b <- nls(estimate~SSmicmen(soil_cinput_tha_re, a, b))



"3-parameter asymptotic exponential" #Error singular gradient
mod2a <- nls(estimate~a-b*exp(-c*soil_cinput_tha_re),
             start=list(a= -3.30, b= 4.85, c= -1.65),
             #start=list(a= 0, b= 5, c= 0.52),
             data = test)
summary(mod2a)


"3-parameter asymptotic exponential: 2nd version"
mod2b <- nls(estimate~a + b*(1-exp(-exp(lrc) * soil_cinput_tha_re)),
             #start=list(a= -3.30, b= 4.85, lrc= -1.65),
             #start=list(a= 0, b= 5, c= 0.52),
             data = test,
             trace = TRUE)
summary(mod2b)



"2-parameter asymptotic exponential: Use b or c"

#######
###b###
#######
mod3b <- nls(estimate~a*(1-exp(-c*soil_cinput_tha_re)), #non significant
             start=list(a= -3.30, c= -1.65), # b= 5,
             #start=list(a= 0, b= 5, c= 0.52),
             data = test)
summary(mod3b)

######
###c##
######
mod3c <- nls(estimate~a*(1-exp(-b*soil_cinput_tha_re)), #Error singular gradient
             start=list(a= -3.30, b= 4.85), # b= 5,
             #start=list(a= 0, b= 5, c= 0.52),
             data = test)
summary(mod3c)





#power functions
mod4 <- nls(estimate~I(soil_cinput_tha_re^power), start=list(power= 1/5), data = test)
summary(mod4)


AIC(mod2b, mod3b, mod4)





