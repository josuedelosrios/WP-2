

# Binding all three soil depths -------------------------------------------


SOC_Hoh_without_outliers<- rbind(SOC.Hohen_tiefe_0_30, SOC.Hohen_tiefe_30_60, SOC.Hohen_tiefe_60_90) %>% as.data.frame()

colnames(SOC_Hoh_without_outliers)



levels(SOC_Hoh_without_outliers$variante) <- c("GC", "NT-GR", "NT-CM", "CT-CM")

# Printing all plots ------------------------------------------------------



library(ggrepel)
library(extrafont)
loadfonts(device = "win")


#######################################################################################

# printing all plots by tiefe: 0_30- 30_60 and 0_90 -------------------------------------

#######################################################################################


SOC_Hoh_without_outliers_plot_2 <- SOC_Hoh_without_outliers %>%
  group_by(parz, block, jahr, akk_year, tiefe, kultur, variante, duengung, vornutzung , vordung) %>% #
  # vornutzung, vordung are introduced as it has an important effect on SOC.
  summarise(
    BD2020_Cstock_mean = mean(C_in_TM*BD_2020*30, na.rm=TRUE), #BDf2020_Cstock_tha_mean 
    BD2020_Nstock_mean = mean(N_in_TM*BD_2020*30, na.rm=TRUE),
    BD2020_Cstock_sd = sd(C_in_TM*BD_2020*30,  na.rm=TRUE),
    BD2020_Nstock_sd = sd(N_in_TM*BD_2020*30,  na.rm=TRUE),
    CN_mean = mean(CN, na.rm=TRUE), #CN_fix
    CN_sd = sd(CN, na.rm=TRUE), #CN_fix
    CN_min = min(CN, na.rm=TRUE), #CN_fix
    CN_max = max(CN, na.rm=TRUE), #CN_fix
    C_in_TM_mean = mean(C_in_TM, na.rm=TRUE),
    C_in_TM_sd = sd(C_in_TM, na.rm=TRUE),
    N_in_TM_mean = mean(N_in_TM, na.rm=TRUE),
    n= n()) %>%
  ungroup() %>%  mutate(C_in_TM_cv= 100*(C_in_TM_sd/C_in_TM_mean)) %>% mutate_if(is.numeric, ~round(., 2)) #%>%  filter(tiefe == "0_30") # tiefe == "30_60") tiefe == "60_90")
#%>% mutate(Annotation = paste(SOC_Hoh_without_outliers_plot$block, SOC_Hoh_without_outliers_plot$parz, sep ="-", colapse=NULL))   #%>% filter(jahr == "2014")


#SOC_Hoh_without_outliers_plot$Annotation <- as.factor(SOC_Hoh_without_outliers_plot$Annotation)



my.formula.linear <- y ~ x
levels(SOC_Hoh_without_outliers_plot_2$variante) <- c("GC", "NT-GR", "NT-CM", "CT-CM")






#######################################################################################      

# Boxplot of using all C% values,  differentiating the treaments: including all parz at tiefe: ??? See dataset -

#######################################################################################


#############
# Version 1 
#############


# variante + duengung ~ tiefe: all blocks together



dev.new()


y <- ggplot(SOC_Hoh_without_outliers_plot_2, aes(as.factor(block), y=C_in_TM_mean, col=duengung)) #, 


y + geom_boxplot(outlier.colour = "dark green", outlier.shape = 3,outlier.fill = "green") + # scale_fill_hue(l=40, c=35) + 
  geom_text(aes(label=paste(parz, jahr, sep = "_")), size=2,check_overlap=TRUE) +     #label = C_in_TM_mean    #paste(parz, block, sep = "_"))
  geom_point(aes(col=duengung),  alpha=0.5, position = position_jitterdodge()) +
  
  facet_grid(tiefe~variante+duengung) + # (tiefe~variante) + #(variante+tiefe~block)   (variante~vornutzung+vordung)
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text( size=16),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.text.x = element_text(size=16),
    axis.text.y= element_text(size = 16),
    legend.text= element_text(size= 16), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    strip.background = element_rect (fill= "white"), 
    strip.text.x = element_text(size= 16, face="bold"),
    strip.text.y = element_text(size= 16, face="bold")) +
  theme(legend.position = "bottom", legend.box = "vertical") +
  theme(legend.title = element_text(size=20, face="bold")) +
  labs(fill= "N rate", x = "Years after beginning of experiment", y = "C %")#y= bquote('C_in_TM_mean ('*'Mg' ~ ha^-1*')'))


ggsave(filename = "boxplot6.png")




#####################################################################

# Outlier removal using Boxplot method too see directly in dataset --------
#table of outliers: activate the filter(!is.na(outlier)) to select them or filter(is.na(outlier)) to removed them------

#####################################################################


# function to detect outliers in boxplot----------------------------------------------------------------
#https://stackoverflow.com/questions/33524669/labeling-outliers-of-boxplots-in-r


is_outlier <- function(x) {
  return(x < quantile(x, 0.25,na.rm = TRUE) - 1.5 * IQR(x,na.rm = TRUE) | x > quantile(x, 0.75,na.rm = TRUE) + 1.5 * IQR(x,na.rm = TRUE))
}


# - -----------------------------------------------------------------------



outliers_out0_30_version1<- SOC_Hoh_without_outliers %>% filter(tiefe == "0_30") %>%  group_by(variante, duengung, block) %>% 
  mutate(outlier=ifelse(is_outlier(C_in_TM), C_in_TM, as.numeric(NA))) %>% filter(is.na(outlier))



outliers_out30_60_version1<- SOC_Hoh_without_outliers %>% filter(tiefe == "30_60") %>%  group_by(variante, duengung, block)  %>% 
  mutate(outlier=ifelse(is_outlier(C_in_TM), C_in_TM, as.numeric(NA))) %>%  filter(is.na(outlier))


outliers_out60_90_version1<- SOC_Hoh_without_outliers %>% filter(tiefe == "60_90") %>%  group_by(variante, duengung, block)  %>% 
  mutate(outlier=ifelse(is_outlier(C_in_TM), C_in_TM, as.numeric(NA))) %>%  filter(is.na(outlier))



# data storage ------------------------------------------------------------ ------------


outliers_out_allplots_version1<- rbind(outliers_out0_30_version1, outliers_out30_60_version1, outliers_out60_90_version1) %>% as.data.frame()



write.table(x = outliers_out_allplots_version1, file = "outlierslist_version1_table.txt",sep = "\t" , dec = ".", row.names = TRUE, col.names = TRUE)








#############
# Version 2 
#############

# variante + duengung: separated by tiefe and by block

dev.new()

y <- ggplot(SOC_Hoh_without_outliers_plot_2, aes(as.factor(akk_year), y=C_in_TM_mean, col=duengung)) #, 


y + geom_boxplot(outlier.colour = "green", outlier.shape = 3,outlier.fill = "green") + # scale_fill_hue(l=40, c=35) + 
  geom_text(aes(label=paste(parz, jahr, sep = "_")), size=2,check_overlap=TRUE) +     #label = C_in_TM_mean    #paste(parz, block, sep = "_"))
  geom_point(aes(col=duengung),  alpha=0.5, position = position_jitterdodge()) +
  #facet_wrap(block~tiefe~vornutzung~variante+duengung) +
  #facet_grid(tiefe+vornutzung~variante+duengung) + # (tiefe~variante) + #(variante+tiefe~block)   (variante~vornutzung+vordung)
  facet_grid(block+tiefe~variante+duengung) + # (tiefe~variante) + #(variante+tiefe~block)   (variante~vornutzung+vordung)
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text( size=16),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.text.x = element_text(size=16),
    axis.text.y= element_text(size = 16),
    legend.text= element_text(size= 16), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    strip.background = element_rect (fill= "white"), 
    strip.text.x = element_text(size= 16, face="bold"),
    strip.text.y = element_text(size= 16, face="bold")) +
  theme(legend.position = "bottom", legend.box = "vertical") +
  theme(legend.title = element_text(size=20, face="bold")) +
  labs(fill= "N rate", x = "Years after beginning of experiment", y = "C %")#y= bquote('C_in_TM_mean ('*'Mg' ~ ha^-1*')'))


ggsave(filename = "boxplot7.png")

#newdataset

SOC_Hoh_without_outliers_plot_2new <- SOC_Hoh_without_outliers_plot_2

# - -----------------------------------------------------------------------



outliers_out0_30_version2<- SOC_Hoh_without_outliers %>% filter(tiefe == "0_30") %>%  group_by(block, tiefe, variante, duengung) %>% 
  mutate(outlier=ifelse(is_outlier(C_in_TM), C_in_TM, as.numeric(NA))) %>% filter(is.na(outlier))



outliers_out30_60_version2<- SOC_Hoh_without_outliers %>% filter(tiefe == "30_60") %>%  group_by(block, tiefe, variante, duengung)  %>% 
  mutate(outlier=ifelse(is_outlier(C_in_TM), C_in_TM, as.numeric(NA))) %>%  filter(is.na(outlier))


outliers_out60_90_version2<- SOC_Hoh_without_outliers %>% filter(tiefe == "60_90") %>%  group_by(block, tiefe, variante, duengung)  %>% 
  mutate(outlier=ifelse(is_outlier(C_in_TM), C_in_TM, as.numeric(NA))) %>%  filter(is.na(outlier))



# data storage ------------------------------------------------------------ ------------


outliers_out_allplots_version2<- rbind(outliers_out0_30_version2, outliers_out30_60_version2, outliers_out60_90_version2) %>% as.data.frame()



write.table(x = outliers_out_allplots_version2, file = "outlierslist_version2_table.txt",sep = "\t" , dec = ".", row.names = TRUE, col.names = TRUE)









#############
# Version 3 
#############

#Variante + duengung x tiefe: all blocks together

dev.new()

y <- ggplot(SOC_Hoh_without_outliers_plot_2, aes(as.factor(block), y=C_in_TM_mean, col=duengung)) #, 


y + geom_boxplot(outlier.colour = "green", outlier.shape = 3,outlier.fill = "green") + # scale_fill_hue(l=40, c=35) + 
  geom_text(aes(label=paste(parz, jahr, sep = "_")), size=2,check_overlap=TRUE) +     #label = C_in_TM_mean    #paste(parz, block, sep = "_"))
  geom_point(aes(col=duengung),  alpha=0.5, position = position_jitterdodge()) +
  #facet_grid(tiefe+vornutzung~variante+duengung) + # (tiefe~variante) + #(variante+tiefe~block)   (variante~vornutzung+vordung)
  facet_grid(tiefe~variante+duengung) + # (tiefe~variante) + #(variante+tiefe~block)   (variante~vornutzung+vordung)
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text( size=16),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.text.x = element_text(size=16),
    axis.text.y= element_text(size = 16),
    legend.text= element_text(size= 16), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    strip.background = element_rect (fill= "white"), 
    strip.text.x = element_text(size= 16, face="bold"),
    strip.text.y = element_text(size= 16, face="bold")) +
  theme(legend.position = "bottom", legend.box = "vertical") +
  theme(legend.title = element_text(size=20, face="bold")) +
  labs(fill= "N rate", x = "Years after beginning of experiment", y = "C %")#y= bquote('C_in_TM_mean ('*'Mg' ~ ha^-1*')'))


ggsave(filename = "boxplot7.png")

#newdataset

SOC_Hoh_without_outliers_plot_2new <- SOC_Hoh_without_outliers_plot_2

# - -----------------------------------------------------------------------



outliers_out0_30_version2<- SOC_Hoh_without_outliers %>% filter(tiefe == "0_30") %>%  group_by(block, tiefe, variante, duengung) %>% 
  mutate(outlier=ifelse(is_outlier(C_in_TM), C_in_TM, as.numeric(NA))) %>% filter(!is.na(outlier))



outliers_out30_60_version2<- SOC_Hoh_without_outliers %>% filter(tiefe == "30_60") %>%  group_by(block, tiefe, variante, duengung)  %>% 
  mutate(outlier=ifelse(is_outlier(C_in_TM), C_in_TM, as.numeric(NA))) %>%  filter(!is.na(outlier))


outliers_out60_90_version2<- SOC_Hoh_without_outliers %>% filter(tiefe == "60_90") %>%  group_by(block, tiefe, variante, duengung)  %>% 
  mutate(outlier=ifelse(is_outlier(C_in_TM), C_in_TM, as.numeric(NA))) %>%  filter(!is.na(outlier))



# data storage ------------------------------------------------------------ ------------


outliers_out_allplots_version2<- rbind(outliers_out0_30_version2, outliers_out30_60_version2, outliers_out60_90_version2) %>% as.data.frame()



write.table(x = outliers_out_allplots_version2, file = "outlierslist_version2_table.txt",sep = "\t" , dec = ".", row.names = TRUE, col.names = TRUE)



#Check plots individually for 2nd and 3rd

X_030<-SOC_Hoh_without_outliers_plot_2 %>% filter(tiefe=="0_30")

X_3060<-SOC_Hoh_without_outliers_plot_2 %>% filter(tiefe=="30_60")

X_6090<-SOC_Hoh_without_outliers_plot_2 %>% filter(tiefe=="60_90")



y <- ggplot(X_030, aes(as.factor(akk_year), y=C_in_TM_mean, col=duengung)) #, 

y <- ggplot(X_3060, aes(as.factor(akk_year), y=C_in_TM_mean, col=duengung)) #, 

y <- ggplot(X_6090, aes(as.factor(akk_year), y=C_in_TM_mean, col=duengung)) #, 

dev.new()

y + geom_boxplot(outlier.colour = "green", outlier.shape = 3,outlier.fill = "green",outlier.size = 4) + # scale_fill_hue(l=40, c=35) + 
  geom_text(aes(label=paste(parz, jahr, sep = "_")), size=2,check_overlap=TRUE) +     #label = C_in_TM_mean    #paste(parz, block, sep = "_"))
  geom_point(aes(col=duengung),  fill=NA, alpha=0.2, position = position_jitterdodge()) +
  geom_smooth(aes(x=as.numeric(akk_year)), method = "lm",  se=TRUE) +

  facet_grid(block~variante+duengung) +  #(block~variante+duengung+vornutzung)
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text( size=16),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.text.x = element_text(size=16),
    axis.text.y= element_text(size = 16),
    legend.text= element_text(size= 16), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    strip.background = element_rect (fill= "white"), 
    strip.text.x = element_text(size= 16, face="bold"),
    strip.text.y = element_text(size= 16, face="bold")) +
  theme(legend.position = "bottom", legend.box = "vertical") +
  theme(legend.title = element_text(size=20, face="bold")) +
  labs(fill= "N rate", x = "Years after beginning of experiment", y = "C %")#y= bquote('C_in_TM_mean ('*'Mg' ~ ha^-1*')'))




ggsave(filename = "Boxplot 60_90-variante-duengung-by block.png")



######################################################################################################

#as  done in the R file: SOC analysis Hohenschulen_for figures.R

# binding all dataset after boxplot outlier removal_ for plotting based on variante, duengung and tiefe -

######################################################################################################


#Rerun the code with is.na(), not with !is.na()




#outliers_out_allplots_version1

outliers_removed_plot <- outliers_out_allplots_version1 %>%
  group_by(jahr, akk_year, tiefe, kultur, variante, duengung) %>% #vornutzung  block, tiefe, vornutzung, vordung
  summarise(
    BD2020_Cstock_mean = mean(BD_2020*30*C_in_TM, na.rm=TRUE),
    C_in_TM_mean_mean = mean(C_in_TM, na.rm=TRUE),
    n= n()) %>%
  ungroup()




#outliers_out_allplots_version2

outliers_removed_plot <- outliers_out_allplots_version2  %>%
  group_by(jahr, akk_year, tiefe, kultur, variante, duengung) %>% #vornutzung  block, tiefe, vornutzung, vordung
  summarise(
    BD2020_Cstock_mean = mean(BD_2020*30*C_in_TM, na.rm=TRUE),
    C_in_TM_mean_mean = mean(C_in_TM, na.rm=TRUE),
    n= n()) %>%
  ungroup()




outliers_out_allplots_version3

outliers_removed_plot <- outliers_out_allplots_version1 %>%
  group_by(jahr, akk_year, tiefe, kultur, variante, duengung) %>% #vornutzung  block, tiefe, vornutzung, vordung
  summarise(
    BD2020_Cstock_mean = mean(BD_2020*30*C_in_TM, na.rm=TRUE),
    C_in_TM_mean_mean = mean(C_in_TM, na.rm=TRUE),
    n= n()) %>%
  ungroup()





"BD2020_Cstock_mean"

my.formula.linear <- y ~ x
dev.new()


y <- ggplot(outliers_removed_plot, aes(akk_year, BD2020_Cstock_mean, shape=duengung, linetype=duengung)) #specify x and y in the aes()


y +  
  geom_point(size=2)  +
  geom_smooth(method = "lm", na.rm = TRUE, se = FALSE, color="black") +
  scale_linetype_manual(name= "N-rate", values=c("solid", "dashed")) +
  scale_shape_manual(name= "N-rate", values=c(1, 2)) + 
  facet_grid(tiefe~variante,switch = "y") + # vornutzung and vordung is included on 03.02.2020
  
  # Tiefe together ------------------------------------------------------------

#facet_grid(.~variante) + 
#geom_line(color= "black") + #for alltiefe
stat_poly_eq(formula = my.formula.linear,  aes(label = paste(stat(eq.label), stat(rr.label), sep = "~~~")),  parse = TRUE, size=3) +
  # -------------------------------------------------------------------------

theme_bw(base_size=20, base_family = "Times New Roman")  +
  theme(
    text=element_text(family="Times New Roman"),
    axis.line= element_line(colour= "black", size= 0.5),
    #plot.title = element_text( size=16),
    #axis.title.x = element_text(size=16),
    #axis.title.y = element_text(size=16),
    #axis.text.x = element_text(size=16),
    #axis.text.y= element_text(size = 16),
    #legend.text= element_text(size= 16), 
    panel.background= element_blank(),
    panel.border= element_rect(fill=NULL, colour="black", size= 0.5),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    strip.background= element_rect(fill="White", color="black",size=0.5), 
    strip.text.x = element_text(size= 16, face="bold"),
    strip.text.y = element_text(size= 16, face="bold"),
    legend.position = "bottom", 
    legend.box = "vertical", 
    legend.text= element_text(colour="black", face="bold"),
    legend.title = element_text(colour="black", face = "bold")) +
  labs(col= "N-rate", x = "Year", y= bquote('Measured SOC ('*'Mg' ~ ha^-1*')'))

getwd()

ggsave(filename="outliers_removed_plot2.png") # A4 width:16 size



############################################################################################

# - Individual Blocks in One plot-----------------------------------------------------------

############################################################################################

colnames(outliers_out_allplots_version1)

SOC_Hoh_without_outliers_plot <- outliers_out_allplots_version2 %>%  group_by(block,jahr, akk_year, tiefe, kultur, variante, duengung) %>% summarise(
  BD2020_Cstock_mean = mean(C_in_TM*BD_2020*30, na.rm=TRUE),  BD2020_Nstock_mean = mean(N_in_TM*BD_2020*30, na.rm=TRUE),  BD2020_Cstock_sd = sd(C_in_TM*BD_2020,  na.rm=TRUE),
  BD2020_Nstock_sd = sd(N_in_TM*BD_2020,  na.rm=TRUE),    CN_mean = mean(CN, na.rm=TRUE),   CN_sd = sd(CN, na.rm=TRUE),     CN_min = min(CN, na.rm=TRUE), 
  CN_max = max(CN, na.rm=TRUE),   C_in_TM_mean = mean(C_in_TM, na.rm=TRUE),  C_in_TM_sd = sd(C_in_TM, na.rm=TRUE),  N_in_TM_mean = mean(N_in_TM, na.rm=TRUE),  n= n()) %>% ungroup()




SOC_Hoh_without_outliers_plot <- outliers_out_allplots_version1 %>%  group_by(block,jahr, akk_year, tiefe, kultur, variante, duengung) %>%  summarise(
  BD2020_Cstock_mean = mean(BD2020_Cstock_mean, na.rm=TRUE), n= n()) %>% ungroup()



y <- ggplot(SOC_Hoh_without_outliers_plot, aes(akk_year, BD2020_Cstock_mean, shape=as.factor(block), color=as.factor(block), linetype=as.factor(block))) #specify x and y in the aes()



dev.new()

y +  
  geom_point(size=2)  +
  geom_smooth(data =. %>% filter(block == "1"),method = "lm", na.rm = TRUE, se = FALSE) +
  geom_smooth(data =. %>% filter(block == "2"),method = "lm", na.rm = TRUE, se = FALSE) +
  geom_smooth(data =. %>% filter(block == "3"),method = "lm", na.rm = TRUE, se = FALSE) +
  geom_smooth(data =. %>% filter(block == "4"),method = "lm", na.rm = TRUE, se = FALSE) +
  
  #geom_smooth(method = "lm", na.rm = TRUE, se = FALSE, color="black") +
  # scale_linetype_manual(name= "N-rate", values=c("solid", "dashed")) +
  #scale_shape_manual(name= "N-rate", values=c(1, 2)) + 
  facet_grid(tiefe~variante+duengung,switch = "y") + # vornutzung and vordung is included on 03.02.2020
  
  # Tiefe together ------------------------------------------------------------

#facet_grid(.~variante) + 
#geom_line(color= "black") + #for alltiefe
stat_poly_eq(formula = my.formula.linear,  aes(label = paste(stat(eq.label), stat(rr.label), sep = "~~~")),  parse = TRUE, size=3) +
  # -------------------------------------------------------------------------

theme_bw(base_size=20, base_family = "Times New Roman")  +
  theme(
    text=element_text(family="Times New Roman"),
    axis.line= element_line(colour= "black", size= 0.5),
    #plot.title = element_text( size=16),
    #axis.title.x = element_text(size=16),
    #axis.title.y = element_text(size=16),
    #axis.text.x = element_text(size=16),
    #axis.text.y= element_text(size = 16),
    #legend.text= element_text(size= 16), 
    panel.background= element_blank(),
    panel.border= element_rect(fill=NULL, colour="black", size= 0.5),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    strip.background= element_rect(fill="White", color="black",size=0.5), 
    strip.text.x = element_text(size= 16, face="bold"),
    strip.text.y = element_text(size= 16, face="bold"),
    legend.position = "bottom", 
    legend.box = "vertical", 
    legend.text= element_text(colour="black", face="bold"),
    legend.title = element_text(colour="black", face = "bold")) +
  labs(title= "allblocks", linetype= "block", shape= "block", col= "block", x = "Year", y= bquote('Measured SOC ('*'Mg' ~ ha^-1*')'))


getwd()
ggsave(filename="Cperc_slopes_2014-2020_allblocks_no outliers removed_byblock.png") 



