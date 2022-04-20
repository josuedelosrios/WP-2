

# Figures model estimates ------------------------------------------------------------


#Data preparation
shell.exec("HOH SOC stocks stats.R")

#Load ready data
load(file="Hoh SOC output.Rdata")



#Rename factor variables

levels(plot_fitted_values$variante) <- c("CT-CM", "GC", "NT-CM", "NT-GR") 

#This will reorder the levels of fruchtff to the desire plot order
plot_fitted_values$variante <- factor(plot_fitted_values$variante, levels = c("GC", "NT-GR", "NT-CM", "CT-CM"))  


levels(plot_fitted_values$tiefe)<- c("0-30", "30-60", "60-90") 




#Summary data
plot_fitted_summary <- plot_fitted_values %>%
  group_by(variante, tiefe, jahr, akk_year) %>% 
  summarise(c_stock_mean = mean(Cstock_tha_BDfix_2020, na.rm=TRUE), #use to compare it with fixed effect estimates "c_stock_mean_fixed"
            c_stock_mean_fitted = mean(.fitted, na.rm=TRUE),
            c_stock_mean_fixed  = mean(.fixed, na.rm=TRUE)) %>% ungroup() %>% clean_names()



# Add number of replicates
plot_fitted_rep <- plot_fitted_values %>%
  group_by(variante, tiefe, jahr, akk_year) %>% mutate( replicates = n()) %>% ungroup() %>% filter(akk_year == 0) %>% 
  dplyr::select("variante", "tiefe", "replicates")




# - Figure -----------------------------------------------------------------------

my.formula.linear <- y ~ x

y <- ggplot(plot_fitted_summary , aes(akk_year, c_stock_mean_fixed))

plot <- y + 
  geom_point(aes(y=c_stock_mean_fitted), size=2) + 
  geom_smooth(aes(y=c_stock_mean_fixed),  method= lm,  na.rm = TRUE, se=FALSE, color="black") + 
  stat_poly_eq(formula = my.formula.linear,  aes(label = paste(stat(eq.label), sep = "~~~")), family = "Helvetica",  
               parse = TRUE, size=4,  label.y= 0.1) + #, stat(rr.label)
  facet_grid(tiefe~variante, scales = "free_y", switch = "y",as.table = TRUE) +
  scale_shape_manual(name= "N-rate", values = c(1,2)) +
  theme_bw(base_size=20, base_family = "Helvetica") +
  theme(
    text=element_text(family="Helvetica"),
    axis.line= element_line(colour= "black", size= 0.5), 
    axis.text = element_text(color = "black"),
    panel.grid= element_blank(),
    panel.background= element_blank(),
    panel.border= element_rect(fill=NULL, colour="black", size= 0.5), 
    strip.background= element_rect(fill="White", color="black",size=0.5), 
    strip.text= element_text(colour="black", face="bold"),  
    legend.position = "bottom",
    legend.text= element_text(colour="black", face="bold"), 
    legend.title = element_text(colour="black", face = "bold")) + 
  labs(linetype= "N-rate", shape="N-rate", x = "Year", y= bquote('SOC stock ('*'Mg' ~ ha^-1*')'))

dev.new() ; plot




# 2nd figure --------------------------------------------------------------

signif_pvalues<- syst_estimates %>% filter(p.value <=0.05)



syst_est_param_mod4 <- syst_estimates %>%  
  mutate(param_est = case_when(
  str_detect(term, ':akk_year') == TRUE ~ "slope", 
  str_detect(term, ':akk_year') == FALSE ~ "intercept"
  )) %>% mutate(variante = case_when(
  str_detect(term, 'varianteGC') == TRUE ~ "GC", 
  str_detect(term, 'varianteNT_GR') == TRUE ~ "NT_GR",
  str_detect(term, 'varianteNT_CM') == TRUE ~ "NT_CM", 
  str_detect(term, 'varianteCT_CM') == TRUE ~ "CT_CM"
  # )) %>% mutate(Nrate = case_when(
  #  str_detect(term, 'duengungN0') == TRUE ~ "N0", 
  #  str_detect(term, 'duengungN1') == TRUE ~ "N1"
)) %>% mutate(tiefe = case_when(
  str_detect(term, ':tiefe0_30') == TRUE ~ "0_30", 
  str_detect(term, ':tiefe30_60') == TRUE ~ "30_60",
  str_detect(term, ':tiefe60_90') == TRUE ~ "60_90" 
)) %>%  filter(param_est =="slope")

syst_est_param_mod4$annotation <- paste(syst_est_param_mod4$system, syst_est_param_mod4$tiefe, sep=" ")

#Change define factor variables
syst_est_param_mod4 <-syst_est_param_mod4 %>% 
  mutate(across(c(variante, tiefe, annotation), as.factor))


#rename factor levels
levels(syst_est_param_mod4$variante) <- c("CT-CM", "GC", "NT-CM", "NT-GR") 



#This will reorder the levels of fruchtff to the desire plot order
syst_est_param_mod4$variante <- factor(syst_est_param_mod4$variante, levels = c("GC", "NT-GR", "NT-CM", "CT-CM"))  

levels(syst_est_param_mod4$tiefe)<- c("0-30", "30-60", "60-90") 


#Define annotations for figure
syst_est_mod4_test <-syst_est_param_mod4 %>% 
  mutate(p.score= case_when(
    p.value < 0.05 & p.value > 0.01  ~"*",
    p.value < 0.01 & p.value > 0.001 ~ "**",
    p.value < 0.001 & p.value > 0.0001 ~ "***",
    p.value < 0.0001  ~ "****",
    p.value > 0.05  ~ ""))


syst_est_mod4_test <- syst_est_mod4_test %>% 
  rename ("layer" = "tiefe")


syst_est_mod4_test2 <- syst_est_mod4_test %>% 
  dplyr::select("variante", "layer", "estimate", "std.error", "statistic", "p.value", "p.score") %>% 
  rename("Crop.syst." = "variante",   "bx"="estimate", "s.e."="std.error", "t value"="statistic", "p value"="p.value", " "="p.score") %>%
  group_by(layer) %>% 
  arrange(desc(bx)) %>% ungroup() %>% 
  #mutate_if(is.numeric, round,2) %>% 
  mutate_at(c("bx", "s.e.", "t value"), round,2) %>% 
  mutate_at("p value", round,4)


#Add these tables
Layer0030 <- syst_est_mod4_test2 %>% filter(layer =="0-30") %>% arrange(factor(`Crop.syst.`, levels = c("GC", "NT-GR", "NT-CM", "CT-CM")))

Layer3060 <- syst_est_mod4_test2 %>% filter(layer =="30-60") %>% arrange(factor(`Crop.syst.`, levels = c("GC", "NT-GR", "NT-CM", "CT-CM")))

Layer6090 <- syst_est_mod4_test2 %>% filter(layer =="60-90") %>% arrange(factor(`Crop.syst.`, levels = c("GC", "NT-GR", "NT-CM", "CT-CM")))


data.tb <- tibble(x = c(1, 1, 1), y = c(1, 1, 1), 
                  layer =  c("0-30", "30-60", "60-90"),
                  tb = list(Layer0030, Layer3060, Layer6090))




#Figure production
figure <- ggplot(syst_est_mod4_test , aes(variante, estimate)) +
  
  geom_point() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_text(label= syst_est_mod4_test$p.score, 
            nudge_y = 0.8, check_overlap = TRUE, size=10) +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1, alpha=1) +
  facet_grid(.~layer) +
  theme_bw(base_size=20, base_family = "Helvetica") + #base_size=14 #, 
  theme(
    text=element_text(family="Helvetica"),
    axis.line= element_line(colour= "black", size= 0.5),
    axis.text = element_text(color = "black"), #size = 10 
    #axis.title= element_text(color = "black", face="bold"), #, size = 12
    panel.grid= element_blank(),
    panel.background= element_blank(),
    panel.border= element_rect(fill=NULL, colour="black", size= 0.5), 
    strip.background= element_rect(fill="White", color="black",size=0.5), 
    strip.text= element_text(colour="black", face="bold"),  
    strip.text.y= element_text(angle = 270),
    legend.position = "bottom",
    legend.text= element_text(colour="black", face="bold"), 
    legend.title = element_text(colour="black", face = "bold")) + 
  ylim(-4,4) + 
  labs(y= bquote('Annual SOC change ('*'Mg' ~ ha^-1*')'), x= "Cropping System")

dev.new() ; figure