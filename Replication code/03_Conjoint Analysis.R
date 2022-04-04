
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(grid)
library(tidyverse)
library(stargazer)
library(readr)
library(dplyr)
library(caret)
library(Hmisc)
library(expss)
library(MASS)
library(devtools)
library(cowplot)
library(urbnmapr)
library(usmap)
library(readxl)
library(broom)
library(psych)
library(haven)
library(stringr)
library(tidyr)
library(rio)
library(matrixStats)
library(naniar)
library(zoo)
library(lfe)
library(aod)
library(lmtest)
library(rstatix)
library(ivreg)
library(ggridges)
library(writexl)
library(xlsx)
library(mifa)
library(mice)
library(lazyeval)
library(cregg)
library(estimatr)
library(xtable)
library(knitr)
library(hrbrthemes)
library(Hmisc)

setwd("/Users/kristiankiel/Documents/Statskundskab/KU /11. Semester (Speciale)/Masters thesis")

#1.0 ############# Overview of results  ###########

    # Checking for random distribution of data 
    table(choice_df$ALDER)
    table(choice_df$ANTAL_R_P_ARBEJDSMARKEDET)
    table(choice_df$HELBREDSTILSTAND)
    table(choice_df$PROFESSION)
    table(choice_df$K_N)
    
  #Setting formula with constraints between profession and years on labour market 
  formula.main<-CHOICE_INDICATOR~ALDER+ANTAL_R_P_ARBEJDSMARKEDET*PROFESSION+HELBREDSTILSTAND+K_N

#1.1 Marginal means for main model 
  mm <- cj(data=choice_df, 
           formula.main,
           id = ~ RESPONDENT_ID,
           estimate = "mm", 
           h0=0.33,
           feature_labels = feature_list)
  

  plot<-plot(mm,vline=NULL,feature_headers=FALSE)+
    theme_bw() +
    xlab("Marginal means for Pr(Support for retirement recipient)")+
    theme(strip.background = element_rect(fill = "black"),
          strip.text = element_text(size = 7, colour = "white"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          panel.background = element_rect(fill = "white", 
                                          colour = "black",
                                          size = 1),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          axis.ticks =        element_line(colour = "grey50"),
          legend.title=element_blank(),
          legend.key=element_blank(),
          legend.position = "none",
          text=element_text(family="Arial")) +
    ggtitle("") +
    guides(colour = guide_legend(reverse=TRUE))+
    scale_colour_manual(values=rep("black", 5))+
    geom_vline(aes(xintercept=0.333),linetype="longdash")+
    ggplot2::facet_wrap(~feature, ncol = 1L,
                        scales = "free_y", strip.position = "right")
   
  #Latex output 
  print(xtable(mm[4:8]), include.rownames=FALSE)
  
  ggsave(filename = "./Outputs/Conjoint analysis/Complete_model_MM.png",
         plot = plot, 
         width = 8, 
         height = 9, 
         dpi = 320)

  #1.2 Percentiles 
  model <- lm_robust(formula =   CHOICE_INDICATOR~ALDER+ANTAL_R_P_ARBEJDSMARKEDET*PROFESSION+HELBREDSTILSTAND+K_N,
                     data = choice_df,
                     clusters = RESPONDENT_ID)
  

  # Predict probability for every profile
  predicted <- bind_cols(choice_df, 
                         as.data.frame(predict(object = model, 
                                               newdata = choice_df,
                                               se.fit = TRUE,
                                               interval = "confidence")))
  
  # select percentiles
  # Make a vector of percentiles to be selected
  percentiles <- quantile(predicted$fit.fit, c(0.0098, 0.25, 0.50, 0.75, 0.999))
  
  # Calculate distance to percentile 
  predicted <- predicted %>% 
    mutate(dist1 = abs(percentiles[1] - fit.fit),
           dist25 = abs(percentiles[2] - fit.fit),
           dist50 = abs(percentiles[3] - fit.fit),
           dist75 = abs(percentiles[4] - fit.fit),
           dist99 = abs(percentiles[5] - fit.fit))
  
  # select profiles with the minimum distances to percentiles
  profiles <- predicted %>% 
    filter(dist1==min(dist1) | dist25==min(dist25) | dist50==min(dist50) | dist75==min(dist75) | dist99==min(dist99)) %>% 
    select(ALDER,K_N,HELBREDSTILSTAND,PROFESSION,ANTAL_R_P_ARBEJDSMARKEDET, fit.fit, fit.lwr, fit.upr, se.fit) %>% 
    distinct() %>% 
    arrange(fit.fit) %>% 
    mutate(percentiles = c(1, 25, 50, 75, 99),
           feature = "Profile Types")

    # Correct fits and confidence interval such that they can't go below zero
  profiles <- profiles %>% 
    mutate(fit.lwr = ifelse(fit.lwr<0, 0.00, fit.lwr))
  
  profiles <- profiles %>% 
    mutate(fit.fit = ifelse(fit.fit<0, 0.00, fit.fit))
  
  profiles <- profiles %>% 
    mutate(fit.upr = ifelse(fit.upr<0, 0.00, fit.upr))
  #Add text label to profiles
  # Add label as a variable
  profiles <- profiles %>% 
    mutate(label = paste(paste(ALDER),
                         paste(K_N),
                         paste(HELBREDSTILSTAND),
                         paste(PROFESSION),
                         paste(ANTAL_R_P_ARBEJDSMARKEDET),
                         sep = "\n"))
  
  # Draw text
  text1 <- textGrob(label = profiles$label[1], 
                    gp = gpar(fontsize = 10))
  
  text25 <- textGrob(label = profiles$label[2], 
                     gp = gpar(fontsize = 10))
  
  text50 <- textGrob(label = profiles$label[3], 
                     gp = gpar(fontsize = 10))
  
  text75 <- textGrob(label = profiles$label[4], 
                     gp = gpar(fontsize = 10))
  
  text99 <- textGrob(label = profiles$label[5], 
                     gp = gpar(fontsize = 10))
  profiles
  #Visualise results
  plot <- profiles %>% 
    ggplot(., aes(x = percentiles, 
                  y = fit.fit)) +
    geom_point(fill = "black",
               size = 2.5) +
    geom_linerange(aes(ymin = fit.lwr ,
                       ymax = fit.upr),
                   lwd = 1, position = position_dodge(width = 1/2)) +
    scale_x_continuous("Percentiles", 
                       breaks = c(1, 25, 50, 75, 99),
                       expand = c(0.05, 0.2)) +
    scale_y_continuous("Estimated Pr(Support for recipient)",
                       breaks = c(0.0, 0.25, 0.50, 0.75, 1)) +
    facet_grid(feature ~ .,
               scales = "free_y",
               space = "free_y") +
    annotation_custom(text1, 
                      xmin = 1,
                      xmax = 1,
                      ymin = -0.20,
                      ymax = -0.30) + 
    annotation_custom(text25, 
                      xmin = 25,
                      xmax = 25,
                      ymin = -0.20,
                      ymax = -0.30) + 
    annotation_custom(text50,
                      xmin = 50, 
                      xmax = 50,
                      ymin = -0.20,
                      ymax = -0.30) + 
    annotation_custom(text75,
                      xmin = 75,
                      xmax = 75,
                      ymin = -0.20,
                      ymax = -0.30) + 
    annotation_custom(text99,
                      xmin = 99,
                      xmax = 99,
                      ymin = -0.20,
                      ymax = -0.30) + 
    coord_cartesian(clip = "off") +
    theme(strip.background = element_rect(fill = "black"),
          strip.text = element_text(size = 7, colour = "white"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          panel.background = element_rect(fill = "white", 
                                          colour = "black",
                                          size = 1),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          axis.ticks =        element_line(colour = "grey50"),
          legend.title=element_blank(),
          legend.key=element_blank(),
          legend.position = "none",
          text=element_text(family="Arial")) +
    theme(plot.margin = unit(c(1, 2, 8, 1), "lines"))
  
  
    ggsave("./Outputs/Conjoint analysis/Percentiles.png", 
         plot = plot, 
         width = 15, 
         height = 7, 
         dpi = 320)
  
  #latex output
  profiles<-profiles%>%
    select(percentiles,ALDER,K_N,HELBREDSTILSTAND,PROFESSION,ANTAL_R_P_ARBEJDSMARKEDET,fit.fit,se.fit)
  
  #Setting column names 
  colnames(profiles) <- c("Percentile","Age","Gender","Health Status","Profession","Years on labour market","fit.fit","se.fit")
  
  #Printing latex output 
  print(xtable(profiles), include.rownames=FALSE)

  #2.0 ############# Main results ###################
  #2.1 Marginnal means for all respondents - Plotting Health status#
  mm <- cj(data=choice_df, 
           formula.main,
           id = ~ RESPONDENT_ID,
           estimate = "mm", 
           h0=0.33,
           feature_labels = feature_list)
  
  #Plot
  plot<-plot(subset(mm,feature=="Health Status"),vline=NULL,feature_headers=FALSE)+
    theme_bw() +
    xlab("Marginal means for Pr \n (Support for retirement recipient)")+
    theme(strip.background = element_rect(fill = "black"),
          strip.text = element_text(size = 7, colour = "white"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          panel.background = element_rect(fill = "white", 
                                          colour = "black",
                                          size = 1),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          axis.ticks =        element_line(colour = "grey50"),
          legend.title=element_blank(),
          legend.key=element_blank(),
          legend.position = "none",
          text=element_text(family="Arial")) +
    ggtitle("") +
    guides(colour = guide_legend(reverse=TRUE))+
    scale_colour_manual(values=rep("black", 5))+
    geom_vline(aes(xintercept=0.333),linetype="longdash")+
    ggplot2::facet_wrap(~feature, ncol = 1L,
                        scales = "free_y", strip.position = "right")
  plot
  
  
  
  ggsave(filename = "./Outputs/Conjoint analysis/H1_mm.png",
         plot = plot, 
         width = 6, 
         height = 3, 
         dpi = 320)
  
  ## 2.2 Amce-plot: All respondents- Health status
  
  amce<- cj(data=choice_df, 
  formula.main,
  id = ~ RESPONDENT_ID,
  estimate = "amce", 
  feature_labels = feature_list)
  
  #Plot
  plot<-plot(subset(amce,feature=="Health Status"),vline=NULL,feature_headers=FALSE)+
    theme_bw() +
    xlab("AMCE for Pr (Support for retirement recipient)")+
    theme(strip.background = element_rect(fill = "black"),
          strip.text = element_text(size = 7, colour = "white"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          panel.background = element_rect(fill = "white", 
                                          colour = "black",
                                          size = 1),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          axis.ticks =        element_line(colour = "grey50"),
          legend.title=element_blank(),
          legend.key=element_blank(),
          legend.position = "none",
          text=element_text(family="Arial"))+
    ggtitle("") +
    guides(colour = guide_legend(reverse=TRUE))+
    scale_colour_manual(values=rep("black", 5))+
    geom_vline(aes(xintercept=0),linetype="longdash")+
    ggplot2::facet_wrap(~feature, ncol = 1L,
                        scales = "free_y", strip.position = "right")
  plot
  
  
  ggsave(filename = "./Outputs/Conjoint analysis/H1_AMCE.png",
         plot = plot, 
         width = 8, 
         height = 4, 
         dpi = 320)

  #Health status x profession 
  ## 2.3 mm-plot: HEALTH_STATUS X PROFESSION -------------------
  mm<- cj(data=choice_df,
          formula=CHOICE_INDICATOR~ALDER+HELBREDSTILSTAND+K_N+ANTAL_R_P_ARBEJDSMARKEDET,
          id = ~RESPONDENT_ID,
          estimate = "mm", 
          by=~PROFESSION,
          h0=0.33,
          feature_labels=feature_list)
  
  plot<-plot(subset(mm,feature=="Health Status"),vline=NULL,feature_headers=FALSE)+
    theme_bw() +
    xlab("Marginal means for Pr (Support for retirement recipient)")+
    theme(strip.background = element_rect(fill = "black"),
          strip.text = element_text(size = 7, colour = "white"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          panel.background = element_rect(fill = "white", 
                                          colour = "black",
                                          size = 1),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          axis.ticks =        element_line(colour = "grey50"),
          legend.title=element_blank(),
          legend.key=element_blank(),
          legend.position = "none",
          text=element_text(family="Arial")) +
    ggtitle("") +
    guides(colour = guide_legend(reverse=TRUE))+
    scale_colour_manual(values=rep("black", 5))+
    geom_vline(aes(xintercept=0.333),linetype="longdash")+
    ggplot2::facet_wrap(~BY, ncol = 3L)+
    scale_x_continuous(breaks=c(0.00,0.25,0.50,0.75,1.00))
  
  plot 
  #Latex output 
  H1_int_F_test<-cj_anova(data=choice_df,formula=CHOICE_INDICATOR~ALDER+HELBREDSTILSTAND+K_N+ANTAL_R_P_ARBEJDSMARKEDET,
                          by=~PROFESSION)
  stargazer(H1_int_F_test,type="latex",summary=FALSE)
  
  mm<-mm%>%
    select(BY,feature,level,estimate,std.error,z,p)
  print(xtable(mm), include.rownames=FALSE)

  ggsave(filename = "./Outputs/Conjoint analysis/H1_interaction_MM.png",
         plot = plot, 
         width = 9, 
         height = 12, 
         dpi = 320)
  
  ## 2.3 AMCE Plot for health status * profession
  amce<- cj(data=choice_df,
          formula=CHOICE_INDICATOR~ALDER+HELBREDSTILSTAND+K_N+ANTAL_R_P_ARBEJDSMARKEDET,
          id = ~RESPONDENT_ID,
          estimate = "amce", 
          by=~PROFESSION,
          feature_labels=feature_list)
  
  plot<-plot(subset(amce,feature=="Health Status"),vline=NULL,feature_headers=FALSE)+
                 theme_bw() +
                 xlab("AMCE for Pr (Support for retirement recipient)")+
                 theme(strip.background = element_rect(fill = "black"),
                       strip.text = element_text(size = 7, colour = "white"),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       axis.line = element_blank(),
                       panel.background = element_rect(fill = "white", 
                                                       colour = "black",
                                                       size = 1),
                       axis.title = element_text(color = "black"),
                       axis.text = element_text(color = "black"),
                       axis.text.x = element_text(color = "black"),
                       axis.text.y = element_text(color = "black"),
                       axis.ticks =        element_line(colour = "grey50"),
                       legend.title=element_blank(),
                       legend.key=element_blank(),
                       legend.position = "none",
                       text=element_text(family="Arial")) +
                 ggtitle("") +
                 guides(colour = guide_legend(reverse=TRUE))+
                 scale_colour_manual(values=rep("black", 5))+
                 geom_vline(aes(xintercept=0),linetype="longdash")+
                 ggplot2::facet_wrap(~BY, ncol = 3L)
  
  plot

  #Latex output 
    amce<-amce%>%
      select(BY,feature,level,estimate,std.error,z,p)
    print(xtable(amce), include.rownames=FALSE)
    
  ggsave(filename = "./Outputs/Conjoint analysis/H1_Interaction_AMCE.png",
         plot = plot, 
         width = 9, 
         height = 12, 
         dpi = 320)
  
  #2.4 Back problems plot 
  
  mm <- cj(data=choice_df, 
           formula=CHOICE_INDICATOR~ALDER+K_N+HELBREDSTILSTAND+ANTAL_R_P_ARBEJDSMARKEDET,
           id = ~ RESPONDENT_ID,
           estimate = "mm", 
           h0=0.33,
           by=~PROFESSION,
           level_order = "descending",
           feature_labels=feature_list)
  
  plot<-plot(subset(mm,level=="Back problems"),group="PROFESSION",vline=NULL,feature_headers=FALSE)+
               xlab("Marginal means for Pr(Support for retirement recipient)")+
               aes(color = PROFESSION)+
               scale_color_manual(name = "Profession",values=c("#e6194b", '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', "#000075"))+
               theme(strip.background = element_rect(fill = "black"),
                     strip.text = element_text(size = 5, colour = "white"),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     axis.line = element_blank(),
                     panel.background = element_rect(fill = "white", 
                                                     colour = "black",
                                                     size = 1),
                     axis.title = element_text(color = "black"),
                     axis.text = element_text(color = "black"),
                     axis.text.x = element_text(color = "black"),
                     axis.text.y = element_text(color = "black"),
                     axis.ticks =        element_line(colour = "grey50"),
                     text=element_text(family="Arial"))+
               ggplot2::facet_wrap(~feature, ncol = 1L,
                                   scales = "free_y", strip.position = "right")+
               geom_vline(aes(xintercept=0.3333),linetype="longdash")
  

             
  ggsave(filename = "./Outputs/Conjoint analysis/Back problems.png",
         plot = plot, 
         width = 11, 
         height = 7, 
         dpi = 320)
  
  ## 2.5 MM Plot for years on labor market 
  mm<- cj(data=choice_df,
          formula=formula.main,
          id = ~RESPONDENT_ID,
          estimate = "mm", 
          h0=0.333,
          feature_labels=feature_list)
  
  #Plot
  plot<-plot(subset(mm,feature=="Years on labor market"),vline=NULL,feature_headers=FALSE)+
    theme_bw() +
    xlab("Marginal means for Pr (Support for retirement recipient)")+
    theme(strip.background = element_rect(fill = "black"),
          strip.text = element_text(size = 7, colour = "white"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          panel.background = element_rect(fill = "white", 
                                          colour = "black",
                                          size = 1),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          axis.ticks =        element_line(colour = "grey50"),
          legend.title=element_blank(),
          legend.key=element_blank(),
          legend.position = "none",
          text=element_text(family="Arial")) +
    ggtitle("") +
    guides(colour = guide_legend(reverse=TRUE))+
    scale_colour_manual(values=rep("black", 5))+
    geom_vline(aes(xintercept=0.333),linetype="longdash")+
    ggplot2::facet_wrap(~feature, ncol = 1L,
                        scales = "free_y", strip.position = "right")
  
  ggsave(filename = "./Outputs/Conjoint analysis/H3_mm.png",
         plot = plot, 
         width = 6, 
         height = 3, 
         dpi = 320)
  
  ## 2.6 AMCE Plot for years on labour market 
  amce<- cj(data=choice_df,
            formula.main,
            id = ~RESPONDENT_ID,
            feature_labels = feature_list,
            estimate = "amce")
  
  #Plot
  plot<-plot(subset(amce,feature=="Years on labor market"),vline=NULL,feature_headers=FALSE)+
    theme_bw() +
    xlab("AMCE for Pr (Support for retirement recipient)")+
    theme(strip.background = element_rect(fill = "black"),
          strip.text = element_text(size = 7, colour = "white"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          panel.background = element_rect(fill = "white", 
                                          colour = "black",
                                          size = 1),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          axis.ticks =        element_line(colour = "grey50"),
          legend.title=element_blank(),
          legend.key=element_blank(),
          legend.position = "none",
          text=element_text(family="Arial"))+
    ggtitle("") +
    guides(colour = guide_legend(reverse=TRUE))+
    scale_colour_manual(values=rep("black", 5))+
    geom_vline(aes(xintercept=0),linetype="longdash")+
    ggplot2::facet_wrap(~feature, ncol = 1L,
                        scales = "free_y", strip.position = "right")
  
  ggsave(filename = "./Outputs/Conjoint analysis/H3_AMCE.png",
         plot = plot, 
         width = 6, 
         height = 3, 
         dpi = 320)
  
  #######################################
  #3.0-------Subgroup Analysis-----------
  #######################################
  
  #3.1 Subgroup analysis - MM by Egalitarianism
  mm_1<- cj(data=choice_df,
          formula.main,
          id = ~RESPONDENT_ID,
          by= ~egal_grouped_labelled,
          estimate = "mm", 
          feature_labels=feature_list)
  
  diff_mm<- cj(data=choice_df,
          formula.main,
          id = ~RESPONDENT_ID,
          by= ~egal_grouped_labelled,
          estimate = "mm_diff", 
          feature_labels=feature_list)
  
  mm=(rbind(mm_1, diff_mm))
  
  plot<-plot(subset(mm,feature=="Health Status"),vline=NULL)+
    theme_bw() +
    xlab("Marginal means for Pr (Support for retirement recipient)")+
    theme(strip.background = element_rect(fill = "black"),
          strip.text = element_text(size = 7, colour = "white"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          panel.background = element_rect(fill = "white", 
                                          colour = "black",
                                          size = 1),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          axis.ticks =        element_line(colour = "grey50"),
          legend.title=element_blank(),
          legend.key=element_blank(),
          legend.position = "none",
          text=element_text(family="Arial")) +
    ggtitle("") +
    guides(colour = guide_legend(reverse=TRUE))+
    facet_wrap(~BY, ncol = 3L)+
    scale_colour_manual(values=rep("black", 5))+
    geom_vline(data=filter(mm, statistic=="mm"), aes(xintercept=0.333),linetype="longdash")+
    geom_vline(data=filter(mm, statistic=="mm_difference"), aes(xintercept=0),linetype="longdash")
  
  
  #F_test to formally test for interactionn 
  f_test<-cj_anova(choice_df, CHOICE_INDICATOR ~ ALDER + K_N + HELBREDSTILSTAND+ANTAL_R_P_ARBEJDSMARKEDET*PROFESSION, by = ~egal_grouped_labelled)
  
  #Latex output 
  print(xtable(mm_1[5:12]), include.rownames=FALSE)
  print(xtable(diff_mm[5:12]), include.rownames=FALSE)
  print(xtable(f_test), include.rownames=FALSE)
  

  ggsave(filename = "./Outputs/Conjoint analysis/H4_int_mm.png",
         plot=plot,
         width = 9,
         height=4, 
         dpi = 320)
  
  #3.2 Subgroup analysis - MM by humanitarianism
  mm_1<- cj(data=choice_df,
          formula.main,
          id = ~RESPONDENT_ID,
          by= ~hum_grouped_labelled,
          estimate = "mm", 
          feature_labels=feature_list)
  
  diff_mm<- cj(data=choice_df,
               formula.main,
               id = ~RESPONDENT_ID,
               by= ~hum_grouped_labelled,
               estimate = "mm_diff", 
               feature_labels=feature_list)
  
  mm=(rbind(mm_1, diff_mm))
  
  plot<-plot(subset(mm,feature=="Health Status"),vline=NULL)+
    theme_bw() +
    xlab("Marginal means for Pr (Support for retirement recipient)")+
    theme(strip.background = element_rect(fill = "black"),
          strip.text = element_text(size = 7, colour = "white"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          panel.background = element_rect(fill = "white", 
                                          colour = "black",
                                          size = 1),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          axis.ticks =        element_line(colour = "grey50"),
          legend.title=element_blank(),
          legend.key=element_blank(),
          legend.position = "none",
          text=element_text(family="Arial")) +
    ggtitle("") +
    guides(colour = guide_legend(reverse=TRUE))+
    facet_wrap(~BY, ncol = 3L)+
    scale_colour_manual(values=rep("black", 5))+
    geom_vline(data=filter(mm, statistic=="mm"), aes(xintercept=0.333),linetype="longdash")+
    geom_vline(data=filter(mm, statistic=="mm_difference"), aes(xintercept=0),linetype="longdash")
  
  #Ftest
  f_test<-cj_anova(choice_df, CHOICE_INDICATOR ~ ALDER + K_N + HELBREDSTILSTAND+ANTAL_R_P_ARBEJDSMARKEDET*PROFESSION, by = ~hum_grouped_labelled)
  
  #Print latex output
  print(xtable(mm_1[5:12]), include.rownames=FALSE)
  print(xtable(diff_mm[5:12]), include.rownames=FALSE)
  print(xtable(f_test), include.rownames=FALSE)
  
  
  ggsave(filename = "./Outputs/Conjoint analysis/H5_int_mm.png",
         plot=plot,
         width = 9,
         height=4, 
         dpi = 320)
  
  #3.3 - MM by sophistication
  mm_1<-cj(data=choice_df,
          formula.main,
          id = ~RESPONDENT_ID,
          by= ~soph_simple_grouped_labelled,
          estimate = "mm", 
          feature_labels=feature_list)
  
  diff_mm<- cj(data=choice_df,
               formula.main,
               id = ~RESPONDENT_ID,
               by= ~soph_simple_grouped_labelled,
               estimate = "mm_diff", 
               feature_labels=feature_list)
  
  mm=(rbind(mm_1, diff_mm))
  
  plot<-plot(subset(mm,feature=="Health Status"),vline=NULL)+
    theme_bw() +
    xlab("Marginal means for Pr (Support for retirement recipient)")+
    theme(strip.background = element_rect(fill = "black"),
          strip.text = element_text(size = 5, colour = "white"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          panel.background = element_rect(fill = "white", 
                                          colour = "black",
                                          size = 1),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          axis.ticks =        element_line(colour = "grey50"),
          legend.title=element_blank(),
          legend.key=element_blank(),
          legend.position = "none",
          text=element_text(family="Arial")) +
    ggtitle("") +
    guides(colour = guide_legend(reverse=TRUE))+
    facet_wrap(~BY, ncol = 3L)+
    scale_colour_manual(values=rep("black", 5))+
    geom_vline(data=filter(mm, statistic=="mm"), aes(xintercept=0.333),linetype="longdash")+
    geom_vline(data=filter(mm, statistic=="mm_difference"), aes(xintercept=0),linetype="longdash")
  
   #Ftest 
   f_test<-cj_anova(choice_df, CHOICE_INDICATOR ~ ALDER + K_N + HELBREDSTILSTAND+ANTAL_R_P_ARBEJDSMARKEDET*PROFESSION, by = ~soph_simple_grouped_labelled)
   #Latex output 
   print(xtable(mm_1[5:12]), include.rownames=FALSE)
   print(xtable(diff_mm[5:12]), include.rownames=FALSE)
   print(xtable(f_test), include.rownames=FALSE)
   
  ggsave(filename = "./Outputs/Conjoint analysis/H6_int_mm.png",
         plot=plot,
         width = 9,
         height=4, 
         dpi = 320)
  
  #3.4 Response time 
  mm_1<- cj(data=choice_df,
          formula.main,
          id = ~RESPONDENT_ID,
          by= ~response_time_cut_labeled,
          estimate = "mm", 
          feature_labels=feature_list)
  
  diff_mm<- cj(data=choice_df,
               formula.main,
               id = ~RESPONDENT_ID,
               by= ~response_time_cut_labeled,
               estimate = "mm_diff", 
               feature_labels=feature_list)
  
  mm=(rbind(mm_1, diff_mm))
  
  plot<-plot(subset(mm,feature=="Health Status"),vline=NULL)+
    theme_bw() +
    xlab("Marginal means for Pr (Support for retirement recipient)")+
    theme(strip.background = element_rect(fill = "black"),
          strip.text = element_text(size = 7, colour = "white"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          panel.background = element_rect(fill = "white", 
                                          colour = "black",
                                          size = 1),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          axis.ticks =        element_line(colour = "grey50"),
          legend.title=element_blank(),
          legend.key=element_blank(),
          legend.position = "none",
          text=element_text(family="Arial")) +
    ggtitle("") +
    guides(colour = guide_legend(reverse=TRUE))+
    facet_wrap(~BY, ncol = 3L)+
    scale_colour_manual(values=rep("black", 5))+
    geom_vline(data=filter(mm, statistic=="mm"), aes(xintercept=0.333),linetype="longdash")+
    geom_vline(data=filter(mm, statistic=="mm_difference"), aes(xintercept=0),linetype="longdash")
  
  plot
  

  f_test<-cj_anova(choice_df, CHOICE_INDICATOR ~ ALDER + K_N + HELBREDSTILSTAND+ANTAL_R_P_ARBEJDSMARKEDET*PROFESSION, by = ~response_time_cut_labeled)
  print(xtable(mm_1[5:12]), include.rownames=FALSE)
  print(xtable(diff_mm[5:12]), include.rownames=FALSE)
  print(xtable(f_test), include.rownames=FALSE)
  
  ggsave(filename = "./Outputs/Conjoint analysis/H7_int_mm.png",
         plot=plot,
         width = 9,
         height=4, 
         dpi = 320)
  
  #####################################
  #4.0 Priming experiment -------------
  #####################################
  
  #4.1 
  #Subgroup analysis - MM by cue between subjects 
  mm_1<- cj(data=choice_df_2,
          formula.main,
          id = ~RESPONDENT_ID,
          by= ~cue_given,
          estimate = "mm", 
          h0=0.333,
          feature_labels=feature_list)
  
  diff_mm<- cj(data=choice_df_2,
               formula.main,
               id = ~RESPONDENT_ID,
               by= ~cue_given,
               estimate = "mm_diff", 
               feature_labels=feature_list)
  
  mm<-rbind(mm_1,diff_mm)
  
  plot<-plot(subset(mm,feature=="Years on labor market"|feature=="Health Status"),vline=NULL)+
    theme_bw() +
    xlab("Marginal means for Pr (Support for retirement recipient)")+
    theme(strip.background = element_rect(fill = "black"),
          strip.text = element_text(size = 7, colour = "white"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          panel.background = element_rect(fill = "white", 
                                          colour = "black",
                                          size = 1),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          axis.ticks =        element_line(colour = "grey50"),
          legend.title=element_blank(),
          legend.key=element_blank(),
          legend.position = "none",
          text=element_text(family="Arial")) +
    ggtitle("") +
    guides(colour = guide_legend(reverse=TRUE))+
    ggplot2::facet_wrap(~feature, ncol = 1L,
    scales = "free_y", strip.position = "right")+
    facet_wrap(~BY, ncol = 3L)+
    scale_colour_manual(values=rep("black", 5))+
    geom_vline(data=filter(mm, statistic=="mm"), aes(xintercept=0.333),linetype="longdash")+
    geom_vline(data=filter(mm, statistic=="mm_difference"), aes(xintercept=0),linetype="longdash")
  
  #ftest 
  f_test<-cj_anova(data=choice_df_2, formula=CHOICE_INDICATOR ~ ALDER + K_N + HELBREDSTILSTAND+ANTAL_R_P_ARBEJDSMARKEDET*PROFESSION, by = ~cue_given)
  
  #Lateex output 
  print(xtable(mm_1[5:12]), include.rownames=FALSE)
  print(xtable(diff_mm[5:12]), include.rownames=FALSE)
  print(xtable(f_test), include.rownames=FALSE)
  
  ggsave(filename = "./Outputs/Conjoint analysis/H9_int_mm.png",
         plot=plot,
         width = 11,
         height=8, 
         dpi = 320)
  
  #4.2 
  #Subgroup analysis - Difference within subjects - reciprocity 
  choice_df_cue_reciprocity<-choice_df_cue%>%
    filter(cue_given=="Reciprocity")
  
  mm_1<- cj(data=choice_df_cue_reciprocity,
            formula.main,
            id = ~RESPONDENT_ID,
            by= ~CONJOINT_SET,
            estimate = "mm", 
            h0=0.33,
            feature_labels=feature_list)
  
  diff_mm<- cj(data=choice_df_cue_reciprocity,
                 formula.main,
                 id = ~RESPONDENT_ID,
                 by= ~CONJOINT_SET,
                 estimate = "mm_diff", 
                 feature_labels=feature_list)
  
  mm<-rbind(mm_1,diff_mm)
  
  plot<-plot(subset(mm,feature=="Years on labor market"),vline=NULL)+
    theme_bw() +
    xlab("Marginal means for Pr(Support for retirement recipient)")+
    theme(strip.background = element_rect(fill = "black"),
          strip.text = element_text(size = 7, colour = "white"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          panel.background = element_rect(fill = "white", 
                                          colour = "black",
                                          size = 1),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          axis.ticks =        element_line(colour = "grey50"),
          legend.title=element_blank(),
          legend.key=element_blank(),
          legend.position = "none",
          text=element_text(family="Arial")) +
    ggtitle("") +
    guides(colour = guide_legend(reverse=TRUE))+
    facet_wrap(~BY, ncol = 3L)+
    scale_colour_manual(values=rep("black", 5))+
    geom_vline(data=filter(mm, statistic=="mm"), aes(xintercept=0.333),linetype="longdash")+
    geom_vline(data=filter(mm, statistic=="mm_difference"), aes(xintercept=0),linetype="longdash")
  
  #ftest
  f_test<-cj_anova(data=choice_df_cue_reciprocity, formula=CHOICE_INDICATOR ~ ALDER + K_N + HELBREDSTILSTAND+ANTAL_R_P_ARBEJDSMARKEDET*PROFESSION, by = ~CONJOINT_SET)
  
  #Latex output 
  print(xtable(mm_1[5:12]), include.rownames=FALSE)
  print(xtable(diff_mm[5:12]), include.rownames=FALSE)
  print(xtable(f_test), include.rownames=FALSE)
  
  ggsave(filename = "./Outputs/Conjoint analysis/H9_int_mm_rec.png",
         plot=plot,
         width = 7,
         height=3, 
         dpi = 320)
  
  #4.3 Subgroup analysis - within subjects - Need 
  choice_df_cue_need<-choice_df_cue%>%
    filter(cue_given=="Need")

  mm_1<- cj(data=choice_df_cue_need,
          formula.main,
          id = ~RESPONDENT_ID,
          by= ~CONJOINT_SET,
          estimate = "mm", 
          h0=0.33,
          feature_labels=feature_list)
  
  diff_mm<- cj(data=choice_df_cue_need,
               formula.main,
               id = ~RESPONDENT_ID,
               by= ~CONJOINT_SET,
               estimate = "mm_diff", 
               feature_labels=feature_list)
  
  mm<-rbind(mm_1,diff_mm)
  
  plot<-plot(subset(mm,feature=="Health Status"),vline=NULL)+
    theme_bw() +
    xlab("Marginal means for Pr(Support for retirement recipient)")+
    theme(strip.background = element_rect(fill = "black"),
          strip.text = element_text(size = 7, colour = "white"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          panel.background = element_rect(fill = "white", 
                                          colour = "black",
                                          size = 1),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          axis.ticks =        element_line(colour = "grey50"),
          legend.title=element_blank(),
          legend.key=element_blank(),
          legend.position = "none",
          text=element_text(family="Arial")) +
    ggtitle("") +
    guides(colour = guide_legend(reverse=TRUE))+
    facet_wrap(~BY, ncol = 3L)+
    scale_colour_manual(values=rep("black", 5))+
    geom_vline(data=filter(mm, statistic=="mm"), aes(xintercept=0.333),linetype="longdash")+
    geom_vline(data=filter(mm, statistic=="mm_difference"), aes(xintercept=0),linetype="longdash")
  
  #ftest 
  f_test<-cj_anova(data=choice_df_cue_need, formula=CHOICE_INDICATOR ~ ALDER + K_N + HELBREDSTILSTAND+ANTAL_R_P_ARBEJDSMARKEDET*PROFESSION, by = ~CONJOINT_SET)
  
  #stargazer 
  print(xtable(mm_1[5:12]), include.rownames=FALSE)
  print(xtable(diff_mm[5:12]), include.rownames=FALSE)
  print(xtable(f_test), include.rownames=FALSE)
  
  ggsave(filename = "./Outputs/Conjoint analysis/H9_int_mm_need.png",
         plot=plot,
         width = 7,
         height=3, 
         dpi = 320)
  
  #Regression on support for policy based on treatment or control group
  exp_model<-estimatr::lm_robust(formula=POL_approval~cue_given_num,data=choice_df_2,clusters=RESPONDENT_ID)
  exp_model<-tidy(exp_model)
  
  exp_model<-exp_model%>%
    filter(term=="cue_given_num")%>%
    mutate(term_renamed=case_when(term=="cue_given_num"~"Reciprocity frame"))
  
  reg_plot<-ggplot(data=exp_model,aes(x=estimate,y=term_renamed))+
    geom_point()+
    geom_errorbar(data=exp_model, aes(xmin=conf.low,xmax=conf.high,width=0),alpha=0.5,color="black")+
    xlab("Change in opinion towards earlier retirement")+
    ylab(NULL)+
    ggtitle("")+
    theme(axis.title.y=element_text(size=8),
         axis.text.x=element_text(angle=45,hjust=1,colour="black"),
         axis.line = element_line(colour = "black"),
         panel.background = element_rect(fill="white"),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         plot.title = element_text(size = 9,hjust=0.5))+
        geom_vline(aes(xintercept=0),linetype="solid")+
    scale_x_continuous(limits = c(-0.5,0.5))
  
  #printing latex output 
  print(xtable(exp_model), include.rownames=FALSE)
  
  ggsave(filename = "./Outputs/Conjoint analysis/Simple_regression.png",
         plot=reg_plot,
         width = 4,
         height=4, 
         dpi = 320)
      
