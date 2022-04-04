#########################################
##########-Robustness tests-#############
#########################################

##1.0 MM by choice set 
mm <- cj(data=choice_df, 
         formula=formula.main,
         id = ~ RESPONDENT_ID,
         estimate = "mm", 
         h0=0.33,
         by=~CHOICE_SET,
         feature_labels=feature_list)

plot<-plot(mm,group="CHOICE_SET",vline=NULL,feature_headers=FALSE)+
  xlab("Marginal means for Pr(Support for retirement recipient)")+
  aes(shape = CHOICE_SET)+
  scale_colour_continuous(guide = FALSE)+
  scale_shape_manual(name = "Contest no.",values=c(1, 2, 3, 4, 5))+
  scale_colour_manual(values=rep("black", 5))+
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
  geom_vline(aes(xintercept=0.3333),linetype="longdash")+
  guides(color = FALSE) 


#Ftest 
f_test<-cj_anova(data=choice_df, formula=CHOICE_INDICATOR ~ ALDER + K_N + HELBREDSTILSTAND+ANTAL_R_P_ARBEJDSMARKEDET*PROFESSION, by = ~CHOICE_SET)
print(xtable(f_test), include.rownames=FALSE)
print(xtable(mm[5:12]), include.rownames=FALSE)


ggsave(filename = "./Outputs/Robustness tests/Complete_mm_robust.png",
       plot = plot, 
       width = 8, 
       height = 9, 
       dpi = 320)

#################################
#2.0--------Balancetests----------
#################################
#
#2.1 Gender and level distribution first conjoint block
mm <- cj(data = choice_df,
         formula = Kon_dich ~ 
           ALDER + 
           HELBREDSTILSTAND + 
           ANTAL_R_P_ARBEJDSMARKEDET + 
           K_N+PROFESSION,
         id = ~RESPONDENT_ID,
         estimate = "mm",
         feature_labels = feature_list,
         h0 = mean(choice_df$Kon_dich))

plot_1<-plot(mm,vline=NULL,feature_headers=FALSE)+
  theme_bw() +
  xlab("Marginal means for Pr(Man)")+
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
  scale_colour_manual(values=rep("black", 5))+
  geom_vline(aes(xintercept=mean(choice_df$Kon_dich)),linetype="longdash")+
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right")


print(xtable(mm[4:8]), include.rownames=FALSE)

ggsave(filename = "./Outputs/Robustness tests/Robust_balance_gender.png",
       plot = plot, 
       width = 8, 
       height = 9, 
       dpi = 320)

#2.2 Age and level distribution
mm <- cj(data = choice_df_cue,
         formula = Q4_ALDER ~ 
           ALDER + 
           HELBREDSTILSTAND + 
           ANTAL_R_P_ARBEJDSMARKEDET + 
           K_N+PROFESSION,
         id = ~RESPONDENT_ID,
         estimate = "mm",
         feature_labels = feature_list,
         h0 = mean(choice_df$Q4_ALDER))

plot_2<-plot(mm,vline=NULL,feature_headers=FALSE)+
  theme_bw() +
  xlab("Marginal means for mean age")+
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
  scale_colour_manual(values=rep("black", 5))+
  geom_vline(aes(xintercept=mean(choice_df$Q4_ALDER)),linetype="longdash")+
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right")


print(xtable(mm[4:8]), include.rownames=FALSE)

ggsave(filename = "./Outputs/Robustness tests/Robust_balance_age.png",
       plot = plot, 
       width = 8, 
       height = 9, 
       dpi = 320)

#2.3 Education and level distribution
mm <- cj(data = choice_df_cue,
         formula = Edu_cat_num ~ 
           ALDER + 
           HELBREDSTILSTAND + 
           ANTAL_R_P_ARBEJDSMARKEDET + 
           K_N+PROFESSION,
         id = ~RESPONDENT_ID,
         estimate = "mm",
         feature_labels = feature_list,
         h0 = mean(choice_df$Edu_cat_num))

plot_3<-plot(mm,vline=NULL,feature_headers=FALSE)+
  theme_bw() +
  xlab("Marginal means for further education")+
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
  scale_colour_manual(values=rep("black", 5))+
  geom_vline(aes(xintercept=mean(choice_df$Edu_cat_num)),linetype="longdash")+
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right")

print(xtable(mm[4:8]), include.rownames=FALSE)

ggsave(filename = "./Outputs/Robustness tests/Robust_balance_education.png",
       plot = plot, 
       width = 8, 
       height = 9, 
       dpi = 320)


plot <- grid.arrange(plot_1, plot_2, plot_3, nrow = 1)

ggsave(filename = "./Outputs/Robustness tests/Balance_test_feature_distribution.png",
       plot = plot, 
       width = 16, 
       height = 8, 
       dpi = 320)

#2.4 Level distribution for second conjoint block 
#Making dataframe only containing second conjoint block, but including background variables 
choice_df_cue_2<-choice_df_cue%>%
  filter(CONJOINT_SET=="After priming",!is.na(Kon_dich))
#2.5 Gender and level distribution second conjoint block
mm <- cj(data = choice_df_cue_2,
         formula = Kon_dich ~ 
           ALDER + 
           HELBREDSTILSTAND + 
           ANTAL_R_P_ARBEJDSMARKEDET + 
           K_N+PROFESSION,
         id = ~RESPONDENT_ID,
         estimate = "mm",
         feature_labels = feature_list,
         h0 = mean(choice_df_cue_2$Kon_dich))

plot_1<-plot(mm,vline=NULL,feature_headers=FALSE)+
  theme_bw() +
  xlab("Marginal means for Pr(Man)")+
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
  scale_colour_manual(values=rep("black", 5))+
  geom_vline(aes(xintercept=mean(choice_df_cue_2$Kon_dich)),linetype="longdash")+
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right")

print(xtable(mm[4:8]), include.rownames=FALSE)

#2.6 Age and level distribution second conjoint 
mm <- cj(data = choice_df_cue_2,
         formula = Q4_ALDER ~ 
           ALDER + 
           HELBREDSTILSTAND + 
           ANTAL_R_P_ARBEJDSMARKEDET + 
           K_N+PROFESSION,
         id = ~RESPONDENT_ID,
         estimate = "mm",
         feature_labels = feature_list,
         h0 = mean(choice_df_cue_2$Q4_ALDER))

plot_2<-plot(mm,vline=NULL,feature_headers=FALSE)+
  theme_bw() +
  xlab("Marginal means for mean age")+
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
  scale_colour_manual(values=rep("black", 5))+
  geom_vline(aes(xintercept=mean(choice_df_cue_2$Q4_ALDER)),linetype="longdash")+
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right")


print(xtable(mm[4:8]), include.rownames=FALSE)


#2.7 Education and level distribution second conjoint 
mm <- cj(data = choice_df_cue_2,
         formula = Edu_cat_num ~ 
           ALDER + 
           HELBREDSTILSTAND + 
           ANTAL_R_P_ARBEJDSMARKEDET + 
           K_N+PROFESSION,
         id = ~RESPONDENT_ID,
         estimate = "mm",
         feature_labels = feature_list,
         h0 = mean(choice_df_cue_2$Edu_cat_num))

plot_3<-plot(mm,vline=NULL,feature_headers=FALSE)+
  theme_bw() +
  xlab("Marginal means for further education")+
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
  scale_colour_manual(values=rep("black", 5))+
  geom_vline(aes(xintercept=mean(choice_df_cue_2$Edu_cat_num)),linetype="longdash")+
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right")

print(xtable(mm[4:8]), include.rownames=FALSE)

#Making grid 
plot <- grid.arrange(plot_1, plot_2, plot_3, nrow = 1)

ggsave(filename = "./Outputs/Robustness tests/Balance_test_feature_distribution_second_conjoint.png",
       plot = plot, 
       width = 16, 
       height = 8, 
       dpi = 320)

#2.8 Feature distribution - first conjoint block
cj_freq_1<-cj_freqs(
  data=choice_df,
  formula= ~ 
    ALDER + 
    HELBREDSTILSTAND + 
    ANTAL_R_P_ARBEJDSMARKEDET + 
    K_N+PROFESSION, 
  id =~RESPONDENT_ID,
  feature_labels = feature_list)

#Making frequencies into percentage 
cj_freq_pct_1<-cj_freq_1%>%
  group_by(feature)%>% 
  mutate(estimate = (estimate / sum(estimate)*100))

#Making object the class"cj_freqs" 
class(cj_freq_pct_1)<-class(cj_freq_1)

plot_1<-plot(cj_freq_pct_1,feature_headers=TRUE)+
  theme_bw() +
  ylab("Percentage")+
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
        text=element_text(family="Arial"),
        plot.title = element_text(size = 12)) +
  ggtitle("Before priming experiment (n=27975)")+
  ggplot2::scale_fill_manual(values = rep("black", 9))

print(xtable(cj_freq_1), include.rownames=FALSE)


#2.9 Feature distribution - second conjoint block
cj_freq_2<-cj_freqs(
  data=choice_df_2,
  formula= ~ 
    ALDER + 
    HELBREDSTILSTAND + 
    ANTAL_R_P_ARBEJDSMARKEDET + 
    K_N+PROFESSION, 
  id =~RESPONDENT_ID,
  feature_labels = feature_list)

#Making frequencies into percentage 
cj_freq_pct_2<-cj_freq_2%>%
  group_by(feature)%>% 
  mutate(estimate = (estimate / sum(estimate)*100))

#Making object the class"cj_freqs" 
class(cj_freq_pct_2)<-class(cj_freq_2)

plot_2<-plot(cj_freq_pct_2,feature_headers=TRUE)+
  theme_bw() +
  ylab("Percentage")+
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
        text=element_text(family="Arial"),
        plot.title = element_text(size = 12)) +
  ggtitle("After priming experiment (n=25860)")+
  ggplot2::scale_fill_manual(values = rep("black", 9))

print(xtable(cj_freq_2),include.rownames=FALSE)

Robustness_grid<-ggarrange(plot_1,plot_2)

ggsave(filename = "./Outputs/Robustness tests/Feature_distribution.png",
       plot = Robustness_grid, 
       width = 13, 
       height = 9, 
       dpi = 320)

#3.0 Distribution of primes 
#Creating dataframe 
prime_dist <- choice_df_2 %>%
  group_by(cue_given) %>%
  tally() %>%
  mutate(andel = (n / sum(n))*100,
         feature = "Priming experiment")

stargazer(mm,type="latex",summary=FALSE)

# Plot
plot <- prime_dist %>%
  ggplot(., aes(x = andel,
                y = reorder(cue_given, desc(cue_given)))) +
  geom_col(fill = "black") +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 100)) +
  ylab("") +
  xlab("Percentage") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y")+
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(size = 8, colour = "white"),
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
        text=element_text(family="Arial"))

# Create LaTeX output
output <- kable(x = prime_dist,
                format = "latex",
                digits = 2)


ggsave(filename = "./Outputs/Robustness tests/Prime_distribution_simple.png",
       plot = plot, 
       width = 9, 
       height = 7, 
       dpi = 320)

# Create LaTeX output
output_gender <- kable(x = gender,
                       format = "latex",
                       digits = 2)

#3.1
### Priming experiment - calculation of prime on background variables  
#make datafarme
df <- data.frame(cue_given = c("Need",
                               "Reciprocity"))
# Select data
prime<- choice_df_cue%>%   
  filter(!is.na(Kon_dich),CONJOINT_SET=="After priming")%>%
  select(RESPONDENT_ID, Q4_ALDER, Edu_cat_num, Kon_dich, cue_given)%>%
  distinct()

levels(choice_df_cue$CONJOINT_SET)
# Gender 
model <- lm(formula = Kon_dich ~ cue_given, data = prime)

gender <- data.frame(resp_eksperiment = c("Need cue",
                                          "Reciprocity cue"),
                     predict(object = model,
                             newdata = df,
                             interval = "confidence",
                             type = "response"),
                     feature = "Experimental situation")

output_gender <- kable(x = gender,
                       format = "latex",
                       digits = 2)

# Visualise results
plot1 <- gender %>%  
  ggplot(., aes(x = fit, 
                y = resp_eksperiment)) +
  geom_point(fill = "black") +
  geom_errorbar(aes(xmin = lwr, 
                    xmax = upr),
                height = 0.2,
                color = "black") +
  geom_vline(xintercept = mean(prime$Kon_dich),
             linetype = "longdash",
             color = "black") +
  xlab("Estimated Pr(Man)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y")+
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(size = 8, colour = "white"),
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
        text=element_text(family="Arial"))

#Age
model <- lm(formula = Q4_ALDER ~ cue_given, data = prime)


age <- data.frame(resp_eksperiment = c("Need cue",
                                       "Reciprocity cue"),
                  predict(object = model,
                          newdata = df,
                          interval = "confidence",
                          type = "response"),
                  feature = "Experimental situation")

# Visualise results
plot2 <- age %>%  
  ggplot(., aes(x = fit, 
                y = resp_eksperiment)) +
  geom_point(fill = "black") +
  geom_errorbar(aes(xmin = lwr, 
                    xmax = upr),
                height = 0.2,
                color = "black") +
  geom_vline(xintercept = mean(prime$Q4_ALDER),
             linetype = "longdash",
             color = "black") +
  xlab("Estimated mean age") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y")+
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(size = 8, colour = "white"),
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
        text=element_text(family="Arial"))

# Create LaTeX output
output_age <- kable(x = age,
                    format = "latex",
                    digits = 2)

#Education 
model <- lm(formula = Edu_cat_num ~ cue_given, data = prime)


edu <- data.frame(resp_eksperiment = c("Need cue",
                                       "Reciprocity cue"),
                  predict(object = model,
                          newdata = df,
                          interval = "confidence",
                          type = "response"),
                  feature = "Experimental situation")

output_edu <- kable(x = edu,
                    format = "latex",
                    digits = 2)

plot3 <- edu %>%  
  ggplot(., aes(x = fit, 
                y = resp_eksperiment)) +
  geom_point(fill = "black") +
  geom_errorbar(aes(xmin = lwr, 
                    xmax = upr),
                height = 0.2,
                color = "black") +
  geom_vline(xintercept = mean(prime$Edu_cat_num),
             linetype = "longdash",
             color = "black") +
  xlab("Estimated Pr(Further education)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y")+
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(size = 8, colour = "white"),
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
        text=element_text(family="Arial"))

# Combine into one
plot <- grid.arrange(plot1, plot2, plot3, nrow = 1)

ggsave(filename = "./Outputs/Robustness tests/Balance_test_Prime_distribution.png",
       plot = plot, 
       width = 15, 
       height = 6, 
       dpi = 320)