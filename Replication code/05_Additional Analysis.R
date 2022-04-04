#Additional analysis 


#1.0isease plot 
#Labelling diseases for plot in research design, based on the codebook found at:  
### https://dataverse.harvard.edu/file.xhtml?persistentId=doi%3A10.7910%2FDVN%2FTIXGF8%2F8WHW2G&version=1.2&fbclid=IwAR3e2Kv_j4idY8h96LvqK5HWshPvzc9UPt3XjacDlBm0jJvO6cLyARqOFZo

disease_df<-disease_df%>%
  mutate(problem_labelled=case_when(problem==1~"Fracture",
                                    problem==2~"Diabetes",
                                    problem==3~"Cancer",
                                    problem==4~"Obesity",
                                    problem==5~"Allergy",
                                    problem==6~"Depression",
                                    problem==7~"Flu",
                                    problem==8~"Cardiac problems",
                                    problem==9~"Respiratory problems",
                                    problem==10~"Back problems",
                                    problem==11~"Arthritis",
                                    problem==12~"Osteoporosis",
                                    problem==13~"HIV",
                                    problem==14~"Kidney stone",
                                    problem==15~"Appendicitis",
                                    problem==16~"Epilepsy",
                                    problem==17~"Liver inflammation",
                                    problem==18~"Chronic Bronchitis ('Smokers's lungs')",
                                    problem==19~"Psychosis",
                                    problem==20~"Astma"))

#Making data for highlighting colors in plot 
highlight_high_control<-disease_df%>%
  filter(problem==12|problem==11)
highlight_medium_control<-disease_df%>%
  filter(problem==10)
highlight_low_control<-disease_df%>%
  filter(problem==18|problem==4)


#Making plot containing diseases 
disease_plot<-ggplot(disease_df,aes(x= sick, y= governhelp,label=problem_labelled))+
  geom_point()+
  geom_text(hjust=0, vjust=0)+
  geom_point(data=highlight_high_control, 
  aes(x=sick,y=governhelp), 
  color='green',
  size=3)+geom_point(data=highlight_medium_control, 
  aes(x=sick,y=governhelp), 
  color='yellow',
  size=3)+geom_point(data=highlight_low_control, 
  aes(x=sick,y=governhelp), 
  color='red',
  size=3)+theme(panel.background = element_rect(fill="white"),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
  text=element_text(family="Arial"))+
  labs(x = "Is problem caused by Disease?", y="Should the Government Help People with Problem?")

ggsave(filename = "./Outputs/Additional analyses/Disease_plot.png",
       plot = disease_plot, 
       width = 12, 
       height = 8, 
       dpi = 320)



####################################
##########Power analysis############
####################################
#2.0 Model without innteraction 
df = cjpowr_amce(amce = 0.03, power = 0.8, levels = 6)
df$n/(3*5)
df$n
cjpowr_amce_vec <- Vectorize(cjpowr_amce)
d <- expand.grid(amce = c(0.02, 0.03, 0.05, 0.1),n = seq(from = 100, to = 20000, length.out = 1000))
df <- t(cjpowr_amce_vec(amce = d$amce, n = d$n, sims = 100000, levels = 6, alpha = 0.05, delta0 = 0.5))
df <- data.frame(df)
df[] <- lapply(df, unlist)

ggplot(df, aes(x = n, y = exp_typeM, color = amce)) +
  geom_point()

power_analysis_plot<-plotly::plot_ly(df, x = ~n, y = ~exp_typeM, type = 'scatter', mode = 'lines', linetype = ~amce, color = I('black')) %>%
  layout(
    xaxis = list(title = "Efficient sample size",
                 zeroline = F),

    yaxis = list(title = "Exaggeration ratio",
                 range = c(0,10),
                 zeroline = F),
    legend=list(title=list(text='<b> AMCE </b>'))
  ) 

sf = cjpowr_amce(amce = 0.03, power = 0.8, levels = 17)

sf$n/(3*5)


#Model with innteraction 
  df<-cjpowr_amcie(delta1 = 0, delta2 = 0, delta3 = 0.05, levels1=6, levels2=2, power=0.8)
  
  df$n/15
  
  d <- plyr::mdply(power, cjpowr_amcie,
                                    delta3 = seq(from = 0.02, to = 0.1, length.out = 1000),
                                    delta2 = 0,
                                    delta1 = 0,
                                    alpha = 0.05,
                                    levels1 = 17,
                                    levels2 = 2,
                                    delta0=0.5, sims = NULL)
                   
                    plot_ly(d, x = ~delta3, y = ~n, type = 'scatter', mode = 'lines', linetype = ~power,color = I('black'))%>%
                     layout(
                       xaxis = list(title = "AMCIE", zeroline = F, hoverformat = '.4f'),
                       yaxis = list(title = "Minimum Effective Sample Size", zeroline = F, hoverformat = '.2f'),
                       legend=list(title=list(text='<b> Power </b>')),
                       hovermode = "x unified"
                     )
                   
                    #Generating an interactive plot for type M error:
                   
                    cjpowr_amcie_vec <- Vectorize(cjpowr_amcie)
                    d <- expand.grid(delta3 = c(0.01, 0.02, 0.03, 0.05), n = seq(from = 100, to = 50000, length.out = 1000))
                    df <- t(cjpowr_amcie_vec(delta3 = d$delta3, n = d$n, sims = 10000, levels1 = 6, levels2=2, alpha = 0.05, delta0 = 0.5))
                    df <- data.frame(df)
                    df[] <- lapply(df, unlist)
                   
                    plot_ly(df, x = ~n, y = ~exp_typeM, type = 'scatter', mode = 'lines', color = I('black'),linetype = ~delta3) %>%
                     layout(
                       xaxis = list(title = "Effective Sample Size",
                                    zeroline = F,
                                    hoverformat = '.0f'),
                       yaxis = list(title = "Exaggeration Ratio",
                                    range = c(0,10),
                                    zeroline = F,
                                    hoverformat = '.2f'),
                       legend=list(title=list(text='<b> AMCIE </b>')),
                       hovermode = "x unified"
                     )

  