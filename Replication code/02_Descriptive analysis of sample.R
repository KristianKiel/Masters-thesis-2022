###################################################
#########Descirpitive analysis of data#############
###################################################

#1.1 ############# Decriptive plots ###########
plot_egal<-choice_df %>%
  select(RESPONDENT_ID,egal)%>%
  distinct()%>%
  ggplot( aes(x=egal)) +
  geom_histogram( binwidth=1, fill="grey", color="black", alpha=0.9) +
  theme(panel.background = element_rect(fill = "white", colour = "white"))+
  theme(text = element_text(size=20))+
  xlab("Egalitarianism")

ggsave(filename = "./Outputs/Sample description/Egal_desc.png",
       plot = plot_egal, 
       width = 8, 
       height = 9, 
       dpi = 320)

plot_hum<-choice_df %>%
  select(RESPONDENT_ID,hum)%>%
  distinct()%>%
  ggplot( aes(x=hum)) +
  geom_histogram( binwidth=1, fill="grey", color="black", alpha=0.9) +
  theme(panel.background = element_rect(fill = "white", colour = "white"))+
  theme(text = element_text(size=20))+
  xlab("Humanitarianism")

ggsave(filename = "./Outputs/Sample description/Hum_desc.png",
       plot = plot_hum, 
       width = 8, 
       height = 9, 
       dpi = 320)


plot_soph<-choice_df %>%
  select(RESPONDENT_ID,soph_simple)%>%
  distinct()%>%
  ggplot( aes(x=soph_simple)) +
  geom_histogram( binwidth=1, fill="grey", color="black", alpha=0.9) +
  theme(panel.background = element_rect(fill = "white", colour = "white"))+
  theme(text = element_text(size=20))+
  xlab("Sophistication")

ggsave(filename = "./Outputs/Sample description/Soph_desc.png",
       plot = plot_soph, 
       width = 8, 
       height = 9, 
       dpi = 320)

