# figure 4 confusion matrix
library(tidyverse)

YoloRes <- read.csv("../chumputer-manuscript/data/EnsemblingPredwithImageID_512_Finalyolo2.csv") %>%
  mutate(PreProcess = "Yolo")

SamRes <- read.csv("../chumputer-manuscript/data/EnsemblingPredwithImageID_512_Final.csv") %>%
  mutate(PreProcess = "Sam")


ModelRes <- YoloRes %>%
  bind_rows(.,SamRes) 

#Compare age spec estimation
nScales_Model <- ModelRes %>%
  group_by(PreProcess,label)%>%
  summarize(nScales = n())

PredictionSummary <- ModelRes %>%
  mutate(Agree = ifelse(combined_test_predictions_round == label,1,0))%>%
  group_by(PreProcess,combined_test_predictions_round,label) %>%
  left_join(.,nScales_Model,by=c("PreProcess",'label'))%>%
  group_by(PreProcess,combined_test_predictions_round,label,nScales) %>%
  dplyr::summarize(nPred= n())%>%
  mutate(PropPred = nPred/nScales) %>%
  mutate(PreProcess = case_match(PreProcess, "Sam" ~ "SAM",
                                 "Yolo" ~ "YOLO"))

png("./Output/Figures/Fig4_confusion_matrix.png",width=8,height=9.5,units="in",res=300)
ggplot(PredictionSummary %>% filter(label!=7),aes(x=as.factor(label),y=combined_test_predictions_round,fill=PropPred))+
  geom_tile()+
  facet_wrap(~PreProcess,ncol=1)+
  ylab('Rounded Model Age Prediction')+
  xlab('Human Age Estimate')+
  # scale_fill_viridis_c(
  #   begin=0.5, end=1,
  #   name = "Proportion\nPredictions",
  #   breaks = 0.25 * 0:4,
  #   labels = scales::percent(0.25 * 0:4),
  #   option = "magma"
  #   )+
  
  scale_fill_gradient(low='gray90',high='gray50',
                      name='Proportion\nPredictions',
                      breaks = 0.25*0:4, labels = scales::percent(0.25*0:4) )+
  geom_text(data=PredictionSummary%>% filter(label!=7),aes(x=label,y=combined_test_predictions_round,label=paste(round(PropPred*100,1),"%",sep="")))+
  theme_classic()+
  scale_x_discrete(labels= paste(seq(1,7,by=1)," (n=", nScales_Model$nScales,")",sep="")[c(1:7)],
                   breaks = c(1:7))+ 
  scale_y_continuous(breaks = c(1:7))+ 
  theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=.25)
  )
dev.off()

ggsave("./Output/Figures/Fig4_confusion_matrix.pdf", width=8,height=9.5,units="in",dpi=300)