# Figure 3 
library(tidyverse)

ModelDat <- read.csv("../chumputer-manuscript/data/model_train_val_test_data.csv")

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

nScales_Agency <- ModelRes %>%
  mutate('SampleID' = tolower(gsub(pattern=".tif|.TIF|.png|.PNG|.JPG|.jpg|.JPEG|.jpeg",replacement="",x=filename))) %>%
  left_join(., ModelDat %>%
              mutate('SampleID' = tolower(gsub(pattern=".tif|.TIF|.png|.PNG|.JPG|.jpg|.JPEG|.jpeg",replacement="",file_name))),
            by='SampleID') %>%
  rename('label' = 'label.x') %>%
  filter(PreProcess == 'Sam')%>%
  group_by(agency,label) %>%
  summarize(nScales = n())

AgencyRes <- ModelRes %>%
  mutate('SampleID' = tolower(gsub(pattern=".tif|.TIF|.png|.PNG|.JPG|.jpg|.JPEG|.jpeg",replacement="",x=filename))) %>%
  left_join(., ModelDat %>%
              mutate('SampleID' = tolower(gsub(pattern=".tif|.TIF|.png|.PNG|.JPG|.jpg|.JPEG|.jpeg",replacement="",file_name))),
            by='SampleID')%>%
  rename('label' = 'label.x') %>%
  select(filename,agency,label,PreProcess, effnetv2l,NASNetLarge,ConvNeXt, combined_test_predictions_round)%>%
  reshape2::melt(id.vars=c('filename','agency','label','PreProcess'))%>%
  mutate(AgeEst_rnd = round(value),
         Correct = ifelse(AgeEst_rnd == label,1,0))%>%
  group_by(label,agency,PreProcess,variable) %>%
  mutate(variable = recode(variable,
                           'combined_test_predictions_round' = 'ensemble'))%>%
  summarize(nCorrect = sum(Correct,na.rm = T))%>%
  left_join(.,nScales_Agency,by=c('agency','label'))%>%
  mutate(PropCorrect = nCorrect/nScales)

# wong color palette
wong_colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#994F00", "#0072B2", "#D55E00", "#D35FB7")

#Graph of All models, facet by preprocessing method
f3a<-ModelRes %>%
  filter(label!=7)%>%
  select(filename,label,PreProcess, effnetv2l,NASNetLarge,ConvNeXt, combined_test_predictions_round)%>%
  reshape2::melt(id.vars=c('filename','label','PreProcess'))%>%
  mutate(AgeEst_rnd = round(value),
         Correct = ifelse(AgeEst_rnd == label,1,0))%>%
  group_by(label,PreProcess,variable) %>%
  mutate(variable = recode(variable,
                           'combined_test_predictions_round' = 'ensemble'))%>%
  summarize(nCorrect = sum(Correct,na.rm = T))%>%
  left_join(.,nScales_Model)%>%
  mutate(PropCorrect = nCorrect/nScales,
         PreProcess = case_match(PreProcess, "Sam" ~ "SAM",
                                 "Yolo" ~ "YOLO")
  )%>%
  ggplot(.,aes(x=label,y=PropCorrect,  fill=variable, color=variable, shape=variable))+
  geom_point(stat="identity", 
             position = position_dodge(width = 0.9),
             size=4#,
             #color="black"#,
             #shape=21
  )+
  geom_text(
    aes(
      #y = 0.2,  # fixed mid-height placement
      #label = scales::percent(PropCorrect, accuracy = 0.1),
      label = round(PropCorrect*100,1),
      group = variable
    ),
    position = position_dodge(width = 0.9),
    angle=90,
    vjust = 0.5,   
    hjust = 1.8,
    size = 5,
    color = "black"  # good contrast inside colored bars
  ) +
  theme_bw()+
  ylab("Agreement")+
  # xlab("Age")+
  geom_vline(xintercept = seq(1.5, 5.5 , by = 1), 
             color = "black", linetype = "dashed") +
  facet_wrap(.~PreProcess,ncol=2)+
  scale_fill_manual(values = wong_colors)+
  scale_color_manual(values = wong_colors)+
  scale_y_continuous(labels=scales::percent, limits=c(0,1))+
  scale_x_continuous(labels=seq(1,7,by=1),
                     breaks = seq(1,7,by=1),
                     limits = c(0.5,6.5),
                     expand = c(0,0))+
  guides(fill=guide_legend(title="Model"),
         color = guide_legend(title="Model"),
         shape = guide_legend(title="Model"))+
  theme(#legend.position = c(0.06,0.25),
    axis.title.x=element_blank(),
    axis.text = element_text(size=20),
    axis.title = element_text(size=20),
    legend.text = element_text(size=15),
    legend.title = element_text(size=15) )


f3b<-AgencyRes %>%
  filter(!is.na(agency))%>%
  filter(label %in% c(1:6))%>%
  filter(variable=='ensemble') %>%
  filter(nScales >=10)%>%
  mutate(PreProcess=case_match(PreProcess, 
                               "Sam"~"SAM",
                               "Yolo" ~ "YOLO"),
         agency=case_match(agency, 
                           "AFSC" ~ "NOAA",
                           "NSRAA" ~ "NSRAA         ", # hack for legend spacing
                           .default = agency)) %>%
  ggplot(.,aes(x=label,y=PropCorrect, fill=agency, color=agency, shape=agency))+
  geom_point(stat="identity",
             position = position_dodge(width = 0.9),
             size=4#,
             #color="black",
             #fill = wong_colors,
             #shape=21
  )+
  geom_text(
    aes(label = round(PropCorrect*100,1),
        group = agency
    ),
    position = position_dodge(width = 0.9),
    angle=90,
    vjust = 0.5,   
    hjust = 1.8,
    size = 5,
    color = "black"  
  ) +
  theme_bw()+
  ylab("Agreement")+
  xlab("Age Label")+
  facet_wrap(~PreProcess, nrow=1)+
  # add black lines between x axis integers
  geom_vline(xintercept = seq(1.5, 5.5 , by = 1), 
             color = "black", linetype = "dashed") +
  scale_fill_manual(values = wong_colors[5:8])+
  scale_color_manual(values = wong_colors[5:8])+
  scale_shape_manual(values = c(5,6,7,8))+
  scale_y_continuous(labels=scales::percent, limits=c(0,1))+
  scale_x_continuous(labels=seq(1,7,by=1),
                     breaks = seq(1,7,by=1),
                     limits = c(0.5,6.5),
                     expand = c(0,0))+
  guides(fill=guide_legend(title="Agency"),
         color = guide_legend(title="Agency"),
         shape = guide_legend(title="Agency"))+
  theme(#legend.position = c(0.06,0.25),
    axis.text = element_text(size=20),
    axis.title = element_text(size=20),
    legend.text = element_text(size=15),
    legend.title = element_text(size=15))


f3 <-cowplot::plot_grid(f3a, f3b,
                        labels = "AUTO", ncol = 1
)

f3

ggsave("Output/Figures/fig3.pdf", width = 12, height = 7.5, dpi = 300)
