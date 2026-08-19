# Learning rates
library(tidyverse)
ConvNeXt_Sam <- read.csv("./data/ConvNeXtLargesam_history.csv") %>%
  mutate(PreProcess = "Sam",
         Model = "ConvNeXt",
         epoch = seq(1,nrow(.),by=1)) %>%
  select(-c(lr,val_loss,loss)) %>%
  reshape2::melt(.,id.vars=c("Model","PreProcess","epoch"))

ConvNeXt_Yolo <- read.csv("./data/ConvNeXtLargeyolo_history.csv")%>%
  mutate(PreProcess = "Yolo",
         Model = "ConvNeXt",
         epoch = seq(1,nrow(.),by=1)) %>%
  select(-c(lr,val_loss,loss)) %>%
  reshape2::melt(.,id.vars=c("Model","PreProcess","epoch"))

effnetv2_Sam <- read.csv("./data/effnetv2Largesam_history.csv")%>%
  mutate(PreProcess = "Sam",
         Model = "effnetv2",
         epoch = seq(1,nrow(.),by=1)) %>%
  select(-c(lr,val_loss,loss)) %>%
  reshape2::melt(.,id.vars=c("Model","PreProcess","epoch"))

effnetv2_Yolo <- read.csv("./data/effnetv2Largeyolo_history.csv")%>%
  mutate(PreProcess = "Yolo",
         Model = "effnetv2",
         epoch = seq(1,nrow(.),by=1)) %>%
  select(-c(lr,val_loss,loss)) %>%
  reshape2::melt(.,id.vars=c("Model","PreProcess","epoch"))

NASNet_Sam <- read.csv("./data/NASNetLargesam_history.csv")%>%
  mutate(PreProcess = "Sam",
         Model = "NASNet",
         epoch = seq(1,nrow(.),by=1)) %>%
  select(-c(lr,val_loss,loss)) %>%
  reshape2::melt(.,id.vars=c("Model","PreProcess","epoch"))

NASNet_Yolo <- read.csv("./data/NASNetLargeyolo_history.csv")%>%
  mutate(PreProcess = "Yolo",
         Model = "NASNet",
         epoch = seq(1,nrow(.),by=1)) %>%
  select(-c(lr,val_loss,loss)) %>%
  reshape2::melt(.,id.vars=c("Model","PreProcess","epoch"))

TrainVal <- bind_rows(ConvNeXt_Sam, ConvNeXt_Yolo,
                      effnetv2_Sam, effnetv2_Yolo,
                      NASNet_Sam, NASNet_Yolo)


# wong color palette
wong_colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#994F00", "#0072B2", "#D55E00", "#D35FB7")



TrainVal %>%
  mutate(variable = recode(variable,
                           mse = 'Training',
                           val_mse = 'Validation'),
         PreProcess=case_match(PreProcess, "Sam" ~ "SAM",
                               "Yolo" ~ "YOLO"))%>%
  ggplot(.,aes(x=epoch,y=value,color=variable))+
  geom_line(linewidth=1)+
  facet_grid(PreProcess~Model,scales="free")+
  ylab("MSE")+
  xlab("Epoch")+
  scale_color_manual(values = wong_colors)+
  coord_cartesian(ylim=c(0,0.3))+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = c(0.9,0.9))

ggsave("./Output/Figures/model_learning_rates.png", width = 8, height = 5, dpi = 300)