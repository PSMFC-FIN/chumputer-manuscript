# Table 1
library(tidyverse)
ModelDat <- read.csv("./data/model_train_val_test_data.csv")

ModelDat %>% 
  group_by(agency,label)%>%
  summarize(nScales = n())%>%
  reshape2::dcast(agency ~ label, value.var="nScales")

OtherDat <- read.csv("./data/other_data_df.csv") %>%
  filter(agency!="WDFW")

TotalDat <- ModelDat %>%
  bind_rows(.,OtherDat)

TotalDat %>%
  filter(is.na(label))

TotRow <- tibble(agency='Total',
                 TotalDat %>% 
                   group_by(label)%>%
                   summarize(nScales = n()))%>%
  reshape2::dcast(agency ~ label, value.var="nScales")

#Total Collection
TotColl <- TotalDat %>% 
  group_by(agency,label)%>%
  summarize(nScales = n())%>%
  reshape2::dcast(agency ~ label, value.var="nScales") %>%
  bind_rows(TotRow)%>%
  mutate(Collection = 'Total')%>%
  relocate(Collection,.before='agency')

#Model Collection
TotRow <- tibble(agency='Total',
                 ModelDat %>% 
                   group_by(label)%>%
                   summarize(nScales = n()))%>%
  reshape2::dcast(agency ~ label, value.var="nScales")

ModColl <- ModelDat %>% 
  group_by(agency,label)%>%
  summarize(nScales = n())%>%
  reshape2::dcast(agency ~ label, value.var="nScales")%>%
  bind_rows(TotRow)%>%
  mutate(Collection = 'Model')%>%
  relocate(Collection,.before='agency')

# table
FinalTab <- TotColl %>%
  bind_rows(ModColl)

FinalTab[c(2:5,7:10),1]<-NA

options(knitr.kable.NA = '')

FinalTab %>% 
  select(!"NA")%>%
  mutate(Total = rowSums(across(where(is.numeric)),na.rm=T)) %>%
  kableExtra::kable(booktabs=T, linesep = "",format.args = list(big.mark = ","))%>%
  kableExtra::add_header_above(c(" " = 2,"Labelled Age" = 7," "=1)) %>%
  kableExtra::kable_styling("striped", full_width = FALSE, html_font="Times New Roman",    position = "center",
                            font_size = 10) %>%
  kableExtra::save_kable("./Output/Tables/table_1.png", density = 300,
                         #  margin  = 0.3,                  # <-- adds space around the table (inches-ish)
                         vwidth  = 900,                  # <-- image width  (px)
                         vheight = 420,                  # <-- image height (px) ~ controls shape
                         zoom    = 2                     # crisper rendering if needed
  )

FinalTab %>% 
  select(!"NA")%>%
  mutate(Total = rowSums(across(where(is.numeric)),na.rm=T)) %>%
  write.csv("./Output/Tables/table_1_data.csv", row.names=FALSE)

