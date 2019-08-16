library(rmarkdown)
library(tidyverse)

RawData <- "RawData/IPROdata_7-12-2019.csv"
  
#Adult Reports####
AdultProviders <- read_csv(RawData) %>% 
  filter(AgeCat == "Adult") %>% 
  rename( Provider_Label = Provider_Master_Label_DC) %>% 
  mutate(Network = as.factor(ifelse(grepl("OON", Provider_Label), "OON", "In-Network")),
         BeginDate = as.Date(RptDateBeg, "%m/%d/%Y"),
         EndDate = as.Date(RptDateEnd, "%m/%d/%Y")) %>% 
  filter(Network != "OON" & Frequency == "Monthly") %>% 
  group_by(Provider_Label) %>% 
  summarize(TotalDenom = sum(Denom)) %>% 
  filter(TotalDenom >= 20)

slices <- unique(AdultProviders$Provider_Label)

for(v in slices){
  render("ProviderReportCards_AIP_Adult.rmd",
         output_file = paste0("AdultResults/Adult_ProviderReportCard_", 
                              str_replace_all(str_to_title(str_remove_all(v, "[[:punct:]]"))," ","-") ,
                              "_", 
                              Sys.Date(), 
                              ".pdf"),
         params = list(Provider = v))
}


#Child Reports####
ChildProviders <- read_csv(RawData) %>% 
  filter(AgeCat == "Child") %>% 
  rename( Provider_Label = Provider_Master_Label_DC) %>% 
  mutate(Network = as.factor(ifelse(grepl("OON", Provider_Label), "OON", "In-Network")),
         BeginDate = as.Date(RptDateBeg, "%m/%d/%Y"),
         EndDate = as.Date(RptDateEnd, "%m/%d/%Y")) %>% 
  filter(Network != "OON" & Frequency == "Monthly") %>% 
  group_by(Provider_Label) %>% 
  summarize(TotalDenom = sum(Denom)) %>% 
  filter(TotalDenom >= 20)

slices2 <- unique(ChildProviders$Provider_Label)

#this works, prints to project directory
for(v in slices2){
  render("ProviderReportCards_AIP_Child.rmd",
         output_file = paste0("ChildResults/Child_ProviderReportCard_", 
                              str_replace_all(str_to_title(str_remove_all(v, "[[:punct:]]"))," ","-") ,
                              "_", 
                              Sys.Date(), 
                              ".pdf"),
         params = list(Provider = v), 
         clean = TRUE)
}

  
#readme file
#render("README_ProviderReportCards.rmd",
 #      output_file = "H:/Child Acute Services/Qtrly-AIP-Provider-Report-Cards/README_ProviderReportCards.pdf",
  #     clean = TRUE)



