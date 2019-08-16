# Overview ####
# Clinical would like some provider level report cards to use during provider meetings
# data would be needed quarterly for the time being
# Grain on some information could be monthly
# data source is HEDIS metrics that we also use on 7-30-30 dash. Would eventually use SQL to pull in code (and after that R
#to pull from SQL directly) but for now using a quick dump from backend of dashboard for testing

#libraries####
library(tidyverse)
library(tidylog)
library(zoo)
library(skimr)
library(scales)
library(zoo)

#reading and cleaning in data####
#All this done in one step because data is already exceptionally clean from SQL code.
Hedis <- readxl::read_xlsx("RawData/IPRO export 6.3.18.xlsx") %>% 
  mutate(AgeCat = as.factor(AgeCat),
         Measure_Name = as.factor(Measure_Name),
         Network = as.factor(ifelse(grepl("OON", Provider_Master_Label_DC), "OON", "In-Network")),
         MinDenom = ifelse(Denom >= 20, "Yes", "No"),
         BeginDate = as.Date(`Begin Date`, "%m/%d/%Y"),
         EndDate = as.Date(`End Date`, "%m/%d/%Y"), 
         RefreshDate = as.Date(Date, "%m/%d/%Y")) %>% 
  filter(Frequency == "Monthly") %>% 
  select(-Frequency, -'refresh date', -'Conditional Index', -Provider_Master_BK_DC, -'Begin Date', -'End Date') %>% 
  rename( Provider_Label = Provider_Master_Label_DC)


#Creating goal dataset
Measures <- Hedis %>% 
  group_by(Measure_Name) %>% 
  summarize(n()) %>%  
  arrange(Measure_Name) %>% 
  select(Measure_Name)
Goals2019 <- c(.4795, .3195, .7500, .6300, .1300)
GoalOwner <- c("State", "State", "PeopleStat", "PeopleStat", "PeopleStat")
  
Goals <- cbind(Measures, Goals2019, GoalOwner)

Goals %>% 
  select(Measure_Name, Goals2019) %>% 
  rename("2019 Goals" = Goals2019) %>% 
  kable(booktabs = T) %>% 
  kable_styling(latex_options = c("striped"))

#checking data
skim(Hedis)
head(Hedis)
View(Hedis)

#Test parameter
Provider <- "DEVEREUX CHILDREN'S BEHAVIORAL HEALTH CENTER(88089)"
Age <- "Child"

unique(Hedis$Measure_Name)
#HEDIS 30 figure####
Hedis %>% 
  filter(Measure_Name %in% c("HEDIS-30 Day Follow-Up") & AgeCat == Age) %>% 
  mutate(RCproviderflag = ifelse(Provider_Label == Provider, Provider_Label, "All Other Providers")) %>% 
  group_by(RCproviderflag, BeginDate) %>% 
  summarize(Denom = sum(Denom),
            Num = sum(Num),
            FUP = Num/Denom) %>%
  ggplot(aes(x = as.yearmon(BeginDate), y = FUP, color = RCproviderflag)) +
  geom_line(linetype = 2) +
  geom_point() +
  geom_smooth(alpha = .2) +
  geom_hline(data = Goals, yintercept = Goals$Goals2019[Goals$Measure_Name =="HEDIS-30 Day Follow-Up"], 
              size = 1.5, linetype = 1, alpha = .4) +
 # geom_text(inherit.aes = F, vjust = -1,
  #          aes(x = 2018.5, y = Goals$Goals2019[Goals$Measure_Name =="HEDIS-30 Day Follow-Up"],
   #             label = paste0("2019 Goal set by ", Goals$GoalOwner[Goals$Measure_Name == "HEDIS-30 Day Follow-Up"]))) +
  labs(title = "HEDIS: 30 Day Follow-Up for All Providers",
       x = "Discharge Month",
       y = "Percent FUP",
       color = "Providers",
       caption = paste0("Total Number of Eligible Discharges: \n", 
                        Provider, ": ", 
                        sum(Hedis$Denom[Hedis$Provider_Label == Provider & Hedis$Measure_Name == "HEDIS-30 Day Follow-Up"]), 
                        "; All Other Providers: ", 
                        sum(Hedis$Denom[Hedis$Provider_Label != Provider & Hedis$Measure_Name == "HEDIS-30 Day Follow-Up"]))) +
  scale_x_yearmon() +
  scale_y_continuous(label = percent, limits = c(0, 1), 
                     breaks = sort(c(seq(0, 1, length.out=6), Goals$Goals2019[Goals$Measure_Name == "HEDIS-30 Day Follow-Up"])))+
  theme(legend.position = "bottom")
  

#Hedis 7 fig####
Hedis %>% 
  filter(Measure_Name %in% c("HEDIS-7 Day Follow-Up") & AgeCat == Age) %>% 
  mutate(RCproviderflag = ifelse(Provider_Label == Provider, Provider_Label, "All Other Providers")) %>% 
  group_by(RCproviderflag, BeginDate) %>% 
  summarize(Denom = sum(Denom),
            Num = sum(Num),
            FUP = Num/Denom) %>% 
  ggplot(aes(x = as.yearmon(BeginDate), y = FUP, color = RCproviderflag)) +
  geom_line(linetype = 2) +
  geom_point() +
  geom_smooth(alpha = .2) +
  geom_hline(data = Goals, yintercept = Goals$Goals2019[Goals$Measure_Name =="HEDIS-7 Day Follow-Up"], 
              size = 1.5, linetype = 1, alpha = .4) +
  #geom_text(inherit.aes = F, vjust = -1,
    #        aes(x = 2018.5, y = Goals$Goals2019[Goals$Measure_Name =="HEDIS-7 Day Follow-Up"],
   #             label = paste0("2019 Goal set by ", Goals$GoalOwner[Goals$Measure_Name == "HEDIS-7 Day Follow-Up"]))) +
  labs(title = "HEDIS: 7 Day Follow-Up for All Providers",
       x = "Discharge Month",
       y = "Percent FUP",
       color = "Providers",
       caption = paste0("Total Number of Eligible Discharges: \n", 
                        Provider, ": ", 
                        sum(Hedis$Denom[Hedis$Provider_Label == Provider & Hedis$Measure_Name == "HEDIS-7 Day Follow-Up"]), 
                        "; All Other Providers: ", 
                        sum(Hedis$Denom[Hedis$Provider_Label != Provider & Hedis$Measure_Name == "HEDIS-7 Day Follow-Up"]))) +
  scale_x_yearmon() +
  scale_y_continuous(label = percent, limits = c(0, 1), 
                     breaks = sort(c(seq(0, 1, length.out=6), Goals$Goals2019[Goals$Measure_Name == "HEDIS-7 Day Follow-Up"])))+
  theme(legend.position = "bottom")


#PA 30 figure####
Hedis %>% 
  filter(Measure_Name %in% c("PA-30 Day Follow-Up") & AgeCat == Age) %>% 
  mutate(RCproviderflag = ifelse(Provider_Label == Provider, Provider_Label, "All Other Providers")) %>% 
  group_by(RCproviderflag, BeginDate) %>% 
  summarize(Denom = sum(Denom),
            Num = sum(Num),
            FUP = Num/Denom) %>% 
  ggplot(aes(x = as.yearmon(BeginDate), y = FUP, color = RCproviderflag)) +
  geom_line(linetype = 2) +
  geom_point() +
  geom_smooth(alpha = .2) +
  geom_hline(data = Goals, yintercept = Goals$Goals2019[Goals$Measure_Name =="PA-30 Day Follow-Up"], 
              size = 1.5, linetype = 1, alpha = .4) +
#  geom_text(inherit.aes = F, vjust = -1,
 #           aes(x = 2018.5, y = Goals$Goals2019[Goals$Measure_Name =="PA-30 Day Follow-Up"],
  #              label = paste0("2019 Goal set by ", Goals$GoalOwner[Goals$Measure_Name == "PA-30 Day Follow-Up"]))) +
  labs(title = "PA-Specific: 30 Day Follow-Up for All Providers",
       x = "Discharge Month",
       y = "Percent FUP",
       color = "Providers",
       caption = paste0("Total Number of Eligible Discharges: \n", 
                        Provider, ": ", 
                        sum(Hedis$Denom[Hedis$Provider_Label == Provider& Hedis$Measure_Name == "PA-30 Day Follow-Up"]), 
                        "; All Other Providers: ", 
                        sum(Hedis$Denom[Hedis$Provider_Label != Provider & Hedis$Measure_Name == "PA-30 Day Follow-Up"]))) +
  scale_x_yearmon() +
  scale_y_continuous(label = percent, limits = c(0, 1), 
                     breaks = sort(c(seq(0, 1, length.out=6), Goals$Goals2019[Goals$Measure_Name == "PA-30 Day Follow-Up"])))+
  theme(legend.position = "bottom")


#PA 7 fig####
Hedis %>% 
  filter(Measure_Name %in% c("PA-7 Day Follow-Up") & AgeCat == Age) %>% 
  mutate(RCproviderflag = ifelse(Provider_Label == Provider, Provider_Label, "All Other Providers")) %>% 
  group_by(RCproviderflag, BeginDate) %>% 
  summarize(Denom = sum(Denom),
            Num = sum(Num),
            FUP = Num/Denom) %>% 
  ggplot(aes(x = as.yearmon(BeginDate), y = FUP, color = RCproviderflag)) +
  geom_line(linetype = 2) +
  geom_point() +
  geom_smooth(alpha = .2) +
  geom_hline(data = Goals, yintercept = Goals$Goals2019[Goals$Measure_Name =="PA-7 Day Follow-Up"], 
              size = 1.5, linetype = 1, alpha = .4) +
  #geom_text(inherit.aes = F, vjust = -1,
   #         aes(x = 2018.5, y = Goals$Goals2019[Goals$Measure_Name =="PA-7 Day Follow-Up"],
    #            label = paste0("2019 Goal set by ", Goals$GoalOwner[Goals$Measure_Name == "PA-7 Day Follow-Up"]))) +
  labs(title = "PA-Specific: 7 Day Follow-Up for All Providers",
       x = "Discharge Month",
       y = "Percent FUP",
       color = "Providers",
       caption = paste0("Total Number of Eligible Discharges: \n", 
                        Provider, ": ", 
                        sum(Hedis$Denom[Hedis$Provider_Label == Provider & Hedis$Measure_Name == "PA-7 Day Follow-Up"]), 
                        "; All Other Providers: ", 
                        sum(Hedis$Denom[Hedis$Provider_Label != Provider & Hedis$Measure_Name == "PA-7 Day Follow-Up"]))) +
  scale_x_yearmon() +
  scale_y_continuous(label = percent, limits = c(0, 1), 
                     breaks = sort(c(seq(0, 1, length.out=6), Goals$Goals2019[Goals$Measure_Name == "PA-7 Day Follow-Up"])))+
  theme(legend.position = "bottom")


#Rea fig####
Hedis %>% 
  filter(Measure_Name %in% c("Readmission within 30 Days") & AgeCat == Age) %>% 
  mutate(RCproviderflag = ifelse(Provider_Label == Provider, Provider_Label, "All Other Providers")) %>% 
  group_by(RCproviderflag, BeginDate) %>% 
  summarize(Denom = sum(Denom),
            Num = sum(Num),
            FUP = Num/Denom) %>% 
  ggplot(aes(x = as.yearmon(BeginDate), y = FUP, color = RCproviderflag)) +
  geom_line(linetype = 2) +
  geom_point() +
  geom_smooth(alpha = .2) +
  geom_hline(data = Goals, yintercept = Goals$Goals2019[Goals$Measure_Name =="Readmission within 30 Days"], 
              size = 1.5, linetype = 1, alpha = .4) +
 # geom_text(inherit.aes = F, vjust = -1,
  #          aes(x = 2018.5, y = Goals$Goals2019[Goals$Measure_Name =="Readmission within 30 Days"],
   #             label = paste0("2019 Goal set by ", Goals$GoalOwner[Goals$Measure_Name == "Readmission within 30 Days"]))) +
  labs(title = "Readmission Within 30 Days for All Providers",
       x = "Discharge Month",
       y = "Percent Readmission",
       color = "Providers",
       caption = paste0("Total Number of Eligible Discharges: \n", 
                        Provider, ": ", 
                        sum(Hedis$Denom[Hedis$Provider_Label == Provider & Hedis$Measure_Name == "Readmission within 30 Days"]), 
                        "; All Other Providers: ", 
                        sum(Hedis$Denom[Hedis$Provider_Label != Provider & Hedis$Measure_Name == "Readmission within 30 Days"]))) +
  scale_x_yearmon() +
  scale_y_continuous(label = percent, limits = c(0,.3),
                     breaks = sort(c(seq(0, .3, length.out=4), Goals$Goals2019[Goals$Measure_Name == "Readmission within 30 Days"])))+
  theme(legend.position = "bottom")


#Overall Rank Readmit####
ReadmitRank <- Hedis %>% 
  filter(Measure_Name %in% c("Readmission within 30 Days") &
          AgeCat == Age &
         as.yearmon(EndDate) >= as.yearmon(max(EndDate))-5/12) %>%  #filters to last 6 months of data in report. 
  mutate(RCproviderflag = ifelse(Provider_Label == Provider, Provider_Label, "All Other Providers")) %>% 
  group_by(RCproviderflag, Provider_Label) %>% 
  summarize(Denom = sum(Denom),
            Num = sum(Num),
            FUP = Num/Denom) %>% 
  filter(Denom >= 20) %>% #filtering out super small providers
  ungroup() %>% 
  mutate(rank = rank(FUP)) %>% 
  ggplot(aes(x= reorder(Provider_Label, FUP), y = FUP, fill = RCproviderflag)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = rank)) +
  geom_hline(data = Goals, yintercept = Goals$Goals2019[Goals$Measure_Name =="Readmission within 30 Days"], 
              size = 1.5, linetype = 1, alpha = .3) +
  labs(title = "Overall Performance on Readmission Within 30 Days",
       subtitle = paste0("Based on last 6 months of available data (", 
                         as.yearmon(max(Hedis$EndDate))-5/12, " - ", as.yearmon(max(Hedis$EndDate)), ")"),
       caption = "Limited to providers with >= 20 discharges",
       x = "Providers",
       y = "Percent Reamission",
       fill = "Providers") +
  scale_fill_manual(values = c("grey", "gold")) +
  theme(axis.text.x = element_blank()) +
  theme(legend.position = "bottom")
         
ReadmitRank + scale_y_continuous(label = percent,
                   breaks = sort(c(ggplot_build(ReadmitRank)$layout$panel_params[[1]]$y.major_source, 
                                   Goals$Goals2019[Goals$Measure_Name == "Readmission within 30 Days"])))


#Overall Rank PA7####
PA7Rank <- Hedis %>% 
  filter(Measure_Name %in% c("PA-7 Day Follow-Up") &
           AgeCat == Age &
           as.yearmon(EndDate) >= as.yearmon(max(EndDate))-5/12) %>%  #filters to last 6 months of data in report. 
  mutate(RCproviderflag = ifelse(Provider_Label == Provider, Provider_Label, "All Other Providers")) %>% 
  group_by(RCproviderflag, Provider_Label) %>% 
  summarize(Denom = sum(Denom),
            Num = sum(Num),
            FUP = Num/Denom) %>% 
  filter(Denom >= 20) %>% #filtering out super small providers
  ungroup() %>% 
  mutate(rank = rank(-FUP)) %>% 
  ggplot(aes(x= reorder(Provider_Label, -FUP), y = FUP, fill = RCproviderflag)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = rank)) +
  geom_hline(data = Goals, yintercept = Goals$Goals2019[Goals$Measure_Name =="PA-7 Day Follow-Up"], 
              size = 1.5, linetype = 1, alpha = .3) +
  labs(title = "Overall Performance on PA-7 Day Follow-Up",
       subtitle = paste0("Based on last 6 months of available data (", 
                         as.yearmon(max(Hedis$EndDate))-5/12, " - ", as.yearmon(max(Hedis$EndDate)), ")"),
       caption = "Limited to providers with >= 20 discharges",
       x = "Providers",
       y = "Percent FUP",
       fill = "Providers") +
  scale_fill_manual(values = c("grey", "gold")) +
  theme(axis.text.x = element_blank()) +
  theme(legend.position = "bottom")

PA7Rank + scale_y_continuous(label = percent,
                                 breaks = sort(c(ggplot_build(PA7Rank)$layout$panel_params[[1]]$y.major_source, 
                                                 Goals$Goals2019[Goals$Measure_Name == "PA-7 Day Follow-Up"])))


#Overall Rank PA30####
PA30Rank <- Hedis %>% 
  filter(Measure_Name %in% c("PA-30 Day Follow-Up") &
           AgeCat == Age &
           as.yearmon(EndDate) >= as.yearmon(max(EndDate))-5/12) %>%  #filters to last 6 months of data in report. 
  mutate(RCproviderflag = ifelse(Provider_Label == Provider, Provider_Label, "All Other Providers")) %>% 
  group_by(RCproviderflag, Provider_Label) %>% 
  summarize(Denom = sum(Denom),
            Num = sum(Num),
            FUP = Num/Denom) %>% 
  filter(Denom >= 20) %>% #filtering out super small providers
  ungroup() %>% 
  mutate(rank = rank(-FUP)) %>% 
  ggplot(aes(x= reorder(Provider_Label, -FUP), y = FUP, fill = RCproviderflag)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = rank)) +
  geom_hline(data = Goals, yintercept = Goals$Goals2019[Goals$Measure_Name =="PA-30 Day Follow-Up"], 
            size = 1.5, linetype = 1, alpha = .3) +
  labs(title = "Overall Performance on PA-30 Day Follow-Up",
       subtitle = paste0("Based on last 6 months of available data (", 
                         as.yearmon(max(Hedis$EndDate))-5/12, " - ", as.yearmon(max(Hedis$EndDate)), ")"),
       caption = "Limited to providers with >= 20 discharges",
       x = "Providers",
       y = "Percent FUP",
       fill = "Providers") +
  scale_fill_manual(values = c("grey", "gold")) +
  theme(axis.text.x = element_blank()) +
  theme(legend.position = "bottom")

PA30Rank + scale_y_continuous(label = percent,
                             breaks = sort(c(ggplot_build(PA30Rank)$layout$panel_params[[1]]$y.major_source, 
                                             Goals$Goals2019[Goals$Measure_Name == "PA-30 Day Follow-Up"])))


#Overall Rank HEDIS30####
HEDIS30Rank <- Hedis %>% 
  filter(Measure_Name %in% c("HEDIS-30 Day Follow-Up") &
           AgeCat == Age &
           as.yearmon(EndDate) >= as.yearmon(max(EndDate))-5/12) %>%  #filters to last 6 months of data in report. 
  mutate(RCproviderflag = ifelse(Provider_Label == Provider, Provider_Label, "All Other Providers")) %>% 
  group_by(RCproviderflag, Provider_Label) %>% 
  summarize(Denom = sum(Denom),
            Num = sum(Num),
            FUP = Num/Denom) %>% 
  filter(Denom >= 20) %>% #filtering out super small providers
  ungroup() %>% 
  mutate(rank = rank(-FUP)) %>% 
  ggplot(aes(x= reorder(Provider_Label, -FUP), y = FUP, fill = RCproviderflag)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = rank)) +
  geom_hline(data = Goals, yintercept = Goals$Goals2019[Goals$Measure_Name =="HEDIS-30 Day Follow-Up"], 
             size = 1.5, linetype = 1, alpha = .3) +
  labs(title = "Overall Performance on HEDIS-30 Day Follow-Up",
       subtitle = paste0("Based on last 6 months of available data (", 
                         as.yearmon(max(Hedis$EndDate))-5/12, " - ", as.yearmon(max(Hedis$EndDate)), ")"),
       caption = "Limited to providers with >= 20 discharges",
       x = "Providers",
       y = "Percent FUP",
       fill = "Providers") +
  scale_fill_manual(values = c("grey", "gold")) +
  theme(axis.text.x = element_blank()) +
  theme(legend.position = "bottom")

HEDIS30Rank + scale_y_continuous(label = percent,
                              breaks = sort(c(ggplot_build(HEDIS30Rank)$layout$panel_params[[1]]$y.major_source, 
                                              Goals$Goals2019[Goals$Measure_Name == "HEDIS-30 Day Follow-Up"])))

#Overall Rank HEDIS7####
HEDIS7Rank <- Hedis %>% 
  filter(Measure_Name %in% c("HEDIS-7 Day Follow-Up") &
           AgeCat == Age &
           as.yearmon(EndDate) >= as.yearmon(max(EndDate))-5/12) %>%  #filters to last 6 months of data in report. 
  mutate(RCproviderflag = ifelse(Provider_Label == Provider, Provider_Label, "All Other Providers")) %>% 
  group_by(RCproviderflag, Provider_Label) %>% 
  summarize(Denom = sum(Denom),
            Num = sum(Num),
            FUP = Num/Denom) %>% 
  filter(Denom >= 20) %>% #filtering out super small providers
  ungroup() %>% 
  mutate(rank = rank(-FUP)) %>% 
  ggplot(aes(x= reorder(Provider_Label, -FUP), y = FUP, fill = RCproviderflag)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = rank)) +
  geom_hline(data = Goals, yintercept = Goals$Goals2019[Goals$Measure_Name =="HEDIS-7 Day Follow-Up"], 
             size = 1.5, linetype = 1, alpha = .3) +
  labs(title = "Overall Performance on HEDIS-7 Day Follow-Up",
       subtitle = paste0("Based on last 6 months of available data (", 
                         as.yearmon(max(Hedis$EndDate))-5/12, " - ", as.yearmon(max(Hedis$EndDate)), ")"),
       caption = "Limited to providers with >= 20 discharges",
       x = "Providers",
       y = "Percent FUP",
       fill = "Providers") +
  scale_fill_manual(values = c("grey", "gold")) +
  theme(axis.text.x = element_blank()) +
  theme(legend.position = "bottom")

HEDIS7Rank + scale_y_continuous(label = percent,
                                 breaks = sort(c(ggplot_build(HEDIS7Rank)$layout$panel_params[[1]]$y.major_source, 
                                                 Goals$Goals2019[Goals$Measure_Name == "HEDIS-7 Day Follow-Up"])))

tinytex::tlmgr_update(all = TRUE)

-------------------------
  #Code Checks based on final reports####
  
#checking out to see what's going on with July 2018 data in devereaux

Provider <- "DEVEREUX CHILDREN'S BEHAVIORAL HEALTH CENTER(88089)"
Age <- "Child"

Hedis %>% 
  filter(AgeCat == Age & Provider_Label == Provider) %>% View()

#PA7 Rank
Hedis %>% 
  filter(Measure_Name %in% c("PA-7 Day Follow-Up") &
           AgeCat == Age &
           as.yearmon(EndDate) >= as.yearmon(max(EndDate))-5/12) %>%  #filters to last 6 months of data in report. 
  mutate(RCproviderflag = ifelse(Provider_Label == Provider, Provider_Label, "All Other Providers")) %>% 
  group_by(RCproviderflag, Provider_Label) %>% 
  summarize(Denom = sum(Denom),
            Num = sum(Num),
            FUP = Num/Denom) %>% 
  filter(Denom >= 20) %>% #filtering out super small providers
  ungroup() %>% 
  mutate(rank = rank(-FUP))

#PA7 day FUH
 Hedis %>% 
  filter(Measure_Name %in% c("PA-7 Day Follow-Up") & AgeCat == Age & Provider_Label == Provider)%>% View()
  mutate(RCproviderflag = ifelse(Provider_Label == Provider, Provider_Label, "All Other Providers")) %>% 
  group_by(RCproviderflag, BeginDate) %>% 
  summarize(Denom = sum(Denom),
            Num = sum(Num),
            FUP = Num/Denom) %>% View()
  
  Hedis %>% 
    filter(Provider_Label == Provider) %>% 
    mutate(Grain = EndDate - BeginDate) %>% View()
  
  
#All Metric Rank, focus on kidspeace - making sure code is correct and kidspeace is in 30 day metrics but not 7 day
  Provider <- "KidsPeace Hospital (99436)"
  Age <- "Child"
  
   Hedis %>% 
    filter(Measure_Name != "Readmission within 30 Days" & AgeCat == Age &
             as.yearmon(EndDate) >= as.yearmon(max(EndDate))-5/12) %>%  #filters to last 6 months of data in report. 
    mutate(RCproviderflag = ifelse(Provider_Label == Provider, Provider_Label, "All Other Providers")) %>% 
    group_by(RCproviderflag, Provider_Label, Measure_Name) %>% 
    summarize(Denom = sum(Denom),
              Num = sum(Num),
              FUP = Num/Denom) %>% 
    arrange(Measure_Name, Provider_Label) %>% View()

  Hedis %>% 
    filter(Measure_Name != "Readmission within 30 Days" & AgeCat == Age &
    as.yearmon(EndDate) >= as.yearmon(max(EndDate))-5/12 & Provider_Label == Provider) %>% View() #looks like it's correct, no qualifying DC's in 7/2018 for 7 days, but there for 30 days
  