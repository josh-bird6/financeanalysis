library(tidyverse)

basedata <- read_csv("Income_success.csv")

#################
#TIDYVERSE METHOD

basedata <- read_csv("Income_success.csv") %>%
  mutate(sfc_funding = round(SFC_funding *100, 1),
         ftfe_attainment = round(FTFE_attainment*100,1),
         Group=ifelse(college_name == "SCOTLAND AVERAGE", "Scotland Average", "Colleges"))


ggplot(data = basedata, aes(x = sfc_funding, y = ftfe_attainment)) +
  geom_point(aes(
    colour = ifelse(
      college_name == "SCOTLAND AVERAGE",
      "Scotland Average",
      "Colleges"))) +
  # geom_smooth(method = lm) +
  facet_wrap( ~ academic_year) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(
    x = "SFC grant as proportion of total income (%)",
    y = "Success rate of FTFE students (%)",
    title = "Sexy title*",
    caption = "*Omits unincorporated college"
  )
#########################################

library(ggpubr)
 
          
ggscatter(
  basedata,
  x = "sfc_funding",
  y = "ftfe_attainment",
  add = "reg.line",
  title = "Relationship between SFC funding and full-time further education student outcomes, 2014-15 to 2017-18*",
  xlab = "SFC grant as proportion of total income (%)",
  ylab = "Success rate of FTFE students (%)",
  caption = "*Omits unincorporated college",
  color = "Group",
  shape = "Group",
  add.params = list(color = "blue"),
  ggtheme = theme_bw()
  # label = "college_name",
  # repel = TRUE,
  # label.select = list(criteria = "`x` >85 | `y` <60 | `y`>73" )) +
) + 
  facet_wrap( ~ academic_year) +
  # stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  stat_cor()

ggpar(plot,
      xlim = c(0,100),
      ylim = c(0,100))           
           
