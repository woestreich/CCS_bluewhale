## plot SSHA 


library(ggplot2)
library(dplyr)

SSH %>% filter(time >= '2015-03-16' & time <= '2018-12-15') %>% 
  ggplot(aes(x = time, y = SSHA, fill = as.factor(Region))) + 
  geom_bar(stat = "identity", position = position_dodge2(padding = 0, reverse = TRUE)) + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  scale_fill_manual(name = "Region", labels = c("Cordell Bank", "Monterey Bay"), values = c("#ffa360", "#3e7bad")) + 
  xlab("Year") + 
  ylab("Sea Surface Height Anomaly (cm)") + 
  ggtitle("Quarterly Seasonal Sea Surface Height Anomalies") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.title = element_text(face = "bold"))  # center/bold the title
  
