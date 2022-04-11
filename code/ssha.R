## plot SSHA 

library(ggplot2)

ggplot(data = S, aes(x = time, y = SSHA, fill = as.factor(Region))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(name = "Region", labels = c("Cordell Bank", "Monterey Bay"), values = c("#ffa360", "#3e7bad")) + 
  xlab("Year") + 
  ylab("Sea Surface Height Anomaly (cm)") + 
  ggtitle("Quarterly Seasonal Sea Surface Height Anomalies (2015-2018)") + 
  theme_classic() 
