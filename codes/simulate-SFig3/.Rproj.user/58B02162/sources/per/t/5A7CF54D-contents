library(data.table)
library(ggplot2)
library(dplyr)
df<- fread("plot_SFig3A.csv") %>% as.data.frame() %>% select(-V1)

ggplot(df, aes(x=clb5_K_Theta,y=cdh1_K_Theta, z = Mean_Time)) + 
  geom_contour()

faithfuld