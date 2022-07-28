pacman::p_load(tidyverse, ggpubr, readxl,ggdark,plotrix,ggpubr)

"ICC_data_df" <-read_excel("ICC_data.xlsx")

sixtymin_df <- ICC_data_df %>% 
  filter(trial==1) %>% 
  filter(treatment == "veh60" | treatment == "mbcd60")
  
#Summary Stats
summary_df <-sixtymin_df %>% 
  group_by(treatment) %>% 
  summarise(
    count = n(),
    mean = mean(relative_ratio),
    sd = sd(relative_ratio)
  )


#Plot it out
sixtymin_df %>% 
  ggplot(mapping = aes(treatment,relative_ratio))+
  geom_violin(fill = '#FDC314')+
  dark_mode(theme_classic())+
  labs(x = "Time (min)", #Adds the labels
       y = "Surface/Internal mGluR2")


#Is it normal

with(sixtymin_df,shapiro.test(relative_ratio[treatment == "mbcd60"]))

with(sixtymin_df,shapiro.test(relative_ratio[treatment == "veh60"]))


res.ftest <- var.test(relative_ratio~treatment, data = sixtymin_df)
res.ftest

t.test(relative_ratio~treatment, data = sixtymin_df,
       var.equal = TRUE)

wilcox.test(relative_ratio~treatment, data = sixtymin_df,
                   exact = FALSE)

