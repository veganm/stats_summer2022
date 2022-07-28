pacman::p_load(tidyverse, ggpubr, readxl,ggdark,plotrix,ggpubr)

# Read in the data file
"ICC_data_df" <-read_excel("ICC_data_Week8.xlsx")

# Take a peek
glimpse(ICC_data_df)

# What are we doing here?
sixtymin_df <- ICC_data_df %>% 
  filter(trial==1) %>% 
  filter(treatment == "veh60" | treatment == "mbcd60")
glimpse(sixtymin_df)
  
#Summary Stats
summary_df <-sixtymin_df %>% 
  group_by(treatment) %>% 
  summarise(
    count = n(),
    mean = mean(relative_ratio),
    sd = sd(relative_ratio)
  )
summary_df

#Plot it out
sixtymin_df %>% 
  ggplot(mapping = aes(treatment,relative_ratio))+
  geom_violin(fill = '#FDC314')+
  dark_mode(theme_classic())+
  theme(text=element_text(size=18, color="white"),
        axis.text = element_text(size=18, color="white"))+
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

