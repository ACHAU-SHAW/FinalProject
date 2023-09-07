rm(list=ls())
library(ggplot2)
library(patchwork)
library(openxlsx)
library(lme4)
library(growthcurver)
## Pesticide Selection
## Bifidobacterium Responses to Candidate Pesticide Exposure
# Use 'growthcurver' to calculate the Carrying capacity and Growth Rate
pesdata <- read.xlsx("rawdata.xlsx", sheet = "plate3")
# pesdata <- read.xlsx("rawdata.xlsx", sheet = "plate1")
# pesdata <- read.xlsx("rawdata.xlsx", sheet = "plate2")
# pesdata <- read.xlsx("rawdata.xlsx", sheet = "plate4")
# pesdata <- read.xlsx("rawdata.xlsx", sheet = "plate5")
# pesdata <- read.xlsx("rawdata.xlsx", sheet = "plate6")
gc_out3 <- SummarizeGrowthByPlate(pesdata)
data_file <- "dataana.xlsx"
write.xlsx(gc_out, data_file, rowNames = FALSE)
gc_out3 <- SummarizeGrowthByPlate(pesdata, plot_fit = TRUE, plot_file = "gc_plots3.pdf")
# Generate figures
data1 <- read.xlsx("rawdata.xlsx", sheet = "Spirotetramat")
g1=ggplot(data = data.frame(Group = data1$c, Values = data1$k), aes(x = Group, y = Values)) +
  geom_point(size = 2, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(x = "Spirotetramat Concentration (mg/L)", y = "Maximum Carrying Capacity (OD630)", title = "Spirotetramat Impact on Bifidobacterium") +
  theme_classic()
data2 <- read.xlsx("rawdata.xlsx", sheet = "Cyhalothrin")
g2=ggplot(data = data.frame(Group = data2$c, Values = data2$k), aes(x = Group, y = Values)) +
  geom_point(size = 2, color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "green") +  
  labs(x = "Cyhalothrin Concentration (mg/L)", y = "Maximum Carrying Capacity (OD630)", title = "Cyhalothrin Impact on Bifidobacterium") +
  theme_classic()
data3 <- read.xlsx("rawdata.xlsx", sheet = "Fipronil")
g3=ggplot(data = data.frame(Group = data3$c, Values = data3$k), aes(x = Group, y = Values)) +
  geom_point(size = 2, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(x = "Fipronil Concentration (ug/L)", y = "Maximum Carrying Capacity (OD630)", title = "Fipronil Impact on Bifidobacterium") +
  theme_classic()
data4 <- read.xlsx("rawdata.xlsx", sheet = "Glyphosate")
g4=ggplot(data = data.frame(Group = data4$c, Values = data4$k), aes(x = Group, y = Values)) +
  geom_point(size = 2, color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +  
  labs(x = "Glyphosate Concentration (mg/L)", y = "Maximum Carrying Capacity (OD630)", title = "Glyphosate Impact on Bifidobacterium") +
  theme_classic()
combined_plots_1 <- g1 + g2 + g3 + g4
combined_plots_1
# Spirotetramat analysis
m1_1= lmer(c~k+(1 | group), data=data1)
summary(m1_1)
m1_2= lmer(c~(1 | group), data=data1)
lr_test1 <- anova(m1_1, m1_2)
p_value_1 <- 1 - pchisq(lr_test1$Chisq, df = lr_test1$Df)
# Cyhalothrin analysis
m2_1= lmer(c~k+(1 | group), data=data2)
summary(m2_1)
m2_2= lmer(c~(1 | group), data=data2)
lr_test2 <- anova(m2_1, m2_2)
p_value_2 <- 1 - pchisq(lr_test2$Chisq, df = lr_test2$Df)
data2_1=subset(data2, c<5)
data2_1$c[(data2_1$c!=1)]=2
anova_2<-aov(k~c, data = data2_1)
summary(anova_2)
# Fipronil analysis
m3_1= lmer(c~k+(1 | group), data=data3)
summary(m3_1)
m3_2= lmer(c~(1 | group), data=data3)
lr_test3 <- anova(m3_1, m3_2)
p_value_3 <- 1 - pchisq(lr_test3$Chisq, df = lr_test3$Df)
data3_1=subset(data3, c>3 | c==1)
data3_1$c[(data3_1$c!=1)]=2
anova_3<-aov(k~c, data = data3_1)
summary(anova_3)
# Glyphosate analysis
m4_1= lmer(c~k+(1 | group), data=data4)
summary(m4_1)
m4_2= lmer(c~(1 | group), data=data4)
lr_test4 <- anova(m4_1, m4_2)
p_value_4 <- 1 - pchisq(lr_test4$Chisq, df = lr_test4$Df)


## Hypothesis 2:Adaptive laboratory evolution experiments enhance the resistance of bee gut bacteria to pesticides.
## Fipronil Resistance of Bifidobacterium Evolved on MEGA-Plate
# Use 'growthcurver' to calculate the Carrying capacity and Growth Rate
wildtype1=read.xlsx("rawdata.xlsx", sheet = "wild1")
evolved1=read.xlsx("rawdata.xlsx", sheet = "evo1")
gc_out1 <- SummarizeGrowthByPlate(wildtype1)
data_file <- "dataana.xlsx"
write.xlsx(gc_out1, data_file, rowNames = FALSE)
gc_out1 <- SummarizeGrowthByPlate(wildtype1, plot_fit = TRUE, plot_file = "gc_plots1.pdf")
gc_out2 <- SummarizeGrowthByPlate(evolved1)
write.xlsx(gc_out2, data_file, rowNames = FALSE)
gc_out2 <- SummarizeGrowthByPlate(evolved1, plot_fit = TRUE, plot_file = "gc_plots2.pdf")
data6= read.xlsx("rawdata.xlsx", sheet = "Mc1")
# Create the scatter plot with different colors for treatments
g6=ggplot(data6, aes(x = as.factor(concen), y = value, color = strain)) +
  geom_point(size = 2) +
  geom_smooth(aes(group = strain), method = "loess", se = T, size = 1, span = 1.8) +
  labs(x = "Fipronil Concentration (mg/L)", y = "Carrying capacity (OD630)", size=14) +
  scale_color_manual(values = c("ok" = "blue", "ek" = "red")) +
  theme_classic()+ theme(axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))
data6= read.xlsx("rawdata.xlsx", sheet = "Gr1")
# Create the scatter plot with different colors for treatments
g6=ggplot(data6, aes(x = as.factor(concen), y = value, color = strain)) +
  geom_point(size = 2) +
  geom_smooth(aes(group = strain), method = "loess", se = T, size = 1, span = 1.8) +
  labs(x = "Fipronil Concentration (mg/L)", y = "Growth Rate (OD630)") +
  scale_color_manual(values = c("or" = "blue", "er" = "red")) +
  theme_classic()
combined_plots_2 <- g5 + g6
combined_plots_2
anova_5<-aov(value~strain, data = data5)
summary(anova_5)
data7=subset(data5, concen<1)
anova_7<-aov(value~strain, data = data7)
summary(anova_7)
data8=subset(data5, concen>1)
anova_8<-aov(value~strain, data = data8)
summary(anova_8)
anova_6<-aov(value~strain, data = data6)
summary(anova_6)
## Fipronil Resistance of Bifidobacterium Evolved in Liquid Media
# Panel 1: Bifidobacterium Responses to 0mg/L Fipronil
ana_data1 <- read.xlsx("rawdata.xlsx", sheet = "1")
strain_order1 <- c('ko','k1','k2','k3','k4','k5')
ana_data1$strain <- factor(ana_data1$strain, levels = strain_order1)
custom_colors <- c('red', 'green', 'blue', 'purple', 'orange', 'pink')
g7 <- ggplot(data = ana_data1, aes(x = factor(strain), y = k, fill = strain)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = ana_data1[!(ana_data1$k > (quantile(ana_data1$k, 0.75) + 1.5 * IQR(ana_data1$k)) |
                                   ana_data1$k < (quantile(ana_data1$k, 0.25) - 1.5 * IQR(ana_data1$k))), ],
              alpha = 0.6, width = 0.1, color = "blue") +
  geom_point(data = ana_data1[ana_data1$k > (quantile(ana_data1$k, 0.75) + 1.5 * IQR(ana_data1$k)) |
                                ana_data1$k < (quantile(ana_data1$k, 0.25) - 1.5 * IQR(ana_data1$k)), ],
             aes(x = as.factor(strain), y = k), color = "red", size = 3, shape = 18, alpha = 0.8) +
  labs(x = "Strains in Liquid Media", y = "Carrying capacity (OD630)",
       title = "Bifidobacterium Responses to 0mg/L Fipronil") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  scale_fill_manual(values = custom_colors)
# Panel 2: Bifidobacterium Responses to 0.2mg/L Fipronil
ana_data2 <- read.xlsx("rawdata.xlsx", sheet = "2")
strain_order2 <- c('ko','k1','k2','k3','k4','k5')
ana_data2$strain <- factor(ana_data2$strain, levels = strain_order2)
#boxplot(k ~ concen, data = ana_data1, xlab = "Concentration (mg/L)", ylab = "Carrying capacity (OD630)", main = "Spirotetramat-Carrying capacity")
#boxplot(r ~ concen, data = ana_data1, xlab = "Concentration (mg/L)", ylab = "Growth rate (OD630)", main = "Spirotetramat-Growth rate")
g8 <- ggplot(data = ana_data2, aes(x = factor(strain), y = k, fill = strain)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = ana_data2[!(ana_data2$k > (quantile(ana_data2$k, 0.75) + 1.5 * IQR(ana_data2$k)) |
                                   ana_data2$k < (quantile(ana_data2$k, 0.25) - 1.5 * IQR(ana_data2$k))), ],
              alpha = 0.6, width = 0.1, color = "blue") +
  geom_point(data = ana_data2[ana_data2$k > (quantile(ana_data2$k, 0.75) + 1.5 * IQR(ana_data2$k)) |
                                ana_data2$k < (quantile(ana_data2$k, 0.25) - 1.5 * IQR(ana_data2$k)), ],
             aes(x = as.factor(strain), y = k), color = "red", size = 3, shape = 18, alpha = 0.8) +
  labs(x = "Strains in Liquid Media", y = "Carrying capacity (OD630)",
       title = "Bifidobacterium Responses to 0.2mg/L Fipronil") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  scale_fill_manual(values = custom_colors)
# Panel 3: Bifidobacterium Responses to 1mg/L Fipronil
ana_data3 <- read.xlsx("rawdata.xlsx", sheet = "3")
strain_order3 <- c('ko','k1','k2','k3','k4','k5')
ana_data3$strain <- factor(ana_data3$strain, levels = strain_order3)
#boxplot(k ~ concen, data = ana_data1, xlab = "Concentration (mg/L)", ylab = "Carrying capacity (OD630)", main = "Spirotetramat-Carrying capacity")
#boxplot(r ~ concen, data = ana_data1, xlab = "Concentration (mg/L)", ylab = "Growth rate (OD630)", main = "Spirotetramat-Growth rate")
g9 <- ggplot(data = ana_data3, aes(x = factor(strain), y = k, fill = strain)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = ana_data3[!(ana_data3$k > (quantile(ana_data3$k, 0.75) + 1.5 * IQR(ana_data3$k)) |
                                   ana_data3$k < (quantile(ana_data3$k, 0.25) - 1.5 * IQR(ana_data3$k))), ],
              alpha = 0.6, width = 0.1, color = "blue") +
  geom_point(data = ana_data3[ana_data3$k > (quantile(ana_data3$k, 0.75) + 1.5 * IQR(ana_data3$k)) |
                                ana_data3$k < (quantile(ana_data3$k, 0.25) - 1.5 * IQR(ana_data3$k)), ],
             aes(x = as.factor(strain), y = k), color = "red", size = 3, shape = 18, alpha = 0.8) +
  labs(x = "Strains in Liquid Media", y = "Carrying capacity (OD630)",
       title = "Bifidobacterium Responses to 1mg/L Fipronil") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  scale_fill_manual(values = custom_colors)
# Panel 4: Bifidobacterium Responses to 5mg/L Fipronil
ana_data4 <- read.xlsx("rawdata.xlsx", sheet = "4")
strain_order4 <- c('ko','k1','k2','k3','k4','k5')
ana_data4$strain <- factor(ana_data4$strain, levels = strain_order4)
#boxplot(k ~ concen, data = ana_data1, xlab = "Concentration (mg/L)", ylab = "Carrying capacity (OD630)", main = "Spirotetramat-Carrying capacity")
#boxplot(r ~ concen, data = ana_data1, xlab = "Concentration (mg/L)", ylab = "Growth rate (OD630)", main = "Spirotetramat-Growth rate")
g10 <- ggplot(data = ana_data4, aes(x = factor(strain), y = k, fill = strain)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = ana_data4[!(ana_data4$k > (quantile(ana_data4$k, 0.75) + 1.5 * IQR(ana_data4$k)) |
                                   ana_data4$k < (quantile(ana_data4$k, 0.25) - 1.5 * IQR(ana_data4$k))), ],
              alpha = 0.6, width = 0.1, color = "blue") +
  geom_point(data = ana_data4[ana_data4$k > (quantile(ana_data4$k, 0.75) + 1.5 * IQR(ana_data4$k)) |
                                ana_data4$k < (quantile(ana_data4$k, 0.25) - 1.5 * IQR(ana_data4$k)), ],
             aes(x = as.factor(strain), y = k), color = "red", size = 3, shape = 18, alpha = 0.8) +
  labs(x = "Strains in Liquid Media", y = "Carrying capacity (OD630)",
       title = "Bifidobacterium Responses to 5mg/L Fipronil") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  scale_fill_manual(values = custom_colors)
# Panel 5: Bifidobacterium Responses to 25mg/L Fipronil
ana_data5 <- read.xlsx("rawdata.xlsx", sheet = "5")
strain_order5 <- c('ko','k1','k2','k3','k4','k5')
ana_data5$strain <- factor(ana_data5$strain, levels = strain_order5)
#boxplot(k ~ concen, data = ana_data1, xlab = "Concentration (mg/L)", ylab = "Carrying capacity (OD630)", main = "Spirotetramat-Carrying capacity")
#boxplot(r ~ concen, data = ana_data1, xlab = "Concentration (mg/L)", ylab = "Growth rate (OD630)", main = "Spirotetramat-Growth rate")
g11 <- ggplot(data = ana_data5, aes(x = factor(strain), y = k, fill = strain)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = ana_data5[!(ana_data5$k > (quantile(ana_data5$k, 0.75) + 1.5 * IQR(ana_data5$k)) |
                                   ana_data5$k < (quantile(ana_data5$k, 0.25) - 1.5 * IQR(ana_data5$k))), ],
              alpha = 0.6, width = 0.1, color = "blue") +
  geom_point(data = ana_data5[ana_data5$k > (quantile(ana_data5$k, 0.75) + 1.5 * IQR(ana_data5$k)) |
                                ana_data5$k < (quantile(ana_data5$k, 0.25) - 1.5 * IQR(ana_data5$k)), ],
             aes(x = as.factor(strain), y = k), color = "red", size = 3, shape = 18, alpha = 0.8) +
  labs(x = "Strains in Liquid Media", y = "Carrying capacity (OD630)",
       title = "Bifidobacterium Responses to 25mg/L Fipronil") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  scale_fill_manual(values = custom_colors)
# Panel 6: Bifidobacterium Responses to 125mg/L Fipronil
ana_data6 <- read.xlsx("rawdata.xlsx", sheet = "6")
strain_order6 <- c('ko','k1','k2','k3','k4','k5')
ana_data6$strain <- factor(ana_data6$strain, levels = strain_order6)
#boxplot(k ~ concen, data = ana_data1, xlab = "Concentration (mg/L)", ylab = "Carrying capacity (OD630)", main = "Spirotetramat-Carrying capacity")
#boxplot(r ~ concen, data = ana_data1, xlab = "Concentration (mg/L)", ylab = "Growth rate (OD630)", main = "Spirotetramat-Growth rate")
g12 <- ggplot(data = ana_data6, aes(x = factor(strain), y = k, fill = strain)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = ana_data6[!(ana_data6$k > (quantile(ana_data6$k, 0.75) + 1.5 * IQR(ana_data6$k)) |
                                   ana_data6$k < (quantile(ana_data6$k, 0.25) - 1.5 * IQR(ana_data6$k))), ],
              alpha = 0.6, width = 0.1, color = "blue") +
  geom_point(data = ana_data6[ana_data6$k > (quantile(ana_data6$k, 0.75) + 1.5 * IQR(ana_data6$k)) |
                                ana_data6$k < (quantile(ana_data6$k, 0.25) - 1.5 * IQR(ana_data6$k)), ],
             aes(x = as.factor(strain), y = k), color = "red", size = 3, shape = 18, alpha = 0.8) +
  labs(x = "Strains in Liquid Media", y = "Carrying capacity (OD630)",
       title = "Bifidobacterium Responses to 125mg/L Fipronil") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  scale_fill_manual(values = custom_colors)
combined_plots_3 <- (g7 + g8) / (g9 + g10) / (g11 + g12)

## Hypothesis 3: Adding evolved gut bacteria to the bees' diet can strengthen the bees' resistance to pesticides as probiotics.
## Impact of Microbial Supplementation on Bumblebee Fipronil Resistance
data9 <- read.xlsx("rawdata.xlsx", sheet = "sp")
data9_c=subset(data9,data9$trt=='c')
i=8
tem=subset(data9_c,data9_c$time==i)
mean=mean(tem$sr)
CI=t.test(tem$sr, conf.level = 0.95)$conf.int
mean
CI
data9_o=subset(data9,data9$trt=='o')
i=8
tem=subset(data9_o,data9_o$time==i)
mean=mean(tem$sr)
CI=t.test(tem$sr, conf.level = 0.95)$conf.int
mean
CI
data9_e1=subset(data9,data9$trt=='e1')
i=8
tem=subset(data9_e1,data9_e1$time==i)
mean=mean(tem$sr)
CI=t.test(tem$sr, conf.level = 0.95)$conf.int
mean
CI
data9_e2=subset(data9,data9$trt=='e2')
i=8
tem=subset(data9_e2,data9_e2$time==i)
mean=mean(tem$sr)
CI=t.test(tem$sr, conf.level = 0.95)$conf.int
mean
CI
datax=read.xlsx("rawdata.xlsx", sheet = "sr")
datay=read.xlsx("rawdata.xlsx", sheet = "fi")
# Create the scatter plot with different colors for treatments
g14=ggplot(datax, aes(x = time, y = mean, color = trt)) +
  #geom_point(size = 2) +
  geom_rect(aes(xmin = time , xmax = time + 1, ymin = small, ymax = big, fill =trt), alpha = 0.3) +
  geom_step(aes(group = trt), size = 1) +
  labs(x = "Time(day)", y = "Survival proportion (%)") +
  scale_color_manual(values = c("p" = "red","p-o" = "orange", "p-e1" = "yellow", "p-e2" = "green")) +
  scale_fill_manual(values = c("p" = "red","p-o" = "orange", "p-e1" = "yellow", "p-e2" = "green")) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))
g15=ggplot(datay, aes(x = time, y = mean, color = trt)) +
  #geom_point(size = 2) +
  geom_rect(aes(xmin = time , xmax = time + 1, ymin = small, ymax = big, fill =trt), alpha = 0.3) +
  geom_step(aes(group = trt), size = 1) +
  labs(x = "Time(day)", y = "Survival proportion (%)") +
  scale_color_manual(values = c("c" = "red","o" = "orange", "e1" = "yellow", "e2" = "green")) +
  scale_fill_manual(values = c("c" = "red","o" = "orange", "e1" = "yellow", "e2" = "green")) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))
combined_plots_4 <- g14+g15
combined_plots_4

