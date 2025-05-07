library(tidyverse)
library(gridExtra)
library(data.table)
library(lme4)
library(lmerTest)
library(patchwork)

alld <- fread("your_datasets.csv")

################################################################################
#Figure 1B: plot correlation
corr_d <- alld %>% 
  dplyr::select(id,tp,pop_state12,brood,total) %>% 
  filter(tp==0)

corr_brooding <- corr_d %>% ggplot(aes(x=pop_state12,y=brood))+
  geom_point(size=5)+
  geom_smooth(col="black",method = "lm")+
  theme_classic()+
  theme(text = element_text(size=48))

corr_total <-corr_d %>% ggplot(aes(x=pop_state12,y=total))+
  geom_point(size=5)+
  geom_smooth(col="black",method = "lm")+
  theme_classic()+
  theme(text = element_text(size=48))

ggsave(".././figures/corr_brooding.png", plot = corr_brooding, width = 12, height = 10, dpi = 300)
ggsave(".././figures/corr_total.png", plot = corr_total, width = 12, height = 10, dpi = 300)

################################################################################
#Figure 2 plot ANOVA 
anovad <- alld %>% 
  dplyr::select(id,tp,arm,pop_state12,switch,tp12to2,tp12to5,tp1to12)

val <- "pop_state12"

allda <- anovad %>% dplyr::select(tp,arm,val)
allda <- allda %>%
  filter(!rowSums(is.na(.)))

colnames(allda) <- c("tp","arm","val")
allda$tp <- allda$tp %>% as.factor()
mu_da <- allda %>% 
  group_by(tp,arm) %>% 
  summarize(mean = mean(val),se=sd(val)/sqrt(n()))

bg_data <- data.frame(arm = "HC")

aovp <- allda %>% 
  group_by(tp,arm) %>% 
  ggplot() +
  geom_rect(data = bg_data, aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = NA,col="black",linewidth=1)+
  geom_bar(data = mu_da,aes(x=tp,y=mean,fill=arm),alpha=0.8,stat="identity") + 
  geom_jitter(width = 0.3, height = 0, aes(x=tp,colour=arm,y=val),
              col="gray25", alpha=0.8) + 
  scale_color_manual(values = c("#548235","royalblue","#FFBF00"))+ 
  scale_fill_manual(values = c("#548235","royalblue","#FFBF00"))+
  geom_errorbar(data = mu_da,aes(x=tp,ymin = mean - se, ymax = mean + se),
                width = .25,size=1)+
  geom_point(data = mu_da,aes(x=tp,y= mean),size=2.5)+
  geom_line(data = mu_da,aes(x=tp,y=mean,group=arm),
            linewidth=1)+
  theme(text = element_text(size = 12))+
  theme_classic()+
  theme(text = element_text(size=24))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  facet_wrap(~arm, nrow = 1)+
  ggtitle(val)

ggsave(".././figures/aovp.png", plot =aovp, width = 12, height = 10, dpi = 300)



