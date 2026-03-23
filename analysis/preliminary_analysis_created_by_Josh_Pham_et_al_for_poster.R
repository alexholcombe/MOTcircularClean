library(tidyverse)
library(gt)
library(plotrix)
library(extrafont)
library(ggpubr)
library(rstatix)
library(readxl)

whole <- read_excel("fuckthisman.xlsx")

less <- whole %>% select(PID, Age, AgeGroup, Gender, Education, Medical, VidGame, PRI, Avg_24, Avg_28, Avg_34, Avg_38, 
                         Pilot, TF_24, TF_28, TF_34, TF_38)

moreless <- less %>% 
  subset(Pilot!="Yes") %>% 
  subset(TF_24!="NA") %>% 
  select(-Pilot) %>% 
  select(-Medical)

#checking temp freq and anova 
knew <- moreless %>% 
  mutate(TF_4 = (TF_24 + TF_34)/2) %>% 
  mutate(TF_8 = (TF_28 + TF_38)/2)

long <- moreless %>% 
  pivot_longer(cols = starts_with("TF"), 
               names_to = "grp",
               values_to = "valoos") %>% 
  separate(grp, c("yuck", "num"), sep = "_") %>% 
  separate(num, c("targets", "objects"), sep = 1)

long$AgeGroup <- factor(long$AgeGroup, levels = c("Young", "Old"))

##summary stats 

summ <- long %>% group_by(AgeGroup, targets, objects) 

sum1 <- summ %>% get_summary_stats(valoos, type = "mean_sd")

print(sum1)

###repeated measures mixed effects anova for TF

letsee <- long %>% anova_test(dv = valoos, wid = PID, between = AgeGroup, within = c(targets, objects))
get_anova_table(letsee)

### repeatred measures mixed effects anova for Speed

sped <- moreless %>% 
  select(PID, Age, AgeGroup, Gender, PRI, starts_with("Avg")) %>% 
  pivot_longer(
    cols = starts_with("Avg"),
    names_to = "bleh",
    values_to = "valyou") %>% 
  separate(bleh, c("nup", "noom"), sep = "_") %>% 
  separate(noom, c("targets", "objects"), sep = 1)

spedthresh <- sped %>% anova_test(dv = valyou, wid = PID, between = AgeGroup, within = c(targets, objects))

get_anova_table(spedthresh)

###examining interaction effects

twoway <- long %>%
  group_by(objects) %>%
  anova_test(dv = valoos, wid = PID, between = AgeGroup, within = targets)

get_anova_table(twoway)

###simple main effect of age

ageeffect <- long %>%
  group_by(targets, objects) %>%
  anova_test(dv = valoos, wid = PID, between = AgeGroup) %>%
  get_anova_table()

ageeffect

##main effect of targets
targeffect <- long %>%
  group_by(AgeGroup, objects) %>%
  anova_test(dv = valoos, wid = PID, within = targets) %>%
  get_anova_table()

targeffect

###main effect of objects
objeffect <- long %>%
  group_by(AgeGroup, targets) %>%
  anova_test(dv = valoos, wid = PID, within = objects) %>%
  get_anova_table()

objeffect

###pairwise compartsons

pwcage <- long %>%
  group_by(targets, objects) %>%
  pairwise_t_test(
    valoos ~ AgeGroup, paired = FALSE, 
    p.adjust.method = "bonferroni"
  ) 
pwcage

pwcobj <- long %>%
  group_by(targets, AgeGroup) %>%
  pairwise_t_test(
    valoos ~ objects, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) 
pwcobj



# data visualisation
ohman <- ggplot(long, aes(x = AgeGroup, y = valoos, fill = targets)) +
  facet_wrap(objects ~ Gender, ncol = 4) +
  geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Temporal Frequency (dots/s)") +
  scale_fill_discrete(name = "Targets") 

print(ohman)


  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = means-se, ymax = means+se), width = 0.5, position = "dodge") +
  scale_x_discrete(labels = NULL) +
  theme_minimal() +
  scale_fill_discrete(name = "Level of Education") +
  theme(strip.text.x = element_text(family="Arial Narrow", size = 14, face = "bold"),
        strip.text.y = element_text(family="Arial Narrow", size = 13, face = "bold"),
        axis.title.y = element_text(family="Arial Narrow", size = 14, face = "bold")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 3.5)) +
  xlab("") +
  ylab("Mean Perceived Risk")
  
  
#trying to clean up data to get speed thresholds 
newlong <- long %>% 
  pivot_longer(
    cols = starts_with("Avg"),
    names_to = "placeholder",
    values_to = "volo"
    ) %>% 
  separate(placeholder, c("shit", "butt"), sep = "_") %>% 
  separate(butt, c("tgt", "obj"), sep = 1)

newman <- ggplot(newlong, aes(x = AgeGroup, y = volo, fill = tgt)) +
  facet_wrap(obj ~ Gender, ncol = 4) +
  geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Rotational Speed (rev/s)") +
  scale_fill_discrete(name = "Targets") 

print(newman)

# fullgraph

yay <- ggarrange(newman, ohman, ncol = 1, nrow = 2)
print(yay)
