##====== Evaluation of plants based commercial products for equine cyathostomin control  ======
library(readr)
library(ggplot2)
library(rstatix)
library(patchwork)
library(tidyr)
# BiocManager::install("mixOmics")
library(mixOmics)
library(ggpubr)
library(geepack)
library(nlme)
library(dplyr)
library(reshape2)
library(cowplot)
library(lme4)
library(pwr)
library(effsize)
theme_set(theme_bw())

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)}

path <- "~/Scripts/Commercial products"
setwd(path)

dat=read.csv(file='EUPAOr.csv', header=T,sep=';', dec=',',fileEncoding="latin1")
head(dat)
#    Horse n Age Box   Group Weight Fecal_matter  FEC Volume L3_obs Nb_egg_FM L3_exp Dev  Day   facet
# 1  W742 1   4   2 Mugwort     NA           NA  585     NA     NA        NA     NA  NA d-11 Mugwort
# 2  W744 2   3   3 Mugwort     NA           NA  250     NA     NA        NA     NA  NA d-11 Mugwort
# 3  W725 3   5   2 Mugwort     NA           NA  360     NA     NA        NA     NA  NA d-11 Mugwort
# 4  W745 4   3   3 Mugwort     NA           NA  420     NA     NA        NA     NA  NA d-11 Mugwort
# 5  W754 5   2   4 Mugwort     NA           NA  550     NA     NA        NA     NA  NA d-11 Mugwort
# 6  W760 6   1   4 Mugwort     NA           NA 1080     NA     NA        NA     NA  NA d-11 Mugwort

## OUtliers
OUT=dat %>%
  group_by(Group) %>%
  identify_outliers(FEC)
OUT
#   Group   Horse     n   Age   Box Weight Fecal_matter   FEC Volume L3_obs Nb_egg_FM L3_exp   Dev Day   facet     id    is.outlier is.extreme
# 1 Control W757      7     1    12    193         75.9  2800 212520   41.2    213000 119480  56.2 d10   Mugwort   45    TRUE       FALSE     
# 2 Control W757      7     1    12    194         75.5  2600 196300   40.3    196000  66092  33.7 d15   Echinacea 65    TRUE       FALSE     
# 3 Control W757      7     1    12    198         75.2  3550 266960   27      266960  55080  20.6 d30   Curcumin  91    TRUE       TRUE      
# 4 Mugwort W760      6     1     4    189         70    2400 168000   34.5    168000  67390  40.1 d10   Mugwort   44    TRUE       FALSE  

#Remove the horse containing an extreme outlier (W757)
dat=dat[dat$Horse!="W757",]

# Pre treatment #########
dat_pre=dat[dat$Day=="d-11",]

##Age####
Age_summary=data_summary(dat_pre, varname="Age",groupnames='Group')
Age_summary
#       Group      Age       sd
# 1   Control 3.500000 1.760682
# 2  Curcumin 2.833333 1.471960
# 3 Echinacea 3.500000 2.738613
# 4   Mugwort 3.000000 1.414214

#Normality
shapiro_test(residuals(lm(Age ~ Group, data = dat_pre)))
#   variable                                   statistic p.value
# 1 residuals(lm(Age ~ Group, data = dat_pre))     0.950   0.276

dat_pre %>%
  group_by(Group) %>%
  shapiro_test(Age)
#    Group     variable statistic     p
# 1  Control   Age          0.832 0.111
# 2 Curcumin  Age          0.958 0.804
# 3 Echinacea Age          0.897 0.357
# 4 Mugwort   Age          0.982 0.960

#Equality of variance
dat_pre %>% levene_test(Age ~ Group)
# df1   df2 statistic     p
#   3    21     1.47 0.252

#ANOVA
res.aov <- dat_pre %>% anova_test(Age ~ Group)
res.aov
# ANOVA Table (type II tests)
#   Effect DFn DFd     F     p p<.05   ges
# 1  Group   3  21 0.192 0.901       0.028

mod_Age_pre = lme(Age ~ Group  ,
                  random =~ 1|Horse,
                  data = dat_pre)
anova(mod_Age_pre)
#         numDF denDF  F-value p-value
# (Intercept)     1    20 66.91874  <.0001
# Group           3    20  0.19187  0.9007

summary(mod_Age_pre)
# Fixed effects:  Age ~ Group 
#                     Value Std.Error DF   t-value p-value
# (Intercept)     3.500000 0.7843964 20  4.462029  0.0002
# GroupCurcumin  -0.666667 1.1093041 20 -0.600977  0.5546
# GroupEchinacea  0.000000 1.1093041 20  0.000000  1.0000
# GroupMugwort   -0.500000 1.1093041 20 -0.450733  0.6570

##FEC####
# Mean 
pre_FEC_summary=data_summary(dat_pre, varname="FEC",groupnames='Group')
pre_FEC_summary
#       Group      FEC       sd
# 1   Control 600.0000 504.9752
# 2  Curcumin 553.3333 112.1457
# 3 Echinacea 507.0000 413.7995
# 4   Mugwort 540.8333 291.4175

pre_FEC_summary$error = qnorm(0.975)*pre_FEC_summary$sd/sqrt(10)
pre_FEC_summary$Low = pre_FEC_summary$FEC - pre_FEC_summary$error
pre_FEC_summary$High = pre_FEC_summary$FEC + pre_FEC_summary$error
pre_FEC_summary
#       Group      FEC       sd     error      Low     High
# 1   Control 600.0000 504.9752 312.98115 287.0189 912.9811
# 2  Curcumin 553.3333 112.1457  69.50737 483.8260 622.8407
# 3 Echinacea 507.0000 413.7995 256.47085 250.5291 763.4709
# 4   Mugwort 540.8333 291.4175 180.61913 360.2142 721.4525

mod_dat_pre = lm(FEC ~ Group,
                  data = dat_pre)
anova(mod_dat_pre)
# Response: FEC
# Df  Sum Sq Mean Sq F value Pr(>F)
# Group      3   26663    8888  0.0679 0.9764

summary(mod_dat_pre)
# Fixed effects:  FEC ~ Group 
#                   Value Std.Error DF   t-value p-value
# (Intercept)    600.0000  147.7231 20  4.061653  0.0006
# GroupCurcumin  -46.6667  208.9120 20 -0.223380  0.8255
# GroupEchinacea -93.0000  208.9120 20 -0.445163  0.6610
# GroupMugwort   -59.1667  208.9120 20 -0.283213  0.7799


# Post-treatment #########
dat_post=dat[dat$Day!="d-11",]
dat_post$id=row.names(dat_post)
dat_post$Group<-factor(dat_post$Group, levels= c("Control",'Mugwort',"Echinacea","Curcumin"))
dat_post$facet<-factor(dat_post$facet, levels= c("Control",'Mugwort',"Echinacea","Curcumin"))

##Weight####
BW_summary=data_summary(dat_post, varname="Weight",groupnames='Day')
BW_summary

#   Day   Weight       sd
# 1  d0 275.4444 53.89007
# 2 d10 279.2500 53.95137
# 3 d15 264.9167 60.88508
# 4 d30 266.7500 56.12668

## FEC_post_treatment ##########
#Mean
post_FEC_summary=data_summary(dat_post, varname="FEC",groupnames=c('Group',"Day"))
post_FEC_summary
#      Control  d0  516.6667 364.6110
# 2    Control d10  983.3333 711.1024
# 3    Control d15  766.6667 427.3952
# 4    Control d30  825.0000 598.9574
# 5    Mugwort  d0  816.6667 399.5831
# 6    Mugwort d10 1008.3333 793.3578
# 7  Echinacea  d0  983.3333 640.0521
# 8  Echinacea d15  941.6667 580.0144
# 9   Curcumin  d0  850.0000 451.6636
# 10  Curcumin d30  775.0000 361.5937

post_FEC_summary$error = qnorm(0.975)*post_FEC_summary$sd/sqrt(10)
post_FEC_summary$Low = post_FEC_summary$FEC - post_FEC_summary$error
post_FEC_summary$High = post_FEC_summary$FEC + post_FEC_summary$error
post_FEC_summary
#        Group Day           FEC       sd    error      Low      High
# 1    Control  d0      516.6667 364.6110 225.9841 290.6826  742.6507
# 2    Control d10      983.3333 711.1024 440.7378 542.5956 1424.0711
# 3    Control d15      766.6667 427.3952 264.8974 501.7692 1031.5641
# 4    Control d30      825.0000 598.9574 371.2308 453.7692 1196.2308
# 5    Mugwort  d0      816.6667 399.5831 247.6596 569.0070 1064.3263
# 6    Mugwort d10     1008.3333 793.3578 491.7192 516.6141 1500.0526
# 7  Echinacea  d0      983.3333 640.0521 396.7011 586.6322 1380.0344
# 8  Echinacea d15      941.6667 580.0144 359.4900 582.1766 1301.1567
# 9   Curcumin  d0      850.0000 451.6636 279.9389 570.0611 1129.9389
# 10  Curcumin d30      775.0000 361.5937 224.1140 550.8860  999.1140

### Statistic analysis ####
#d0
dat_post_d0=dat_post[dat_post$Day=="d0",]
dat_post_d0 = lme(FEC ~ Group,
                 random =~ 1|Horse,
                 data = dat_post_d0)
summary(dat_post_d0)

# Fixed effects:  FEC ~ Group 
#                   Value Std.Error DF  t-value p-value
# (Intercept)    516.6667  196.2143 20 2.633176  0.0159
# GroupMugwort   300.0000  277.4889 20 1.081124  0.2925
# GroupEchinacea 466.6667  277.4889 20 1.681749  0.1082
# GroupCurcumin  333.3333  277.4889 20 1.201249  0.2437

#Mugwort
dat_post_M=dat_post[dat_post$facet=='Mugwort',]

dat_post_M = lme(FEC ~ Group*Day  ,
                 random =~ 1|Horse,
                 data = dat_post_M)
anova(dat_post_M)
#             numDF denDF   F-value p-value
# (Intercept)     1    10 28.206123  0.0003
# Group           1    10  0.269480  0.6150
# Day             1    10  4.823775  0.0528
# Group:Day       1    10  0.841707  0.3805

summary(dat_post_M)
# Fixed effects:  FEC ~ Group * Day 
#                         Value Std.Error DF    t-value p-value
# (Intercept)          516.6667  245.4092 10  2.1053269  0.0615
# GroupMugwort         300.0000  347.0611 10  0.8644012  0.4076
# Dayd10               466.6667  211.9519 10  2.2017573  0.0523
# GroupMugwort:Dayd10 -275.0000  299.7453 10 -0.9174457  0.3805

#Bonferroni correction
p_values_dat_post_M <- summary(dat_post_M)$tTable[, "p-value"]
bonferroni_p_values_dat_post_M <- p.adjust(p_values_dat_post_M, method = "bonferroni")
print(bonferroni_p_values_dat_post_M)
# (Intercept)        GroupMugwort              Dayd10  GroupMugwort:Dayd10 
# 0.2460952           1.0000000           0.2091412            1.0000000

#Cohen's effect size  
coefs_EPG_post_M <- fixef(dat_post_M)

residuals_EPG_post_M <- resid(dat_post_M)
sd_residuals_EPG_post_M <- sd(residuals_EPG_post_M)

d_cohen_EPG_post_M <- coefs_EPG_post_M["GroupMugwort"] / sd_residuals_EPG_post_M
d_cohen_EPG_post_M
# 1.117818

#Echinacea
dat_post_E=dat_post[dat_post$facet=='Echinacea',]
dat_post_E = lme(FEC ~ Group*Day  ,
                 random =~ 1|Horse,
                 data = dat_post_E)
anova(dat_post_E)
#           numDF denDF  F-value p-value
# (Intercept)     1    10 50.03206  <.0001
# Group           1    10  2.00128  0.1875
# Day             1    10  0.28171  0.6072
# Group:Day       1    10  0.55215  0.4745

summary(dat_post_E)
# Fixed effects:  FEC ~ Group * Day 
#                           Value Std.Error DF    t-value p-value
# (Intercept)            516.6667  212.0747 10  2.4362480  0.0351
# GroupEchinacea         466.6667  299.9190 10  1.5559758  0.1508
# Dayd15                 250.0000  277.5513 10  0.9007344  0.3889
# GroupEchinacea:Dayd15 -291.6667  392.5168 10 -0.7430680  0.4745

#Bonferroni correction
p_values_dat_post_E <- summary(dat_post_E)$tTable[, "p-value"]
bonferroni_p_values_dat_post_E <- p.adjust(p_values_dat_post_E, method = "bonferroni")
print(bonferroni_p_values_dat_post_E)
# (Intercept)        GroupEchinacea                Dayd15 GroupEchinacea:Dayd15 
# 0.1402935             0.6030729             1.0000000             1.0000000 

#Cohen's effect size 
coefs_EPG_post_E <- fixef(dat_post_E)

residuals_EPG_post_E <- resid(dat_post_E)
sd_residuals_EPG_post_E <- sd(residuals_EPG_post_E)

d_cohen_EPG_post_E <- coefs_EPG_post_E["GroupEchinacea"] / sd_residuals_EPG_post_E
d_cohen_EPG_post_E
# 1.113238 

#Curcumin
dat_post_C=dat_post[dat_post$facet=='Curcumin',]
dat_post_C = lme(FEC ~ Group*Day  ,
                 random =~ 1|Horse,
                 data = dat_post_C)
anova(dat_post_C)
#             numDF denDF  F-value p-value
# (Intercept)     1    10 47.26838  <.0001
# Group           1    10  0.43115  0.5262
# Day             1    10  0.57193  0.4669
# Group:Day       1    10  1.54362  0.2424

summary(dat_post_C)
#                          Value Std.Error DF   t-value p-value
# (Intercept)           516.6667  187.5463 10  2.754875  0.0203
# GroupCurcumin         333.3333  265.2305 10  1.256768  0.2374
# Dayd30                308.3333  218.1679 10  1.413285  0.1879
# GroupCurcumin:Dayd30 -383.3333  308.5360 10 -1.242427  0.2424

#Bonferroni correction
p_values_dat_post_C <- summary(dat_post_C)$tTable[, "p-value"]
bonferroni_p_values_dat_post_C <- p.adjust(p_values_dat_post_C, method = "bonferroni")
print(bonferroni_p_values_dat_post_C)
# (Intercept)        GroupCurcumin               Dayd30 GroupCurcumin:Dayd30 
# 0.08123046           0.94959935           0.75174070           0.96969880 

#Cohen's effect size 
coefs_EPG_post_C <- fixef(dat_post_C)

residuals_EPG_post_C <- resid(dat_post_C)
sd_residuals_EPG_post_C <- sd(residuals_EPG_post_C)

d_cohen_EPG_post_C <- coefs_EPG_post_C["GroupCurcumin"] / sd_residuals_EPG_post_C
d_cohen_EPG_post_C
#1.088231 

###FECRT-- Bayesian hierarchical model ####
fec_ctl0 = dat_post$FEC[dat_post$Group=='Control' & dat_post$Day=='d10']
fec_m10 = dat_post$FEC[dat_post$Group=='Mugwort' & dat_post$Day=='d10']
fec_ctl15 = dat_post$FEC[dat_post$Group=='Control' & dat_post$Day=='d15']
fec_e15 = dat_post$FEC[dat_post$Group=='Echinacea' & dat_post$Day=='d15']
fec_ctl30 = dat_post$FEC[dat_post$Group=='Control' & dat_post$Day=='d30']
fec_c30 = dat_post$FEC[dat_post$Group=='Curcumin' & dat_post$Day=='d30']

mod_m <- eggCounts::fecr_stan(fec_ctl0, fec_m10, rawCounts = FALSE, preCF=50, postCF=50,
                              paired = TRUE, indEfficacy = TRUE)
mod_m$posterior.summary
#                        mean       sd     2.5%       50%     97.5% HPDLow95     mode HPDHigh95
# FECR                 0.3862   0.1528   0.1673    0.3634    0.7634   0.1412   0.2982    0.6931
# meanEPG.untreated 1110.9677 402.2765 545.2101 1038.5830 2095.2377 465.3920 950.9370 1871.6993
# meanEPG.treated    684.1610 308.7555 209.0468  644.3685 1498.8744 106.3709 607.8186 1242.4802

mod_e <- eggCounts::fecr_stan(fec_ctl15, fec_e15, rawCounts = FALSE, preCF=50, postCF=50,
                              paired = TRUE, indEfficacy = TRUE)
mod_e$posterior.summary
#                       mean       sd     2.5%      50%     97.5% HPDLow95     mode HPDHigh95
# FECR                0.2519   0.1016   0.1104   0.2332    0.4936   0.1018   0.1865    0.4662
# meanEPG.untreated 895.0527 296.9313 461.6826 844.4978 1600.2755 410.5971 819.3481 1505.3600
# meanEPG.treated   669.7372 242.7903 324.6709 632.7478 1266.2361 260.6402 563.6465 1152.2028

mod_c <- eggCounts::fecr_stan(fec_ctl30, fec_c30, rawCounts = FALSE, preCF=50, postCF=50,
                              paired = TRUE, indEfficacy = TRUE)
mod_c$posterior.summary
#                       mean       sd     2.5%      50%     97.5% HPDLow95     mode HPDHigh95
# FECR                0.3204   0.1301   0.1332   0.2974    0.6377   0.1068   0.2513    0.5825
# meanEPG.untreated 926.6042 290.4923 510.7136 874.6935 1675.2886 447.9581 831.7409 1513.2640
# meanEPG.treated   627.8883 229.1433 272.8798 595.5227 1166.7184 207.4439 576.7515 1082.2706

## Effect on larval development ##########
dat_Dev_post_summary=data_summary(dat_post, varname="Dev",groupnames=c('Day','Group'))
dat_Dev_post_summary
#    Day     Group      Dev        sd
# 1   d0   Control 25.52667 21.994506
# 2   d0   Mugwort 38.80333 13.162027
# 3   d0 Echinacea 29.31667 18.627317
# 4   d0  Curcumin 22.38833  5.171311
# 5  d10   Control 42.45000 20.182634
# 6  d10   Mugwort 48.97333 23.822403
# 7  d15   Control 53.72667 27.230364
# 8  d15 Echinacea 34.23667  8.213712
# 9  d30   Control 47.01000 37.681129
# 10 d30  Curcumin 28.95333  9.452394

### Plot ####
Dev_plot=ggplot(dat_post, aes(x=Day, y=Dev, fill=Group))+
  geom_boxplot(alpha=0.4)+
  geom_point(aes(x = Day,y= Dev, group = Group), size = 1.5, shape = 1,position = position_jitterdodge(0))+
  facet_wrap(~facet,scales = 'free_x')+
  theme(strip.background = element_blank(), strip.text.x = element_blank())+
  labs(y='Developpement \n percentage (%)', x="Days")+
  theme(axis.line = element_line(size = 1, linetype = "solid"),
        legend.title = element_blank(),legend.position="bottom",
        legend.text = element_text(size=33, family = "Lato"),
        axis.title.x = element_text(size=35, family = "Lato", margin = margin(t = 0.4, unit="cm")), 
        axis.title.y = element_text(size=35, family = "Lato", margin = margin(r = 0.4, unit="cm")),
        axis.text.x = element_text(size=25, face = "bold", family = "Lato"),
        axis.text.y = element_text(size=25, face='bold', family = "Lato"))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_fill_manual(values=c('#737373','#dd3497','#41ab5d',"#feb24c"))
Dev_plot

### Statistic analysis ####
#d0
dat_post_d0=dat_post[dat_post$Day=="d0",]
dat_post_d0 = lme(Dev ~ Group,
                  random =~ 1|Horse,
                  data = dat_post_d0)
summary(dat_post_d0)
# Fixed effects:  Dev ~ Group 
#                Value Std.Error DF t-value p-value
# (Intercept)    25.526667  6.755305 20  3.778759  0.0012
# GroupMugwort   13.276667  9.553444 20  1.389726  0.1799
# GroupEchinacea  3.790000  9.553444 20  0.396716  0.6958
# GroupCurcumin  -3.138333  9.553444 20 -0.328503  0.7459

#Mugwort
dat_post_M=dat_post[dat_post$facet=='Mugwort',]
dat_post_M = lme(Dev ~ Group*Day  ,
                 random =~ 1|Horse,
                 data = dat_post_M)
anova(dat_post_M)
#             numDF denDF  F-value p-value
# (Intercept)     1    10 61.87930  <.0001
# Group           1    10  1.00000  0.3409
# Day             1    10  4.23305  0.0667
# Group:Day       1    10  0.26301  0.6192

summary(dat_post_M)
# Fixed effects:  Dev ~ Group * Day 
#                         Value Std.Error DF    t-value p-value
# (Intercept)         25.526667  8.407199 10  3.0362866  0.0125
# GroupMugwort        13.276667 11.889575 10  1.1166645  0.2902
# Dayd10              16.923333  9.311525 10  1.8174611  0.0992
# GroupMugwort:Dayd10 -6.753333 13.168484 10 -0.5128406  0.6192

#Bonferroni correction
p_values_dat_post_M <- summary(dat_post_M)$tTable[, "p-value"]
bonferroni_p_values_dat_post_M <- p.adjust(p_values_dat_post_M, method = "bonferroni")
print(bonferroni_p_values_dat_post_M)
# (Intercept)        GroupMugwort              Dayd10  GroupMugwort:Dayd10 
# 0.05016728          1.00000000          0.39674908          1.00000000

#Cohen's effect size 
coefs_Dev_post_M <- fixef(dat_post_M)

residuals_Dev_post_M <- resid(dat_post_M)
sd_residuals_Dev_post_M <- sd(residuals_Dev_post_M)

d_cohen_Dev_post_M <- coefs_Dev_post_M["GroupMugwort"] / sd_residuals_Dev_post_M
d_cohen_Dev_post_M
# 1.039537 

#Echinacea
dat_post_E=dat_post[dat_post$facet=='Echinacea',]
dat_post_E = lme(Dev ~ Group*Day  ,
                 random =~ 1|Horse,
                 data = dat_post_E)
anova(dat_post_E)
#             numDF denDF  F-value p-value
# (Intercept)     1    10 66.21024  <.0001
# Group           1    10  0.80025  0.3920
# Day             1    10  4.21817  0.0671
# Group:Day       1    10  2.08406  0.1794

summary(dat_post_E)
# Fixed effects:  Dev ~ Group * Day 
#                           Value Std.Error DF    t-value p-value
# (Intercept)            25.52667   8.42663 10  3.0292854  0.0127
# GroupEchinacea          3.79000  11.91705 10  0.3180316  0.7570
# Dayd15                 28.20000  11.40285 10  2.4730669  0.0329
# GroupEchinacea:Dayd15 -23.28000  16.12606 10 -1.4436261  0.1794

#Bonferroni correction
p_values_dat_post_E <- summary(dat_post_E)$tTable[, "p-value"]
bonferroni_p_values_dat_post_E <- p.adjust(p_values_dat_post_E, method = "bonferroni")
print(bonferroni_p_values_dat_post_E)
# (Intercept)        GroupEchinacea                Dayd15 GroupEchinacea:Dayd15 
# 0.05077047            1.00000000            0.13172593            0.71772569

#Cohen's effect size 
coefs_Dev_post_E <- fixef(dat_post_E)

residuals_Dev_post_E <- resid(dat_post_E)
sd_residuals_Dev_post_E <- sd(residuals_Dev_post_E)

d_cohen_Dev_post_E <- coefs_Dev_post_E["GroupEchinacea"] / sd_residuals_Dev_post_E
d_cohen_Dev_post_E
# 0.2142971 

#Curcumin
dat_post_C=dat_post[dat_post$facet=='Curcumin',]
dat_post_C = lme(Dev ~ Group*Day  ,
                 random =~ 1|Horse,
                 data = dat_post_C)
anova(dat_post_C)
#             numDF denDF  F-value p-value
# (Intercept)     1    10 42.73188  0.0001
# Group           1    10  1.25092  0.2895
# Day             1    10  2.34405  0.1568
# Group:Day       1    10  0.66312  0.4344

summary(dat_post_C)
# Fixed effects:  Dev ~ Group * Day 
#                           Value Std.Error DF    t-value p-value
# (Intercept)           25.526667  9.318926 10  2.7392284  0.0209
# GroupCurcumin         -3.138333 13.178951 10 -0.2381322  0.8166
# Dayd30                21.483333 12.954146 10  1.6584137  0.1282
# GroupCurcumin:Dayd30 -14.918333 18.319929 10 -0.8143226  0.4344

#Bonferroni correction
p_values_dat_post_C <- summary(dat_post_C)$tTable[, "p-value"]
bonferroni_p_values_dat_post_C <- p.adjust(p_values_dat_post_C, method = "bonferroni")
print(bonferroni_p_values_dat_post_C)
# (Intercept)        GroupCurcumin               Dayd30 GroupCurcumin:Dayd30 
# 0.08344143           1.00000000           0.51290111           1.00000000 

#Cohen's effect size 
coefs_Dev_post_C <- fixef(dat_post_C)

residuals_Dev_post_C <- resid(dat_post_C)
sd_residuals_Dev_post_C <- sd(residuals_Dev_post_C)

d_cohen_Dev_post_C <- coefs_Dev_post_C["GroupCurcumin"] / sd_residuals_Dev_post_C
d_cohen_Dev_post_C
# -0.1525113

## Blood sample ##########
bs=read.csv(file='PS.csv', sep=';', dec=',',fileEncoding="latin1")
bs=bs[bs$horses!="757",]
head(bs)
#   horses n   Group    GB  Lym Mono N.gr  Eos Baso déchets_cell Lym2 Mono2 N.gr2 Eos2 Baso2 déchets_cell2   GR  VGM   Ht TCMHg CCMHg   Hg IDR.SD
# 1   W742 1 Mugwort 10.25 59.0  6.5 22.7  9.7  1.2          1.0  6.0   0.7   2.3  1.0   0.1           0.1 7.41 40.5 30.0  17.2  42.6 12.8   30.5
# 2   W744 2 Mugwort  9.16 49.1  7.9 28.5 12.4  1.3          0.8  4.5   0.7   2.6  1.1   0.1           0.1 7.64 36.4 27.2  15.5  42.5 11.6   29.6
# 3   W725 3 Mugwort 12.96 48.3  8.4 23.9 16.9  1.7          0.7  6.3   1.1   3.1  2.2   0.2           0.1 7.64 36.2 27.6  15.5  42.8 11.8   29.2
# 4   W745 4 Mugwort  8.13 50.2  7.0 28.5 12.3  1.4          0.7  4.1   0.6   2.3  1.0   0.1           0.1 8.59 38.0 32.6  16.1  42.5 13.9   30.0
# 5   W754 5 Mugwort  8.28 43.8  7.0 35.6 11.5  1.4          0.8  3.6   0.6   2.9  1.0   0.1           0.1 9.05 35.2 31.9  15.0  42.6 13.6   29.5
# 6   W760 6 Mugwort 10.49 54.2  6.3 19.6 17.5  1.5          0.9  5.7   0.7   2.1  1.8   0.2           0.1 7.15 36.2 25.9  15.6  43.0 11.1   28.9

# IDR.CV  µGR macro.GR Plt VPM Pct mode  Med  IDP µPlt macroPlt Day id  FEC
# 1    2.4 22.2      0.4  61 7.4 0.0  3.1  4.8  9.3  2.7      8.5  d0  1  650
# 2   22.0 39.4      0.1 127 8.3 0.1  4.0  4.3  9.8  1.5     12.7  d0  2  700
# 3   21.9 40.5      0.1  92 9.5 0.1 17.6  9.8 10.3  2.1     21.5  d0  3  450
# 4   21.4 32.4      0.3 132 7.6 0.1  3.5  4.1  9.5  0.9      8.1  d0  4  750
# 5   22.8 45.6      0.1 190 9.5 0.2 17.6 10.8 10.1  1.4     20.2  d0  5  750
# 6   21.6 40.3      0.1  72 8.9 0.1  4.2  4.7  9.9  2.0     14.1  d0  6 1600

bs_hema=select(bs,horses, Group,Lym,Mono,N.gr, GR, Eos, Baso,day)
bs_hema=gather(bs_hema, key="hemato", value="value", 3:8)

bs_hema$hemato[bs_hema$hemato == "GR"] <- "Red blood cell"
bs_hema$hemato[bs_hema$hemato == "Lym"] <- "Lymphocyte"
bs_hema$hemato[bs_hema$hemato == "Mono"] <- "Monocyte"
bs_hema$hemato[bs_hema$hemato == "N.gr"] <- "Neutrophil"
bs_hema$hemato[bs_hema$hemato == "Eos"] <- "Eosinophil"
bs_hema$hemato[bs_hema$hemato == "Baso"] <- "Basophil"
bs_hema$hemato<- factor(bs_hema$hemato, levels = c('Red blood cell','Lymphocyte',"Monocyte","Neutrophil","Eosinophil","Basophil"))
bs_hema$Group<- factor(bs_hema$Group, levels = c('Control','Mugwort',"Echinacea","Curcumin"))

# sum_hemato=ggplot(bs_hema, aes(x=day, y=value, fill=Group))+
#   geom_boxplot(alpha=0.4)+
#   geom_point(aes(x = day,y= value, group = Group), size = 1.5, shape = 1,position = position_jitterdodge(0))+
#   facet_wrap(~Group+hemato, scales="free",ncol = 6)+
#   theme(legend.position = 'bottom',text = element_text(size = 16),
#         axis.text.x = element_text(size = 17,face = "bold", color="#525252"),
#         legend.title = element_blank())+
#   theme(strip.background = element_blank(), 
#         strip.text.x = element_text(size = 30, family = "Lato", face = "bold", color="#525252"))+
#   labs(title=, y= "Count",  x='day')+
#   theme(axis.line = element_line(size = 1, linetype = "solid"),
#         legend.title = element_blank(),legend.position="bottom",
#         legend.text = element_text(size=33, family = "Lato"),
#         axis.title.x = element_text(size=35, family = "Lato", margin = margin(t = 0.4, unit="cm")), 
#         axis.title.y = element_text(size=35, family = "Lato", margin = margin(r = 0.4, unit="cm")),
#         axis.text.x = element_text(size=25, face = "bold", family = "Lato"),
#         axis.text.y = element_text(size=25, face='bold', family = "Lato"))+
#   theme(panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line = element_line(colour = "black"))+
#   scale_fill_manual(values=c('#737373','#dd3497',"#41ab5d","#feb24c"))
# sum_hemato
# dev.print(device = png, file = "sum_hemato_plot.png",  width = 28, height = 18, units = "in", res = 300)


##### Plot and Statistic ####
#Control
bs_C=bs[bs$Group=='Control',]
bs_C_hema=select(bs_C,horses, Group,Lym,Mono,N.gr, GR, Eos, Baso,day)
bs_C_hema=gather(bs_C_hema, key="hemato", value="value", 3:8)

bs_C_hema$hemato[bs_C_hema$hemato == "GR"] <- "Red blood cell"
bs_C_hema$hemato[bs_C_hema$hemato == "Lym"] <- "Lymphocyte"
bs_C_hema$hemato[bs_C_hema$hemato == "Mono"] <- "Monocyte"
bs_C_hema$hemato[bs_C_hema$hemato == "N.gr"] <- "Neutrophil"
bs_C_hema$hemato[bs_C_hema$hemato == "Eos"] <- "Eosinophil"
bs_C_hema$hemato[bs_C_hema$hemato == "Baso"] <- "Basophil"
bs_C_hema$hemato<- factor(bs_C_hema$hemato, levels = c('Red blood cell','Lymphocyte',"Monocyte","Neutrophil","Eosinophil","Basophil"))
bs_C_hema$Group<- factor(bs_C_hema$Group, levels = c('Control','Mugwort'))

bs_C_hema_summary=data_summary(bs_C_hema, varname="value",groupnames=c('Group','day','hemato'))
bs_C_hema_summary
#      Group day         hemato     value        sd
# 1  Control  d0 Red blood cell  7.672857 0.8250195
# 2  Control  d0     Lymphocyte 47.614286 3.8446623
# 3  Control  d0       Monocyte  6.914286 1.5899985
# 4  Control  d0     Neutrophil 27.871429 2.3120801
# 5  Control  d0     Eosinophil 15.557143 4.0722054
# 6  Control  d0       Basophil  1.100000 0.2708013
# 7  Control d10 Red blood cell  8.222857 1.2571756
# 8  Control d10     Lymphocyte 47.342857 5.4181353
# 9  Control d10       Monocyte  5.442857 1.4909249
# 10 Control d10     Neutrophil 21.400000 3.3625387
# 11 Control d10     Eosinophil 23.214286 3.2860455
# 12 Control d10       Basophil  1.314286 0.3484660
# 13 Control d15 Red blood cell  7.988571 0.6892128
# 14 Control d15     Lymphocyte 50.757143 3.5739134
# 15 Control d15       Monocyte  5.957143 1.1028103
# 16 Control d15     Neutrophil 25.471429 5.0397468
# 17 Control d15     Eosinophil 15.785714 3.7239892
# 18 Control d15       Basophil  1.128571 0.1976047
# 19 Control d30 Red blood cell  8.342857 1.3327755
# 20 Control d30     Lymphocyte 46.542857 5.1120306
# 21 Control d30       Monocyte  9.142857 1.1984116
# 22 Control d30     Neutrophil 31.557143 4.6700668
# 23 Control d30     Eosinophil 10.785714 5.3775548
# 24 Control d30       Basophil  1.128571 0.2138090

plot_hemato_Ctl=ggplot(bs_C_hema, aes(x=day, y=value, fill=Group))+
  geom_boxplot(alpha=0.4)+
  geom_point(aes(x = day,y= value, group = Group), size = 1.5, shape = 1,position = position_jitterdodge(0))+
  facet_wrap(~hemato, scales="free_y")+
  theme(legend.position = 'bottom',text = element_text(size = 16),
        axis.text.x = element_text(size = 17,face = "bold", color="black"),
        legend.title = element_blank())+
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 30, family = "Lato", face = "bold", color="black"))+
  labs(title=, y= "Count",  x='day')+
  theme(axis.line = element_line(size = 1, linetype = "solid"),
        legend.title = element_blank(),legend.position="bottom",
        legend.text = element_text(size=33, family = "Lato"),
        axis.title.x = element_text(size=35, family = "Lato", margin = margin(t = 0.4, unit="cm")), 
        axis.title.y = element_text(size=35, family = "Lato", margin = margin(r = 0.4, unit="cm")),
        axis.text.x = element_text(size=25, face = "bold", family = "Lato"),
        axis.text.y = element_text(size=25, face='bold', family = "Lato"))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_fill_manual(values='#737373')
plot_hemato_Ctl
dev.print(device = png, file = "plot_hemato_Ctl.png",  width = 22, height = 12, units = "in", res = 300)

#Red blood cell
bs_C_rbc=bs_C_hema[bs_C_hema$hemato=="Red blood cell",]
mod_C_rbc = lme(value ~  day ,
                 random =~ 1|horses,
                 data = bs_C_rbc)
anova(mod_C_rbc)
#               numDF denDF  F-value p-value
# (Intercept)     1    18 773.0916  <.0001
# day             3    18   0.8468  0.4862

summary(mod_C_rbc)
#                Value Std.Error DF   t-value p-value
# (Intercept) 7.672857 0.4014462 18 19.113042  0.0000
# dayd10      0.550000 0.4537109 18  1.212226  0.2411
# dayd15      0.315714 0.4537109 18  0.695849  0.4954
# dayd30      0.670000 0.4537109 18  1.476711  0.1570

#Lyphocytes
bs_C_lym=bs_C_hema[bs_C_hema$hemato=="Lymphocyte",]
mod_C_lym = lme(value ~ day ,
                 random =~ 1|horses,
                 data = bs_C_lym)
anova(mod_C_lym)
#               numDF denDF   F-value p-value
# (Intercept)     1    18 1156.2258  <.0001
# day             3    18    2.6579  0.0794

summary(mod_C_lym)
# Value Std.Error DF   t-value p-value
# (Intercept) 47.61429  1.722160 18 27.648011  0.0000
# dayd10      -0.27143  1.606488 18 -0.168958  0.8677
# dayd15       3.14286  1.606488 18  1.956352  0.0661
# dayd30      -1.07143  1.606488 18 -0.666938  0.5133

# Monocyte
bs_C_mono=bs_C_hema[bs_C_hema$hemato=="Monocyte",]
mod_C_mono = lme(value ~ day ,
                  random =~ 1|horses,
                  data = bs_C_mono)
anova(mod_C_mono)
#                numDF denDF  F-value p-value
# (Intercept)     1    18 326.0310  <.0001
# day             3    18  16.7614  <.0001

summary(mod_C_mono)
#                 Value Std.Error DF   t-value p-value
# (Intercept)  6.914286 0.5142042 18 13.446575  0.0000
# dayd10      -1.471429 0.5654137 18 -2.602393  0.0180
# dayd15      -0.957143 0.5654137 18 -1.692819  0.1077
# dayd30       2.228571 0.5654137 18  3.941488  0.0010

# Neutrophil
bs_C_neutro=bs_C_hema[bs_C_hema$hemato=="Neutrophil",]
mod_C_neutro = lme(value ~ day ,
                    random =~ 1|horses,
                    data = bs_C_neutro)
anova(mod_C_neutro)
#       numDF denDF  F-value p-value
# (Intercept)     1    18 827.7294  <.0001
# day             3    18   9.5446   5e-04

summary(mod_C_neutro)
#                 Value Std.Error DF   t-value p-value
# (Intercept) 27.871429  1.510215 18 18.455276  0.0000
# dayd10      -6.471429  1.951091 18 -3.316825  0.0038
# dayd15      -2.400000  1.951091 18 -1.230081  0.2345
# dayd30       3.685714  1.951091 18  1.889053  0.0751

# Eosinophil
bs_C_eo=bs_C_hema[bs_C_hema$hemato=="Eosinophil",]
mod_C_eo = lme(value ~ day ,
                random =~ 1|horses,
                data = bs_C_eo)
anova(mod_C_eo)
#       numDF denDF   F-value p-value
# (Intercept)     1    18 207.61749  <.0001
# day             3    18  16.18526  <.0001

summary(mod_C_eo)
#                 Value Std.Error DF   t-value p-value
# (Intercept) 15.557143  1.583025 18  9.827475  0.0000
# dayd10       7.657143  1.804178 18  4.244118  0.0005
# dayd15       0.228571  1.804178 18  0.126690  0.9006
# dayd30      -4.771429  1.804178 18 -2.644656  0.0165

# Basophil
bs_C_baso=bs_C_hema[bs_C_hema$hemato=="Basophil",]
mod_C_baso = lme(value ~ day ,
                  random =~ 1|horses,
                  data = bs_C_baso)
anova(mod_C_baso)
#              numDF denDF   F-value p-value
# (Intercept)     1    18 210.49017  <.0001
# day             3    18   2.07888  0.1388

summary(mod_C_baso)
#                 Value  Std.Error DF   t-value p-value
# (Intercept) 1.1000000 0.09991493 18 11.009366  0.0000
# dayd10      0.2142857 0.09665611 18  2.216991  0.0397
# dayd15      0.0285714 0.09665611 18  0.295599  0.7709
# dayd30      0.0285714 0.09665611 18  0.295599  0.7709


#Mugwort
bs_CA=bs[bs$day!='d30'& bs$day!='d15'& bs$Group!='Echinacea'& bs$Group!='Curcumin',]
bs_CA_hema=select(bs_CA,horses, Group,Lym,Mono,N.gr, GR, Eos, Baso,day)
bs_CA_hema=gather(bs_CA_hema, key="hemato", value="value", 3:8)

bs_CA_hema$hemato[bs_CA_hema$hemato == "GR"] <- "Red blood cell"
bs_CA_hema$hemato[bs_CA_hema$hemato == "Lym"] <- "Lymphocyte"
bs_CA_hema$hemato[bs_CA_hema$hemato == "Mono"] <- "Monocyte"
bs_CA_hema$hemato[bs_CA_hema$hemato == "N.gr"] <- "Neutrophil"
bs_CA_hema$hemato[bs_CA_hema$hemato == "Eos"] <- "Eosinophil"
bs_CA_hema$hemato[bs_CA_hema$hemato == "Baso"] <- "Basophil"
bs_CA_hema$hemato<- factor(bs_CA_hema$hemato, levels = c('Red blood cell','Lymphocyte',"Monocyte","Neutrophil","Eosinophil","Basophil"))
bs_CA_hema$Group<- factor(bs_CA_hema$Group, levels = c('Control','Mugwort'))

bs_CA_hema_summary=data_summary(bs_CA_hema, varname="value",groupnames=c('Group','day','hemato'))
bs_CA_hema_summary
#      Group day         hemato     value         sd
# 1  Control  d0 Red blood cell  7.672857  0.8250195
# 2  Control  d0     Lymphocyte 47.614286  3.8446623
# 3  Control  d0       Monocyte  6.914286  1.5899985
# 4  Control  d0     Neutrophil 27.871429  2.3120801
# 5  Control  d0     Eosinophil 15.557143  4.0722054
# 6  Control  d0       Basophil  1.100000  0.2708013
# 7  Control d10 Red blood cell  8.222857  1.2571756
# 8  Control d10     Lymphocyte 47.342857  5.4181353
# 9  Control d10       Monocyte  5.442857  1.4909249
# 10 Control d10     Neutrophil 21.400000  3.3625387
# 11 Control d10     Eosinophil 23.214286  3.2860455
# 12 Control d10       Basophil  1.314286  0.3484660
# 13 Mugwort  d0 Red blood cell  7.913333  0.7396396
# 14 Mugwort  d0     Lymphocyte 50.766667  5.2401018
# 15 Mugwort  d0       Monocyte  7.183333  0.8134290
# 16 Mugwort  d0     Neutrophil 26.466667  5.6500147
# 17 Mugwort  d0     Eosinophil 13.383333  3.1166756
# 18 Mugwort  d0       Basophil  1.416667  0.1722401
# 19 Mugwort d10 Red blood cell  8.555000  1.0739227
# 20 Mugwort d10     Lymphocyte 47.733333 10.9161654
# 21 Mugwort d10       Monocyte  5.283333  0.7884584
# 22 Mugwort d10     Neutrophil 25.400000  9.1121896
# 23 Mugwort d10     Eosinophil 19.016667  4.0563120
# 24 Mugwort d10       Basophil  1.250000  0.1760682

bs_CA_hema_summary=data_summary(bs_CA_hema, varname="value",groupnames=c('day','hemato'))
bs_CA_hema_summary
#    day         hemato     value        sd
# 1   d0 Red blood cell  7.783846 0.7640957
# 2   d0     Lymphocyte 49.069231 4.6375972
# 3   d0       Monocyte  7.038462 1.2486916
# 4   d0     Neutrophil 27.223077 4.0626662
# 5   d0     Eosinophil 14.553846 3.6893124
# 6   d0       Basophil  1.246154 0.2757275
# 7  d10 Red blood cell  8.376154 1.1403913
# 8  d10     Lymphocyte 47.523077 8.0231077
# 9  d10       Monocyte  5.369231 1.1735875
# 10 d10     Neutrophil 23.246154 6.6751549
# 11 d10     Eosinophil 21.276923 4.1229346
# 12 d10       Basophil  1.284615 0.2733927

plot_hemato_M=ggplot(bs_CA_hema, aes(x=day, y=value, fill=Group))+
  geom_boxplot(alpha=0.4)+
  geom_point(aes(x = day,y= value, group = Group), size = 1.5, shape = 1,position = position_jitterdodge(0))+
  facet_wrap(~hemato, scales="free_y")+
  theme(legend.position = 'bottom',text = element_text(size = 16),
        axis.text.x = element_text(size = 17,face = "bold", color="#525252"),
        legend.title = element_blank())+
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 30, family = "Lato", face = "bold", color="#525252"))+
  labs(title=, y= "Count",  x='day')+
  theme(axis.line = element_line(size = 1, linetype = "solid"),
        legend.title = element_blank(),legend.position="bottom",
        legend.text = element_text(size=33, family = "Lato"),
        axis.title.x = element_text(size=35, family = "Lato", margin = margin(t = 0.4, unit="cm")), 
        axis.title.y = element_text(size=35, family = "Lato", margin = margin(r = 0.4, unit="cm")),
        axis.text.x = element_text(size=25, face = "bold", family = "Lato"),
        axis.text.y = element_text(size=25, face='bold', family = "Lato"))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_fill_manual(values=c('#737373','#dd3497'))
plot_hemato_M

#Red blood cell
bs_CA_rbc=bs_CA_hema[bs_CA_hema$hemato=="Red blood cell",]
mod_CA_rbc = lme(value ~ Group * day ,
                 random =~ 1|horses,
                 data = bs_CA_rbc)
anova(mod_CA_rbc)
#              numDF denDF  F-value p-value
# (Intercept)     1    11 1178.5145  <.0001
# Group           1    11    0.3677  0.5565
# day             1    11    4.0295  0.0699
# Group:day       1    11    0.0240  0.8797

summary(mod_CA_rbc)
# Fixed effects:  Value ~ Group * day 
#                        Value Std.Error DF   t-value p-value
# (Intercept)         7.672857 0.3785546 11 20.268826  0.0000
# GroupMugwort        0.240476 0.5572173 11  0.431566  0.6744
# dayd10              0.550000 0.4021098 11  1.367786  0.1987
# GroupMugwort:dayd10 0.091667 0.5918896 11  0.154871  0.8797

#Bonferroni correction
p_values_mod_CA_rbc <- summary(mod_CA_rbc)$tTable[, "p-value"]
bonferroni_p_values_mod_CA_rbc <- p.adjust(p_values_mod_CA_rbc, method = "bonferroni")
print(bonferroni_p_values_mod_CA_rbc)
# (Intercept)        GroupMugwort              dayd10 GroupMugwort:dayd10 
# 1.852355e-09        1.000000e+00        7.946902e-01        1.000000e+00 

#Cohen's effect size 
coefs_CA_rbc <- fixef(mod_CA_rbc)

residuals_CA_rbc<- resid(mod_CA_rbc)
sd_residuals_CA_rbc <- sd(residuals_CA_rbc)

d_cohen_CA_rbc <- coefs_CA_rbc["GroupMugwort"] / sd_residuals_CA_rbc
d_cohen_CA_rbc
# GroupMugwort 
# 0.4083243 

# Lymphocyte
bs_CA_lym=bs_CA_hema[bs_CA_hema$hemato=="Lymphocyte",]
mod_CA_lym = lme(value ~ Group * day ,
                 random =~ 1|horses,
                 data = bs_CA_lym)
anova(mod_CA_lym)
#              numDF denDF  F-value p-value
# (Intercept)     1    11 810.9571  <.0001
# Group           1    11   0.2711  0.6129
# day             1    11   0.9749  0.3447
# Group:day       1    11   0.7731  0.3981

summary(mod_CA_lym)
#                        Value Std.Error DF   t-value p-value
# (Intercept)         47.61429  2.545600 11 18.704542  0.0000
# GroupMugwort         3.15238  3.747022 11  0.841303  0.4181
# dayd10              -0.27143  2.133979 11 -0.127194  0.9011
# GroupMugwort:dayd10 -2.76190  3.141132 11 -0.879271  0.3981

#Bonferroni correction
p_values_mod_CA_lym <- summary(mod_CA_lym)$tTable[, "p-value"]
bonferroni_p_values_mod_CA_lym <- p.adjust(p_values_mod_CA_lym, method = "bonferroni")
print(bonferroni_p_values_mod_CA_lym)
# (Intercept)        GroupMugwort              dayd10 GroupMugwort:dayd10 
# 4.379468e-09        1.000000e+00        1.000000e+00        1.000000e+00

#Cohen's effect size 
coefs_CA_lym <- fixef(mod_CA_lym)

residuals_CA_lym<- resid(mod_CA_lym)
sd_residuals_CA_lym <- sd(residuals_CA_lym)

d_cohen_CA_lym <- coefs_CA_lym["GroupMugwort"] / sd_residuals_CA_lym
d_cohen_CA_lym
# GroupMugwort 
# 1.080773

# Monocyte
bs_CA_mono=bs_CA_hema[bs_CA_hema$hemato=="Monocyte",]
mod_CA_mono = lme(value ~ Group * day ,
                  random =~ 1|horses,
                  data = bs_CA_mono)
anova(mod_CA_mono)
#              numDF denDF  F-value p-value
# (Intercept)     1    11 630.3971  <.0001
# Group           1    11   0.0122  0.9140
# day             1    11  11.4095  0.0062
# Group:day       1    11   0.1869  0.6739

summary(mod_CA_mono)
#                         Value Std.Error DF   t-value p-value
# (Intercept)          6.914286 0.4762024 11 14.519637  0.0000
# GroupMugwort         0.269048 0.7009509 11  0.383832  0.7084
# dayd10              -1.471429 0.6734519 11 -2.184905  0.0514
# GroupMugwort:dayd10 -0.428571 0.9912943 11 -0.432335  0.6739

#Bonferroni correction
p_values_mod_CA_mono <- summary(mod_CA_mono)$tTable[, "p-value"]
bonferroni_p_values_mod_CA_mono <- p.adjust(p_values_mod_CA_mono, method = "bonferroni")
print(bonferroni_p_values_mod_CA_mono)
# (Intercept)        GroupMugwort              dayd10 GroupMugwort:dayd10 
# 6.418301e-08        1.000000e+00        2.056933e-01        1.000000e+00

#Cohen's effect size 
coefs_CA_mono <- fixef(mod_CA_mono)

residuals_CA_mono<- resid(mod_CA_mono)
sd_residuals_CA_mono <- sd(residuals_CA_mono)

d_cohen_CA_mono <- coefs_CA_mono["GroupMugwort"] / sd_residuals_CA_mono
d_cohen_CA_mono
# GroupMugwort 
# 0.2276393  

# Neutrophil
bs_CA_neutro=bs_CA_hema[bs_CA_hema$hemato=="Neutrophil",]
mod_CA_neutro = lme(value ~ Group * day ,
                    random =~ 1|horses,
                    data = bs_CA_neutro)
anova(mod_CA_neutro)
#             numDF denDF  F-value p-value
# (Intercept)     1    11 368.1568  <.0001
# Group           1    11   0.2419  0.6325
# day             1    11   6.2823  0.0292
# Group:day       1    11   2.8836  0.1176

summary(mod_CA_neutro)
# Fixed effects:  value ~ Group * day 
#                         Value Std.Error DF   t-value p-value
# (Intercept)         27.871429  2.093104 11 13.315837  0.0000
# GroupMugwort        -1.404762  3.080965 11 -0.455949  0.6573
# dayd10              -6.471429  2.162276 11 -2.992879  0.0122
# GroupMugwort:dayd10  5.404762  3.182784 11  1.698124  0.1176

#Bonferroni correction
p_values_mod_CA_neutro <- summary(mod_CA_neutro)$tTable[, "p-value"]
bonferroni_p_values_mod_CA_neutro <- p.adjust(p_values_mod_CA_neutro, method = "bonferroni")
print(bonferroni_p_values_mod_CA_neutro)
# (Intercept)        GroupMugwort              dayd10 GroupMugwort:dayd10 
# 1.585994e-07        1.000000e+00        4.893832e-02        4.702313e-01

#Cohen's effect size 
coefs_CA_neutro <- fixef(mod_CA_neutro)

residuals_CA_neutro<- resid(mod_CA_neutro)
sd_residuals_CA_neutro <- sd(residuals_CA_neutro)

d_cohen_CA_neutro <- coefs_CA_neutro["GroupMugwort"] / sd_residuals_CA_neutro
d_cohen_CA_neutro
# GroupMugwort 
# -0.4482742 

# Eosinophil
bs_CA_eo=bs_CA_hema[bs_CA_hema$hemato=="Eosinophil",]
mod_CA_eo = lme(value ~ Group * day ,
                random =~ 1|horses,
                data = bs_CA_eo)
anova(mod_CA_eo)
#             numDF denDF  F-value p-value
# (Intercept)     1    11 415.9135  <.0001
# Group           1    11   3.2683  0.0980
# day             1    11  43.4289  <.0001
# Group:day       1    11   0.9780  0.3439

summary(mod_CA_eo)
#                         Value Std.Error DF   t-value p-value
# (Intercept)         15.557143  1.384333 11 11.238010  0.0000
# GroupMugwort        -2.173810  2.037682 11 -1.066805  0.3089
# dayd10               7.657143  1.390276 11  5.507641  0.0002
# GroupMugwort:dayd10 -2.023810  2.046431 11 -0.988946  0.3439

#Bonferroni correction
p_values_mod_CA_eo <- summary(mod_CA_eo)$tTable[, "p-value"]
bonferroni_p_values_mod_CA_eo <- p.adjust(p_values_mod_CA_eo, method = "bonferroni")
print(bonferroni_p_values_mod_CA_eo)
# (Intercept)        GroupMugwort              dayd10 GroupMugwort:dayd10 
# 9.105582e-07        1.000000e+00        7.364262e-04        1.000000e+00

#Cohen's effect size 
coefs_CA_eo <- fixef(mod_CA_eo)

residuals_CA_eo<- resid(mod_CA_eo)
sd_residuals_CA_eo <- sd(residuals_CA_eo)

d_cohen_CA_eo <- coefs_CA_eo["GroupMugwort"] / sd_residuals_CA_eo
d_cohen_CA_eo
# GroupMugwort 
# -1.0896 

# Basophil
bs_CA_baso=bs_CA_hema[bs_CA_hema$hemato=="Basophil",]
mod_CA_baso = lme(value ~ Group * day ,
                  random =~ 1|horses,
                  data = bs_CA_baso)
anova(mod_CA_baso)
#             numDF denDF  F-value p-value
# (Intercept)     1    11 486.6800  <.0001
# Group           1    11   1.2029  0.2962
# day             1    11   0.1992  0.6640
# Group:day       1    11   4.8568  0.0498

summary(mod_CA_baso)
#Fixed effects:  value ~ Group * day 
#                          Value  Std.Error DF   t-value p-value
# (Intercept)          1.1000000 0.09776412 11 11.251572  0.0000
# GroupMugwort         0.3166667 0.14390489 11  2.200528  0.0500
# dayd10               0.2142857 0.11743496 11  1.824718  0.0953
# GroupMugwort:dayd10 -0.3809524 0.17285959 11 -2.203826  0.0498

#Bonferroni correction
p_values_mod_CA_baso <- summary(mod_CA_baso)$tTable[, "p-value"]
bonferroni_p_values_mod_CA_baso <- p.adjust(p_values_mod_CA_baso, method = "bonferroni")
print(bonferroni_p_values_mod_CA_baso)
# (Intercept)        GroupMugwort              dayd10 GroupMugwort:dayd10 
# 8.994396e-07        2.001599e-01        3.811823e-01        1.990101e-01 

#Cohen's effect size 
coefs_CA_baso <- fixef(mod_CA_baso)

residuals_CA_baso<- resid(mod_CA_baso)
sd_residuals_CA_baso <- sd(residuals_CA_baso)

d_cohen_CA_baso <- coefs_CA_baso["GroupMugwort"] / sd_residuals_CA_baso
d_cohen_CA_baso
# GroupMugwort 
# 1.737355  

#Echinacea
bs_CE=bs[bs$day!='d30'& bs$day!='d10'& bs$Group!='Mugwort'& bs$Group!='Curcumin',]
bs_CE_hema=select(bs_CE,horses, Group,Lym,Mono,N.gr, GR, Eos, Baso,day)
bs_CE_hema=gather(bs_CE_hema, key="hemato", value="value", 3:8)

bs_CE_hema$hemato[bs_CE_hema$hemato == "GR"] <- "Red blood cell"
bs_CE_hema$hemato[bs_CE_hema$hemato == "Lym"] <- "Lymphocyte"
bs_CE_hema$hemato[bs_CE_hema$hemato == "Mono"] <- "Monocyte"
bs_CE_hema$hemato[bs_CE_hema$hemato == "N.gr"] <- "Neutrophil"
bs_CE_hema$hemato[bs_CE_hema$hemato == "Eos"] <- "Eosinophil"
bs_CE_hema$hemato[bs_CE_hema$hemato == "Baso"] <- "Basophil"
bs_CE_hema$hemato<- factor(bs_CE_hema$hemato, levels = c('Red blood cell','Lymphocyte',"Monocyte","Neutrophil","Eosinophil","Basophil"))
bs_CE_hema$Group<- factor(bs_CE_hema$Group, levels = c('Control','Echinacea'))

plot_hemato_E=ggplot(bs_CE_hema, aes(x=day, y=value, fill=Group))+
  geom_boxplot(alpha=0.4)+
  geom_point(aes(x = day,y= value, group = Group), size = 1.5, shape = 1,position = position_jitterdodge(0))+
  facet_wrap(~hemato, scales="free_y")+
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 30, family = "Lato", face = "bold", color="#525252"))+
  labs(title=, y= "Count",  x='day')+
  theme(axis.line = element_line(size = 1, linetype = "solid"),
        legend.title = element_blank(),legend.position="bottom",
        legend.text = element_text(size=33, family = "Lato"),
        axis.title.x = element_text(size=35, family = "Lato", margin = margin(t = 0.4, unit="cm")), 
        axis.title.y = element_text(size=35, family = "Lato", margin = margin(r = 0.4, unit="cm")),
        axis.text.x = element_text(size=25, face = "bold", family = "Lato"),
        axis.text.y = element_text(size=25, face='bold', family = "Lato"))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_fill_manual(values=c('#737373','#41ab5d'))
plot_hemato_E

# Red blood cell
bs_CE_rbc=bs_CE_hema[bs_CE_hema$hemato=="Red blood cell",]
mod_CE_rbc = lme(value ~ Group * day ,
                 random =~ 1|horses,
                 data = bs_CE_rbc)
anova(mod_CE_rbc)
#             numDF denDF   F-value p-value
# (Intercept)     1    11 1808.2530  <.0001
# Group           1    11    0.1238  0.7316
# day             1    11    0.1460  0.7097
# Group:day       1    11    1.5172  0.2437

summary(mod_CE_rbc)
# Fixed effects:  Value ~ Group * day 
#                           Value Std.Error DF   t-value p-value
# (Intercept)            7.672857 0.2896904 11 26.486409  0.0000
# GroupEchinacea         0.387143 0.4264127 11  0.907907  0.3834
# dayd15                 0.315714 0.2826063 11  1.117152  0.2877
# GroupEchinacea:dayd15 -0.512381 0.4159853 11 -1.231729  0.2437

#Bonferroni correction
p_values_mod_CE_rbc <- summary(mod_CE_rbc)$tTable[, "p-value"]
bonferroni_p_values_mod_CE_rbc <- p.adjust(p_values_mod_CE_rbc, method = "bonferroni")
print(bonferroni_p_values_mod_CE_rbc)
# (Intercept)        GroupEchinacea                dayd15 GroupEchinacea:dayd15 
# 1.031682e-10          1.000000e+00          1.000000e+00          9.749311e-01

#Cohen's effect size 
coefs_CE_rbc <- fixef(mod_CE_rbc)

residuals_CE_rbc<- resid(mod_CE_rbc)
sd_residuals_CE_rbc <- sd(residuals_CE_rbc)

d_cohen_CE_rbc <- coefs_CE_rbc["GroupEchinacea"] / sd_residuals_CE_rbc
d_cohen_CE_rbc
# GroupEchinacea 
# 0.9636705

# Lymphocyte
bs_CE_lym=bs_CE_hema[bs_CE_hema$hemato=="Lymphocyte",]
mod_CE_lym = lme(value ~ Group * day ,
                 random =~ 1|horses,
                 data = bs_CE_lym)
anova(mod_CE_lym)
#             numDF denDF   F-value p-value
# (Intercept)     1    11 1022.2424  <.0001
# Group           1    11    0.7770  0.3969
# day             1    11    4.7829  0.0512
# Group:day       1    11    0.0002  0.9883

summary(mod_CE_lym)
#      Fixed effects:  value ~ Group * day 
#                          Value Std.Error DF   t-value p-value
# (Intercept)           47.61429  2.264036 11 21.030709  0.0000
# GroupEchinacea        -2.63095  3.332571 11 -0.789466  0.4465
# dayd15                 3.14286  1.946082 11  1.614966  0.1346
# GroupEchinacea:dayd15 -0.04286  2.864555 11 -0.014961  0.9883

#Bonferroni correction
p_values_mod_CE_lym <- summary(mod_CE_lym)$tTable[, "p-value"]
bonferroni_p_values_mod_CE_lym <- p.adjust(p_values_mod_CE_lym, method = "bonferroni")
print(bonferroni_p_values_mod_CE_lym)
# (Intercept)        GroupEchinacea                dayd15 GroupEchinacea:dayd15 
# 1.246066e-09          1.000000e+00          5.384484e-01          1.000000e+00

#Cohen's effect size 
coefs_CE_lym <- fixef(mod_CE_lym)

residuals_CE_lym<- resid(mod_CE_lym)
sd_residuals_CE_lym <- sd(residuals_CE_lym)

d_cohen_CE_lym <- coefs_CE_lym["GroupEchinacea"] / sd_residuals_CE_lym
d_cohen_CE_lym
# GroupEchinacea 
# -0.9836643

#Monocyte
bs_CE_mono=bs_CE_hema[bs_CE_hema$hemato=="Monocyte",]
mod_CE_mono = lme(value ~ Group * day ,
                  random =~ 1|horses,
                  data = bs_CE_mono)
anova(mod_CE_mono)
#             numDF denDF  F-value p-value
# (Intercept)     1    11 490.3104  <.0001
# Group           1    11   0.0560  0.8174
# day             1    11   5.5420  0.0382
# Group:day       1    11   0.0796  0.7831

summary(mod_CE_mono)
#                           Value Std.Error DF   t-value p-value
# (Intercept)            6.914286 0.4713939 11 14.667744  0.0000
# GroupEchinacea         0.035714 0.6938731 11  0.051471  0.9599
# dayd15                -0.957143 0.4987280 11 -1.919168  0.0813
# GroupEchinacea:dayd15  0.207143 0.7341077 11  0.282170  0.7831

#Bonferroni correction
p_values_mod_CE_mono <- summary(mod_CE_mono)$tTable[, "p-value"]
bonferroni_p_values_mod_CE_mono <- p.adjust(p_values_mod_CE_mono, method = "bonferroni")
print(bonferroni_p_values_mod_CE_mono)
# (Intercept)        GroupEchinacea                dayd15 GroupEchinacea:dayd15 
# 5.769471e-08          1.000000e+00          3.250696e-01          1.000000e+00 

#Cohen's effect size 
coefs_CE_mono <- fixef(mod_CE_mono)

residuals_CE_mono<- resid(mod_CE_mono)
sd_residuals_CE_mono <- sd(residuals_CE_mono)

d_cohen_CE_mono <- coefs_CE_mono["GroupEchinacea"] / sd_residuals_CE_mono
d_cohen_CE_mono
# GroupEchinacea 
# 0.0489705

#Neutrophil
bs_CE_neutro=bs_CE_hema[bs_CE_hema$hemato=="Neutrophil",]
mod_CE_neutro = lme(value ~ Group * day ,
                    random =~ 1|horses,
                    data = bs_CE_neutro)
anova(mod_CE_neutro)
#             numDF denDF  F-value p-value
# (Intercept)     1    11 433.4100  <.0001
# Group           1    11   2.4907  0.1428
# day             1    11   3.8547  0.0754
# Group:day       1    11   0.2254  0.6442

summary(mod_CE_neutro)
# Fixed effects:  value ~ Group * day 
# #                          Value Std.Error DF   t-value p-value
# (Intercept)           27.871429  2.162740 11 12.887094  0.0000
# GroupEchinacea         5.111905  3.183466 11  1.605767  0.1366
# dayd15                -2.400000  2.146393 11 -1.118155  0.2873
# GroupEchinacea:dayd15 -1.500000  3.159404 11 -0.474773  0.6442

#Bonferroni correction
p_values_mod_CE_neutro <- summary(mod_CE_neutro)$tTable[, "p-value"]
bonferroni_p_values_mod_CE_neutro <- p.adjust(p_values_mod_CE_neutro, method = "bonferroni")
print(bonferroni_p_values_mod_CE_neutro)
# (Intercept)        GroupEchinacea                dayd15 GroupEchinacea:dayd15 
# 2.228174e-07          5.465082e-01          1.000000e+00          1.000000e+00 

#Cohen's effect size 
coefs_CE_neutro <- fixef(mod_CE_neutro)

residuals_CE_neutro<- resid(mod_CE_neutro)
sd_residuals_CE_neutro <- sd(residuals_CE_neutro)

d_cohen_CE_neutro <- coefs_CE_neutro["GroupEchinacea"] / sd_residuals_CE_neutro
d_cohen_CE_neutro
# GroupEchinacea 
# 1.666215 

#Eosinophil
bs_CE_eo=bs_CE_hema[bs_CE_hema$hemato=="Eosinophil",]
mod_CE_eo = lme(value ~ Group * day ,
                random =~ 1|horses,
                data = bs_CE_eo)
anova(mod_CE_eo)
#             numDF denDF   F-value p-value
# (Intercept)     1    11 238.03640  <.0001
# Group           1    11   0.73662  0.4091
# day             1    11   0.60299  0.4538
# Group:day       1    11   0.38216  0.5490

summary(mod_CE_eo)
#                           Value Std.Error DF   t-value p-value
# (Intercept)           15.557143  1.521435 11 10.225311  0.0000
# GroupEchinacea        -2.357143  2.239491 11 -1.052535  0.3151
# dayd15                 0.228571  1.525468 11  0.149837  0.8836
# GroupEchinacea:dayd15  1.388095  2.245428 11  0.618187  0.5490

#Bonferroni correction
p_values_mod_CE_eo <- summary(mod_CE_eo)$tTable[, "p-value"]
bonferroni_p_values_mod_CE_eo <- p.adjust(p_values_mod_CE_eo, method = "bonferroni")
print(bonferroni_p_values_mod_CE_eo)
# (Intercept)        GroupEchinacea                dayd15 GroupEchinacea:dayd15 
# 2.365879e-06          1.000000e+00          1.000000e+00          1.000000e+00

#Cohen's effect size 
coefs_CE_eo <- fixef(mod_CE_eo)

residuals_CE_eo<- resid(mod_CE_eo)
sd_residuals_CE_eo <- sd(residuals_CE_eo)

d_cohen_CE_eo <- coefs_CE_eo["GroupEchinacea"] / sd_residuals_CE_eo
d_cohen_CE_eo
# GroupEchinacea 
# -1.077379 

#Basophil
bs_CE_baso=bs_CE_hema[bs_CE_hema$hemato=="Basophil",]
mod_CE_baso = lme(value ~ Group * day ,
                  random =~ 1|horses,
                  data = bs_CE_baso)
anova(mod_CE_baso)
#             numDF denDF  F-value p-value
# (Intercept)     1    11 376.9910  <.0001
# Group           1    11   0.0004  0.9839
# day             1    11   0.0396  0.8459
# Group:day       1    11   0.0340  0.8571

summary(mod_CE_baso)
#Fixed effects:  value ~ Group * day 
#                            Value  Std.Error DF   t-value p-value
# (Intercept)            1.1000000 0.09435194 11 11.658478  0.0000
# GroupEchinacea         0.0166667 0.13888229 11  0.120006  0.9066
# dayd15                 0.0285714 0.10533100 11  0.271254  0.7912
# GroupEchinacea:dayd15 -0.0285714 0.15504303 11 -0.184281  0.8571

#Bonferroni correction
p_values_mod_CE_baso <- summary(mod_CE_baso)$tTable[, "p-value"]
bonferroni_p_values_mod_CE_baso <- p.adjust(p_values_mod_CE_baso, method = "bonferroni")
print(bonferroni_p_values_mod_CE_baso)
# (Intercept)        GroupEchinacea                dayd15 GroupEchinacea:dayd15 
# 6.257336e-07          1.000000e+00          1.000000e+00          1.000000e+00

#Cohen's effect size 
coefs_CE_baso <- fixef(mod_CE_baso)
residuals_CE_baso<- resid(mod_CE_baso)
sd_residuals_CE_baso <- sd(residuals_CE_baso)
d_cohen_CE_baso <- coefs_CE_baso["GroupEchinacea"] / sd_residuals_CE_baso
d_cohen_CE_baso
# GroupEchinacea 
# 0.1057945

#Curcumin
bs_CC=bs[bs$day!='d15'& bs$day!='d10'& bs$Group!='Mugwort'& bs$Group!='Echinacea',]
bs_CC_hema=select(bs_CC,horses, Group,Lym,Mono,N.gr, GR, Eos, Baso,day)
bs_CC_hema=gather(bs_CC_hema, key="hemato", value="value", 3:8)

bs_CC_hema$hemato[bs_CC_hema$hemato == "GR"] <- "Red blood cell"
bs_CC_hema$hemato[bs_CC_hema$hemato == "Lym"] <- "Lymphocyte"
bs_CC_hema$hemato[bs_CC_hema$hemato == "Mono"] <- "Monocyte"
bs_CC_hema$hemato[bs_CC_hema$hemato == "N.gr"] <- "Neutrophil"
bs_CC_hema$hemato[bs_CC_hema$hemato == "Eos"] <- "Eosinophil"
bs_CC_hema$hemato[bs_CC_hema$hemato == "Baso"] <- "Basophil"
bs_CC_hema$hemato<- factor(bs_CC_hema$hemato, levels = c('Red blood cell','Lymphocyte',"Monocyte","Neutrophil","Eosinophil","Basophil"))
bs_CC_hema$Group<- factor(bs_CC_hema$Group, levels = c('Control','Curcumin'))

bs_CC_hema_summary=data_summary(bs_CC_hema, varname="value",groupnames=c('day','hemato'))
bs_CC_hema_summary
#    day         hemato     value        sd
# 1   d0 Red blood cell  7.771538 0.7734753
# 2   d0     Lymphocyte 48.561538 5.2157675
# 3   d0       Monocyte  6.669231 1.4522308
# 4   d0     Neutrophil 27.723077 4.0365319
# 5   d0     Eosinophil 14.861538 3.7068717
# 6   d0       Basophil  1.161538 0.2218801
# 7  d30 Red blood cell  8.139231 1.1059420
# 8  d30     Lymphocyte 46.484615 5.0439479
# 9  d30       Monocyte  9.307692 1.1528671
# 10 d30     Neutrophil 31.176923 4.4931715
# 11 d30     Eosinophil 11.107692 5.9662190
# 12 d30       Basophil  1.130769 0.1652504

plot_hemato_C=ggplot(bs_CC_hema, aes(x=day, y=value, fill=Group))+
  geom_boxplot(alpha=0.4)+
  geom_point(aes(x = day,y= value, group = Group), size = 1.5, shape = 1,position = position_jitterdodge(0))+
  facet_wrap(~hemato, scales="free_y")+
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 30, family = "Lato", face = "bold", color="#525252"))+
  labs(title=, y= "Count",  x='day')+
  theme(axis.line = element_line(size = 1, linetype = "solid"),
        legend.title = element_blank(),legend.position="bottom",
        legend.text = element_text(size=33, family = "Lato"),
        axis.title.x = element_text(size=35, family = "Lato", margin = margin(t = 0.4, unit="cm")), 
        axis.title.y = element_text(size=35, family = "Lato", margin = margin(r = 0.4, unit="cm")),
        axis.text.x = element_text(size=25, face = "bold", family = "Lato"),
        axis.text.y = element_text(size=25, face='bold', family = "Lato"))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_fill_manual(values=c('#737373','#feb24c'))
plot_hemato_C

#Red bloos cell
bs_CC_rbc=bs_CC_hema[bs_CC_hema$hemato=="Red blood cell",]
mod_CC_rbc = lme(value ~ Group * day ,
                 random =~ 1|horses,
                 data = bs_CC_rbc)
anova(mod_CC_rbc)
#             numDF denDF   F-value p-value
# (Intercept)     1    11 1376.4855  <.0001
# Group           1    11    0.0699  0.7964
# day             1    11    1.2190  0.2931
# Group:day       1    11    0.9613  0.3479

summary(mod_CC_rbc)
# Fixed effects:  Value ~ Group * day 
#                          Value Std.Error DF   t-value p-value
# (Intercept)           7.672857 0.3699763 11 20.738779  0.0000
# GroupCurcumin         0.213810 0.5445904 11  0.392606  0.7021
# dayd30                0.670000 0.4538486 11  1.476263  0.1679
# GroupCurcumin:dayd30 -0.655000 0.6680471 11 -0.980470  0.3479

#Bonferroni correction
p_values_mod_CC_rbc <- summary(mod_CC_rbc)$tTable[, "p-value"]
bonferroni_p_values_mod_CC_rbc <- p.adjust(p_values_mod_CC_rbc, method = "bonferroni")
print(bonferroni_p_values_mod_CC_rbc)
# (Intercept)        GroupCurcumin               dayd30 GroupCurcumin:dayd30 
# 1.448113e-09         1.000000e+00         6.716711e-01         1.000000e+00

#Cohen's effect size 
coefs_CC_rbc <- fixef(mod_CC_rbc)
residuals_CC_rbc<- resid(mod_CC_rbc)
sd_residuals_CC_rbc <- sd(residuals_CC_rbc)
d_cohen_CC_rbc <- coefs_CC_rbc["GroupCurcumin"] / sd_residuals_CC_rbc
d_cohen_CC_rbc
# 0.2998332  

#Lymphocyte
bs_CC_lym=bs_CC_hema[bs_CC_hema$hemato=="Lymphocyte",]
mod_CC_lym = lme(value ~ Group * day ,
                 random =~ 1|horses,
                 data = bs_CC_lym)
anova(mod_CC_lym)
#              numDF denDF   F-value p-value
# (Intercept)     1    11 1176.6729  <.0001
# Group           1    11    0.1201  0.7355
# day             1    11    4.4599  0.0584
# Group:day       1    11    1.2195  0.2930

summary(mod_CC_lym)
#                         Value Std.Error DF   t-value p-value
# (Intercept)          47.61429  2.003386 11 23.766903  0.0000
# GroupCurcumin         2.05238  2.948905 11  0.695981  0.5009
# dayd30               -1.07143  1.340233 11 -0.799435  0.4410
# GroupCurcumin:dayd30 -2.17857  1.972769 11 -1.104322  0.2930

#Bonferroni correction
p_values_mod_CC_lym <- summary(mod_CC_lym)$tTable[, "p-value"]
bonferroni_p_values_mod_CC_lym <- p.adjust(p_values_mod_CC_lym, method = "bonferroni")
print(bonferroni_p_values_mod_CC_lym)
# (Intercept)        GroupCurcumin               dayd30 GroupCurcumin:dayd30 
# 3.333339e-10         1.000000e+00         1.000000e+00         1.000000e+00 

#Cohen's effect size 
coefs_CC_lym <- fixef(mod_CC_lym)
residuals_CC_lym<- resid(mod_CC_lym)
sd_residuals_CC_lym <- sd(residuals_CC_lym)
d_cohen_CC_lym <- coefs_CC_lym["GroupCurcumin"] / sd_residuals_CC_lym
d_cohen_CC_lym
# 1.162926 

#Monocyte
bs_CC_mono=bs_CC_hema[bs_CC_hema$hemato=="Monocyte",]
mod_CC_mono = lme(value ~ Group * day ,
                  random =~ 1|horses,
                  data = bs_CC_mono)
anova(mod_CC_mono)
#             numDF denDF  F-value p-value
# (Intercept)     1    11 519.6397  <.0001
# Group           1    11   0.0153  0.9038
# day             1    11 103.4537  <.0001
# Group:day       1    11   2.9129  0.1159

summary(mod_CC_mono)
#                          Value Std.Error DF   t-value p-value
# (Intercept)           6.914286 0.5092276 11 13.577987  0.0000
# GroupCurcumin        -0.530952 0.7495627 11 -0.708349  0.4935
# dayd30                2.228571 0.3535090 11  6.304144  0.0001
# GroupCurcumin:dayd30  0.888095 0.5203511 11  1.706723  0.1159

#Bonferroni correction
p_values_mod_CC_mono <- summary(mod_CC_mono)$tTable[, "p-value"]
bonferroni_p_values_mod_CC_mono <- p.adjust(p_values_mod_CC_mono, method = "bonferroni")
print(bonferroni_p_values_mod_CC_mono)
# (Intercept)        GroupCurcumin               dayd30 GroupCurcumin:dayd30 
# 1.294492e-07         1.000000e+00         2.321509e-04         4.636369e-01 

#Cohen's effect size 
coefs_CC_mono <- fixef(mod_CC_mono)
residuals_CC_mono<- resid(mod_CC_mono)
sd_residuals_CC_mono <- sd(residuals_CC_mono)
d_cohen_CC_mono <- coefs_CC_mono["GroupCurcumin"] / sd_residuals_CC_mono
d_cohen_CC_mono
# -1.135055 

#Neutrophil
bs_CC_neutro=bs_CC_hema[bs_CC_hema$hemato=="Neutrophil",]
mod_CC_neutro = lme(value ~ Group * day ,
                    random =~ 1|horses,
                    data = bs_CC_neutro)
anova(mod_CC_neutro)
#             numDF denDF   F-value p-value
# (Intercept)     1    11 1139.7723  <.0001
# Group           1    11    0.1071  0.7496
# day             1    11    3.9192  0.0733
# Group:day       1    11    0.0206  0.8885

summary(mod_CC_neutro)
# Fixed effects:  value ~ Group * day 
#                          Value Std.Error DF   t-value p-value
# (Intercept)          27.871429  1.681179 11 16.578504  0.0000
# GroupCurcumin        -0.321429  2.474628 11 -0.129890  0.8990
# dayd30                3.685714  2.377546 11  1.550218  0.1494
# GroupCurcumin:dayd30 -0.502381  3.499653 11 -0.143552  0.8885

#Bonferroni correction
p_values_mod_CC_neutro <- summary(mod_CC_neutro)$tTable[, "p-value"]
bonferroni_p_values_mod_CC_neutro <- p.adjust(p_values_mod_CC_neutro, method = "bonferroni")
print(bonferroni_p_values_mod_CC_neutro)
# (Intercept)        GroupCurcumin               dayd30 GroupCurcumin:dayd30 
# 1.583311e-08         1.000000e+00         5.974611e-01         1.000000e+00 

#Cohen's effect size 
coefs_CC_neutro <- fixef(mod_CC_neutro)
residuals_CC_neutro<- resid(mod_CC_neutro)
sd_residuals_CC_neutro <- sd(residuals_CC_neutro)
d_cohen_CC_neutro <- coefs_CC_neutro["GroupCurcumin"] / sd_residuals_CC_neutro
d_cohen_CC_neutro
# -0.07703361 

#Eosinophil
bs_CC_eo=bs_CC_hema[bs_CC_hema$hemato=="Eosinophil",]
mod_CC_eo = lme(value ~ Group * day ,
                random =~ 1|horses,
                data = bs_CC_eo)
anova(mod_CC_eo)
#              numDF denDF   F-value p-value
#  (Intercept)     1    11 128.14656  <.0001
# Group           1    11   0.03095  0.8636
# day             1    11   4.87098  0.0495
# Group:day       1    11   0.41759  0.5314

summary(mod_CC_eo)
#   Fixed effects:  value ~ Group * day 
#                          Value Std.Error DF   t-value p-value
# (Intercept)          15.557143  1.945907 11  7.994805  0.0000
# GroupCurcumin        -1.507143  2.864297 11 -0.526182  0.6092
# dayd30               -4.771429  2.317880 11 -2.058532  0.0640
# GroupCurcumin:dayd30  2.204762  3.411826 11  0.646212  0.5314

#Bonferroni correction
p_values_mod_CC_eo <- summary(mod_CC_eo)$tTable[, "p-value"]
bonferroni_p_values_mod_CC_eo <- p.adjust(p_values_mod_CC_eo, method = "bonferroni")
print(bonferroni_p_values_mod_CC_eo)
# (Intercept)        GroupCurcumin               dayd30 GroupCurcumin:dayd30 
# 2.629075e-05         1.000000e+00         2.561091e-01         1.000000e+00

#Cohen's effect size 
coefs_CC_eo <- fixef(mod_CC_eo)
residuals_CC_eo<- resid(mod_CC_eo)
sd_residuals_CC_eo <- sd(residuals_CC_eo)
d_cohen_CC_eo <- coefs_CC_eo["GroupCurcumin"] / sd_residuals_CC_eo
d_cohen_CC_eo
# -0.4209005 

#Basophil
bs_CC_baso=bs_CC_hema[bs_CC_hema$hemato=="Basophil",]
mod_CC_baso = lme(value ~ Group * day ,
                  random =~ 1|horses,
                  data = bs_CC_baso)
anova(mod_CC_baso)
#             numDF denDF  F-value p-value
# (Intercept)     1    11 690.8839  <.0001
# Group           1    11   0.6231  0.4466
# day             1    11   0.2134  0.6531
# Group:day       1    11   0.9262  0.3565

summary(mod_CC_baso)
# Fixed effects:  value ~ Group * day 
#                           Value  Std.Error DF   t-value p-value
# (Intercept)           1.1000000 0.07477031 11 14.711721  0.0000
# GroupCurcumin         0.1333333 0.11005892 11  1.211472  0.2511
# dayd30                0.0285714 0.09076055 11  0.314800  0.7588
# GroupCurcumin:dayd30 -0.1285714 0.13359591 11 -0.962390  0.3565

#Bonferroni correction
p_values_mod_CC_baso <- summary(mod_CC_baso)$tTable[, "p-value"]
bonferroni_p_values_mod_CC_baso <- p.adjust(p_values_mod_CC_baso, method = "bonferroni")
print(bonferroni_p_values_mod_CC_baso)
# (Intercept)        GroupCurcumin               dayd30 GroupCurcumin:dayd30 
# 5.590814e-08         1.000000e+00         1.000000e+00         1.000000e+00 

#Cohen's effect size 
coefs_CC_baso <- fixef(mod_CC_baso)
residuals_CC_baso<- resid(mod_CC_baso)
sd_residuals_CC_baso <- sd(residuals_CC_baso)
d_cohen_CC_baso <- coefs_CC_baso["GroupCurcumin"] / sd_residuals_CC_baso
d_cohen_CC_baso
# 0.9408386 

hemato_plot=ggarrange(plot_hemato_M, plot_hemato_E, plot_hemato_C,
          ncol = 2, nrow = 2)
hemato_plot
dev.print(device = png, file = "hemato_plot.png",  width = 28, height = 18, units = "in", res = 300)
