##====== Evaluation of plants based commercial products for equine cyathostomin control  ======
library(readr)
library(ggplot2)
library(rstatix)
library(patchwork)
library(tidyr)
library(mixOmics)
library(ggpubr)
library(geepack)
library(nlme)
library(dplyr)
library(reshape2)
library(cowplot)
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

path <- "~/work/R_work/Commercial products"
setwd(path)

dat=read.csv(file='EUPAOr.csv', header=T,sep=';', dec=',',fileEncoding="latin1")
head(dat)
#   horses n age   Group Weight MF  FEC  V L3_obs Nb.d..uf.crottin L3_exp Dev  day   facet
# 1   W742 1   4 Mugwort     NA NA  585 NA     NA               NA     NA  NA d-11 Mugwort
# 2   W744 2   3 Mugwort     NA NA  250 NA     NA               NA     NA  NA d-11 Mugwort
# 3   W725 3   5 Mugwort     NA NA  360 NA     NA               NA     NA  NA d-11 Mugwort
# 4   W745 4   3 Mugwort     NA NA  420 NA     NA               NA     NA  NA d-11 Mugwort
# 5   W754 5   2 Mugwort     NA NA  550 NA     NA               NA     NA  NA d-11 Mugwort
# 6   W760 6   1 Mugwort     NA NA 1080 NA     NA               NA     NA  NA d-11 Mugwort

######## Pre treatment #########
dat_pre=dat[dat$day=="d-11",]

dat_pre_summary=data_summary(dat_pre, varname="FEC",groupnames='Group')
dat_pre_summary
#       Group      FEC       sd
# 1   Control 557.1429 474.7180
# 2  Curcumin 553.3333 112.1457
# 3 Echinacea 507.0000 413.7995
# 4   Mugwort 540.8333 291.4175

mod_dat_pre = lme(FEC ~ Group  ,
                  random =~ 1|horses,
                  data = dat_pre)
anova(mod_dat_pre)
#             numDF denDF  F-value p-value
# (Intercept)     1    21 56.84731  <.0001
# Group           3    21  0.02508  0.9945

summary(mod_dat_pre)
# Fixed effects:  FEC ~ Group 
#                   Value Std.Error DF   t-value p-value
# (Intercept)    557.1429  135.4207 21  4.114164  0.0005
# GroupCurcumin   -3.8095  199.3338 21 -0.019111  0.9849
# GroupEchinacea -50.1429  199.3338 21 -0.251552  0.8038
# GroupMugwort   -16.3095  199.3338 21 -0.081820  0.9356


########## Effect on FEC ##########
##### Plot ####
dat_post=dat[dat$day!="d-11",]
dat_post$Group<-factor(dat_post$Group, levels= c("Control",'Mugwort',"Echinacea","Curcumin"))
dat_post$facet<-factor(dat_post$facet, levels= c("Control",'Mugwort',"Echinacea","Curcumin"))

dat_FEC_post_summary=data_summary(dat_post, varname="FEC",groupnames=c('day','Group'))
dat_FEC_post_summary
#    day     Group       FEC        sd
# 1   d0   Control  542.8571  342.5222
# 2   d0  Curcumin  850.0000  451.6636
# 3   d0 Echinacea  983.3333  640.0521
# 4   d0   Mugwort  816.6667  399.5831
# 5  d10   Control 1242.8571  944.9112
# 6  d10   Mugwort 1008.3333  793.3578
# 7  d15   Control 1028.5714  795.2238
# 8  d15 Echinacea  941.6667  580.0144
# 9  d30   Control 1214.2857 1166.0883
# 10 d30  Curcumin  775.0000  361.5937

dat_FEC_post_facetsum=data_summary(dat_post, varname="FEC",groupnames=c('day','facet'))
dat_FEC_post_facetsum
#   day     facet       FEC       sd
# 1  d0   Mugwort  669.2308 389.7320
# 2  d0 Echinacea  746.1538 536.7578
# 3  d0  Curcumin  684.6154 419.0190
# 4 d10   Mugwort 1134.6154 850.5843
# 5 d15 Echinacea  988.4615 677.0505
# 6 d30  Curcumin 1011.5385 886.7435

FEC_plot=ggplot(dat_post, aes(x=day, y=FEC, fill=Group))+
  geom_boxplot(alpha=0.4)+
  geom_point(aes(x = day,y= FEC, group = Group), size = 1.5, shape = 1,position = position_jitterdodge(0))+
  facet_wrap(~facet,scales = 'free_x')+
  theme(strip.background = element_rect(fill="white",size=0),
        strip.text.x = element_text(size = 0, face = "bold", color="#525252"))+
  labs(y='Fecal egg count \n (eggs per gram)', x="Days")+
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
FEC_plot

###### Statistic ####
#Mugwort
dat_post_M=dat_post[dat_post$facet=='Mugwort',]
dat_post_M = lme(FEC ~ Group*day  ,
                 random =~ 1|horses,
                 data = dat_post_M)
anova(dat_post_M)
#             numDF denDF   F-value p-value
# (Intercept)     1    11 30.637638  0.0002
# Group           1    11  0.003612  0.9532
# day             1    11  6.216525  0.0299
# Group:day       1    11  1.843246  0.2018

summary(dat_post_M)
# Fixed effects:  FEC ~ Group * day 
#                         Value Std.Error DF    t-value p-value
# (Intercept)          542.8571  255.9003 11  2.1213618  0.0574
# GroupMugwort         273.8095  376.6751 11  0.7269117  0.4825
# dayd10               700.0000  254.3670 11  2.7519288  0.0188
# GroupMugwort:dayd10 -508.3333  374.4182 11 -1.3576621  0.2018

#Echinacea
dat_post_E=dat_post[dat_post$facet=='Echinacea',]
dat_post_E = lme(FEC ~ Group*day  ,
                 random =~ 1|horses,
                 data = dat_post_E)
anova(dat_post_E)
#             numDF denDF  F-value p-value
# (Intercept)     1    11 44.55061  <.0001
# Group           1    11  0.46001  0.5116
# day             1    11  1.20707  0.2954
# Group:day       1    11  1.42105  0.2583

summary(dat_post_E)
# Fixed effects:  FEC ~ Group * day 
#                           Value Std.Error DF   t-value p-value
# (Intercept)            542.8571  232.2511 11  2.337372  0.0394
# GroupEchinacea         440.4762  341.8643 11  1.288453  0.2240
# dayd15                 485.7143  300.5548 11  1.616059  0.1344
# GroupEchinacea:dayd15 -527.3810  442.4047 11 -1.192078  0.2583

#Curcumin
dat_post_C=dat_post[dat_post$facet=='Curcumin',]
dat_post_C = lme(FEC ~ Group*day  ,
                 random =~ 1|horses,
                 data = dat_post_C)
anova(dat_post_C)
#             numDF denDF  F-value p-value
# (Intercept)     1    11 32.05091  0.0001
# Group           1    11  0.04835  0.8300
# day             1    11  1.82099  0.2043
# Group:day       1    11  2.35915  0.1528

summary(dat_post_C)
#                          Value Std.Error DF   t-value p-value
# (Intercept)           542.8571  262.5359 11  2.067744  0.0630
# GroupCurcumin         307.1429  386.4424 11  0.794796  0.4435
# dayd30                671.4286  330.1522 11  2.033694  0.0668
# GroupCurcumin:dayd30 -746.4286  485.9709 11 -1.535953  0.1528

#FECRT------- Bootstrap aproach 
fec_ctl0 = dat_post$FEC[dat_post$Group=='Control' & dat_post$day=='d0']
fec_ctl10 = dat_post$FEC[dat_post$Group=='Control' & dat_post$day=='d10']
fec_ctl15 = dat_post$FEC[dat_post$Group=='Control' & dat_post$day=='d15']
fec_ctl30 = dat_post$FEC[dat_post$Group=='Control' & dat_post$day=='d30']
fec_m0 = dat_post$FEC[dat_post$Group=='Mugwort' & dat_post$day=='d0']
fec_m10 = dat_post$FEC[dat_post$Group=='Mugwort' & dat_post$day=='d10']
fec_e0 = dat_post$FEC[dat_post$Group=='Echinacea' & dat_post$day=='d0']
fec_e15 = dat_post$FEC[dat_post$Group=='Echinacea' & dat_post$day=='d15']
fec_c0 = dat_post$FEC[dat_post$Group=='Curcumin' & dat_post$day=='d0']
fec_c30 = dat_post$FEC[dat_post$Group=='Curcumin' & dat_post$day=='d30']

#Mugwort
M=dat_post[dat_post$facet=='Mugwort',]
M=M[,c('horses', 'Group','day','FEC')]
colnames(M)=c('ID','Group','day','FEC')
M$Block="Mugwort"

bdatok=M
vecfarm = unique(bdatok$Block)
cicross = NULL
n = 0

for(f in vecfarm){
  ##-- subset farm of interest
  M = bdatok[bdatok$Block==f,]
  ##-- iteration
  n = n + 1
  Eff = array(NA,1000)
  
  for(i in 1:1000){
    ##-- sample control
    tm0 = sample(M$FEC[M$Group=='Control' & M$day=='d0'],size = 7,replace = T)
    tmend = sample(M$FEC[M$Group=='Control' & M$day=='d10'],size = 7,replace = T)
    
    ##-- sample treated
    trt0 = sample(M$FEC[M$Group=='Mugwort' & M$day=='d0'],size = 6,replace = T)
    trtend = sample(M$FEC[M$Group=='Mugwort' & M$day=='d10'],size = 6,replace = T)
    
    ##-- FECR
    tm.0=mean(tm0)
    tm.end=mean(tmend)
    trt.0=mean(trt0)
    trt.end=mean(trtend)
    trt = trt.end/trt.0
    tm = tm.end/tm.0
    Eff[i] = ((1-((trt)/(tm)))*100)
    if(Eff[i]<0){Eff[i]=0} 
  }
  #b0 = fecrtCI(d$PEQ0,d$PEQ14,paired=TRUE,R=1000,alpha=.05)
  cicross$Lot[n] = f
  tm=mean(bdatok$FEC[bdatok$Block==f & bdatok$day=='d10'& bdatok$Group=='Control'])/mean(bdatok$FEC[bdatok$Block==f & bdatok$day=='d0'& bdatok$Group=='Control'])
  trt=mean(bdatok$FEC[bdatok$Block==f & bdatok$day=='d10'& bdatok$Group=='Mugwort'])/mean(bdatok$FEC[bdatok$Block==f & bdatok$day=='d0'& bdatok$Group=='Mugwort'])
  cicross$Efficacy[n] =((1-((trt)/(tm)))*100) 
  cicross$CIdw[n] = quantile(Eff, 0.025, na.rm=T)
  cicross$CIup[n] = quantile(Eff, 0.975, na.rm=T)
  rm(Eff)
  #  rm(b0)
}
cicross=data.frame(cicross)
cicross
#         Lot Efficacy CIdw     CIup
#   1 Mugwort 46.07084    0 80.07855

#Echinacea
E=dat_post[dat_post$facet=='Echinacea',]
E=E[,c('horses', 'Group','day','FEC')]
colnames(M)=c('ID','Group','day','FEC')
E$Block="Echinacea"

bdatok=E
vecfarm = unique(bdatok$Block)
cicross = NULL
n = 0

for(f in vecfarm){
  ##-- subset farm of interest
  E = bdatok[bdatok$Block==f,]
  ##-- iteration
  n = n + 1
  Eff = array(NA,1000)
  
  for(i in 1:1000){
    ##-- sample control
    tm0 = sample(E$FEC[E$Group=='Control' & E$day=='d0'],size = 7,replace = T)
    tmend = sample(E$FEC[E$Group=='Control' & E$day=='d15'],size = 7,replace = T)
    
    ##-- sample treated
    trt0 = sample(E$FEC[E$Group=='Echinacea' & E$day=='d0'],size = 6,replace = T)
    trtend = sample(E$FEC[E$Group=='Echinacea' & E$day=='d15'],size = 6,replace = T)
    
    ##-- FECR
    tm.0=mean(tm0)
    tm.end=mean(tmend)
    trt.0=mean(trt0)
    trt.end=mean(trtend)
    trt = trt.end/trt.0
    tm = tm.end/tm.0
    Eff[i] = ((1-((trt)/(tm)))*100)
    if(Eff[i]<0){Eff[i]=0} 
  }
  #b0 = fecrtCI(d$PEQ0,d$PEQ14,paired=TRUE,R=1000,alpha=.05)
  cicross$Lot[n] = f
  tm=mean(bdatok$FEC[bdatok$Block==f & bdatok$day=='d15'& bdatok$Group=='Control'])/mean(bdatok$FEC[bdatok$Block==f & bdatok$day=='d0'& bdatok$Group=='Control'])
  trt=mean(bdatok$FEC[bdatok$Block==f & bdatok$day=='d15'& bdatok$Group=='Echinacea'])/mean(bdatok$FEC[bdatok$Block==f & bdatok$day=='d0'& bdatok$Group=='Echinacea'])
  cicross$Efficacy[n] =((1-((trt)/(tm)))*100) 
  cicross$CIdw[n] = quantile(Eff, 0.025, na.rm=T)
  cicross$CIup[n] = quantile(Eff, 0.975, na.rm=T)
  rm(Eff)
  #  rm(b0)
}
cicross=data.frame(cicross)
cicross
#         Lot Efficacy CIdw     CIup
# 1 Echinacea 49.45857    0 82.01173

#Curcumin
C=dat_post[dat_post$facet=='Curcumin',]
C=C[,c('horses', 'Group','day','FEC')]
colnames(M)=c('ID','Group','day','FEC')
C$Block="Curcumin"

bdatok=C
vecfarm = unique(bdatok$Block)
cicross = NULL
n = 0

for(f in vecfarm){
  ##-- subset farm of interest
  C = bdatok[bdatok$Block==f,]
  ##-- iteration
  n = n + 1
  Eff = array(NA,1000)
  
  for(i in 1:1000){
    ##-- sample control
    tm0 = sample(C$FEC[C$Group=='Control' & C$day=='d0'],size = 7,replace = T)
    tmend = sample(C$FEC[C$Group=='Control' & C$day=='d30'],size = 7,replace = T)
    
    ##-- sample treated
    trt0 = sample(C$FEC[C$Group=='Curcumin' & C$day=='d0'],size = 6,replace = T)
    trtend = sample(C$FEC[C$Group=='Curcumin' & C$day=='d30'],size = 6,replace = T)
    
    ##-- FECR
    tm.0=mean(tm0)
    tm.end=mean(tmend)
    trt.0=mean(trt0)
    trt.end=mean(trtend)
    trt = trt.end/trt.0
    tm = tm.end/tm.0
    Eff[i] = ((1-((trt)/(tm)))*100)
    if(Eff[i]<0){Eff[i]=0} 
  }
  #b0 = fecrtCI(d$PEQ0,d$PEQ14,paired=TRUE,R=1000,alpha=.05)
  cicross$Lot[n] = f
  tm=mean(bdatok$FEC[bdatok$Block==f & bdatok$day=='d30'& bdatok$Group=='Control'])/mean(bdatok$FEC[bdatok$Block==f & bdatok$day=='d0'& bdatok$Group=='Control'])
  trt=mean(bdatok$FEC[bdatok$Block==f & bdatok$day=='d30'& bdatok$Group=='Curcumin'])/mean(bdatok$FEC[bdatok$Block==f & bdatok$day=='d0'& bdatok$Group=='Curcumin'])
  cicross$Efficacy[n] =((1-((trt)/(tm)))*100) 
  cicross$CIdw[n] = quantile(Eff, 0.025, na.rm=T)
  cicross$CIup[n] = quantile(Eff, 0.975, na.rm=T)
  rm(Eff)
  #  rm(b0)
}
cicross=data.frame(cicross)
cicross
#        Lot Efficacy CIdw    CIup
# 1 Curcumin 59.23875    0 84.2284


#FECRT------- eggCounts package
mod_m <- eggCounts::fecr_stan(fec_m0, fec_m10, rawCounts = TRUE,
                              paired = TRUE, indEfficacy = TRUE)
mod_m$posterior.summary
#                        mean        sd      2.5%       50%      97.5%  HPDLow95      mode HPDHigh95
# FECR                 0.2716    0.1079    0.1169    0.2529     0.5407    0.0973    0.2243     0.481
# meanEPG.untreated 8953.2058 3188.2330 3891.8067 8604.9865 16352.3982 3547.1582 8445.4071 15530.743
# meanEPG.treated   6531.4038 2566.8841 2589.9355 6133.6941 12361.5640 2253.2268 5362.2422 11678.938

mod_e <- eggCounts::fecr_stan(fec_e0, fec_e15, rawCounts = TRUE,
                              paired = TRUE, indEfficacy = TRUE)
mod_e$posterior.summary
#                        mean        sd      2.5%      50%      97.5%  HPDLow95      mode  HPDHigh95
# FECR                 0.3159    0.1219    0.1461    0.296     0.6092    0.1181    0.2374     0.5428
# meanEPG.untreated 8490.2836 2914.2456 3639.5602 8212.179 14982.3788 3306.3759 7083.8557 14371.0142
# meanEPG.treated   5802.5175 2259.9044 2178.7124 5528.184 10913.3243 1809.6275 5218.8349 10234.3605

mod_c <- eggCounts::fecr_stan(fec_c0, fec_c30, rawCounts = TRUE,
                              paired = TRUE, indEfficacy = TRUE)
mod_c$posterior.summary
#                        mean        sd      2.5%       50%      97.5%  HPDLow95      mode  HPDHigh95
# FECR                 0.2805    0.1085    0.1216    0.2621     0.5358    0.1025    0.2126     0.4967
# meanEPG.untreated 8616.7791 3076.8856 3710.7235 8238.6191 15591.5295 3415.6887 7748.6804 14770.0743
# meanEPG.treated   6205.2082 2442.5803 2485.9559 5868.5164 11740.7359 2099.4503 5274.4093 11138.1921


########## Effect on larval development ##########
dat_Dev_post_summary=data_summary(dat_post, varname="Dev",groupnames=c('day','Group'))
dat_Dev_post_summary
#    day     Group      Dev        sd
# 1   d0   Control 31.37857 25.038704
# 2   d0   Mugwort 38.80333 13.162027
# 3   d0 Echinacea 29.31667 18.627317
# 4   d0  Curcumin 22.38833  5.171311
# 5  d10   Control 44.41714 19.145143
# 6  d10   Mugwort 48.97333 23.822403
# 7  d15   Control 50.86143 25.988031
# 8  d15 Echinacea 34.23667  8.213712
# 9  d30   Control 43.24143 35.813934
# 10 d30  Curcumin 28.95333  9.452394

##Outliers
# OUT=dat_post %>%
#   group_by(Group) %>%
#   identify_outliers(Dev)
# dtout=data.frame(Date=OUT$Date,Assay=OUT$Assay, Group=OUT$Group, Conc=OUT$Conc,
#                  Unit=OUT$Unit, L1.L2=OUT$L1.L2, L3=OUT$L3, Dev=OUT$Dev,id=OUT$id, pld=OUT$pld)
# win=anti_join(win,dtout, by = "id")

Dev_plot=ggplot(dat_post, aes(x=day, y=Dev, fill=Group))+
  geom_boxplot(alpha=0.4)+
  geom_point(aes(x = day,y= Dev, group = Group), size = 1.5, shape = 1,position = position_jitterdodge(0))+
  facet_wrap(~facet,scales = 'free_x')+
  theme(strip.background = element_rect(fill="white",size=0),
        strip.text.x = element_text(size = 0, face = "bold", color="#525252"))+
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

#=== Stat ==
#Mugwort
dat_post_M=dat_post[dat_post$facet=='Mugwort',]
dat_post_M = lme(Dev ~ Group*day  ,
                 random =~ 1|horses,
                 data = dat_post_M)
anova(dat_post_M)
#             numDF denDF  F-value p-value
# (Intercept)     1    11 66.00478  <.0001
# Group           1    11  0.35602  0.5628
# day             1    11  3.36254  0.0939
# Group:day       1    11  0.05011  0.8270

summary(dat_post_M)
# Fixed effects:  Dev ~ Group * day 
#                         Value Std.Error DF   t-value p-value
# (Intercept)         31.378571  8.091407 11  3.878012  0.0026
# GroupMugwort         7.424762 11.910228 11  0.623394  0.5457
# dayd10              13.038571  8.705966 11  1.497659  0.1624
# GroupMugwort:dayd10 -2.868571 12.814835 11 -0.223848  0.8270

#Echinacea
dat_post_E=dat_post[dat_post$facet=='Echinacea',]
dat_post_E = lme(Dev ~ Group*day  ,
                 random =~ 1|horses,
                 data = dat_post_E)
anova(dat_post_E)
#             numDF denDF  F-value p-value
# (Intercept)     1    11 75.20817  <.0001
# Group           1    11  1.20435  0.2959
# day             1    11  2.26013  0.1609
# Group:day       1    11  0.73145  0.4107

summary(dat_post_E)
# Fixed effects:  Dev ~ Group * day 
#                            Value Std.Error DF   t-value p-value
# (Intercept)            31.378571  8.179819 11  3.836096  0.0028
# GroupEchinacea         -2.061905 12.040368 11 -0.171249  0.8671
# dayd15                 19.482857 11.568011 11  1.684201  0.1203
# GroupEchinacea:dayd15 -14.562857 17.027652 11 -0.855248  0.4107

#Curcumin
dat_post_C=dat_post[dat_post$facet=='Curcumin',]
dat_post_C = lme(Dev ~ Group*day  ,
                 random =~ 1|horses,
                 data = dat_post_C)
anova(dat_post_C)
#             numDF denDF  F-value p-value
# (Intercept)     1    11 46.84249  <.0001
# Group           1    11  1.54607  0.2396
# day             1    11  1.01824  0.3346
# Group:day       1    11  0.08008  0.7824

summary(dat_post_C)
# Fixed effects:  Dev ~ Group * day 
#                          Value Std.Error DF   t-value p-value
# (Intercept)          31.378571  8.993452 11  3.489046  0.0051
# GroupCurcumin        -8.990238 13.238003 11 -0.679123  0.5111
# dayd30               11.862857 12.718662 11  0.932713  0.3710
# GroupCurcumin:dayd30 -5.297857 18.721364 11 -0.282985  0.7824

plot=(FEC_plot /Dev_plot) + plot_layout(guides = 'collect')&
  theme(legend.position = "bottom")
plot

########## Blood sample ##########
bs=read.csv(file='PS.csv', sep=';', dec=',',fileEncoding="latin1")
head(bs)
#   horses n   Group    GB  Lym Mono N.gr  Eos Baso déchets_cell Lym2 Mono2 N.gr2 Eos2 Baso2 déchets_cell2   GR  VGM   Ht TCMHg CCMHg   Hg IDR.SD
# 1   W742 1 Mugwort 10.25 59.0  6.5 22.7  9.7  1.2          1.0  6.0   0.7   2.3  1.0   0.1           0.1 7.41 40.5 30.0  17.2  42.6 12.8   30.5
# 2   W744 2 Mugwort  9.16 49.1  7.9 28.5 12.4  1.3          0.8  4.5   0.7   2.6  1.1   0.1           0.1 7.64 36.4 27.2  15.5  42.5 11.6   29.6
# 3   W725 3 Mugwort 12.96 48.3  8.4 23.9 16.9  1.7          0.7  6.3   1.1   3.1  2.2   0.2           0.1 7.64 36.2 27.6  15.5  42.8 11.8   29.2
# 4   W745 4 Mugwort  8.13 50.2  7.0 28.5 12.3  1.4          0.7  4.1   0.6   2.3  1.0   0.1           0.1 8.59 38.0 32.6  16.1  42.5 13.9   30.0
# 5   W754 5 Mugwort  8.28 43.8  7.0 35.6 11.5  1.4          0.8  3.6   0.6   2.9  1.0   0.1           0.1 9.05 35.2 31.9  15.0  42.6 13.6   29.5
# 6   W760 6 Mugwort 10.49 54.2  6.3 19.6 17.5  1.5          0.9  5.7   0.7   2.1  1.8   0.2           0.1 7.15 36.2 25.9  15.6  43.0 11.1   28.9

# IDR.CV  µGR macro.GR Plt VPM Pct mode  Med  IDP µPlt macroPlt day id  FEC
# 1    2.4 22.2      0.4  61 7.4 0.0  3.1  4.8  9.3  2.7      8.5  d0  1  650
# 2   22.0 39.4      0.1 127 8.3 0.1  4.0  4.3  9.8  1.5     12.7  d0  2  700
# 3   21.9 40.5      0.1  92 9.5 0.1 17.6  9.8 10.3  2.1     21.5  d0  3  450
# 4   21.4 32.4      0.3 132 7.6 0.1  3.5  4.1  9.5  0.9      8.1  d0  4  750
# 5   22.8 45.6      0.1 190 9.5 0.2 17.6 10.8 10.1  1.4     20.2  d0  5  750
# 6   21.6 40.3      0.1  72 8.9 0.1  4.2  4.7  9.9  2.0     14.1  d0  6 1600


##### Plot and Statistic ####
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

plot_hemato_M=ggplot(bs_CA_hema, aes(x=day, y=value, fill=Group))+
  geom_boxplot(alpha=0.4)+
  geom_point(aes(x = day,y= value, group = Group), size = 1.5, shape = 1,position = position_jitterdodge(0))+
  facet_wrap(~hemato, scales="free_y")+
  theme(strip.background = element_rect(fill="white",size=0),
        strip.text.x = element_text(size = 17, face = "bold", color="#525252"))+
  labs(title=, y= "Count",  x='Day')+
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
# Fixed effects:  Value ~ Group * Day 
#                        Value Std.Error DF   t-value p-value
# (Intercept)         7.672857 0.3785546 11 20.268826  0.0000
# GroupMugwort        0.240476 0.5572173 11  0.431566  0.6744
# dayd10              0.550000 0.4021098 11  1.367786  0.1987
# GroupMugwort:dayd10 0.091667 0.5918896 11  0.154871  0.8797

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
  theme(strip.background = element_rect(fill="white",size=0),
        strip.text.x = element_text(size = 17, face = "bold", color="#525252"))+
  labs(title=, y= "Count",  x='Day')+
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
# Fixed effects:  Value ~ Group * Day 
#                           Value Std.Error DF   t-value p-value
# (Intercept)            7.672857 0.2896904 11 26.486409  0.0000
# GroupEchinacea         0.387143 0.4264127 11  0.907907  0.3834
# dayd15                 0.315714 0.2826063 11  1.117152  0.2877
# GroupEchinacea:dayd15 -0.512381 0.4159853 11 -1.231729  0.2437

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
  theme(strip.background = element_rect(fill="white",size=0),
        strip.text.x = element_text(size = 17, face = "bold", color="#525252"))+
  labs(title=, y= "Count",  x='Day')+
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
# Fixed effects:  Value ~ Group * Day 
#                          Value Std.Error DF   t-value p-value
# (Intercept)           7.672857 0.3699763 11 20.738779  0.0000
# GroupCurcumin         0.213810 0.5445904 11  0.392606  0.7021
# dayd30                0.670000 0.4538486 11  1.476263  0.1679
# GroupCurcumin:dayd30 -0.655000 0.6680471 11 -0.980470  0.3479

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


hemato_plot=ggarrange(plot_hemato_M, plot_hemato_E, plot_hemato_C,
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)
hemato_plot



