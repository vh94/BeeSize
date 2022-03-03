#### HYMENOPTERA COLONY SIMULATION.
rm(list=ls())
###
source(file = "../../media/valentin/3BD1-BCFD/COLONY_FUNC.r")
####

runs = 1000
QUEEN_size<-10
QUEEN_died<-0
#alpha<-1.1
alpha<-seq(1,1.3,0.1)
#SMIN
SMIN<-seq(4,20,2)
# Mort
MORT<-c(0.025,0.05,0.1)
#MORT<-0.05
PROD = 0.2
TMAX = 45
DMIN = 10
MAXEGG =8
min_T=5
####################
SIM <- function(SMIN,runs,alpha,PROD, MORT,TMAX,QUEEN_size) {
  xm<-matrix(0,runs*length(SMIN)*length(alpha)*length(MORT), 8)
  colnames(xm)<-c( "MORT" ,"alpha","SMIN","DMIN","rep_nr","wnT","J","QueenTDeath")
  k=0
for (M in 1:length(MORT)){ 
  for (a in 1:length(alpha)){
      for(S in 1:length(SMIN)){
        for (i in 1:runs) {
          DMIN_i = (SMIN[S]+min_T)
          erg<-COLONY(TMAX=TMAX,
                      SMIN = SMIN[S],
                      DMIN=DMIN_i,
                      alpha = alpha[a],
                      MORT = MORT[M],
                      PROD = PROD,
                      QUEEN_size = QUEEN_size,
                      MAXEGG = MAXEGG)
          k<-k+1
          xm[k,]<-c(MORT[M],alpha[a],SMIN[S],DMIN_i,i, erg)
        }
      }
    }
  }
  SIM_res<-as.data.frame(xm)
  return(SIM_res)
}

 #save(sim_minT,file="../../media/valentin/3BD1-BCFD/.SIMULATION2_300runs")

#### Start Simulation ####
Sys.time()
sim_minT<-SIM(SMIN,runs, alpha, PROD,MORT,TMAX,QUEEN_size )
Sys.time()
sim<-as.data.frame(sim_minT)
saveRDS(sim,file = "Desktop/R_modeling_course/SIM_DATA")
###### Ploting ######


library(dplyr)
library(ggplot2)





SIM_PLOT_1<-sim_minT%>% mutate(alpha=as.factor(alpha),
                   QUEEN_died=ifelse(sim_minT$QueenTDeath==TMAX,0,1)) %>%  
  group_by(SMIN,MORT,alpha) %>% 
  #filter(QUEEN_died==0) %>% 
  summarise(J=mean(J))%>% 
  ggplot(aes(SMIN,J, col=alpha))+
  geom_line()+
  scale_color_discrete()+
  facet_wrap(.~MORT,scales = 'free')



PLOT_density<-sim_minT %>% 
  mutate(alpha=as.factor(alpha),SMIN=as.factor(SMIN),QUEEN_died=ifelse(sim_minT$QueenTDeath==TMAX,0,1))%>%
  ggplot()+
  stat_density(aes(J,fill=SMIN,colour=SMIN),alpha=0.6)+
  scale_x_log10()+
  facet_grid(alpha~MORT, scales='free')

ggsave("Desktop/R_modeling_course/lineplot",plot = SIM_PLOT_1,device = 'png',)
ggsave("Desktop/R_modeling_course/densitplot",plot = PLOT_density,device = 'png')

sim_minT %>% 
  mutate(alpha=as.factor(alpha),
                    SMIN=as.factor(SMIN),
                    QUEEN_died=ifelse(sim_minT$QueenTDeath==TMAX,0,1))%>%
  group_by(SMIN,DMIN,alpha,MORT,QUEEN_died) %>%
  mutate(meanq.death.time=mean(QueenTDeath))%>%
  tally() 
  

plot(tapply(sim_minT$J,sim_minT$SMIN,mean))


plot(tapply(sim_minT$J,sim_minT$SMIN,median))

summary(sim_minT)

### TODO anteil gestorbene Königinnen
#### SImulation DMIN heruntersetzen
#### DMIN SMIN koppeln : DMIN= SMIN^(v) (+c)   ; v < 1  c<- 5
 ####
