# Wed Mar 17 18:38:10 2021 ------------------------------
rm(list=ls())
####  Colony function: Simulates the growth of a single Colony/ Sample
COLONY <- function(SMIN,DMIN,alpha,PROD,MORT,TMAX,MAXEGG,QUEEN_size) {
  wn<-numeric(TMAX)  # worker number
  BEES<-data.frame(wbirth=numeric(),wsize=numeric(),wadult=numeric(),wdeath=numeric())
  Q_death<-TMAX
  ## Loop over Timesteps
  for (t in 1:TMAX) {
    ## 1   :: volkszählung worker number at current timestep 
    wn[t]<-nrow(BEES)
    ## 2   ::  Becoming adult
    if (wn[t]>0) {
      x<-((BEES$wbirth <= (t-DMIN)) & (BEES$wsize >= SMIN))
      BEES$wadult<-as.numeric(x) 
    }
    ## 3 bringing in / calculating resources at current timestep
    res<-sum(BEES[BEES$wadult==1,]$wsize^alpha)*PROD
    #### 4  Rules for Queens participation:
    ## Queen can only forage, if alive and no workers present:
    if (QUEEN_died==0 & sum(BEES$wadult)<1 ) {
      res<-((QUEEN_size^alpha)*PROD) # Queens net resources
      QUEEN_died<-rbinom(1, 1, MORT)  # Queens mortality 
      if (QUEEN_died) {Q_death<-t }   # mark death 
    }
    ## 5 calculate number of brood and resources needed for them
    brut<-nrow(BEES[BEES$wsize<SMIN,]) # number of bees to feed
    bedarf<-(SMIN/DMIN)*brut           # resources needed for them
    n.layed<-0                         # number of eggs 
    # decide and add eggs to colony
    if (QUEEN_died==0) {
      n.layed<-ifelse(res>bedarf,1,0)*round((res-bedarf)/(SMIN/DMIN),digits = 0)
    }
    n.layed<-min(n.layed,MAXEGG) # number of layed eggs cant exceed maxegg
    if(n.layed > 0){
      EGGS<-matrix(0,n.layed,4)  # create n.layed number of eggs
      EGGS[,1]<-t                # current timestep as birthdate
      colnames(EGGS)<-colnames(BEES)
      BEES<-rbind(BEES,EGGS) 
    }
    ### Feeding the brood
    brut<-nrow(BEES[BEES$wsize<SMIN,])   # update number of bees to feed
    if (brut>0) {
      BEES[BEES$wsize<SMIN,]$wsize<-BEES[BEES$wsize<SMIN,]$wsize+(res/brut)
    }
    ## Dying
    if(sum(BEES$wadult)>=1){
      # only let adult workers die
      BEES$wdeath[1:length(BEES$wdeath)] <- rbinom(nrow(BEES), 1, MORT) * (BEES$wadult[1:length(BEES$wdeath)]==1)
    }
    BEES<-subset(BEES[1:length(BEES$wdeath),],wdeath == 0)## only choose alive workers
  }
  wnT<-nrow(BEES)   # number of workers at last timestep
  res_to_ad<-sum(SMIN-BEES[BEES$wsize<SMIN,]$wsize) ## resources missing for brood
  J<-(((SMIN^alpha)*wnT*PROD)*1/MORT)-res_to_ad ## optimization criterion J
  return(c(wnT,J, Q_death))
}

#### HYMENOPTERA COLONY SIMULATION loop for multiple Samples:.
####################   SImulation loop Function::
SIM <- function(SMIN,runs,alpha,PROD, MORT,TMAX,QUEEN_size) {
  xm<-matrix(0,runs*length(SMIN)*length(alpha)*length(MORT), 8) # output table
  colnames(xm)<-c( "MORT" ,"alpha","SMIN","DMIN","rep_nr","wnT","J","QueenTDeath")
  k=0     ## Track current row for output table
  for (M in 1:length(MORT)){ 
    for (a in 1:length(alpha)){
      for(S in 1:length(SMIN)){
        for (i in 1:runs) {
          DMIN_i = (SMIN[S]+min_T)   # calculate current DMIN
          erg<-COLONY(TMAX=TMAX,     # insert Simulation values
                      SMIN = SMIN[S],
                      DMIN=DMIN_i,
                      alpha = alpha[a],
                      MORT = MORT[M],
                      PROD = PROD,
                      QUEEN_size = QUEEN_size,
                      MAXEGG = MAXEGG)
          k<-k+1 # set output row
          xm[k,]<-c(MORT[M],alpha[a],SMIN[S],DMIN_i,i, erg) # fill output row k
        }
      }
    }
  }
  SIM_res<-as.data.frame(xm)
  return(SIM_res)
}

############## Experiment:

runs = 3       # number of replicates for each comb of parameters

###### Parameters:
QUEEN_size<-10          # Size of the Queen
QUEEN_died<-0           # Queen is not dead
alpha<-seq(1,1.3,0.1)   # alpha (foraging size bonus) 
SMIN<-seq(4,20,2)       # minimal worker sizes
MORT<-c(0.025,0.05,0.1) # Mortality risks
PROD = 0.2              # Productivity rate foragers
TMAX = 45               # number of timesteps
MAXEGG =8               # maximal number of eggs per timesteps
min_T=5                 # addition to Smin for calculating minimal development time (DMIN)

#### Start Simulation ####
# Sys.time()
seed<-123
set.seed(seed)
sim<-SIM(SMIN,runs, alpha, PROD,MORT,TMAX,QUEEN_size)
# Sys.time()

#saveRDS(sim,file = "Desktop/R_modeling_course/SIM_DATA")

###### analysis & plotting ######

library(dplyr)
library(ggplot2)

SIM_PLOT_1<-sim%>% mutate(alpha=as.factor(alpha),
                               QUEEN_died=ifelse(sim$QueenTDeath==TMAX,0,1)) %>%  
  group_by(SMIN,MORT,alpha) %>% 
  #filter(QUEEN_died==0) %>% 
  summarise(J=mean(J))%>% 
  ggplot(aes(SMIN,J, col=alpha))+
  geom_line()+
  scale_color_discrete()+
  xlab("minimal worker size")+
  facet_wrap(.~MORT,scales = 'free')+theme_classic()





ggsave("Desktop/R_modeling_course/lineplot",plot = SIM_PLOT_1,device = 'png',)
#ggsave("Desktop/R_modeling_course/densitplot",plot = PLOT_density,device = 'png')


### TODO anteil gestorbene Königinnen

Q_mort_plot<-sim%>% mutate(alpha=as.factor(alpha),
                           MORT=as.factor(MORT),
                           QUEEN_died=ifelse(sim$QueenTDeath==TMAX,0,1)) %>%  
  group_by(SMIN,alpha) %>% 
  summarise(mean_Queen_death_time=mean(QueenTDeath))%>% 
  ggplot(aes(SMIN,mean_Queen_death_time,col=alpha))+
  geom_line()+
  scale_color_discrete()+
  xlab("minimal worker size")+
  ylab("avg. death time of Queen")+ theme_classic()


ggsave("Desktop/R_modeling_course/Protokoll/Q_mort_plot.png",plot = Q_mort_plot,device = 'png')
