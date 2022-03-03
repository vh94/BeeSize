###  COLONY FUNCTION : 
  # QUEEN_size<-15
  # ## Smin
  # SMIN=5
  # ## other
  # runs = 20
  # alpha = 1
  # PROD = 0.2
  # MORT = 0.05
  # TMAX = 45
  # DMIN = 10
  # MAXEGG =10
  # 
  # ## tracker for debugging
  # wn<-numeric(TMAX)
  # res_c<-numeric(TMAX)
  # bedarf_c<-numeric(TMAX)
  # mean_ws_c<-numeric(TMAX)
  # death_c<-numeric(TMAX)
  # egg_c<-numeric(TMAX)
  # adult_c<- numeric(TMAX)
  # Q_death<-numeric(TMAX)
  # res_to_ad<-numeric(TMAX)
  # n_pupp_smin<-numeric(TMAX)

COLONY <- function(SMIN,DMIN,alpha,PROD,MORT,TMAX,MAXEGG,QUEEN_size) {
    wn<-numeric(TMAX)
    BEES<-data.frame(wbirth=numeric(),wsize=numeric(),wadult=numeric(),wdeath=numeric())
    rbratio<-SMIN/DMIN
    Q_death<-TMAX
    ## Loop over Timesteps
    for (t in 1:TMAX) {
      ## 1   :: volkszählung
      wn[t]<-nrow(BEES)
      ## 2   ::  Becoming adult
      if (wn[t]>0) {
        x<-((BEES$wbirth <= (t-DMIN)) & (BEES$wsize >= SMIN))
        BEES$wadult<-as.numeric(x) 
      }
     # adult_c[t]<-sum(BEES$wadult)
      ## 3 bringing in res:
      res<-sum(BEES[BEES$wadult==1,]$wsize^alpha)*PROD
      #### 4  QUEENSTUFF:::
      ## Queen can only forage, if alive and no workers present:
      if (QUEEN_died==0 & sum(BEES$wadult)<1 ) {
        res<-((QUEEN_size^alpha)*PROD) # Queen only forages if no one else there
        QUEEN_died<-rbinom(1, 1, MORT) 
        if (QUEEN_died) {Q_death<-t }
      }
      ## 5 calculate number of brood and resources needed
      brut<-nrow(BEES[BEES$wsize<SMIN,])
    #  n_pupp_smin[t]<-nrow(BEES[which(BEES$wsize>=SMIN & ((t-BEES$wbirth)<DMIN)),])
      bedarf<-(SMIN/DMIN)*brut
      n.layed<-0
      #add eggs to colony
      if (QUEEN_died==0) {
        n.layed<-ifelse(res>bedarf,1,0)*round((res-bedarf)/(SMIN/DMIN),digits = 0)
      }
      n.layed<-min(n.layed,MAXEGG)
      #print(paste("time:",t,"EGGS",egg_c[t])) 
      if(n.layed > 0){
        EGGS<-matrix(0,n.layed,4)
        EGGS[,1]<-t
        colnames(EGGS)<-colnames(BEES)
        BEES<-rbind(BEES,EGGS) 
      }
      brut<-nrow(BEES[BEES$wsize<SMIN,])
      ### Feeding the brood
      if (brut>0) {
        BEES[BEES$wsize<SMIN,]$wsize<-BEES[BEES$wsize<SMIN,]$wsize+(res/brut)
      }
      ## Dying
      if(sum(BEES$wadult)>=1){
        # only let adult workers die
        BEES$wdeath[1:length(BEES$wdeath)] <- rbinom(nrow(BEES), 1, MORT) * (BEES$wadult[1:length(BEES$wdeath)]==1)
      }
      BEES<-subset(BEES[1:length(BEES$wdeath),],wdeath == 0)## only choose alive workers

      ### Tracking::
      # death_c[t]<-sum(BEES$wadult)
      # wn[t]<-nrow(BEES)
      # res_to_ad[t]<-sum(SMIN-BEES[BEES$wsize<SMIN,]$wsize)## resources missing for brood
      # J[t]<-(((SMIN^alpha)*wn[t]*PROD)*1/MORT)-res_to_ad[t] ## optimization criterion J
      # egg_c[t]<-n.layed
      # bedarf_c[t]<-bedarf
      # res_c[t]<-res
      
  
    }
    ## TMAX :: Performance::
    ## single worker benefit at SIZE SMIN :: (SMIN^alpha)*wnT*PROD
    wnT<-nrow(BEES)
    res_to_ad<-sum(SMIN-BEES[BEES$wsize<SMIN,]$wsize)## resources missing for brood
    J<-(((SMIN^alpha)*wnT*PROD)*1/MORT)-res_to_ad ## optimization criterion J
    return(c(wnT,J, Q_death))
}






#COLONY(SMIN=10,DMIN,alpha=1.2,PROD,MORT=0.05,TMAX,MAXEGG,QUEEN_size)
# 
# 
# cbind(wn,res_c,
#       bedarf_c,
#       death_c,
#       n_pupp_smin,
#       egg_c,
#       adult_c,
#       Q_death,
#       res_to_ad,
#       J)

