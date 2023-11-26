
##Created repeated data function
repeated_data<-function(long_data,pseudo){
      temp<-long_data%>%left_join(long_data,join_by(id==id))
      temp1<-temp%>%filter(period.x==(period.y+1))
      temp1<-temp1%>%left_join(long_data,join_by(id==id))
      temp1<-temp1%>%filter(period.y==(period+1))



      if(pseudo==-1){
        temp2<-temp%>%filter(period.x==2 & period.y==1)
        temp3<-temp%>%filter(period.x==1 & period.y==1)
        temp3<-temp3[,1:9]
        temp_final<-bind_rows(temp1,temp2)
        temp_final<-bind_rows(temp_final,temp3 )
      }else{
        if(pseudo==1){
          temp2<-temp%>%filter(period.x==0 & period.y==-1)
          temp3<-temp%>%filter(period.x==-1 & period.y==-1)
          temp3<-temp3[,1:9]
          temp_final<-bind_rows(temp1,temp2)
          temp_final<-bind_rows(temp_final,temp3)
        }else{
          temp2<-temp%>%filter(period.x==1 & period.y==0)
          temp3<-temp%>%filter(period.x==0 & period.y==0)
          temp3<-temp3[,1:9]
          temp_final<-bind_rows(temp1,temp2)
          temp_final<-bind_rows(temp_final,temp3)
        }
      }
      temp_final[is.na(temp_final)] <- 0
      return(temp_final)
}



##DGP estimation
DGP_estimation<-function(sim_result){
      final<-sim_result$final
      study_tv<-sim_result$study_tv
      full<-TRUE


      ##Repeated Data for non_jumping process
      #First, get the baseline information
      baseline_data<-final%>%select("id", "pre_visits", "age", "bmi", "sex", "race", "data_provider",
                                    "tobacco", "obesity", "diabetes", "lung_disease", "hypertension",
                                    "depression", "corticosteroids", "asthma", "prior_vax", "vax1",
                                    "vax2", "vax3", "vax4", "vax5", "vax6", "vax7", "vax8", "community_poverty",
                                    "community_social")
      #Create the dummy variables for the later fit
      baseline_data<-baseline_data%>%mutate(race1=as.numeric(race==1),race2=as.numeric(race==2),race3=as.numeric(race==3),race4=as.numeric(race==4),race5=as.numeric(race==5))%>%select(-race)




      #Use the long formatted data, which includes patients' history up till and including death
      study_tv_used<-as.data.frame(study_tv%>%select(-c("vax1", "vax2", "vax3", "vax4", "vax5", "vax6", "vax7", "vax8","covid_lag",
                                                        "p_covid", "p_vax", "p_pasc", "p_death", "p_visit")))

      study_tv_used$period<-as.numeric(gsub("t_","",study_tv_used$period))




      #dput(names(temp_final))
      temp_final<-repeated_data(study_tv_used,pseudo=-1)
      #Incoporate baseline
      temp_final<-temp_final%>%left_join(baseline_data,by="id")

      #Get rid of the redundant time information and the death information (since not died)
      #ALSO, make use of last time points "last_vax.y", "last_covid.y", "vax_count.y", "time_since_exposure.y",
      if(!full){
        visit_data<-temp_final%>%select(-c("id",
                                           "covid.x", "vax.x", "metformin.x", "paxlovid.x", "pasc.x", "death.x",
                                           "last_vax.x", "last_covid.x", "vax_count.x", "time_since_exposure.x",
                                           "period.y","death.y",
                                           "period","death","last_vax", "last_covid", "vax_count", "time_since_exposure"))
      }
      #Further, only select out those who have visited for covid, vax, paxlovid
      #It is observed covid given your visit
      covid_data<-temp_final%>%select(-c("id",
                                         "vax.x", "metformin.x", "paxlovid.x", "pasc.x", "death.x",
                                         "last_vax.x", "last_covid.x", "vax_count.x", "time_since_exposure.x",
                                         "period.y","death.y",
                                         "period","death","last_vax", "last_covid", "vax_count", "time_since_exposure"))
      covid_data<-covid_data%>%filter(obs_period.x==1) #Think about this!!! Valid or not to only based on the observed covid and observed history!!! We can correct using some external data modeling, IPTW to make it better.

      #Vaccinated always at period 1.
      vax_data<-temp_final%>%select(-c("id",
                                       "metformin.x", "paxlovid.x", "pasc.x", "death.x",
                                       "last_vax.x", "last_covid.x", "vax_count.x", "time_since_exposure.x",
                                       "period.y","death.y",
                                       "period","death","last_vax", "last_covid", "vax_count", "time_since_exposure"))
      vax_data<-vax_data%>%filter(obs_period.x==1 & period.x!=1)

      metformin_data<-temp_final%>%select(-c("id",
                                             "paxlovid.x", "pasc.x", "death.x",
                                             "last_vax.x", "last_covid.x", "vax_count.x", "time_since_exposure.x",
                                             "period.y","death.y",
                                             "period","death","last_vax", "last_covid", "vax_count", "time_since_exposure"))
      metformin_data<-metformin_data%>%filter(obs_period.x==1)

      paxlovid_data<-temp_final%>%select(-c("id",
                                            "pasc.x", "death.x",
                                            "last_vax.x", "last_covid.x", "vax_count.x", "time_since_exposure.x",
                                            "period.y","death.y",
                                            "period","death","last_vax", "last_covid", "vax_count", "time_since_exposure"))
      paxlovid_data<-paxlovid_data%>%filter(obs_period.x==1)

      #Logistic glmnet
      if(!full){
        visit_fit<-cv.glmnet(x=as.matrix(visit_data%>%select(-obs_period.x)),y=visit_data$obs_period.x, family = "binomial")
      }
      covid_fit<-cv.glmnet(x=as.matrix(covid_data%>%select(-covid.x)),y=covid_data$covid.x, family = "binomial")
      metformin_fit<-cv.glmnet(x=as.matrix(metformin_data%>%select(-metformin.x)),y=metformin_data$metformin.x, family = "binomial")
      vax_fit<-cv.glmnet(x=as.matrix(vax_data%>%select(-vax.x)),y=vax_data$vax.x, family = "binomial")
      paxlovid_fit<-cv.glmnet(x=as.matrix(paxlovid_data%>%select(-paxlovid.x)),y=paxlovid_data$paxlovid.x, family = "binomial")


      ##Repeated Data for jumping processes
      death_data<-temp_final%>%select(-c("id",
                                         "last_vax.x", "last_covid.x", "vax_count.x", "time_since_exposure.x",
                                         "period.y","death.y",
                                         "period","death","last_vax", "last_covid", "vax_count", "time_since_exposure"))

      #pasc
      ##long format
      temp_pasc<-study_tv_used%>%filter(pasc==1)%>%group_by(id)%>%summarise(min_period=min(period))
      temp_pasc<-study_tv_used%>%left_join(temp_pasc,by="id")
      temp_pasc[is.na(temp_pasc)] <- 100
      temp_pasc<-temp_pasc%>%filter(period<=min_period)%>%select(-min_period)

      ##2 time points history
      temp_final<-repeated_data(temp_pasc,pseudo=-1)
      #Incoporate baseline
      temp_final<-temp_final%>%left_join(baseline_data,by="id")

      ##Relevant part
      pasc_data<-temp_final%>%select(-c("id","death.x",
                                        "last_vax.x", "last_covid.x", "vax_count.x", "time_since_exposure.x",
                                        "period.y","pasc.y", "death.y",
                                        "period","pasc", "death", "last_vax", "last_covid", "vax_count", "time_since_exposure"))
      pasc_data<-pasc_data%>%filter(obs_period.x==1)

      #Fit logistic glmnet
      death_fit<-cv.glmnet(x=as.matrix(death_data%>%select(-death.x)),y=death_data$death.x, family = "binomial")
      pasc_fit<-cv.glmnet(x=as.matrix(pasc_data%>%select(-pasc.x)),y=pasc_data$pasc.x, family = "binomial")

      result<-list(visit_fit=visit_fit,covid_fit=covid_fit,vax_fit=vax_fit,metformin_fit=metformin_fit,
                   paxlovid_fit=paxlovid_fit,death_fit=death_fit,pasc_fit=pasc_fit)
      return(result)
}




##Sim for each individual's observation at a given time point provided history
sim_help<-function(temp_final,long_data_temp,t,full=TRUE,fit_list){
        visit_fit<-fit_list$visit_fit
        covid_fit<-fit_list$covid_fit
        vax_fit<-fit_list$vax_fit
        metformin_fit<-fit_list$metformin_fit
        paxlovid_fit<-fit_list$paxlovid_fit
        death_fit<-fit_list$death_fit
        pasc_fit<-fit_list$pasc_fit

        if(!full){
          #Only generate visits when t>1, because it is 1 for t=1
          if(t>=2){
            #Vax sample at time t
            visit_temp<-temp_final%>%select(-c("id",
                                               "covid.x", "vax.x", "metformin.x", "paxlovid.x", "pasc.x", "death.x",
                                               "last_vax.x", "last_covid.x", "vax_count.x", "time_since_exposure.x",
                                               "period.y","death.y",
                                               "period","death","last_vax", "last_covid", "vax_count", "time_since_exposure"))
            visit_sampled<-rbinom(1,1,predict(visit_fit,newx=as.matrix(visit_temp%>%select(-obs_period.x)),s="lambda.min", type = "response"))
            long_data_temp$obs_period[long_data_temp$period==t]<-visit_sampled
            temp_final$ obs_period.x[temp_final$period.x==t]<- visit_sampled
          }else{
            visit_sampled<-1
          }
        }else{
          #Always visit
          visit_sampled<-1
          long_data_temp$obs_period[long_data_temp$period==t]<-visit_sampled
          temp_final$ obs_period.x[temp_final$period.x==t]<- visit_sampled
        }


        if(visit_sampled==0){
          long_data_temp[which(long_data_temp$period==t),c("covid", "vax", "pasc", "metformin", "paxlovid")]<-
            long_data_temp[which(long_data_temp$period==t-1),c("covid", "vax", "pasc", "metformin","paxlovid")]
          temp_final[1,c("covid.", "vax.", "pasc.x", "metformin.x", "paxlovid.x")]<-
            long_data_temp[which(long_data_temp$period==t-1),c("covid", "vax", "pasc", "metformin","paxlovid")]
        }else{
          #Covid sample
          covid_temp<-temp_final%>%select(-c("id",
                                             "vax.x", "metformin.x", "paxlovid.x", "pasc.x", "death.x",
                                             "last_vax.x", "last_covid.x", "vax_count.x", "time_since_exposure.x",
                                             "period.y","death.y",
                                             "period","death","last_vax", "last_covid", "vax_count", "time_since_exposure"))
          #Sample the covid at time t
          covid_sampled<-rbinom(1,1,predict(covid_fit,newx=as.matrix(covid_temp%>%select(-covid.x)),s="lambda.min", type = "response"))
          #Impute it in the data structure
          long_data_temp$covid[long_data_temp$period==t]<-covid_sampled
          temp_final$covid.x[temp_final$period.x==t]<-covid_sampled

          #Vax sample at time t
          if(t>1){
            vax_temp<-temp_final%>%select(-c("id",
                                             "metformin.x", "paxlovid.x", "pasc.x", "death.x",
                                             "last_vax.x", "last_covid.x", "vax_count.x", "time_since_exposure.x",
                                             "period.y","death.y",
                                             "period","death","last_vax", "last_covid", "vax_count", "time_since_exposure"))
            vax_sampled<-rbinom(1,1,predict(vax_fit,newx=as.matrix(vax_temp%>%select(-vax.x)),s="lambda.min", type = "response"))
            long_data_temp$vax[long_data_temp$period==t]<-vax_sampled
            temp_final$vax.x[temp_final$period.x==t]<-vax_sampled
          }

          #Pasc at time t
          if(temp_final$pasc.y==0 &temp_final$pasc==0){
            pasc_temp<-temp_final%>%select(-c("id","death.x",
                                              "last_vax.x", "last_covid.x", "vax_count.x", "time_since_exposure.x",
                                              "period.y","pasc.y", "death.y",
                                              "period","pasc", "death", "last_vax", "last_covid", "vax_count", "time_since_exposure"))
            pasc_sampled<-rbinom(1,1,predict(pasc_fit,newx=as.matrix(pasc_temp%>%select(-pasc.x)),s="lambda.min", type = "response"))
          }else{
            pasc_sampled<-1
          }
          long_data_temp$pasc[long_data_temp$period==t]<-pasc_sampled
          temp_final$pasc.x[temp_final$period.x==t]<-pasc_sampled

          #Metformin sample at time t
          metformin_temp<-temp_final%>%select(-c("id",
                                                 "paxlovid.x", "pasc.x", "death.x",
                                                 "last_vax.x", "last_covid.x", "vax_count.x", "time_since_exposure.x",
                                                 "period.y","death.y",
                                                 "period","death","last_vax", "last_covid", "vax_count", "time_since_exposure"))
          metformin_sampled<-rbinom(1,1,predict(metformin_fit,newx=as.matrix(metformin_temp%>%select(-metformin.x)),s="lambda.min", type = "response"))
          long_data_temp$metformin[long_data_temp$period==t]<-metformin_sampled
          temp_final$metformin.x[temp_final$period.x==t]<-metformin_sampled

          #paxlovid
          paxlovid_temp<-temp_final%>%select(-c("id",
                                                "pasc.x", "death.x",
                                                "last_vax.x", "last_covid.x", "vax_count.x", "time_since_exposure.x",
                                                "period.y","death.y",
                                                "period","death","last_vax", "last_covid", "vax_count", "time_since_exposure"))
          paxlovid_sampled<-rbinom(1,1,predict(paxlovid_fit,newx=as.matrix(paxlovid_temp%>%select(-paxlovid.x)),
                                               s="lambda.min", type = "response"))
          long_data_temp$paxlovid[long_data_temp$period==t]<-paxlovid_sampled
          temp_final$paxlovid.x[temp_final$period.x==t]<-paxlovid_sampled
        }

        #Death hasn't jump at time point t
        death_temp<-temp_final%>%select(-c("id",
                                           "last_vax.x", "last_covid.x", "vax_count.x", "time_since_exposure.x",
                                           "period.y","death.y",
                                           "period","death","last_vax", "last_covid", "vax_count", "time_since_exposure"))
        death_sampled<-rbinom(1,1,predict(death_fit,newx=as.matrix(death_temp%>%select(-death.x)),s="lambda.min", type = "response"))
        long_data_temp$death[long_data_temp$period==t]<-death_sampled
        temp_final$death.x[temp_final$period.x==t]<-death_sampled

        #handle last_vax and vax_count
        if(t>1){
          if(vax_sampled==1){
            last_vax<-30*t
            vax_count<-long_data_temp$vax_count[long_data_temp$period==(t-1)]+1
          }else{
            last_vax<-long_data_temp$last_vax[long_data_temp$period==(t-1)]
            vax_count<-long_data_temp$vax_count[long_data_temp$period==(t-1)]
          }
          long_data_temp$last_vax[long_data_temp$period==t]<-last_vax
          temp_final$last_vax.x[temp_final$period.x==t]<-last_vax

          long_data_temp$vax_count[long_data_temp$period==t]<-vax_count
          temp_final$vax_count.x[temp_final$period.x==t]<-vax_count
        }

        #handle last_covid
        if(covid_sampled==1){
          last_covid<-30*t
        }else{
          if(t==1){
            last_covid<- -999
          }else{
            last_covid<-long_data_temp$last_vax[long_data_temp$period==(t-1)]
          }
        }
        long_data_temp$last_covid[long_data_temp$period==t]<-last_covid
        temp_final$last_covid.x[temp_final$period.x==t]<-last_covid
        #handle time_since_exposure
        if(t>1){
          if(long_data_temp$covid[long_data_temp$period==(t-1)] ==1){
            time_since_exposure<-30
          }
          else{
            time_since_exposure<-long_data_temp$time_since_exposure[long_data_temp$period==(t-1)]+30
          }
          long_data_temp$time_since_exposure[long_data_temp$period==t]<-time_since_exposure
          temp_final$time_since_exposure.x[temp_final$period.x==t]<-time_since_exposure
        }

        return(list(temp_final,long_data_temp))
}


##Sim each individual's whole observation

sim_individual<-function(id,fit_list){
        id1<-id
        baseline_temp<-baseline_data[sample(1:length(baseline_data), 1, replace=TRUE),]
        baseline_temp<-baseline_temp%>%mutate(id=id1)
        #t=1, seq: covid, vax, pasc, metformin, paxlovid, death
        #Artificial long data to initiate the start with time perirod 0,-1
        #Always visit at month one, and always vaccinated at time 1
        long_data_temp<-data.frame(id=id,period=1,obs_period=1,covid=-1,vax=1,pasc=-1,metformin=-1,paxlovid=-1,death=-1,last_vax=0,last_covid=-999,vax_count=1,time_since_exposure=0)
        long_data_temp<-rbind(long_data_temp,c(id,0,0,0,0,0,0,0,0,0,0,0,0))
        long_data_temp<-rbind(long_data_temp,c(id,-1,0,0,0,0,0,0,0.0,0,0,0))
        #Change to repeated data format
        temp_final<-repeated_data(long_data_temp,pseudo = 1)
        temp_final<-temp_final%>%filter(period.x==1)
        temp_final<-temp_final%>%left_join(baseline_temp,by="id")#Incoporate baseline


        temp_result<-sim_help(temp_final,long_data_temp,t=1,fit_list=fit_list)
        long_data_temp<-temp_result[[2]]
        long_data_sim<-long_data_temp%>%filter(period==1)
        death_happened<-sum(long_data_sim$death)

        #t=2
        if(death_happened==0){
          long_data_temp<-rbind(long_data_temp,c(id,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1))
          long_data_temp<-long_data_temp%>%filter(period>(2-3))
          temp_final<-repeated_data(long_data_temp,pseudo = 2)
          temp_final<-temp_final%>%filter(period.x==2)
          temp_final<-temp_final%>%left_join(baseline_temp,by="id")#Incoporate baseline

          temp_result<-sim_help(temp_final,long_data_temp,t=2,fit_list=fit_list)
          long_data_temp<-temp_result[[2]]
          long_data_sim<-rbind(long_data_sim,long_data_temp%>%filter(period==2))
          death_happened<-sum(long_data_sim$death)

          #t>3
          t_current<-3
          while(death_happened==0 & t_current<=15){
            long_data_temp<-rbind(long_data_temp,c(id,t_current,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1))
            long_data_temp<-long_data_temp%>%filter(period>(t_current-3))
            temp_final<-repeated_data(long_data_temp,pseudo = -1)
            temp_final<-temp_final%>%filter(period.x==t_current)
            temp_final<-temp_final%>%left_join(baseline_temp,by="id")#Incoporate baseline

            temp_result<-sim_help(temp_final,long_data_temp,t=t_current,fit_list=fit_list)
            long_data_temp<-temp_result[[2]]
            long_data_sim<-rbind(long_data_sim,long_data_temp%>%filter(period==t_current))
            t_current<-t_current+1
            death_happened<-sum(long_data_sim$death)
          }
        }
        long_data_sim<-long_data_sim%>%left_join(baseline_temp,by="id")#Incoporate baseline
        return(long_data_sim)
}
