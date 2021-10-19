## Multiple response power analysis
library(multcomp)

# n_per_group        is the number of participants in each of the 4 study arms
# Plac_rate          is the proportion of participants in the double placebo arm that will expereince an event
# N_sims             is the number of simulations to run to estimate power
# Risk_red           is the relative risk reduction of ACEi vs placebo and Statin vs placebo
# Risk_red+Risk_red2 is the relative risk reduction of polypill vs ACEi and polypill vs Statin


GetPow=function(n_per_g,Plac_rate,N_sims=500,Risk_red=0.2,Risk_red2=0.05){
  Out=sapply(1:N_sims,function(x){
    #set event rates for each group
    ACEI_Rate=Plac_rate*(1-Risk_red)
    Statin_Rate=Plac_rate*(1-Risk_red)
    Polypill_Rate=Plac_rate*(1-Risk_red-Risk_red2)
    #simulate the number of events in each group
    N_symp = c(rbinom(1,n_per_g,ACEI_Rate),
               rbinom(1,n_per_g,Statin_Rate),
               rbinom(1,n_per_g,Polypill_Rate),
               rbinom(1,n_per_g,Plac_rate))
    #create a vector of outcomes based on the assumed group size and simulated number of events
    Y=c(rep(0,n_per_g-N_symp[1]),rep(1,N_symp[1]),
        rep(0,n_per_g-N_symp[2]),rep(1,N_symp[2]),
        rep(0,n_per_g-N_symp[3]),rep(1,N_symp[3]),
        rep(0,n_per_g-N_symp[4]),rep(1,N_symp[4]))
    #create a design matrix based on the assumed group size and simulated number of events
    X=cbind("ACEI"=c(rep(1,n_per_g),rep(0,n_per_g),rep(1,n_per_g),rep(0,n_per_g)),
            "Statin"=c(rep(0,n_per_g),rep(1,n_per_g),rep(1,n_per_g),rep(0,n_per_g)),
            "Interaction"=c(rep(0,2*n_per_g),rep(1,n_per_g),rep(0,n_per_g)))
    #fit a logistic regression model
    Fit=glm(Y~X, family="binomial")
    #test ACEi and Statin effect and use Shaffer to control for 2 comparisons
    Summ=summary(glht(Fit,matrix(c(0,1,0,0,
                                   0,0,1,0),2,4,byrow=TRUE)),test=adjusted("Shaffer"))
    #save adjusted p values from primary and unadjusted p-values secondary outcome tests
    ACE_p = Summ$test$pvalues[1]
    Statin_p = Summ$test$pvalues[2]
    Poly_p = summary(glht(Fit,matrix(c(0,1,1,1),1,4,byrow=TRUE)))$test$pvalues[1]
    Poly_v_ACE_p = summary(glht(Fit,matrix(c(0,0,1,1),1,4,byrow=TRUE)))$test$pvalues[1]
    Poly_v_Statin_p = summary(glht(Fit,matrix(c(0,1,0,1),1,4,byrow=TRUE)))$test$pvalues[1]
    return(c("ACEi"=ACE_p,"Statin"=Statin_p,"Poly"=Poly_p,
             "Poly_v_ACEi"=Poly_v_ACE_p,"Poly_v_Statin"=Poly_v_Statin_p))
  })
  #estimate power using saved p-values
  return(apply(Out,1,function(x){mean(ifelse(x<0.05,1,0))}))
}

#scenario with a 3.2% symptomatic infection rate
N1=seq(11300,11800,by=100)
Res1=round(sapply(N1,GetPow,Plac_rate=0.032,N_sims=1000),3)*100
colnames(Res1)=N1
Res1
#scenario with a 5% symptomatic infection rate
N2=seq(7200,7800,by=100)
Res2=round(sapply(N2,GetPow,Plac_rate=0.05,N_sims=1000),3)*100
colnames(Res2)=N2
Res2
#scenario with a 10% symptomatic infection rate
N3=seq(3200,3700,by=100)
Res3=round(sapply(N3,GetPow,Plac_rate=0.1,N_sims=1000),3)*100
colnames(Res3)=N3
Res3
#scenario with a 15% symptomatic infection rate
N4=seq(2000,2500,by=100)
Res4=round(sapply(N4,GetPow,Plac_rate=0.15,N_sims=1000),3)*100
colnames(Res4)=N4
Res4

n_per_g = N4[1]
