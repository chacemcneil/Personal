# Implement a backward elimination step-wise regression
 library(data.table)
 
 
 
 
 backward <- function(FUN,formula,data,threshold=0.10,keep=NULL,keepintercept=T,...) {
   removed <- NULL
   reduced <- F
   data <- as.data.frame(data)
   if (class(keep)=="numeric")
     keep <- colnames(data)[keep]
   if(keepintercept)
     keep <- c("(Intercept)",keep)
   
   X <- as.data.frame(model.matrix(formula[-2],data))
   #X[[as.character(formula[[2]])]] <- data[[as.character(formula[[2]])]]
   #keepind <- match(keep,colnames(X))
   #currind <- (1:ncol(X))[-1]
   formula2 <- formula
   formula2[[3]] <- as.name(".")
   
   if (length(formula[[2]])>1)
     resp <- as.character(formula[[2]][[2]])
   else
     resp <- as.character(formula[[2]])
   
   retained <- c(resp,grep(resp,colnames(X),value=T,invert=T))
   
   while(!reduced) {
     mod <- FUN(formula2,data=X[,retained],family=...)
     #mod <- FUN(formula2,data=X[,retained],family="binomial")
     
     tab <- summary(mod)$coef
     vars <- row.names(tab)
     ps   <- tab[,4]
     maxp <- max(ps[! vars %in% keep], na.rm=T)
     removed <- c(removed,names(ps[is.na(ps)]))
     potentialvars <- vars[!vars %in% keep & !is.na(ps)]
     if(maxp > threshold) {
       remove.name <- names(which.max(ps[! vars %in% keep]))
       removed <- c(removed,remove.name)
     }else {
       remove.name <- NULL
       reduced <- T
     }
     #print(remove.name)
     
     retained <- c(resp, keep, setdiff(vars[!(vars %in% keep | is.na(ps))], remove.name) )
     retained <- gsub("`","",retained)
     #X <- as.data.frame(model.matrix(formula2[-2],data[,retained]))
   }
   
   retained <- setdiff(retained,c("(Intercept)"))
   retainedorig <- findnames(retained,colnames(data))
   #print(retained)
   
   list(model=FUN(formula=formula2,data=X[,retained],...), 
        formula=as.formula(paste(as.character(formula2)[2],"~",paste(retainedorig[-1],collapse="+"))), data=X[,retained],removed=removed)
 }
 
 findnames <- function(retained,vars) {
   res <- pmatch(vars,retained)
   result <- retained
   result[res[!is.na(res)]] <- vars[!is.na(res)]#[res[!is.na(res)]]
   result
 }
 
 
 #vars <- c(cond,indep,"Surgery")
 #retained <- c("Age","GenderM","SurgeryTKA","LymphPcnt","Something not there")
 
 
 #md <- backward(glm,InpatientLengthOfStay~.,data=ta[Surgery=="TKA",c("InpatientLengthOfStay",cond,indep),with=F], keep=indep, family="quasipoisson")
 #md$formula
 #summary(md$model)
 #mod <- glm(InpatientLengthOfStay ~ BMI_Before_Surgery+Albumin_Results+LymphPcnt+LymphCount+Age+Angina_Ind+CHF_Ind+COPD_Ind+Paralysis_Ind+PVD_Ind,data=ta[Surgery=="TKA"],family="quasipoisson")
 #mod <- glm(InpatientLengthOfStay ~ BMI_Before_Surgery+Albumin_Results+LymphPcnt+Age+CHF_Ind+COPD_Ind+Paralysis_Ind+PVD_Ind,data=ta[Surgery=="TKA"],family="quasipoisson")
 #summary(mod)
 #exp(summary(mod)$coef[,1] + (qt(.975,647)*summary(mod)$coef[,2] %o% (-1:1)))
 # Small effect of BMI and effect of Albumin_results
 
 
 
 
 
 
 
 
 
# End script
 