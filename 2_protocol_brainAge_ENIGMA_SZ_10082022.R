#### SCZ brain age project
# written by LAura HAn (L.Han@ggzingeest.nl) and Esther Walton (waltonesther@gmail.com)
# Revised by Constantinos Constantinides (cc2557@bath.ac.uk) on 10/08/2022 

# setwd

#create log file
messages=file("2_BrainAGE_SZ.log", open="wt")
#rest=file("rest.Rout", open="wt")
sink(messages, type="message")
sink(messages, type="output")


# install/load libraries
cat("Prep: installing/loading libraries\n")

load.lib <- function(x){
  for( i in x ){
    if( ! require( i , character.only = TRUE ) ){
      install.packages( i , dependencies = TRUE )
      #require( i , character.only = TRUE )
      library(i)
    }
  }
}

#  Then try/install packages...
load.lib( c("ppcor" , "lsmeans" , "multcomp","data.table","plyr","ModelMetrics",
       "caret","gridExtra","Hmisc","pastecs","psych","ggplot2") )


# source functions
cat("Prep: sourcing functions\n")

source("get.means.R")  
source("prepare.files.R")
source("model.fits.brainAge.R")
source("model.fits.brainAge.dx.R")
source("convert.PANSS.SANS.SAPS.R")
source("winsorize.R")

# derive mean volumnes / thickness and surfarea

cat("Reading in data\n")
cat("deriving mean values for thickness, surface and volume\n") 
  
Thick=get.means("CorticalMeasuresENIGMA_ThickAvg.csv") 
Thick$ICV=NULL  

Surf=get.means("CorticalMeasuresENIGMA_SurfAvg.csv") 
Surf$ICV=NULL 

Vol=get.means("SubcorticalMeasuresENIGMA_VolAvg.csv") 

# merged all together
TS=merge(Thick,Surf,by="row.names")

TSV=merge(TS,Vol,by.x="Row.names",by.y="row.names")

# read in covariates
Covs <- read.csv("Covariates.csv"); #Read in the covariates file

# get for missings in Dx, Age, Sex
if (length(table(is.na(Covs[,c("Dx","Sex","Age")])))>1){
  stop("missing data in Dx, Age or Sex")
}

# Check that all of the required columns are present
mcols=c("SubjID","Dx","Sex","Age","SANS_TOTAL","SANS_GLOBAL","PANSS_NEG","SAPS_TOTAL","SAPS_GLOBAL","PANSS_POS","PANSS_TOTAL","LENGTH_OF_ILLNESS","AO","IQ","HAND","PARENT_SES","ETHNICITY","CPZ",
        "SUBTYPE","MEDICATION")
colind=match(mcols,names(Covs))
if(length(which(is.na(colind))) > 0){
  stop('At least one of the required columns in your Covariates.csv file is missing. Make sure that the column names are spelled exactly as listed\n
       It is possible that the problem column(s) is: ', mcols[which(is.na(colind))])
}

# check whether clinical variables are coded NA for HCs
HC=Covs[Covs$Dx==0,]
if (!(all(is.na(HC[,c("SANS_TOTAL","SANS_GLOBAL","PANSS_NEG","SAPS_TOTAL","SAPS_GLOBAL","PANSS_POS","PANSS_TOTAL","LENGTH_OF_ILLNESS","AO","CPZ","SUBTYPE","MEDICATION")])))){
  stop("Looks like some of your HCs have entries for patient-only variables. Please code as 'NA'")
}

rm(HC)

# create dummy variable for Site
#if ("Site" %in% names(Covs)){
#  Site.f = factor(Covs$Site)
  #Covs$Site=NULL
  
#  if (length(grep("Site.f",colnames(Covs)))>0){
#    site_regr=paste(grep("Site.f",colnames(Covs),value=T),collapse="+")
#  }
  
#  Covs=cbind(Covs,model.matrix(~Site.f)[,-1])
#}

if ("Site" %in% names(Covs)){
  Covs$Site.f = as.factor(Covs$Site)
  Site.f = Covs$Site.f
  Covs=cbind(Covs,model.matrix(~Site.f)[,-1])
  Covs <- subset(Covs, select = -c(Site.f))
  if (length(grep("Site.f",colnames(Covs)))>0){
    site_regr=paste(grep("Site.f",colnames(Covs),value=T),collapse="+")
  }
}

# define age^2 for later models
Covs$Age2=Covs$Age^2

# exclude age<18y and >75y from data
if (min(Covs[,c("Age")])<18 | max(Covs[,c("Age")])>75){
  cat("excluding subjects younger than 18y and/or older than 75y \n")
  Covs=Covs[(Covs$Age>=18 & Covs$Age<=75),]
}


# Check for duplicated SubjIDs that may cause issues with merging data sets.
if(anyDuplicated(Covs[,c("SubjID")]) != 0) { stop('You have duplicate SubjIDs in your Covariates.csv file.\nMake sure there are no repeat SubjIDs.') }



#combine the files into one dataframe
data = merge(Covs, TSV, by.x="SubjID", by.y="Row.names")

cat("creating csv files for brainAge estimation\n")
df=prepare.files(data,names(Covs))

males=df$males
females=df$females
rm(df)



cat("Step 3: QC and basic stats for your brainAge measure")


model.fits.brainAge(males,"males_raw_out.csv")
model.fits.brainAge(females,"females_raw_out.csv")



model.fits.brainAge.dx(males,"males_raw_out.csv")
model.fits.brainAge.dx(females,"females_raw_out.csv")

cat("Step 4: Run brainAge SCZ analysis\n")

# read in brainAge for males and females and merge together

# males 
output <- read.csv("males_raw_out.csv", header=T, sep="\t")
males$predAge <- output$age_prediction
males$devAge <- (males$predAge-males$Age)
males$AE <- abs(males$devAge)
rm(output)

# females
output <- read.csv("females_raw_out.csv", header=T, sep="\t")
females$predAge <- output$age_prediction
females$devAge <- (females$predAge-females$Age)
females$AE <- abs(females$devAge)
rm(output)

BA=rbind(males,females)

# check that no sample duplication
cat("duplicated IDs: ",which(duplicated(BA$SubjID)),"\n")

# merge covariate file with brainAge measure
data=merge(data[,names(Covs),],BA[,c("SubjID","predAge","devAge","AE")],by="SubjID")

# convert negative / positive PANSS, SANS, SAPS data
  data=convert.PANSS.SANS.SAPS(data)

# rename odd column headers (edited by CC)
if ('SAPS_GLOBAL.1' %in% names(data)){
  data$SAPS_GLOBAL <- NULL
  cat("rename SAPS column \n")
  names(data)[names(data)=="SAPS_GLOBAL.1"] <- "SAPS_GLOBAL"
}

if ('SANS_GLOBAL.1' %in% names(data)){
  data$SANS_GLOBAL <- NULL
  cat("rename SANS column \n")
  names(data)[names(data)=="SANS_GLOBAL.1"] <- "SANS_GLOBAL"
}

# get sum stats and correlations for all variables in covariate file
cat('basic descriptives before winsorizing \n')
sumstats=stat.desc(data[,2:dim(data)[2]])
tmp=apply(as.matrix(data[,2:dim(data)[2]]),2,quantile, na.rm=T)
sumstats=rbind(sumstats[c(1:5,8:10,12:13),],tmp[c(2,4),])


sumstats.SZ=stat.desc(data[data$Dx==1,2:dim(data)[2]])
tmp=apply(as.matrix(data[data$Dx==1,2:dim(data)[2]]),2,quantile, na.rm=T)
sumstats.SZ=rbind(sumstats.SZ[c(1:5,8:10,12:13),],tmp[c(2,4),])


sumstats.HC=stat.desc(data[data$Dx==0,2:dim(data)[2]])
tmp=apply(as.matrix(data[data$Dx==0,2:dim(data)[2]]),2,quantile, na.rm=T)
sumstats.HC=rbind(sumstats.HC[c(1:5,8:10,12:13),],tmp[c(2,4),])
rm(tmp)


categ=c("Dx","Sex","HAND","SUBTYPE","ETHNICITY","MEDICATION")
counts=apply(data[,categ],2,count)
counts.SZ=apply(data[data$Dx==1,categ],2,count)
counts.HC=apply(data[data$Dx==0,categ],2,count)


cat('checking correlations\n')

dat_matrix=data.matrix(data)
correlations=rcorr(dat_matrix)


save(sumstats,sumstats.HC,sumstats.SZ,
     counts,counts.HC,counts.SZ,correlations,
     file="sumstats.bf.wins.Rdata")


# get sum stats and correlations for all variables in covariate file
cat('basic descriptives after winsorizing \n')


# winsorize brain data
data$devAge=winsorize(data$devAge)
data$AE <- abs(data$devAge)

#
sumstats=stat.desc(data[,2:dim(data)[2]])
tmp=apply(as.matrix(data[,2:dim(data)[2]]),2,quantile, na.rm=T)
sumstats=rbind(sumstats[c(1:5,8:10,12:13),],tmp[c(2,4),])


sumstats.SZ=stat.desc(data[data$Dx==1,2:dim(data)[2]])
tmp=apply(as.matrix(data[data$Dx==1,2:dim(data)[2]]),2,quantile, na.rm=T)
sumstats.SZ=rbind(sumstats.SZ[c(1:5,8:10,12:13),],tmp[c(2,4),])


sumstats.HC=stat.desc(data[data$Dx==0,2:dim(data)[2]])
tmp=apply(as.matrix(data[data$Dx==0,2:dim(data)[2]]),2,quantile, na.rm=T)
sumstats.HC=rbind(sumstats.HC[c(1:5,8:10,12:13),],tmp[c(2,4),])
rm(tmp)


categ=c("Dx","Sex","HAND","SUBTYPE","ETHNICITY","MEDICATION")
counts=apply(data[,categ],2,count)
counts.SZ=apply(data[data$Dx==1,categ],2,count)
counts.HC=apply(data[data$Dx==0,categ],2,count)


cat('checking correlations\n')

dat_matrix=data.matrix(data)
correlations=rcorr(dat_matrix)


save(sumstats,sumstats.HC,sumstats.SZ,
     counts,counts.HC,counts.SZ,correlations,
     file="sumstats.after.wins.Rdata")



# BA diff between SCZ and HC, controlling sex and site (if applicable)
cat('model 1: dx and sex\n')

if (exists("site_regr")){
  form <- as.formula(paste("devAge~as.factor(Dx)+as.factor(Sex)+Age+Age2+",site_regr))
  out=lm(formula = form, data = data)
} else {
  out=lm(devAge ~ as.factor(Dx) + as.factor(Sex) +Age+Age2, data = data)
}

save(out,file="devAge_Dx_Sex_ifSite.RData")

# BA correlation with age, (covarying for site, if needed)
cat('model 2: age\n')

if (exists("site_regr")){
  form <- as.formula(paste("devAge~Age+Age2+",site_regr))
  out=lm(formula = form, data = data)
} else {
  out=lm(devAge ~ Age+Age2, data = data)
}

save(out,file="devAge_Age_ifSite.RData")


### in patients only:
data_SCZ=data[data$Dx==1,]
dim(data_SCZ)

# BA correlation with DURILL, AO, neg/pos/total symptoms, CPZ

for (i in c("LENGTH_OF_ILLNESS","AO","SANS_GLOBAL","SAPS_GLOBAL","PANSS_TOTAL","CPZ")){
  cat("model: ",i,"\n")
  if (i %in% names(data_SCZ)){
    if(!all(is.na(data_SCZ[,i]))){
        form <- as.formula(paste("devAge~",i," + Age + Age2"))
        out=lm(formula = form, data = data_SCZ)
      } else {
        form <- as.formula(paste("devAge~",i," + Age + Age2"))
        out=lm(formula = form, data = data_SCZ)
      }
      save(out,file=paste0("devAge_",i,".RData"))
      rm(out)
    }
  }



# BA diff between AP in SCZ only
cat('model AP\n')

if ("MEDICATION" %in% names(data_SCZ) & length(table(data_SCZ$MEDICATION))>1){
    form <- as.formula(paste("devAge~as.factor(MEDICATION)+Age+Age2+"))
    out=lm(formula = form, data = data_SCZ)
  } else {
    out=lm(devAge ~ as.factor(MEDICATION)+Age+Age2, data = data_SCZ)
  }
  save(out,file="devAge_MED.RData")
 

############ repeat with HAND

# BA diff between SCZ and HC, controlling sex and site (if applicable)

if ("HAND" %in% names(data) & length(table(data$HAND))>1){
  if(!all(is.na(data[,"HAND"]))){
    
    cat('model 1H: dx and sex\n')
    
    if (exists("site_regr")){
      form <- as.formula(paste("devAge~as.factor(Dx)+as.factor(Sex)+as.factor(HAND)+Age+Age2+",site_regr))
      out=lm(formula = form, data = data)
    } else {
      out=lm(devAge ~ as.factor(Dx) + as.factor(Sex)+as.factor(HAND)+Age+Age2, data = data)
    }
    
    save(out,file="devAge_Dx_Sex_HAND_ifSite.RData")
    
    # BA correlation with age, (covarying for site, if needed)
    cat('model 2H: age\n')
    
    if (exists("site_regr")){
      form <- as.formula(paste("devAge~Age+Age2+as.factor(HAND)+",site_regr))
      out=lm(formula = form, data = data)
    } else {
      out=lm(devAge ~ Age+Age2+as.factor(HAND), data = data)
    }
    
    save(out,file="devAge_Age_HAND_ifSite.RData")
    
    
    ### in patients only:
    data_SCZ=data[data$Dx==1,]
    dim(data_SCZ)
    
    # BA correlation with DURILL, AO, neg/pos/total symptoms, CPZ
    
    for (i in c("LENGTH_OF_ILLNESS","AO","SANS_GLOBAL","SAPS_GLOBAL","PANSS_TOTAL","CPZ")){
      cat("model H: ",i,"\n")
      if (i %in% names(data_SCZ)){
        if(!all(is.na(data_SCZ[,i]))){
            form <- as.formula(paste("devAge~",i,"+as.factor(HAND)+Age+Age2"))
            out=lm(formula = form, data = data_SCZ)
          } else {
            form <- as.formula(paste("devAge~",i,"+as.factor(HAND)+Age+Age2"))
            out=lm(formula = form, data = data_SCZ)
          }
          save(out,file=paste0("devAge_",i,"_HAND.RData"))
          rm(out)
        }
      }
    }
    
    
    # BA diff between AP in SCZ only
    cat('model H AP\n')
    
    if ("MEDICATION" %in% names(data_SCZ) & length(table(data_SCZ$MEDICATION))>1){
        form <- as.formula(paste("devAge~as.factor(MEDICATION)+as.factor(HAND)+Age+Age2"))
        out=lm(formula = form, data = data_SCZ)
      } else {
        out=lm(devAge ~ as.factor(MEDICATION)+as.factor(HAND)+Age+Age2, data = data_SCZ)
      }
      save(out,file="devAge_MED_HAND.RData")
    }  
   

############ repeat with PARENT_SES

# BA diff between SCZ and HC, controlling sex and site (if applicable)

if ("PARENT_SES" %in% names(data)){
 if(!all(is.na(data[,"PARENT_SES"]))){
    
    cat('model 1H: dx and sex\n')
    
    if (exists("site_regr")){
      form <- as.formula(paste("devAge~as.factor(Dx)+as.factor(Sex)+PARENT_SES+Age+Age2+",site_regr))
      out=lm(formula = form, data = data)
    } else {
      out=lm(devAge ~ as.factor(Dx) + as.factor(Sex)+PARENT_SES+Age+Age2, data = data)
    }
    
    save(out,file="devAge_Dx_Sex_SES_ifSite.RData")
    
    # BA correlation with age, (covarying for site, if needed)
    cat('model 2H: age\n')
    
    if (exists("site_regr")){
      form <- as.formula(paste("devAge~Age+Age2+PARENT_SES+",site_regr))
      out=lm(formula = form, data = data)
    } else {
      out=lm(devAge ~ Age+Age2+PARENT_SES, data = data)
    }
    
    save(out,file="devAge_Age_SES_ifSite.RData")
    
    
cat('finished with step 4!\n')

sink()
print(readLines("2_BrainAge_SZ.log"))


# also include figures for case/controls

