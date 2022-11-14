#### SCZ brain age project
# written by Laura HAn (L.Han@ggzingeest.nl) and Esther Walton (waltonesther@gmail.com)

# setwd

#setwd("D:/backup/dell-laptop/Documents/Bath/science/ENIGMA_SZ_brainAge/MCIC")

#create log file
messages=file("1_BrainAGE_SZ.log", open="wt")
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

# derive mean volumnes / thickness and surfarea

cat("Step 2: Obtaining measures of brain age\n")
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

# check for missings in Dx, Age, Sex
if (length(table(is.na(Covs[,c("Dx","Sex","Age")])))>1){
  stop("missing data in Dx, Age or Sex")
}

# Check that all of the required columns are present
mcols=c("SubjID","Dx","Sex","Age","SANS_TOTAL","SANS_GLOBAL","PANSS_NEG","SAPS_TOTAL","SAPS_GLOBAL","PANSS_POS","PANSS_TOTAL","LENGTH_OF_ILLNESS","AO","IQ","HAND","PARENT_SES","ETHNICITY","CPZ","SUBTYPE","MEDICATION")
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
if ("Site" %in% names(Covs)){
  Site.f = factor(Covs$Site)
  #Covs$Site=NULL
  
  if (length(grep("Site.f",colnames(Covs)))>0){
    site_regr=paste(grep("Site.f",colnames(Covs),value=T),collapse="+")
  }
  
  Covs=cbind(Covs,model.matrix(~Site.f)[,-1])
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


invisible(readline(prompt="You should see 2 csv files in your working directory:\n
-females_raw.csv\n
-males_raw.csv.\n
These should be up- and downloaded as described in your word document.\n
Once you're dowloaded the output files again, run the second R script as described\n"))


cat('finished with step 1!\n')

sink()
print(readLines("1_BrainAge_SZ.log"))


# also include figures for case/controls

