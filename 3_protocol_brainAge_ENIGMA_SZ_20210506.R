######  SCZ brain age project: Correlations of FreeSurfer ROIs with brain age #######
# Authors: C.Constantinides, E. Walton
# Date: 06/05/2021

# Set working directory 
# setwd("/Volumes/files/Psychology/ResearchProjects/EWalton/BrainAge_SZ/data/Processed_data_CC/Stage_1/SCORE/Formatted")


#create log file
messages=file("3_BrainAGE_SZ.log", open="wt")
#rest=file("rest.Rout", open="wt")
sink(messages, type="message")
sink(messages, type="output")


# install/load libraries
cat("Prep: installing/loading libraries\n")

# install.packages("Hmisc")
library("Hmisc")
#install.packages("dplyr")
library("dplyr")

# source required functions
cat("Prep: sourcing functions\n")

source("get.means.R")
source("prepare.files.R")

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

# get for missings in Dx, Age, Sex 
if (length(table(is.na(Covs[,c("Dx","Sex","Age")])))>1){
  stop("missing data in Dx, Age or Sex")
}

# Check that all of the required columns are present 
mcols=c("SubjID","Dx","Sex","Age")
colind=match(mcols,names(Covs))
if(length(which(is.na(colind))) > 0){
  stop('At least one of the required columns in your Covariates.csv file is missing. Make sure that the column names are spelled exactly as listed\n
       It is possible that the problem column(s) is: ', mcols[which(is.na(colind))])
}

# exclude age<18y and >75y from data (as in the original protocol)
if (min(Covs[,c("Age")])<18 | max(Covs[,c("Age")])>75){
  cat("excluding subjects younger than 18y and/or older than 75y \n")
  Covs=Covs[(Covs$Age>=18 & Covs$Age<=75),]
}

# Check for duplicated SubjIDs that may cause issues with merging data sets.
if(anyDuplicated(Covs[,c("SubjID")]) != 0) { stop('You have duplicate SubjIDs in your Covariates.csv file.\nMake sure there are no repeat SubjIDs.') }


#combine the files into one dataframe
data = merge(Covs, TSV, by.x="SubjID", by.y="Row.names")
rm(TSV)
rm(TS)
rm(Surf)
rm(Thick)
rm(Vol)

cat("creating csv files for brainAge estimation\n")
df=prepare.files(data,names(Covs))

males=df$males
females=df$females
rm(df)

cat("Step 5: Run Freesurfer ROIs-BrainAge correlation analysis")

# males 
output <- read.csv("males_raw_out.csv", header=T, sep="\t")
males$predAge <- output$age_prediction
rm(output)

# females
output <- read.csv("females_raw_out.csv", header=T, sep="\t")
females$predAge <- output$age_prediction
rm(output)

ROI_BA=rbind(males,females)

# str(ROI_BA)

# check that no sample duplication
cat("duplicated IDs: ",which(duplicated(ROI_BA$SubjID)),"\n")

cat('checking correlations\n')

# Check correlations in total sample (i.e. across HCs and SZ)
dat_matrix=data.matrix(ROI_BA)
correlations=rcorr(dat_matrix)

# Repeat for HCs and SZ only

# Extract HCs
ROI_BA_HC <- ROI_BA %>% filter(Dx==0)
# Extract SZs
ROI_BA_SZ <- ROI_BA %>% filter(Dx==1)

# Check correlation for HCs and SZs separately
# HCs
dat_matrix=data.matrix(ROI_BA_HC)
correlations.HC=rcorr(dat_matrix)
# SZs
dat_matrix=data.matrix(ROI_BA_SZ)
correlations.SZ=rcorr(dat_matrix)

save(correlations,correlations.HC, correlations.SZ,
     file="ROI_BA_correlations.Rdata")

cat('finished with step 5!\n')

sink()
print(readLines("3_BrainAge_SZ.log"))

