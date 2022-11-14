get.means=function(fsfile){ 
  
  if(fsfile == "CorticalMeasuresENIGMA_ThickAvg.csv"){
    filetype="thickness"
    whole=" + MThickness"
  } else if(fsfile == "CorticalMeasuresENIGMA_SurfAvg.csv"){
    filetype="surfarea"
    whole=" + FullSurfArea"
  } else if(fsfile == "SubcorticalMeasuresENIGMA_VolAvg.csv"){
    filetype="subcortical"
  } else {
    stop("Your input file isn't named correctly")	
  }
  
  
  # #create log file
  # messages=file(paste0(fsfile,".log"), open="wt")
  # sink(messages, type="message")
  # sink(messages, type="output")
  # 
  # cat('Working on',fsfile,'analysis\n')
  
  Cort <- read.csv(fsfile); #Read in the phenotypes file
  # 
  # #check to make sure all of the necessary columns are present
  if(fsfile == "CorticalMeasuresENIGMA_ThickAvg.csv"){
    CortCols=c("SubjID", "L_bankssts_thickavg", "L_caudalanteriorcingulate_thickavg", "L_caudalmiddlefrontal_thickavg", "L_cuneus_thickavg", "L_entorhinal_thickavg", "L_fusiform_thickavg", "L_inferiorparietal_thickavg", "L_inferiortemporal_thickavg", "L_isthmuscingulate_thickavg", "L_lateraloccipital_thickavg", "L_lateralorbitofrontal_thickavg", "L_lingual_thickavg", "L_medialorbitofrontal_thickavg", "L_middletemporal_thickavg", "L_parahippocampal_thickavg", "L_paracentral_thickavg", "L_parsopercularis_thickavg", "L_parsorbitalis_thickavg", "L_parstriangularis_thickavg", "L_pericalcarine_thickavg", "L_postcentral_thickavg", "L_posteriorcingulate_thickavg", "L_precentral_thickavg", "L_precuneus_thickavg", "L_rostralanteriorcingulate_thickavg", "L_rostralmiddlefrontal_thickavg", "L_superiorfrontal_thickavg", "L_superiorparietal_thickavg", "L_superiortemporal_thickavg", "L_supramarginal_thickavg", "L_frontalpole_thickavg", "L_temporalpole_thickavg", "L_transversetemporal_thickavg", "L_insula_thickavg", "R_bankssts_thickavg", "R_caudalanteriorcingulate_thickavg", "R_caudalmiddlefrontal_thickavg", "R_cuneus_thickavg", "R_entorhinal_thickavg", "R_fusiform_thickavg", "R_inferiorparietal_thickavg", "R_inferiortemporal_thickavg", "R_isthmuscingulate_thickavg", "R_lateraloccipital_thickavg", "R_lateralorbitofrontal_thickavg", "R_lingual_thickavg", "R_medialorbitofrontal_thickavg", "R_middletemporal_thickavg", "R_parahippocampal_thickavg", "R_paracentral_thickavg", "R_parsopercularis_thickavg", "R_parsorbitalis_thickavg", "R_parstriangularis_thickavg", "R_pericalcarine_thickavg", "R_postcentral_thickavg", "R_posteriorcingulate_thickavg", "R_precentral_thickavg", "R_precuneus_thickavg", "R_rostralanteriorcingulate_thickavg", "R_rostralmiddlefrontal_thickavg", "R_superiorfrontal_thickavg", "R_superiorparietal_thickavg", "R_superiortemporal_thickavg", "R_supramarginal_thickavg", "R_frontalpole_thickavg", "R_temporalpole_thickavg", "R_transversetemporal_thickavg", "R_insula_thickavg", "LThickness", "RThickness", "LSurfArea", "RSurfArea", "ICV")
    cortcolind=match(CortCols,names(Cort))
    if(length(which(is.na(cortcolind))) > 0){
      stop('At least one of the required columns in your ', fsfile, ' file is missing. Make sure that the column names are spelled exactly as listed in the protocol\n')
    }
  } else if (fsfile == "CorticalMeasuresENIGMA_SurfAvg.csv"){
    CortCols=c("SubjID", "L_bankssts_surfavg", "L_caudalanteriorcingulate_surfavg", "L_caudalmiddlefrontal_surfavg", "L_cuneus_surfavg", "L_entorhinal_surfavg", "L_fusiform_surfavg", "L_inferiorparietal_surfavg", "L_inferiortemporal_surfavg", "L_isthmuscingulate_surfavg", "L_lateraloccipital_surfavg", "L_lateralorbitofrontal_surfavg", "L_lingual_surfavg", "L_medialorbitofrontal_surfavg", "L_middletemporal_surfavg", "L_parahippocampal_surfavg", "L_paracentral_surfavg", "L_parsopercularis_surfavg", "L_parsorbitalis_surfavg", "L_parstriangularis_surfavg", "L_pericalcarine_surfavg", "L_postcentral_surfavg", "L_posteriorcingulate_surfavg", "L_precentral_surfavg", "L_precuneus_surfavg", "L_rostralanteriorcingulate_surfavg", "L_rostralmiddlefrontal_surfavg", "L_superiorfrontal_surfavg", "L_superiorparietal_surfavg", "L_superiortemporal_surfavg", "L_supramarginal_surfavg", "L_frontalpole_surfavg", "L_temporalpole_surfavg", "L_transversetemporal_surfavg", "L_insula_surfavg", "R_bankssts_surfavg", "R_caudalanteriorcingulate_surfavg", "R_caudalmiddlefrontal_surfavg", "R_cuneus_surfavg", "R_entorhinal_surfavg", "R_fusiform_surfavg", "R_inferiorparietal_surfavg", "R_inferiortemporal_surfavg", "R_isthmuscingulate_surfavg", "R_lateraloccipital_surfavg", "R_lateralorbitofrontal_surfavg", "R_lingual_surfavg", "R_medialorbitofrontal_surfavg", "R_middletemporal_surfavg", "R_parahippocampal_surfavg", "R_paracentral_surfavg", "R_parsopercularis_surfavg", "R_parsorbitalis_surfavg", "R_parstriangularis_surfavg", "R_pericalcarine_surfavg", "R_postcentral_surfavg", "R_posteriorcingulate_surfavg", "R_precentral_surfavg", "R_precuneus_surfavg", "R_rostralanteriorcingulate_surfavg", "R_rostralmiddlefrontal_surfavg", "R_superiorfrontal_surfavg", "R_superiorparietal_surfavg", "R_superiortemporal_surfavg", "R_supramarginal_surfavg", "R_frontalpole_surfavg", "R_temporalpole_surfavg", "R_transversetemporal_surfavg", "R_insula_surfavg", "LThickness", "RThickness", "LSurfArea", "RSurfArea", "ICV")
    cortcolind=match(CortCols,names(Cort))
    if(length(which(is.na(cortcolind))) > 0){
      stop('At least one of the required columns in your ', fsfile, ' file is missing. Make sure that the column names are spelled exactly as listed in the protocol\n')
    }
  } else {
    #  CortCols=c("SubjID","LLatVent","RLatVent","Lthal","Rthal","Lcaud","Rcaud","Lput","Rput","Lpal","Rpal","Lhippo","Rhippo","Lamyg","Ramyg","Laccumb","Raccumb","ICV")
    names(Cort)=gsub("LatVent","vent",names(Cort))
    CortCols=c("SubjID","Lvent","Lthal","Lcaud","Lput","Lpal","Lhippo","Lamyg","Laccumb","Rvent","Rthal","Rcaud","Rput","Rpal","Rhippo","Ramyg","Raccumb","ICV")
    cortcolind=match(CortCols,names(Cort))
    if(length(which(is.na(cortcolind))) > 0){
      stop('At least one of the required columns in your ', fsfile, ' file is missing. Make sure that the column names are spelled exactly as listed in the protocol\n')
    }
    Cort=Cort[ , CortCols]
  }
  
  
  
  
  # 
  
  ####
  ####calculate mean values
  ####
  
  meanCort=NULL
  
  
  if(fsfile == "SubcorticalMeasuresENIGMA_VolAvg.csv"){
    for(x in 2:9){
      meanCort = c(meanCort, ((Cort[,x] + Cort[x+8])/2))
      names(meanCort)[[x-1]]=sub('.', 'M', names(Cort)[x])
    }
    # add whGMvol here, if possible
    #  meanCort = c(meanCort, ((Cort[,"LGMv"] + Cort["RGMv"])/2))
    #  names(meanCort)[35]="MThickness"
    #  names(meanCort)[36]="FullSurfArea"
  } else {
    #calculate means
    for(x in 2:35){
      meanCort = c(meanCort, ((Cort[,x] + Cort[x+34])/2))
    }
    meanCort = c(meanCort, ((Cort[,70] + Cort[71])/2))
    meanCort = c(meanCort, ((Cort[,72] + Cort[73])))
    
    for(x in 1:34){
      tmp=strsplit(names(meanCort)[x],"_")
      names(meanCort)[x]=paste0("M_",tmp[[1]][2],"_",tmp[[1]][3])
    }
    names(meanCort)[35]="MThickness"
    names(meanCort)[36]="FullSurfArea"
    
    #drop ICV from Cort file
    #Cort = Cort[,-ncol(Cort)]
  }
  meanCort=as.data.frame(meanCort)
  
  #combine Cort file with the newly calculated means
  rownames(meanCort) = Cort$SubjID
  meanCort$ICV=Cort$ICV
  
  return(meanCort)
}
