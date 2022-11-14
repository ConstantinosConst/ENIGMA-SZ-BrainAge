# qc.brainAge=function(){
#   # "age_prediction" column is added to the uploaded .csv file
#   # output_males <- read.csv("males_raw_out.csv", header=T, sep="\t")
#   # males$predAge <- output_males$age_prediction
#   # 
#   # output_females <- read.csv("females_raw_out.csv", header=T, sep="\t")
#   # females$predAge <- output_females$age_prediction
#   # 
#   # 
#   # output_males <- read.csv("males_scz_raw_out.csv", header=T, sep="\t")
#   # males_scz$predAge <- output_males$age_prediction
#   # 
#   # output_females <- read.csv("females_scz_raw_out.csv", header=T, sep="\t")
#   # females_scz$predAge <- output_females$age_prediction
#   # 
#   # output_males <- read.csv("males_hc_raw_out.csv", header=T, sep="\t")
#   # males_hc$predAge <- output_males$age_prediction
#   # 
#   # output_females <- read.csv("females_hc_raw_out.csv", header=T, sep="\t")
#   # females_hc$predAge <- output_females$age_prediction
#   
#   ##variable <- males
#   ##variable <- females
#   ##variable <- males_scz
#   ##variable <- females_scz
#   ##variable <- males_hc
#   variable <- females_hc
#   
model.fits.brainAge.dx=function(model, brain_age_output){
    variable=model
    
    output <- read.csv(brain_age_output, header=T, sep="\t")
    variable$predAge <- output$age_prediction
    
    variable$devAge <- (variable$predAge-variable$Age)
    variable$AE <- abs(variable$devAge)
    ##Create agebins.
    agebreaks <- c(10,20,30,40,50,60,76)
    agelabels <- c("10-19","20-29","30-39","40-49","50-59","60-76")
    setDT(variable)[ , agegroups := cut(Age,
                                        breaks = agebreaks,
                                        right = FALSE,
                                        labels = agelabels)]
    
    
    variable$Dx[variable$Dx==0] <- "HC"
    variable$Dx[variable$Dx==1] <- "SCZ"
    
    for (i in c("HC","SCZ")){
      
    # Calculate model metrics
    
      variable.dx=variable[variable$Dx==i,]
      cat("For model >",brain_age_output,", ",i,"< your MAE is: ",mae(variable.dx$Age, variable.dx$predAge), "\n")
      cat("For model >",brain_age_output,", ",i,"< your MSE is: ",mse(variable.dx$Age, variable.dx$predAge), "\n")
      cat("For model >",brain_age_output,", ",i,"< your age~predAge cor is: ",cor(variable.dx$Age, variable.dx$predAge), "\n")
      cat("For model >",brain_age_output,", ",i,"< your R2 is: ",caret::R2(variable.dx$Age, variable.dx$predAge), "\n")
      brain_PAD <- (variable.dx$predAge-variable.dx$Age)
      cat("For model >",brain_age_output,", ",i,"< your mean predAge-age difference is: ",mean(brain_PAD), "\n")
      cat("For model >",brain_age_output,", ",i,"< your absolute SD is: ",sd(abs(brain_PAD)), "\n")
      
    }

    
    # Plot the MAE and brain-PAD per site and age group.
    # The model generates larger errors in individuals >60 years old,
    # so please apply caution and visualize the MAE and brain-PAD per (age) group.
    #
    # There is a known slight bias of the model such that it overpredicts young samples and
    # underpredicts old samples, resulting in a regression to the mean (the brain-PAD estimates
    # are correlated to chronological age).
    # Le, T. T., Kuplicki, R. T., McKinney, B. A., Yeh, H. W., Thompson, W. K., Paulus, M. P., & Tulsa 1000 Investigators. (2018).
    # A nonlinear simulation framework supports adjusting for age when analyzing BrainAGE.
    # Frontiers in aging neuroscience, 10.
    #
    # The age range of your cases and controls should be similar if you plan to compare the groups.
    # Always include chronological age in your subsequent analyses, potentially nonlinear age effects as well (e.g. Age^2, Age^3) .


    ##Make a plot of MAE distribution per Dx
      variable$Dx=factor(variable$Dx)
      p00 <- ggplot(variable, aes(y=AE, x=Dx)) + geom_boxplot(alpha=0.5)+ geom_smooth() + labs(x = "", y = "")


      p00 <- p00 + theme_bw() +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"))
 
    ##Make a plot of MAE distribution per age bin and dx
    variable$agegroups=factor(variable$agegroups)
    p02 <- ggplot(variable, aes(y=AE, x=agegroups, fill=Dx)) + geom_boxplot(alpha=0.5)+ geom_smooth() + labs(x = "", y = "")

    p02 <- p02 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

    ##Make a plot of devAge distribution per Dx
       variable$Dx=factor(variable$Dx)
       p03 <- ggplot(variable, aes(y=devAge, x=Dx)) + geom_boxplot(alpha=0.5)+ geom_smooth() + labs(x = "", y = "")

       p03 <- p03 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


    ##Make a plot of devAge distribution per age bin and dx
    variable$agegroups=factor(variable$agegroups)
    p04 <- ggplot(variable, aes(y=devAge, x=agegroups, fill=Dx)) + geom_boxplot(alpha=0.5)+ geom_smooth() + labs(x = "", y = "")

    p04 <- p04 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

    pdf(paste0(deparse(substitute(model)),"_Dx_BA_descriptives.pdf"))
    app01 <- grid.arrange(p00, p03, p02, p04, nrow=2, left="MAE", right="Brain-PAD")
    # Plot Observed vs. Predicted.
    # Piñeiro, G., Perelman, S., Guerschman, J. P., & Paruelo, J. M. (2008).
    # How to evaluate models: observed vs. predicted or predicted vs. observed?. Ecological Modelling, 216(3-4), 316-322.

       variable$Dx <- factor(variable$Dx)
      p1 <- ggplot(variable, aes(x=predAge, y=Age, color=Dx))+ geom_point(alpha=0.5)+
        scale_x_continuous(limits=(c(0, 80)),breaks=c(0,10,20,30,40,50,60,70,80))+
        scale_y_continuous(limits=(c(0, 80)), breaks=c(0,10,20,30,40,50,60,70,80))+
        scale_colour_hue(l=50)+ geom_smooth(method=lm, colour="black", size=0.5, se=TRUE, fullrange=TRUE)+
        labs(x = "Predicted brain age (years)", y = "Chronological Age (years)")

      p1 <- p1 + geom_abline(intercept = 0, slope = 1, linetype='dotted')
      print(p1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    axis.line = element_line(colour = "black")))
     dev.off()

    # If you use PHOTON and our ENIGMA FreeSurfer Brain Age Model, please make sure that you quote the following reference in any publications:
    # PHOTON Brain Age model website PHOTON repository
    # Han et al., (in prep). Brain Aging in Major Depressive Disorder: results from the ENIGMA Major Depressive Disorder working group.
  }

  # model_fits_brainAge(males,"males_raw_out.csv")
  # model_fits_brainAge(females,"females_raw_out.csv")

  # if no site variable
#}