convert.PANSS.SANS.SAPS=function(data){
  ############################ convert PANSS_neg to SANS, if needed ################
  
  cat('check, whether PANSS_NEG, SANS_TOTAL or SANS_GLOBAL negative symptom scores in data\n')
  
  
  if ('SANS_GLOBAL' %in% colnames(data==TRUE) & !(all(is.na(data$SANS_GLOBAL)))) {
    cat('keep SANS_GLOBAL variable','\n')
  } else if ('PANSS_NEG' %in% colnames(data==TRUE) & !(all(is.na(data$PANSS_NEG)))) {
    SANS_GLOBAL = -2.0671 + (0.665 * data$PANSS_NEG)
    data$PANSS_NEG <- SANS_GLOBAL
    names(data)[names(data) == 'PANSS_NEG'] <- 'SANS_GLOBAL.1'
    cat('converted PANSS_NEG to SANS_GLOBAL.1','\n')
  } else if ('SANS_TOTAL' %in% colnames(data==TRUE) & !(all(is.na(data$SANS_TOTAL)))) {
    SANS_GLOBAL = 1.0863 + (0.2943 * data$SANS_TOTAL)
    data$SANS_TOTAL <- SANS_GLOBAL
    names(data)[names(data) == 'SANS_TOTAL'] <- 'SANS_GLOBAL.1'
    cat('converted SANS_TOTAL to SANS_GLOBAL.1\n')
  } else {
    cat('Cannot find PANSS_NEG, SANS_TOTAL or SANS_GLOBAL column\n')
  }
  ############################ convert PANSS_pos to SAPS, if needed ################
  
  cat('check, whether PANSS_POS, SAPS_TOTAL or SAPS_GLOBAL positive symptom scores in data\n')
  
  
  if ('SAPS_GLOBAL' %in% colnames(data==TRUE) & !(all(is.na(data$SAPS_GLOBAL)))) {
    cat('keep SAPS_GLOBAL variable\n')
  } else if ('PANSS_POS' %in% colnames(data==TRUE) & !(all(is.na(data$PANSS_POS)))) {
    SAPS_GLOBAL = -3.222 + (0.567 * data$PANSS_POS)
    data$PANSS_POS <- SAPS_GLOBAL
    names(data)[names(data) == 'PANSS_POS'] <- 'SAPS_GLOBAL.1'
    cat('converted PANSS_POS to SAPS_GLOBAL.1','\n')
  } else if ('SAPS_TOTAL' %in% colnames(data==TRUE) & !(all(is.na(data$SAPS_TOTAL)))) {
    SAPS_GLOBAL = 2.3526 + (0.1932 * data$SAPS_TOTAL)
    data$SAPS_TOTAL <- SAPS_GLOBAL
    names(data)[names(data) == 'SAPS_TOTAL'] <- 'SAPS_GLOBAL.1'
    cat('converted SAPS_TOTAL to SAPS_GLOBAL.1\n')
  } else {cat('Cannot find PANSS_POS, SAPS_TOTAL or SAPS_GLOBAL column\n')}
  return(data)
}
