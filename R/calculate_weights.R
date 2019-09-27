
#' estimates sampling probabilities from sample size and strata populations.
stratify.weights<-function(pop_strata,sample_strata){

  # remove sample_strata names with no data (can happen when only a subset of the data is used)
  sample_strata %>% hasdata -> sample_strata
  # only use populations that appear in current sample:
  pop_strata<-pop_strata[names(sample_strata)]

  # insure that all names of sample strata are found in names of population strata
  if(!all(names(sample_strata)%in%names(pop_strata))){stop(paste(
    "all data strata must exist in sampling frame. The following could not be found:\n",
    paste(names(sample_strata)[names(sample_strata)%in%names(pop_strata)],collapse="\n")))}
  sample_global<-sum(sample_strata)
  pop_global<-sum(pop_strata)
  weights = (pop_strata/pop_global) / (sample_strata/sample_global)
  return(weights)

}

#' Get sample sizes for weight calculations.
#' Checks if all strata appear in sampling frame
stratify.count.sample<-function(data.strata,sf.strata){
  # count samples per stratum
  samplecounts<-table(data.strata)
  # check which ones can be found in sampling frame
  strataexists<-(names((samplecounts)) %in% names(sf.strata))
  data.strata.not.in.sampleframe<-samplecounts[!strataexists]
  # throw error if data strata not found in sampling frame

  if(length(data.strata.not.in.sampleframe)!=0){
    stop(paste0("problem with ", cat(crayon::bgYellow(crayon::red(" sampling frame "))),
                "in the data, we find the following strata names that don't exist in the sampling frame, or have no valid population numbers:\n",
                crayon::cyan(paste0(names(data.strata.not.in.sampleframe),collapse="\n")))
    )
  }
  # return sample counts
  return(samplecounts[strataexists])
}
