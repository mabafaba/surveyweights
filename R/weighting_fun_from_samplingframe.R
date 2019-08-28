#' creates a weighting function from a sampling frame
#'
#' @param sampling.frame.file data frame containing the sampling frame. should contain columns "stratum" and "population", otherwise column names must be specified.
#' @param sampling.frame.population.column sampling frame name of column holding population counts. defaults to "population"
#' @param sampling.frame.stratum.column sampling frame name of column holding stratum names. defaults to "stratum". Stratum names must match exactly values in:
#' @param data.stratum.column data column name that holds the record's strata names
#' @param data optional but recommended: you can provide an example data frame of data supposed to match the sampling frame to check if the provided variable names match and whether all strata in the data appear in the sampling frame.
#' @return returns a new function that takes a data frame as input returns a vector of weights corresponding to each row in the data frame.
#' @examples

#' # load data and sampling frames:
#' mydata<-read.csv("mydata.csv")
#' mysamplingframe<-read.csv("mysamplingframe.csv")
#' # create weighting function:
#' weighting<-weighting_fun_from_samplingframe(sampling.frame = mysamplingframe,
#'                                  data.stratum.column = "strata_names",
#'                                  sampling.frame.population.column = "pop",
#'                                  sampling.frame.stratum.column = "strat_name")
#' # use weighting function:
#' mydata$weights<-weighting(mydata)
#'
#' # this also works on subsets of the data:
#' mydata_subset<-mydata[1:100,]
#' subset_weights<- weighting(mydata)
#'
#' @importFrom magrittr %>%
#'
#' @export
weighting_fun_from_samplingframe <- function(sampling.frame,
                                             data.stratum.column,
                                             sampling.frame.population.column="population",
                                             sampling.frame.stratum.column="stratum",data=NULL){


  if(length(data.stratum.column)!=1){stop("must provide exactly one data.stratum.column. If your strata are based on multiple columns, concatenate them first into a new variable.")}


  # check input

  # load file (loading from csv depreciated; just renaming)
  sampling.frame<-as.data.frame(sampling.frame,stringsAsFactors = FALSE)
  if(!is.null(data)){
    data<-as.data.frame(data,stringsAsFactors = FALSE)
  }
  sf_raw<-sampling.frame
  if(any(duplicated(sf_raw[, sampling.frame.stratum.column]))){
    sf_raw<-sf_raw[!duplicated(sf_raw[sampling.frame.stratum.column]),]
    # .write_to_log(paste("SERIOUS ISSUE: duplicate stratum names in the sampling frame:\n",
    #                     cyan(paste0(names(sf_raw[duplicated(sf_raw[sampling.frame.stratum.column]),sampling.frame.stratum.column] %>% table),collapse="\n"))))
    warning("duplicate stratum names in sampling frame; using first entries and ignoring all other.")
  }
  # sf_raw<-sf
  # get unique strata names from sampling frame
  unique_strata <- sf_raw[, sampling.frame.stratum.column]
  # make sure strata are unique

  # standardise internal sampling frame format
  # - data.stratum.column: the name of the data column holding strata names (is function argument)
  # - population.counts: named vector with counts as values and strata names as names
  # use: population.counts[stratum_name_string]
  population.counts<-sf_raw[[sampling.frame.population.column]]
  names(population.counts)<-as.character(unique_strata)

  # error if any stratum has zero population
  if(any(population.counts==0,na.rm = T)){.write_to_log("CRITICAL: strata in sampling frame can not have population 0, please remove the stratum from your sampling frame and data. (how did you even sample from that)")}
  # make sure all strata have data:
  population.counts <- population.counts[(
    !is.na(population.counts) &
      !is.na(population.counts) &
      population.counts > 0)]


  # make sure all data has strata in samplingframe:
  if(!is.null(data)){
    is_data_in_sf<-unique(data[,data.stratum.column]) %in% sf_raw[,sampling.frame.stratum.column]
    if(any(!(is_data_in_sf))){
      warning(paste0("there are records that can not be found in the sampling frame:\n",
                     crayon::cyan(paste0(data[is_data_in_sf,data.stratum.column] %>% unique,collapse="\n"))))
    }
  }


  # closure function that calculates weights on the fly
  # uses immutable data provided to load_samplingframe()
  weights_of<- function(df) {

    if(!is.data.frame(df)){stop("df must be a data.frame")}

    # in case of tibble.. just to be sure (as tibbles weren't originally accounted for)
    df<-as.data.frame(df,stringsAsFactors = FALSE)
    # factors scare me:
    df<-lapply(df,function(x){if(is.factor(x)){return(as.character(x))};x}) %>% as.data.frame(stringsAsFactors=FALSE)
    # # insure stratum column exists in df:
    if (!all(data.stratum.column %in% names(df))){stop(paste0("data frame column '",data.stratum.column, "'not found."))}

    # make sure df is handled as characters, not factors. otherwise we match factor id's instead of names
    df[[data.stratum.column]]<-as.character(df[[data.stratum.column]])
    df <- df[!is.na(data.stratum.column),]
    df <- df[!(df[[data.stratum.column]] %in% c("NA", "N/A", "#N/A")),]

    # count number of records in each stratum
    sample.counts<-stratify.count.sample(data.strata = df[[data.stratum.column]],sf.strata = population.counts)

    # make sure all record's strata can be found in the sampling frame:
    if("weights" %in% names(df)){warning("'weights' is used as a column name (will not be calculated from the sampling frame)")}
    if(!all(names(sample.counts) %in% names(population.counts))){stop("all strata names in column '",
                                                                      data.stratum.column,"' must also appear in the loaded sampling frame.")}
    # population counts taken from weights_of() enclosing environment, created in load_samplingframe()
    weights <- stratify.weights(pop_strata = population.counts,
                                sample_strata = sample.counts)
    # final test that mean of weights == 1
    # insure(that.all=mean(weights[df[[data.stratum.column]]]) %almost.equals% 1,
    #        err="Weighting calculation failed internally, this is our fault. Sorry! Contact the Reach Initiatives data unit to get this fixed!")
    return(weights[df[[data.stratum.column]]])


  }

  return(weights_of)
}


#
# auto.weight<-function(df,weight.by=NULL){
#   # A) if no weight.by provided, I'll try calculating myself:
#   if(is.null(weight.by)){temp.weights<-weights_of(df)}
#   # B) character string provided
#   else if(is.character(weight.by)){
#     warning("using the weight.by argument is not recommended. You should call load_samplingframe(), and then weighted functions without weight.by argument.")
#     insure.string.is.column.header(df,weight.by)
#     insure(length(weight.by)==1,err="weight by should be NULL for auto weighting, or a single character string naming the data column that stores the weights")
#     df[[weight.by]] <- gsub(",", ".", df[[weight.by]]) %>% as.numeric
#     temp.weights<-df[[weight.by]]
#   }else{
#     insure.has.data(temp.weights)
#   }
#   return(temp.weights)
# }




