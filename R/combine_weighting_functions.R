#' Combine weight functions from two sampling frames
#'
#' With multi-stage sampling, it is sometimes necessary to combine the weights from two sampling frames (e.g. stratified cluster sampling).
#' This function let's you create a new weight function from two existing weight functions created with weighting_fun_from_samplingframe().
#' @param weights_function_1 The weight function from the _outer_ sampling frame (the 'larger' scale; Records in one group of the _outer_ sampling frame can belong to different strata in the _inner_ sampling frame.)
#' @param weights_function_2 The weight function from the _inner_ sampling frame (the 'smaller' scale; Records in the same group of the _inner_ sampling frame must also belong to the same group in the _outer_ sampling frame.)
#' @details The returned function combines two sets of weights so that
#' - the sum of weights of each sampling frame's groups remain proportional to each other
#' - the total sum of weights equals the number of rows in the input data frame
#' @return returns a function that takes a dataframe as input and returns a vector of weights
#' @export
combine_weighting_functions<-function(weights_function_1,weights_function_2){
  if(!is.function(weights_function_1)){stop("first input must be a function (see ?load_samplingframe)")}
  if(!is.function(weights_function_2)){stop("second input must be a function (see ?load_samplingframe)")}

  normweights<-function(w){w*length(w)/sum(w)}
  normW2byW1<-function(w1,w2){


    w1_proportions_is<-(w2 %>% split(names(w1)) %>% sapply(sum)) %>% normweights()
    w1_proportions_should<-w1 %>% split(names(w1)) %>% sapply(sum) %>% normweights
    w1_proportions_should<-w1_proportions_should[match(names(w1_proportions_is),names(w1_proportions_should))]
    w1_proportion_factor<-w1_proportions_should/w1_proportions_is

    w_combined<-w2*w1_proportion_factor[names(w1)]
    w_combined<-normweights(w_combined)

    w_combined
  }

  combined_fun<-function(df){

    w1 <- weights_function_1(df)
    w2 <- weights_function_2(df)
    w_combined<-normW2byW1(w1,w2)

    return(w_combined)
  }

  return(combined_fun)
}



