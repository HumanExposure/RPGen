# Contents Test
# AE, ORAU, 2020

#' Tests if a column in a dataframe is within a specified threshold. 
#'
#' Used for testing if pool inputs are within correct bounds. Will
#' report if values exceed threshold, if values are missing, or both.
#' Works for both characters and numerics.
#'
#' @param   filename name and directory of AHS input file.
#' @param   column name of the column entered as a character.
#' @param   threshold user specified threshold.
#' @return  test results. If column contents are within or exceed threshold.


contents.test<-function(df,column,threshold){
 
  if (column %in% colnames(df)){
   
  possibilities <- names(table(df[[column]], useNA = "ifany"))
  possibilities[is.na(possibilities)]<- "NA"
  possibilities[is.nan(possibilities)] <- "NaN"
  
  missing<- setdiff(as.character(threshold),possibilities)
  extra<- possibilities[!possibilities %in% as.character(threshold)]
  
  missing<-paste(unlist(missing), collapse = " ")
  extra  <-paste(unlist(extra), collapse = " ")
  
  both <- paste(missing,extra)
  
  if (both == " "){
    result <- paste("Pass.",column, "contents are as specified in threshold.") 
  } else if(both != " "){
    if (nchar(extra) == 0){
      result<- paste("Fail.",column,"column is missing:", missing,".")
    } else if (nchar(missing) == 0){
      result<- paste("Fail.",column, "column has unexpected value(s):",extra,".")
    } else {
      result<- paste("Fail.",column, "column is missing:",missing," and has unexpected value(s):",extra,".")
    }
    
  }
  
  } else
  result<- paste("Column not in data frame.")
  
  return(result)   
}




              