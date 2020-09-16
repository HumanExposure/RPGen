# Report Card
# AE, ORAU, 2020

#' Prints a comprehensive test of a dataframe containing pools to the user.
#'
#' Calls both contents.test and pool.reader to test inputs to pool and then 
#' list pools not included in the dataframe. 
#'
#' @param   x a dataframe with RPGen pool structure.
#' @return  A report on the pool determinants and pools within the dataframe.

RPGen_reportcard <-function(x){
cat("Testing:")
cat(" \n")

cat("",contents.test(x,"urban",0:1),"\n",
    contents.test(x,"region",1:4), "\n",
    contents.test(x,"location",1:8),"\n",
    contents.test(x,"housetyp",1:3),"\n",
    contents.test(x,"famcat",1:4), "\n",
    contents.test(x,"inccat",1:3), "\n",
    contents.test(x,"pool",1:288))
cat("\n")
cat(" \n")

if (sum(setdiff(1:288,x$pool)) > 0){
  missing <- pool.reader(setdiff(1:288, x$pool))
  cat("Household Pools not Included:",sep = "\n")
  cat(missing, sep ="\n")
  cat(" \n")
}
}


