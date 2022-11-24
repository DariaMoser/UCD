#' Table of random re-samplings of age-at-death estimations of archaeological skeletal remains with assumed normal distribution of probability
#'
#' This function generates a table of counts by age group and iteration of randomly sampled iterations of exact
#' age-at-death estimations of archaeological remains. For details on the random sampling process, see .
#' @param est_min a vector specifying the lower margin of the age-at-death estimate for every individual.
#' @param est_max a vector specifying the upper margin of the age-at-death estimate for every individual.
#' @param age_breaks a vector specifing the age groups to `table()` by.
#' @param n_it number of iterations to be rendered. Default = 2000.
#' @keywords re-sampling; osteology; archaeology
#' @export
#' @examples
#' # Creating input dataset containing the estimated minimum and maximum age of 5 individuals,
#' # 5, 6-10, 9-13, 3-6 and 4-8 years old.
#'
#' df <- as.data.frame(cbind(c(5,6,9,3,4),c(5,10,13,6,8)))
#'
#' colnames(df) <- c("min","max")
#'
#' ucd_table_unif(df$min, df$max, age_breaks=seq(0,14,2), n_it=10)
#'
#'         V1 V2 V3 V4 V5 V6 V7 V8 V9 V10
#' (0,2]    0  0  0  0  0  0  0  0  0   0
#' (2,4]    0  1  1  0  1  0  0  0  1   1
#' (4,6]    2  3  2  2  1  2  3  2  2   2
#' (6,8]    1  0  1  1  2  1  1  1  1   1
#' (8,10]   2  1  0  1  1  2  1  2  1   0
#' (10,12]  0  0  1  1  0  0  0  0  0   0
#' (12,14]  0  0  0  0  0  0  0  0  0   1
#' rows = age intervals; columns = iterations
#'



ucd_table_unif <- function(est_min, est_max,age_breaks, n_it=2000){
  raw_res <- as.data.frame(do.call(cbind,purrr::rerun(n_it,(est_min-1)+((est_max+1)-(est_min-1))*runif(length(est_min)))))
  raw_table <- sapply(raw_res,function(x){table(cut(x,breaks=age_breaks))})
   raw_table
}
