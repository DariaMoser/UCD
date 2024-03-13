#' Table of standard deviation of random samplings of age-at-death estimations of archaeological skeletal remains with assumed uniform probability distribution
#'
#'
#' This function generates a table of standard deviation of counts by age group and iteration of randomly
#' sampled iterations of exact age-at-death estimations of archaeological remains. For details on the
#' random sampling process, see.
#' @param est_min a vector specifying the lower margin of the age-at-death estimate for every individual.
#' @param est_max a vector specifying the upper margin of the age-at-death estimate for every individual.
#' @param age_breaks a vector specifing the age groups to `table()` by.
#' @param n_it number of iterations to be rendered. Default = 2000.
#' @keywords random sampling; osteology; archaeology
#' @export
#' @examples
#' # Creating input dataset containing the estimated minimum and maximum age of 5 individuals,
#' # 5, 6-10, 9-13, 3-6 and 4-8 years old.
#'
#' df <- as.data.frame(cbind(c(5,6,9,3,4),c(5,10,13,6,8)))
#'
#' colnames(df) <- c("min","max")
#'
#' ucd_sd_unif(df$min, df$max, age_breaks=seq(0,14,2))
#'
#'     (0,2]     (2,4]     (4,6]     (6,8]    (8,10]   (10,12]   (12,14]
#' 0.0000000 0.6114597 0.7673475 0.7936843 0.7669870 0.6075850 0.4738272
#'



ucd_sd_unif <- function(est_min, est_max, age_breaks, n_it=2000){
  raw_res <- as.data.frame(do.call(cbind,purrr::rerun(n_it,(est_min-1)+((est_max+1)-(est_min-1))*runif(length(est_min)))))
  tab_res <- sapply(raw_res,function(x){table(cut(x,breaks=age_breaks))})
  sd_res <- apply(tab_res, 1, FUN=sd)
  sd_res
}


