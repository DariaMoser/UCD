#' Table of upper 95%-confidence bounds of random sampling of age-at-death estimations of archaeological skeletal remains with assumed normal probability distribution
#'
#' This function generates a table of upper 95%-confidence bounds of counts by age group and iteration of randomly
#' sampled iterations of exact age-at-death estimations of archaeological remains. For details on the
#' random sampling process, see.
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
#' ucd_upper_CI_norm(df$min, df$max, age_breaks=seq(0,14,2))
#'
#' (0,2]   (2,4]   (4,6]   (6,8]  (8,10] (10,12] (12,14]
#'   0       1       3       2       2       1       1
#'



ucd_upper_CI_norm <- function(est_min, est_max,n_ind, age_breaks, n_it=2000){
  raw_res <- as.data.frame(do.call(cbind,purrr::rerun(n_it,
                                                      rnorm(length(est_min), (est_min+est_max)/2,
                                                            (((est_min+est_max)/2)-est_min)/3))))
  tab_res <- sapply(raw_res,function(x){table(cut(x,breaks=age_breaks))})
  upper <- apply(tab_res, 1, function(x) quantile(x, 0.975))
  upper
}
