#' Random sampling of age-at-death estimations of archaeological skeletal remains with assumed uniform probability distribution
#'
#' This function generates randomly sampled iterations of exact age-at-death estimations of archaeological remains.
#' @param est_min a vector specifying the lower margin of the age-at-death estimate for every individual.
#' @param est_max a vector specifying the upper margin of the age-at-death estimate for every individual.
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
#' ucd_raw_unif(df$min, df$max, n_it=10)
#'
#'       V1        V2        V3       V4        V5        V6        V7        V8        V9       V10
#' 1  5.857529  4.061994  5.861995 5.007461  5.820511  5.649194  5.725379  4.774488  4.126411  4.023960
#' 2  9.406048  7.329166  5.309466 9.821312 10.456361  9.668272  9.993989  8.235749  7.185523  5.400802
#' 3 11.573547 10.957346 11.882876 8.224482  8.239743 12.036655 11.331103 13.656789 13.557271 12.467719
#' 4  5.888177  5.534762  3.669243 4.063005  4.667849  3.686799  3.784361  6.422429  2.973179  2.012116
#' 5  6.529573  5.613122  7.797817 4.462624  8.432753  5.983132  6.796087  6.730050  5.090240  6.645774
#' # rows = individuals; columns = iterations



ucd_raw_unif <- function(est_min, est_max, n_it=2000){
  raw_res <- as.data.frame(do.call(cbind,purrr::rerun(n_it,
              (est_min-1)+((est_max+1)-(est_min-1))*runif(length(est_min)))))
  raw_res
}

