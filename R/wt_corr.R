#' Survey Weighting - adjustment for nonresponse
#' Adjust the baseline NJP weights for nonresponse in a particular variable
#' @param data dataset. must contain strata, permid, wt_n, wt_Nc
#' @param wt_var variable to weight, adjusting for nonresponse. input as character
#' @return An object of class \code{survey.design}
#' @note This code was tranlasted from Stata wt_corr.ado, written by GMAC  For example, you can set alpha=0.10 to obtain one-sided test at 0.05 significance level.
#' @author David Aaby <david.aaby@@northwestern.edu>
#' @export wt_corr
#' @import stats
#' @import survey
#' @examples
#'\dontrun{
#'df = read_dta("data/ddis2_hw.dta")
#'df = data.frame(df)
#'wt_corr(data=df, wt_var="anypdsm_t0")
#'}


wt_corr = function(data, wt_var) {

  # count non-missing values #
  t_avail = aggregate(list(t_avail = data[,wt_var]),
                      by = list(strata=data[,"strata"]),
                      function(x) {sum(!is.na(x))})


  # merge with data and sort by permid #
  data2 = merge(data, t_avail, by="strata")
  data2 = data2[order(data2$permid),]

  df = data2[complete.cases(data2[,wt_var]),]

  # compute corrected case weights #
  df$t_strata = df$strata
  df$t_corr = df$wt_n / df$t_avail
  df$t_Nc = df$wt_Nc * df$t_corr
  df$t_WT = df$wt_Nc / df$t_avail
  df$t_wt = df$t_WT / mean(df$t_WT, na.rm=T)


  adj.svydesign = svydesign(ids = ~permid,
                            strata = ~t_strata,
                            weights = ~t_wt,
                            data = df)

  return(adj.svydesign)

}


