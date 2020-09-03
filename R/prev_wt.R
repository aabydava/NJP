#' Weighted prevalence - adjusted for nonresponse
#' Calcuated weighted prevalence for a given variable
#' @param data dataset. must contain strata, permid, wt_n, wt_Nc
#' @param wt_var variable to estimate, adjusting for nonresponse. input as character
#' @param subpop subpopulation
#' @return data frame
#' @author David Aaby <david.aaby@@northwestern.edu>
#' @export prev_wt
#' @import stats
#' @import survey
#' @examples
#'\dontrun{
#'df = read_dta("data/ddis2_hw.dta")
#'df = data.frame(df)
#'prev_wt(data=df, wt_var="anypdsm_t0")
#'}



prev_wt= function(data, wt_var, subpop = NULL) {

  if(is.null(subpop)) {
    wt.design = wt_corr(data=data, wt_var=wt_var)
    var = as.formula(paste("~",wt_var, sep=""))
    prev = svymean(var, wt.design, na.rm=T)
    prev = data.frame(prev)
    names(prev) = c("mean", "se")
    prev = prev * 100
    prev$var = wt_var
    prev$subpop = NA
    return(prev)
  }
  else {
    wt.design = wt_corr(data=data, wt_var=wt_var)
    subpop.design = subset(wt.design, eval(parse(text=subpop)))
    var = as.formula(paste("~",wt_var, sep=""))
    prev = svymean(var, subpop.design, na.rm=T)
    prev = data.frame(prev)
    names(prev) = c("mean", "se")
    prev = prev * 100
    prev$var = wt_var
    prev$subpop = subpop
    return(prev)
  }
}









