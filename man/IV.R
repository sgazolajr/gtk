#' IV
#'
#' Compute IV of your covariable.
#'
#' @param x Your covar.
#' @return Information Value.
#' @import dplyr 
#' @export
IV = function(x,
              y,
              flag_bad=1,
              max_cat = 20,
              return_table=F,
              na.rm = F,
              warn = NULL
){
  
  if(!is.null(warn)){
    default_w <- getOption("warn")
    options(warn = warn)
  }
  
  x = as.factor(x)
  
  if(na.rm){
    y = y[!is.na(y)]
    x = x[!is.na(x)]
  }
  
  if(length(unique(x)) > max_cat) stop("'x' have too many categories, chance max_cat param.")
  if(length(unique(y)) > 2) stop("'y' have > 2 levels. Y need to be a flag.")
  if(sum(is.na(y))) stop("NA found in 'y'.")
  if(sum(is.na(x))) stop("NA found in 'x'.")
  if(length(x) != length(y)) stop("length(x) != length(y)")
  
  if(flag_bad == 0){
    y_tmp = ifelse(y==1,0,1)
    y = y_tmp
  }
  
  df = data.frame(x = x, y = y)
  n_bad = nrow(df[df$y==1,])
  n_good = nrow(df[df$y==0,])
  n_tot = nrow(df)
  
  iv_table = df |> group_by(x) |> 
    summarise(
      rep = (n()/n_tot)
      ,bad_rate = mean(y,na.rm=T)
      ,count_bads = sum(y==1)
      ,count_goods = sum(y==0)
      ,p_bads = (count_bads/n_bad)
      ,p_goods = (count_goods/n_good)
      ,WoE = log((p_goods/p_bads))
      ,IV = (p_goods - p_bads)*WoE
    ) |> as.data.frame()
  
  if(!is.null(warn)) options(warn = default_w)
  
  if(return_table){
    return(iv_table)
  }else return(sum(iv_table$IV))
}