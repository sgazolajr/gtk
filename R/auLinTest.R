#' autoLinTest
#'
#' This function auto transfrom  your vars and test the linearity for regression models.
#'
#' @param data R data frame object.
#' @param y Name of you target column.
#' @param train_subset Vector of 0 and 1, with the same len of data.
#' @param include List with name of variables to be analyzed by the algorithm.
#' @param exclude If you do not want to pass the name of the Data Frame variables to be analyzed by the algorithm, you also have to pass the list of data frame variables that are not analyzed, for example the key variables, and everything else will be analyzed.
#' @param bonus_orig Bonus give to original var.
#' @param show_r2 Min value of R2 to print.
#' @param method_cor Method to calculate the correlation with the y.
#' @param n_qts Number of points the function will try to cut your var to analyze.
#' @param pct_na_allowed Pct of NA allowed in each covariable.
#' @param quantil_variability_exclude Pct of qunatil, to remove vars that not present variability before discretize.
#' @param trim Pct of trim vars, to remove outliers before discretize.
#' @param warn Indicate if function will show warnings.
#' @param subsample_rows Proportion of your data that function will run.
#' @param seed Seed to subsample rows.
#' @param debug debug.
#' @return Dont return anything, just print the R squares.
#' @import dplyr 
#' @import tidyr
#' @importFrom stats quantile
#' @importFrom stats cor
#'@examples
#'\dontrun{
#'autoLinTest(data = data
#'            ,y = "FPD"
#'            ,train_subset = data$Flag_DES
#'            ,include = include
#'            ,show_cor = 0.7
#' )
#'}
#' @export
autoLinTest <- function(data,
                       y,
                       train_subset = c(rep(1,nrow(data))),
                       exclude=NULL,
                       include=NULL,
                       bonus_orig = 0.05,
                       show_r2= 0.7,
                       method_cor = "pearson",
                       n_qts = 10,
                       pct_na_allowed = 0.9,
                       quantil_variability_exclude = 0.015,
                       trim = 0.01,
                       warn = NULL,
                       subsample_rows=1,
                       seed=NULL,
                       debug = FALSE
                       ){
  
  if(!is.null(warn)){
    default_w <- getOption("warn")
    options(warn = -1)
  }
  
  temp <-NULL
  
  col_nameset <- colnames(data)
  
  if(!is.null(exclude) & !is.null(include)) stop("Just one param: 'include' or 'exclude', must be passed.")
  if(!(y %in% col_nameset)) stop(paste(y," not found in data."))
  if(!is.null(include)) if(sum(!(include %in% col_nameset))) stop("Some 'include' var not found in data.")
  
  if(!is.null(seed)) set.seed(seed)
  
  if(subsample_rows<1-0.00001) data = data[sample(nrow(data),nrow(data)*subsample_rows),]
  data_train = data[train_subset==1,]
  if(sum(is.na(data_train[,y]))) stop("NA Found in y (train subset).")
  
  if(is.null(include)){
    Xnames = col_nameset[!col_nameset %in% c(y,exclude)]
  }else Xnames = include
  
  for (var in Xnames) if(!(is.numeric(as.matrix(data[,var])))){
    Xnames = Xnames[!Xnames %in% var]
  }else{
    if(quantil_variability_exclude > 0 &
       (as.numeric(quantile(as.matrix(data_train[,var]),quantil_variability_exclude,na.rm = TRUE)) == as.numeric(quantile(as.matrix(data_train[,var]),1-quantil_variability_exclude,na.rm = TRUE)))){
      Xnames = Xnames[!Xnames %in% var]
    }else{
      if(mean(is.na(data_train[,var])) > pct_na_allowed){
        Xnames = Xnames[!Xnames %in% var]
      }else if(trim > 0 & trim < 1){
        trim_inf = trim
        trim_sup = 1-trim
        q_inf = as.numeric(quantile(as.matrix(data_train[,var]),trim_inf,na.rm = TRUE))
        q_sup = as.numeric(quantile(as.matrix(data_train[,var]),trim_sup,na.rm = TRUE))
        
        data_train[,var] = as.numeric(ifelse(data_train[,var] < q_inf, q_inf,
                                             ifelse(data_train[,var] > q_sup, q_sup, data_train[,var])))
      }
    }
  }
  
  best_r2_transf_name = 0
  print("Auto lin. analyzer")
  print("##################")
  len_vars_remains = length(Xnames)
  count_var=0

  for (var in Xnames) {
    count_var=count_var+1
    if(debug) print(var)
    
    best_r2 = 0
    
    transfs = list()
    
    transfs$Original = data_train[,var]
    transfs$Sqr = data_train[,var]**2
    transfs$Cubic = data_train[,var]**3
    transfs$Sqrt = data_train[,var]**(1/2)
    transfs$CubicRt = data_train[,var]**(1/3)
    transfs$Inv = data_train[,var]**-1
    transfs$InvSqr = data_train[,var]**-2
    transfs$InvCubic = data_train[,var]**-3

    if(min(data_train[,var],na.rm = TRUE) > 0) {
      transfs$Log = log(data_train[,var])
      transfs$LogInv = 1/(log(data_train[,var]))
    }
    
    i=0
    for(transf in transfs){
      i = i+1
      
      temp_r2 = 0
      
      data_train$temp = transf
      
      cuts = unique(quantile(data_train$temp 
                               ,probs = seq(0, 1, 1/10)
                               ,na.rm = TRUE))
      n_points = length(cuts)-1
      
      if(n_points > 2){
        points = data_train |> group_by(cut(data_train$temp 
                                            ,breaks = cuts,labels = F)) |> 
          summarize(avg_x = mean(temp,na.rm = TRUE),avg_y = mean(get(y),na.rm = TRUE)) |> drop_na()
        
        temp_r2 = cor(points$avg_x,points$avg_y, use = "complete.obs", method = method_cor)**2
        
        if(is.nan(temp_r2)) temp_r2 = 0
        
        if(names(transfs[i]) == "Original") temp_r2 = temp_r2 + bonus_orig
        
        if(temp_r2 > best_r2){
          best_r2 = temp_r2
          best_r2_transf_name = names(transfs[i])
        }
      }

    }
    
    # if((count_var/len_vars_remains)*100 % 10 ==0) print(paste(round((count_var/len_vars_remains)*100,2)))
    
    if(best_r2_transf_name == "Original") best_r2 = best_r2-bonus_orig
    
    if(abs(best_r2) > show_r2){
      print(paste(round((count_var/len_vars_remains)*100,0),"% | Var:",var,"| Transf:",best_r2_transf_name,"| R2:",round(best_r2,3),"| n_qtds:",n_points))
    }
  }
  if(!is.null(warn)) options(warn = default_w)
}

