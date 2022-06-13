autoLinTest <- function(data,
                       y,
                       train_subset = c(rep(1,nrow(data))),
                       exclude=NULL,
                       include=NULL,
                       bonus_orig = 0.05,
                       show_cor= 0.7,
                       type_cor = "pearson",
                       n_qts = 10,
                       pct_na_allowed = 0.9,
                       quantil_variability_exclude = 0.015,
                       trim = 0.01,
                       subsample_rows=1,
                       debug = FALSE
                       ){
  
  # default_w <- getOption("warn")
  # options(warn = -1)
  
  col_nameset <- colnames(data)
  
  if(!is.null(exclude) & !is.null(include)) stop("Just one param: 'include' or 'exclude', must be passed.")
  if(!(y %in% col_nameset)) stop(paste(y," not found in data."))
  if(!is.null(include)) if(sum(!(include %in% col_nameset))) stop("Some 'include' var not found in data.")
  
  set.seed(57)
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
        q_inf = as.numeric(quantile(as.matrix(data_train[,var]),trim_inf,na.rm = T))
        q_sup = as.numeric(quantile(as.matrix(data_train[,var]),trim_sup,na.rm = T))
        
        data_train[,var] = as.numeric(ifelse(data_train[,var] < q_inf, q_inf,
                                             ifelse(data_train[,var] > q_sup, q_sup, data_train[,var])))
      }
    }
  }
  
  best_cor_transf_name = 0
  print("Auto lin. analyzer")
  print("##################")
  len_vars_remains = length(Xnames)
  count_var=0

  for (var in Xnames) {
    count_var=count_var+1
    if(debug) print(var)
    
    best_cor = 0
    
    transfs = list()
    
    transfs$Original = data_train[,var]
    transfs$Sqr = data_train[,var]**2
    transfs$Cubic = data_train[,var]**3
    transfs$Sqrt = data_train[,var]**(1/2)
    transfs$CubicRt = data_train[,var]**(1/3)
    transfs$Inv = data_train[,var]**-1
    transfs$InvSqr = data_train[,var]**-2
    transfs$InvCubic = data_train[,var]**-3

    if(min(data_train[,var],na.rm = T) > 0) {
      transfs$Log = log(data_train[,var])
      transfs$LogInv = 1/(log(data_train[,var]))
    }
    
    i=0
    for(transf in transfs){
      i = i+1
      
      data_train$temp = transf
      
      cuts = unique(quantile(data_train$temp 
                               ,probs = seq(0, 1, 1/10)
                               ,na.rm = T))
      n_points = length(cuts)-1
      
      points = data_train |> group_by(cut(data_train$temp 
                                          ,breaks = cuts,labels = F)) |> 
        summarize(avg_x = mean(temp,na.rm = T),avg_y = mean(get(y),na.rm = T)) |> drop_na()
      
      temp_cor =  cor.test(points$avg_x,points$avg_y, na.action = na.omit, method = type_cor)$estimate**2
      
      if(is.nan(temp_cor)) temp_cor = 0
      
      if(names(transfs[i]) == "Original") temp_cor = temp_cor + bonus_orig
        
      if(temp_cor > best_cor){
        best_cor = temp_cor
        best_cor_transf_name = names(transfs[i])
      }
    }
    
    # if((count_var/len_vars_remains)*100 % 10 ==0) print(paste(round((count_var/len_vars_remains)*100,2)))
    
    if(best_cor_transf_name == "Original") best_cor = best_cor-bonus_orig
    
    if(abs(best_cor) > show_cor){
      print(paste(round((count_var/len_vars_remains)*100,0),"% | Var:",var,"| Transf:",best_cor_transf_name,"| R2:",round(best_cor,3),"| n_qtds:",n_points))
    }
  }
  # options(warn = default_w)
}

