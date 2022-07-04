woe_tbls = function(data,
                    y,
                    flag_bad=1,
                    sample_col,
                    exclude=NULL,
                    include=NULL,
                    warn = NULL,
                    debug = FALSE
){
  data = as.data.frame(data)
  col_nameset = colnames(data)
  
  if(!is.null(exclude) & !is.null(include)) stop("Just one param: 'include' or 'exclude', must be passed.")
  if(!(y %in% col_nameset)) stop(paste(y," not found in data."))
  if(!is.null(include)) if(sum(!(include %in% col_nameset))) stop("Some 'include' var not found in data.")
  if(!(sample_col %in% col_nameset)) stop(paste("Error:",sample_col," not found in data."))
  
  data$sample_col = data[,sample_col]
  samples_names = unique(data$sample_col)
  
  if(is.null(include)){
    Xnames = col_nameset[!col_nameset %in% c(y,exclude)]
  }else Xnames = include
  
  if(!is.null(warn)){
    default_w <- getOption("warn")
    options(warn = warn)
  }
  
  woe_tbls = list()
  
  for (var in Xnames) {
    if(debug) print(var)
    
    woe = list()
    
    for (sample_name in samples_names) {
      if(sum(is.na(data[data$sample_col == sample_name,][,y])) == 0){
        woe_tmp  = woe(data[data$sample_col == sample_name,var],
                       data[data$sample_col == sample_name,y])
        woe[[sample_name]] = woe_tmp
      }
    }
    
    woe$TOT = woe(data[!is.na(data[,y]),var],
                  data[!is.na(data[,y]),y])
    
    woe = woe |> as.data.frame()
    colnames(woe) = c("cat",colnames(woe[-1]))
    colnames(woe) = str_replace_all(colnames(woe),".woe","")
    
    woe = woe |> select(-contains("x"))
    
    woe_tbls[[var]] = woe 
  }
  
  if(!is.null(warn)) options(warn = default_w)
  
  return(woe_tbls)
}