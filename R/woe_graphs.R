#' woe_graphs
#'
#' Save WoE Graphs of your covariables.
#'
#' @param data R data frame object.
#' @return Information Value.
#' @import dplyr 
#' @export
woe_graphs = function(data,
                      y,
                      flag_bad=1,
                      sample_col,
                      exclude=NULL,
                      include=NULL,
                      path_save = getwd(),
                      max_cat = 10,
                      warn = -1,
                      debug = FALSE
){
  data = as.data.frame(data)
  col_nameset = colnames(data)
  
  if(!is.null(exclude) & !is.null(include)) stop("Just one param: 'include' or 'exclude', must be passed.")
  if(!(y %in% col_nameset)) stop(paste(y," not found in data."))
  if(!is.null(include)) if(sum(!(include %in% col_nameset))) stop("Some 'include' var not found in data.")
  if(!(sample_col %in% col_nameset)) stop(paste("Error:",sample_col," not found in data."))
  
  if(!is.null(warn)){
    default_w <- getOption("warn")
    options(warn = warn)
  }
  
  data$sample_col = data[,sample_col]
  samples_names = unique(data$sample_col)
  
  if(is.null(include)){
    Xnames = col_nameset[!col_nameset %in% c(y,exclude)]
  }else Xnames = include
  
  
  
  for (var in Xnames) {
    if(debug) print(var)
    if(!(is.numeric(as.matrix(data[,var])))){
      if(length(unique(data[,var])) > max_cat | length(unique(data[,var])) == 1){
        Xnames = Xnames[!Xnames %in% var]
      }
    }
  }
  
  if(sum(is.na(data[,c(Xnames,y)]))) stop("NA found in data.")
  
  woe_tbls = list()
  
  for (var in Xnames) {
    if(debug) print(var)
    
    woe = list()
    
    for (sample_name in samples_names) {
      if(sum(is.na(data[data$sample_col == sample_name,][,y])) == 0){
        woe_tmp  = IV(data[data$sample_col == sample_name,var],
                      data[data$sample_col == sample_name,y],return_table=TRUE)
        
        woe[[sample_name]] = woe_tmp[,c("x","WoE")]
      }
    }
    
    woe_tmp = IV(data[!is.na(data[,y]),var],
                 data[!is.na(data[,y]),y],return_table=TRUE)
    
    woe$TOT = woe_tmp[,c("x","WoE")]
    
    woe = woe |> as.data.frame()
    colnames(woe) = c("CAT",colnames(woe[-1]))
    colnames(woe) = str_replace_all(colnames(woe),".WoE","")
    
    woe = woe |> select(-contains("x"))
    
    woe_tbls[[var]] = woe 
    
    dir.create(paste0(path_save,"/tables_woe/"))
    
    write.table(woe,file = paste0(path_save,"/tables_woe/",var,"_woe.csv")
                ,quote = F
                ,sep = ";"
                ,na = ""
                ,row.names = F
                ,dec = ",")
    
    woe = reshape(woe,direction = "long"
                  ,idvar = "CAT"
                  ,varying = colnames(woe[-1])
                  ,times = colnames(woe[-1])
                  ,v.names = "WoE")
    
    g_tmp = ggplot(data=woe, aes(x=time, y=WoE, group=CAT,color = CAT)) +
      geom_line() +
      labs(x=NULL)+
      theme_bw()
    
    dir.create(paste0(path_save,"/graphs_woe/"))
    ggsave(paste0(path_save,"/graphs_woe/",var,"_woe.png"),g_tmp)
  }
  if(!is.null(warn)) options(warn = default_w)
}