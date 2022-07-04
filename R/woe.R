#' @param deploy_r TRUE or FALSE.
#' @return A data_frame with the vars DC.
#' @import dplyr 
#'@examples
#' @export
IV = function(x,
                y,
               flag_bad=1,
               max_cat = 20,
               return_table=F,
               warn = NULL
){
  
  if(!is.null(warn)){
    default_w <- getOption("warn")
    options(warn = warn)
  }
  
  x = as.factor(x)
  
  if(length(unique(x)) > max_cat) stop("'x' have too many categories, chance max_cat param.")
  if(length(unique(y)) > 2) stop("'y' have > 2 levels. Y need to be a flag.")
  if(sum(is.na(y))) stop("NA found in 'y'.")
  if(sum(is.na(x))) stop("NA found in 'x'.")
  
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

Base_escorada = Base_escorada[Base_escorada$Amostra=="DES",]
ivt = iv_table(Base_escorada$CAT_bk_303_012.ind_juventude,Base_escorada$FPD)
iv=ivt$IV |> sum()


woe_graphs = function(data,
                    y,
                    flag_bad=1,
                    sample_col,
                    exclude=NULL,
                    include=NULL,
                    path_save = getwd(),
                    warn = -1,
                    max_cat = 10,
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

woe_graphs(data = Base_escorada
         ,y = "FPD"
         ,sample_col = "Amostra"
         ,include = c("CAT_dados.score_SWCSVAR002","CAT_bk_303_012.ind_juventude")
         ,path_save = r'(/home/junior/Área de trabalho)'
         )

Base_escorada$Amostra |> table()
Base_escorada = Base_escorada[Base_escorada$Amostra != "OUT",]
getwd()

setwd(r'(/home/junior/Área de trabalho/)')
# 
# colnames(d[1]) = c("var")
# colnames(d) = c("var",colnames(d[-1]))
# d 
# 
# 
# for (t in w) {
#   print(t)
#   
#   
# }
# w[,-1]
# w
# d = w$CAT_Idade
# colnames()
# 
# 

