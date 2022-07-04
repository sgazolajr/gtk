#' @param deploy_r TRUE or FALSE.
#' @return A data_frame with the vars DC.
#' @import dplyr 
#'@examples
#' @export
woe = function(x,
                y,
               flag_bad=1 
){
  
  x = as.factor(x)
  df = data.frame(x = x, y = y)
  n_bad = nrow(df[df$y==1,])
  n_good = nrow(df[df$y==0,])
  woe = df |> group_by(x) |> 
    summarise(p_bads = sum(y==1)/n_bad,p_goods = sum(y==0)/n_good,woe = log((p_goods/p_bads))) |> as.data.frame()
  
  return(woe[,c("x","woe")])
}

# Base_escorada = Base_escorada[Base_escorada$Amostra=="DES",]
# woe(Base_escorada$CAT_bk_303_012.ind_juventude,Base_escorada$FPD)
# 
# 
# 
# 
# 
# 
# 
# w  =woe_tbls(Base_escorada
#          ,y = "FPD"
#          ,sample_col = "marca"
#          ,include = c("CAT_Idade","CAT_bk_302_006.qtde_ubs_5_km"))
# 
# d = w$CAT_Idade
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
# d_r = reshape(d,direction = "long",idvar = "CAT",varying = colnames(d[-1]),times = colnames(d[-1]),v.names = "WoE")
# 
# ggplot(data=d_r, aes(x=time, y=WoE, group=CAT,color = cat)) +
#   geom_line()
