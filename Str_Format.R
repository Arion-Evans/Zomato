str_format <- function(df){
  df_sum = data.frame(Variable = names(df),
                  Class = sapply(df, class),
                  Cardinality = sapply(df, function(x){
                    if(class(x) == "factor"){
                      return(length(levels(x)))
                    }
                    return(length(unique(x)))
                  }),
                  Levels = sapply(df, function(x){
                    if(class(x) == "factor"){
                      return(paste0(head(levels(x)), collapse = ", "))
                    }
                    return(paste0(head(unique(x)), collapse = ", "))
                  }),
                  First_Values = sapply(df, function(x) paste0(head(x),  collapse = ", ")),
                  row.names = NULL) 
  
  colnames(df_sum)[5] = "First Values"
  colnames(df_sum)[4] = "First Levels"
  
  return(df_sum)
}