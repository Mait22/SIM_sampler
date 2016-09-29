#Helpers
empty_df <- function(rows,cols,row.names = NULL,col.names = NULL){
  
  df <- as.data.frame(matrix(data = rep(NA, times = rows*cols),nrow = rows,ncol = cols))
  
  if(is.null(col.names) == FALSE & dim(df)[2] == length(col.names)){
    names(df) <- col.names  
  }
  
  if(is.null(row.names) == FALSE & dim(df)[1] == length(col.names)){
    row.names(df) <- row.names  
  }
  
  return(df)
  
  
}



append_vec <- function(original, add){
  original[(length(original)+1):(length(original)+length(add))] <- add
  return(original)
}


#Classes
setClass("sample.object", 
         representation(group.1 = "character",
                        group.2 = "character",
                        group.1.levels = "character",
                        group.2.levels = "character",
                        stratas = "character",
                        df.original = "data.frame",
                        df.sample = "data.frame",
                        df.sample.description = "data.frame",
                        sample.round = "numeric",
                        strata.lab = "character",
                        sampled.lab = "character",
                        sample.prop = "numeric"
         ))



#Worker func
sampler <- function(df,
                    group.1,
                    group.2,
                    ID.col,
                    strata.lab = "Strata",
                    sampled.lab = c("Valimis","Jah","Ei"),
                    sample.round = 0.05,
                    df.sample.description.labs = 
                      c("Grupp I", "Grupp II", "Kiht","n valimis","N populatsioonis","Valimi osakaal populatsioonist"),
                    sample.prop
){
  
  return_results <- new("sample.object")
  
  return_results@group.1 <- if(class(group.1) == "numeric"){names(df)[group.1]} 
  else if (class(group.1) == "character"){group.1} 
  return_results@group.2 <- if(class(group.2) == "numeric"){names(df)[group.2]} 
  else if (class(group.2) == "character"){group.2}
  
  w_data <- df[,c(ID.col,group.1,group.2)]
  w_data[,c(strata.lab)] <- paste(w_data[,group.1], w_data[,group.2], sep =  " ### ")
  w_data[,sampled.lab[1]] <- rep(NA, times = dim(w_data)[1])
  
  stratas <- unique(w_data[,c(strata.lab)])
  
  
  return_results@stratas <- stratas
  return_results@group.1.levels <- unique(df[,group.1])
  return_results@group.2.levels <- unique(df[,group.2])
  return_results@sample.round <- sample.round
  return_results@sample.prop <- sample.prop
  return_results@sampled.lab <- sampled.lab
  return_results@strata.lab <- strata.lab
  
  
  sampled_IDs <- c()
  subset_dim <- c()
  for (s in stratas){
    subset <- w_data[w_data[,c(strata.lab)] == s,]
    N_subset <- dim(subset)[1]
    
    subset_dim <- append_vec(subset_dim,N_subset)
    
    #Correcting for rounding error in small samples
    if(round(N_subset*sample.prop,0) < (N_subset*sample.prop) & 
       abs( ((round(N_subset*sample.prop,0))-(N_subset*sample.prop)) /(N_subset*sample.prop) ) >= sample.round){
      sample_size <- (N_subset*sample.prop)%/%1 + 1
    }
    
    if(round(N_subset*sample.prop,0) < (N_subset*sample.prop) & 
       abs( ((round(N_subset*sample.prop,0))-(N_subset*sample.prop)) /(N_subset*sample.prop) ) < sample.round){
      sample_size <- (N_subset*sample.prop)%/%1
    }
    
    
    if(round(N_subset*sample.prop,0) >= (N_subset*sample.prop)){
      sample_size <- round(N_subset*sample.prop,0)
    }
    
    
    sample <- sample(subset[,ID.col],size = sample_size,replace = FALSE)
    if(sum(sample %in% sampled_IDs) > 0){
      warning(paste("Units with ID-s", as.character(sample[sample %in% sampled_IDs]), 
                    "already included in other stratas",sep = " "))
    }
    sampled_IDs <- append_vec(sampled_IDs,sample)
    
  }
  
  
  
  
  for(i in c(1:dim(w_data)[1])){
    if(w_data[i,ID.col] %in% sampled_IDs == TRUE){
      w_data[i,sampled.lab[1]] <- sampled.lab[2]
    }
    if(w_data[i,ID.col] %in% sampled_IDs == FALSE){
      w_data[i,sampled.lab[1]] <- sampled.lab[3]
    }
  }
  
  return_results@df.sample <- w_data[w_data[,sampled.lab[1]] == sampled.lab[2],]
  return_results@df.original <- w_data
  
  
  #Sample description df
  sample_description <- empty_df(rows = length(stratas),
                                 cols = length(df.sample.description.labs),col.names =  df.sample.description.labs)
  sample_description[,3] <- stratas
  sample_description[,1] <- unlist(lapply(strsplit(sample_description[,3],split = " ### "),"[",1)) 
  sample_description[,2] <- unlist(lapply(strsplit(sample_description[,3],split = " ### "),"[",2)) 
  
  
  for(i in c(1:dim(sample_description)[1])){
    sample_description[i,4] <- sum(sample_description[i,3] == w_data[,c(strata.lab)] & 
                                     w_data[,sampled.lab[1]] == sampled.lab[2])
    sample_description[i,5] <- sum(sample_description[i,3] == w_data[,c(strata.lab)])
    sample_description[i,6] <- sum(sample_description[i,3] == w_data[,c(strata.lab)] & 
                                     w_data[,sampled.lab[1]] == sampled.lab[2]) /
      sum(sample_description[i,3] == w_data[,c(strata.lab)])
    
    if(sum(sample_description[i,3] == w_data[,c(strata.lab)] & w_data[,sampled.lab[1]] == sampled.lab[2]) != 
       sum(sample_description[i,3] == return_results@df.sample[,c(strata.lab)])){
      
      warning("Inconsistent number of obs. between DF-s")
      
    }
  }
  
  return_results@df.sample.description <- sample_description
  
  
  return(return_results)
}

#Call demo
#a <- sampler(df = structure,
#              group.1 = "ASUTUSE.VEERG", 
#              group.2 = "JUHTIMISTASANDI.VEERG", 
#              ID.col = "ID.REA.NR",
#              sample.prop = 0.20)
