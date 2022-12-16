random_eff_reliabilities <- function(model_tech4){
  
  fs_reliabilities <- list()
  between <- list()
  for(i in 1:length(model_tech4)){
    fs_reliabilities[i] <- 
      model_tech4[[i]][2]
    between[[i]] <- fs_reliabilities[[i]][[1]]
  }
  between_cov_var <- list()
  between_var <- list()
  for(i in 1:length(between)){
    between_cov_var[[i]] <- between[[i]][[2]]
    between_var[[i]] <- list()
    for(j in 1:ncol(between_cov_var[[i]])){
      between_var[[i]][[j]] <- between_cov_var[[i]][j,j]
    }
  }
  
  
  within <- list()
  within_mean_est <- list()
  for(i in 1:length(fs_reliabilities)){
    within[[i]] <- fs_reliabilities[[i]][-1]
    within_mean_est[[i]] <- list()
    for(j in 1:length(within[[i]])){
      within_mean_est[[i]][[j]] <-within[[i]][[j]][[1]]
    }
  }
  
  fs_rel_list <- list()
  est <- list()
  for(i in 1:length(fs_reliabilities)){
    fs_rel_list[[i]] <- list()
    est[[i]] <- list()
    for(j in 1:length(between_var[[i]])){
      fs_rel_list[[i]][[j]] <- list()
      est[[i]][[j]] <- list()
      for(k in 1:length(within_mean_est[[i]])){
        est[[i]][[j]][[k]] <- within_mean_est[[i]][[k]][[j]]
      } 
      fs_rel_list[[i]][[j]] <- (var(do.call("rbind", est[[i]][[j]]))/between_var[[i]][[j]])  
    }
  }
  
  
  fs_rel_rows <- list()
  for(i in 1:length(fs_rel_list)){
    fs_rel_rows[[i]] <- do.call("cbind", fs_rel_list[[i]])
    colnames(fs_rel_rows[[i]]) <- colnames(between[[i]]$latMeansEst)
  }
  return(fs_rel_rows)
}
