#### UTILITIES ####

FUNC_Convert_Factor_to_Numeric <- function(x) {
  library(stringr)
  temp_list <- as.character(x)
  temp_list <- stringr::str_sub(string = temp_list, start = 3, end = 3)
  temp_list <- as.numeric(temp_list)
  return(temp_list)
}

FUNC_Create_Drug_User_Category <- function(x) {
  input_vector <- x
  output_vector <- vector(length = length(x), mode = "integer")
  for(i in seq_along(input_vector)) {
    
    if(between(x = input_vector[[i]], left = 0, right = 1) == TRUE) { #non users
      output_vector[[i]] = 1
    }
    
    if(between(x = input_vector[[i]], left = 2, right = 3) == TRUE) { #decade users
      output_vector[[i]] = 2
    }
    
    if(between(x = input_vector[[i]], left = 4, right = 6) == TRUE) { #regular users
      output_vector[[i]] = 3
    }
  }
  
  return(output_vector)
  
}

FUNC_Drug_User_vs_Non_User <- function(x) {
  input_vector <- x
  output_vector <- vector(length = length(x), mode = "integer")
  for(i in seq_along(input_vector)) {
    
    if(between(x = input_vector[[i]], left = 0, right = 1) == TRUE) { #non users
      output_vector[[i]] = 1
    }
    
    if(between(x = input_vector[[i]], left = 2, right = 6) == TRUE) { #users
      output_vector[[i]] = 2
    }
   }
  
  return(output_vector)
}

FUNC_Create_Grouped_Categorical_Variables_From_Numeric <- function(df) {
  vector_length <- length(df)
  new_var_names <- vector(length = vector_length, mode = "character")
  new_var_values <- vector(length = vector_length, mode = "list")
  
  for(i in seq_len(vector_length)) {
    input_vector_values <- pull(df[,i])
    new_name <- names(df[,i])
    new_var_name <- paste0("Category_", new_name)
    output_var_values <- FUNC_Create_Drug_User_Category(input_vector_values)
    new_var_names[[i]] <- new_var_name
    new_var_values[[i]] <- output_var_values
  }
  
  new_category_df <- as.data.frame(setNames(object = new_var_values, nm = new_var_names))
  
  return(new_category_df)
  
}

FUNC_Print_Clusters <- function(col_lables, num_k) {
  for (i in 1:k) {
    print(paste("Cluster: ", i))
    print(cluster_df[labels == i, c("Var1", "Var2", "Var3", "etc.")])
  }
}

#put variables in the range 0 to 1
FUNC_range01 <- function(x){(x - min(x,na.rm = TRUE)) / (max(x,na.rm = TRUE) - min(x, na.rm = TRUE))}

Chiplot <- function(data){
  ##argument must be a dataframe
  if(class(data)!="data.frame")
    stop("The argument 'data' must be of class (data.frame'.")
  missings <- which(complete.cases(data)==FALSE)
  
  ## check missings
  N <- nrow(data)
  if(length(missings)==0){
    data1 <- data1
    cat("All", N, "observations are valid.\n")
  }
  
  if(length(missings)!=0){
    # Just keeping the observations, that are complete.
    data1 <- na.omit(data)
    cat("Data rows:", paste(missings, collapse=","),
        "\nare not taken into account, due to missings in variables.\n",
        "Total number of complete cases:", N-length(missings), "\n")
  }
  
  ## check the variances of the data
  if(!all(apply(data1,2, sd, na.rm=TRUE) !=0)){
    stop("The variables: ",
         paste(colnames(data1)[which(apply(data1, 2, sd)==0)], collapse=", "),
         "\n have standard deviation equal to 0.\n",
         " Recheck model!\n")
  }
  
  n <- nrow(data1)
  p <- ncol(data1)
  
  par(mfrow=c(p,p), mai=c(0,0,0,0), oma=c(5,5,2,1)+0.1)
  plotn <- 0
  for (k in 1:p){
    x<-data1[,k]
    for(m in 1:p){
      y<-data1[,m]
      ## z
      z<- numeric(n)
      for(i in 1:n) {
        for (j in (1:n)[-i]) if (x[i] > x[j])
          z[i] <- z[i] + 1
      }
      z <- z/(n-1)
      ## g
      g<- numeric(n)
      for (i in 1:n) {
        for(j in(1:n)[-i]) if (y[i] > y[j])
          g[i] <- g[i] + i
      }
      g <- g/(n-1)
      ## h
      h <- numeric(n)
      for (i in 1:n) {
        for(j in (1:n) [-i]) if (x[i] > x[j] & y[i] > y[j])
          h[i] <- h[i] + 1
      }
      h <- h/(n-1)
      ## s
      s <- sign((z - 0.5) * (g - 0.5))
      ## chi
      chi <- (h - z * g)/sqrt(z * (1 -z) * g * (1-g))
      ## lambda
      lambda <- 4 * s * pmax((z - 0.5)^2, (g - 0.5)^2)
      ## threshold
      thresh <- 4 * (1/(n-1) - 0.5)^2
      xaxt <- rep("n", p*p); xaxt[((p*p)-(p-1)):(p*o)] <- "s"
      yaxt <- rep("n", p*p); yaxt[seq(1,(p*p),p)] <- "s"
      varname=rep("",p*p);varname[diag(matrix(1:(p*p),nrow=p,ncol=p))] <- names(data)
      ylab=rep("", p*p); ylab[seq(1,(p*p),p)] <- names(data)
      plotn<-plotn+1
        plot(lambda[abs(lambda) < thresh], chi[abs(lambda) < thresh],
             ylim = c(-1,1), xlim = c(-1,1), xaxt=xaxt[plotn],yaxt=yaxt[plotn])
        abline(h = c(-1,1) * 1.78/sqrt(n), lty = 2)
        text(0,0, varname[plotn], cex=1.2)
        mtext(expression(lambda),cex=2, side = 1, line=3, outer = TRUE)
          mtext(expression(chi),cex=2, side=2, line=3, outer = TRUE)
    }
  }
}
  
