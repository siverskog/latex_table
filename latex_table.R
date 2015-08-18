########################################################################################
##### LATEX TABLE ######################################################################
########################################################################################

require(stringr)

.table <- function(x) {
  
  z <- NULL
  
  for(i in 1:nrow(x)) {
    
    z <- c(z, str_c(c(str_c(c(rownames(x)[i], x[i,]), collapse = " & "), "\\\\"), collapse = " "))
    
  }
  
  z <- str_replace_all(z, "%", "\\\\%")
  
  return(z)
  
}

########################################################################################

.begin.table <- function(ncol, type = "tabularx", align = c("l", rep("X", ncol))) {
  
  if(type=="tabularx") {
    begin <- paste("\\begin{", type, "}{\\textwidth}{", paste(align, collapse = ""), "}", sep = "")
  } else {
    begin <- paste("\\begin{", type, "}{", paste(align, collapse = ""), "}", sep = "")
  }
  
  
  return(begin)
}

########################################################################################

.end.table <- function(type = "tabularx") {
  end <- paste("\\end{", type, "}", sep = "")
  return(end)
}

########################################################################################

.multicol <- function(size, names) {
  
  if(length(names)>1) {
    multicol <- str_c(c(str_c(c("", paste("\\multicolumn{", 3, "}", "{c}{", names, "}", sep = "")), collapse = " & "), "\\\\"), collapse = " ")
  } else {
    multicol <- paste("\\multicolumn{", size, "}{c}{", names, "} \\\\")
  }
  
  return(multicol)
}

########################################################################################

.cmidrule <- function(size, length) {
  cmidrule <- paste("\\cmidrule(lr){", paste(seq(2,length+1-size,size), seq(1+size,length,size), sep = "-"), "}", sep = "")
  return(cmidrule)
}

########################################################################################

.header <- function(names) {
  header <- str_c(c(str_c(c("", names), collapse = " & "), "\\\\"), collapse = " ")
  return(header)
}

########################################################################################

header <- function(h, cmid = TRUE) {
  
  n <- length(h[[length(h)]])
  
  if(length(h)>2) {
    
    a <- .multicol(n/length(h[[1]]) + 1, h[[1]])
    b <- .multicol(n/length(h[[2]]), h[[2]])
    c <- .cmidrule(n/length(h[[2]]), n + 1)
    d <- .header(h[[3]])
    
    header <- c("\\toprule", a, "\\toprule", b, c, d, "\\midrule")
    
  } else if(length(h)==2 & cmid) {
    
    a <- .multicol(n/length(h[[1]]), h[[1]])
    b <- .cmidrule(n/length(h[[1]]), n + 1)
    c <- .header(h[[2]])
    
    header <- c("\\toprule", a, b, c, "\\midrule")
    
  } else if (length(h)==2 & !cmid) {
    
    a <- .multicol(n/length(h[[1]]) + 1, h[[1]])
    b <- .header(h[[2]])
    
    header <- c("\\toprule", a, "\\toprule", b, "\\midrule")
  
  } else {
    
    a <- .header(h[[1]])
    header <- c("\\toprule", a, "\\midrule")
    
  }
  
  return(header)
}

########################################################################################

latex.table <- function(h, t, size, type = "tabularx", align = c("l", rep("X", ncol))) {
  
  begin <- .begin.table(size, type)
  end <- .end.table(type)
  
  tab <- NULL
  
  for(i in 1:length(h)) {
    tab <- c(tab, h[[i]], .table(t[[i]]))
  }
  
  tab <- c(begin, tab, "\\bottomrule", end)
  return(tab)
  
}
