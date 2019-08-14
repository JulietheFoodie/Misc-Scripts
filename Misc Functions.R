# Test for and convert 1,0 factors

booRange <- function(a) {
  if ((max(na.omit(a) - min(na.omit(a)))) == 1 
      | ((max(na.omit(a) - min(na.omit(a))) == 0))) 
    return(TRUE) 
  else  return(FALSE)
}

df_sub2 <- df

df_sub2  %<>%
  select_if(is.integer) %>%
  select_if(booRange) %>% 
  mutate_all(factor, levels = c(0, 1),
             labels = c("n", "y"))

boo <- colnames(df_sub2)

df[boo] <- df_sub2[boo]



# Filter out columns by low content

lowval_filt <- function(df, max){
  d <- df %>% 
    as.data.frame() %>%
    summarise_each(funs(100*mean(is.na(.))))
  
  #filter
  d[nrow(d) + 1,] <- 0
  d2 <- d[, colSums(d) > max]
  cols <- colnames(d2)
  drop <- c(cols)
  
  output <- df[setdiff(names(df), drop)]
  
  return(output)
}



# Add column name to element 
cols <- colnames(df[, names(df) != "ID"])

df[cols] <- as.data.frame(sapply(cols,
                                 function(name){
                                   paste(name, df[, name], sep = ".")
                                 }))




# Count data which meets filter setting
# put output df in input df to add lines
N <- 0
erow_df <- data.frame(Name = rep("", N), nRow = rep("", N),
                      stringsAsFactors = FALSE)
wd <- getwd()

## needs dplyr

templ_nrow <- function(data, abr, Category, filt, indf) {
  d <- paste(abr, "_row", sep = "") 
  d <- filter(data, filt)
  r <- nrow(d)
  
  x <- data.frame(Category, r)
  names(x) <- c("Name", "nRow")
  outdf <- rbind(indf, x)
  
  return(outdf)
  
}



# Delete Rows with cell that doesn't contain given string

xtrarow <- function(d, col){
  d2 <- filter(d, grepl("Name", d$Name))
  return(d2)
}

df3 <- xtrarow(df3, "ID")
 


# Categorize data and prep for apriori, categories overlap
## abr= category abreviation, Cname= category

Categ2trans_filt <- function(data, abr, fname, filt) {
  
  ## data by Category
  d <- filter(data, fname) #dplyr
  dt <- d
  
  dt$Category <- Category
  ## Td <- data.frame("ID" = dt$ID,
  ##                   "Category" = dt$Category)
  
  ## Random Sample of rows, optional
  nr <- ifelse(nrow(d) >= 300, nrow(d)/10, nrow(d))
  nc <- ncol(d)
  
  d <- d[sample(nrow(d), nr), sample(ncol(d), nc)]
  cols <- colnames(d[, names(d) != "ID"])
  
  ## Data by Category CSV
  wd <- getwd()
  pathC <- paste0(wd, "/Data by Category CSV/", sep = "")
  csvC <- paste0(Category, ".csv")
  
  finalT <- paste0(pathT, csvC)
  write.csv(dt, file = (finalT), quote = FALSE, row.names = FALSE)
  
  
  ## Reformat Data
  d$combine <- do.call(paste, c(d[cols], sep = ","))
  itemList <- data.frame("All" = d$combine)
  
  twocols <- colnames(itemList)
  itemList[twocols] <- lapply(itemList[twocols], as.factor)
  
  ## create output
  ncols <- ncol(d)
  csv <- paste(d, "csv", sep = ".")
  path <- paste0(wd, "/Trans CSV/", sep = "")
  tname <- paste(Category, "Trans", sep = " ")
  
  final <- paste0(path, paste0(tname, ".csv"))
  write.csv(itemList, file = (final), quote = FALSE, row.names = FALSE)
  
  tr <- read.transactions(final, format = 'basket', sep = ',', cols = ncols, quote = "", header = TRUE)
  
  return(tr)
  
  
}



# Run apriori on categorized data

## need readr
## abr= category abreviation, Cname= category
templ_apriori <- function(data, abr, Cname ) {
  
  nom <- get(paste(abr, "_tr", sep = ""))
  
  ap <- apriori(nom, parameter = list(maxlen = 3, target = "rules"))
  ap_final <- ap[!is.redundant(ap)] #remove duplicates
  
  df <- data.frame(
    lhs = labels(lhs(ap_final)),
    rhs = labels(rhs(ap_final)),
    ap_final@quality)
  
  wd <- getwd()
  dfname <- paste(Cname, "Rules", sep = " ")
  csv <- paste(df, "csv", sep = ".")
  path <- paste0(wd, "/Rules CSV/", sep = "")
  
  fpath <- paste0(path, paste0(dfname, ".csv"))
  write.csv(df, file = fpath, quote = FALSE, row.names = FALSE)
  
  return(ap_final)
}

## apriori with parameters
templ_aprioripar <- function(data, abr, tname, sup, con, min, max #, lhs
) {
  
  d <- get(paste(abr, "_tr", sep = ""))
  
  ap <- apriori(d, parameter = list(supp = sup, conf = con, minlen = min, maxlen = max) 
                #  ,appearance = list(lhs = lhs)
  )
  ap_final <- ap[!is.redundant(ap)]
  
  return(ap_final)
}



