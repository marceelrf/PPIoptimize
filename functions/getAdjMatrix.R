getAdjMatrix <- function(df, cor_min = .8){
  a <- corr_tbl %>%
    column_to_rownames("term") %>%
    as.matrix()

  a[is.na(a)] <- 0

  a[a < cor_min & a > -cor_min] <- 0
  a[a >= cor_min | a <= -cor_min] <- 1

  return(a)
}
