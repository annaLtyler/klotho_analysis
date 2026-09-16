#this function plots a heatmap for a matrix with NAs
#If an annotation row matrix is supplied, it uses the 
#the rows as the row order. Otherwise the covariance matrix 
#is clustered and used to order the rows. Columns are not
#clustered by default

pheatmap_with_nas <- function(mat, annot_row_df = NULL, cluster_cols = FALSE){
  if(!is.null(annot_row_df)){
    row.order <- order(annot_row_df[,1])
    pheatmap(mat[row.order,], cluster_rows = FALSE, 
      cluster_cols = FALSE, annotation_row = annot_row_df)
  }else{
    cov.mat <- cov(t(mat), use = "pairwise.complete.obs")
    row.order <- hclust(dist(cov.mat))$order
    pheatmap(mat[row.order,], cluster_rows = FALSE, cluster_cols = cluster_cols)
  }
}
