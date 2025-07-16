#This function uses the hypergeometric distribution
#to test whether one gene set is enrihed in another.
#gene_set_A is 

gene_set_enrich <- function(gene_set_A = c("Gene1", "Gene2", "Gene3", "Gene4", "Gene5"), 
    gene_set_B = c("Gene3", "Gene4", "Gene6", "Gene7"), background_genes = 
    c("Gene1", "Gene2", "Gene3", "Gene4", "Gene5", "Gene6", "Gene7", "Gene8", "Gene9", "Gene10")){

    
    # Calculate values for the hypergeometric test
    N <- length(background_genes)                     # Total number of genes
    K <- length(intersect(gene_set_B, background_genes))  # Genes in B that are in the background
    n <- length(intersect(gene_set_A, background_genes))  # Genes in A that are in the background
    k <- length(intersect(gene_set_A, gene_set_B))    # Overlap between A and B

    # Perform the hypergeometric test (upper tail)
    p_value <- phyper(q = k - 1, m = K, n = N - K, k = n, lower.tail = FALSE)

    # Output the result
    #cat("P-value for enrichment:", p_value, "\n")
    return(p_value)
}
