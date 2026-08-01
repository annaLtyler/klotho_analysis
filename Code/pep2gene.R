#get the gene label for a peptide.
#This one requires internet access.

pep2gene <- function(id) {
  url <- paste0(
    "https://rest.uniprot.org/uniprotkb/",
    id,
    "?fields=accession,gene_names"
  )

    resp <- request(url) |>
            req_perform()

    dat <- resp_body_json(resp)
    
    gene.name = dat$genes[[1]]$geneName$value
    if(is.null(gene.name)){
        gene.name <- NA
    }
  result <- c("accession" = id,"gene_name" = gene.name)
  return(result)

}
