#This function finds mouse or human orthologs 
#for mouse gene names or human gene names.
#put in a vector of names for one or the other.
#allowed chromosomes filters the tables to only
#genes that are localized on the chromosomes. The
#patches in the human data were giving me crazy 
#results. Set allowed.chr to NULL to suppress filtering.

#this can take a few minutes, so if a filename is provided
#and that file exists, that file is read in instead of doing
#the full query

get_orthologs <- function(mouse.id = NULL, human.id = NULL, id.type = "external_gene_name",
    allowed.chr = c(1:22, "X", "Y", "MT"), file.name = "ortholog_table.csv"){

    if(file.exists(file.name)){
        merged.table <- read.csv(file.name)
    }else{

        require(biomaRt)

        all.var <- ls()
        lib.loaded <- as.logical(length(which(all.var == "mouse")))

        if(!lib.loaded){
            mouse <- useEnsembl(biomart = "genes", dataset = "mmusculus_gene_ensembl")
            human <- useEnsembl(biomart = "genes", dataset = "hsapiens_gene_ensembl")
        }

        if(!is.null(mouse.id)){
            query <- mouse.id
            input.mart <- mouse
            ortho.mart <- human
            input.append <- "mouse_"
            ortho.append <- "human_"
            ortho.feature <- "hsapiens_homolog_associated_gene_name"
        }
        if(!is.null(human.id)){
            query <- human.id
            input.mart <- human
            ortho.mart <- mouse
            input.append <- "human_"
            ortho.append <- "mouse_"
            ortho.feature <- "mmusculus_homolog_associated_gene_name"
        }

        #get ortholog names
        input.info <- getBM(attributes = c("ensembl_gene_id", 
            "external_gene_name", "entrezgene_id", "chromosome_name", 
            "start_position", "end_position"),
            filters = id.type,
            values = query,
            mart = input.mart)

        #filter to allowed chromosomes
        if(!is.null(allowed.chr)){
            allowed.idx <- which(input.info[,"chromosome_name"] %in% allowed.chr)
            input.info <- input.info[allowed.idx,]
            #head(input.info)
        }

        ortho.names <- getBM(attributes = c("ensembl_gene_id", 
            "external_gene_name", ortho.feature),
            filters = id.type,
            values = query,
            mart = input.mart)

        found.names <- ortho.names[,ortho.feature]
        found.names <- unique(found.names[which(found.names != "")])
        
        #get additional information for orthologs
        ortho.info <- getBM(attributes = c("ensembl_gene_id", "external_gene_name",
            "entrezgene_id", "chromosome_name", "start_position", "end_position"),
            filters = "external_gene_name",
            values = found.names,
            mart = ortho.mart)

        #filter to allowed chromosomes
        if(!is.null(allowed.chr)){
            allowed.idx <- which(ortho.info[,"chromosome_name"] %in% allowed.chr)
            ortho.info <- ortho.info[allowed.idx,]
            #head(ortho.info)
        }

        #input genes may be represented multiple times in the ortholog
        #table with different ensembl IDs. Often one of these will have
        #an ortholog, and others won't. I'm not certain why this is. I
        #think if one instance of a gene doesn't have a homolog, and another
        #does, the first probably maps to some chromosome patch. I'm not 
        #totally clear on what those are, but we only want genes that 
        #are located definitively on the human chromosomes. We can't 
        #ask BioMart for entrez IDs in the ortholog query for some reason,
        #so we are stuck doing the best matches we can with names.
        
        #merge all tables based on identified ortholog names
        merge_info <- function(ortho.gene.name){
            ortho.info.idx <- which(ortho.info[,"external_gene_name"] == ortho.gene.name)
            ortho.name.idx <- which(ortho.names[,ortho.feature] == ortho.gene.name)
            
            #some genes have more than one ortholog
            #ortho.names[ortho.name.idx,]
            input.ensembl <- ortho.names[ortho.name.idx,"ensembl_gene_id"]
            input.idx <- unlist(lapply(input.ensembl, function(x) which(input.info[,"ensembl_gene_id"] == x)))

            input.table <- input.info[input.idx,,drop=FALSE]
            colnames(input.table) <- paste0(input.append, colnames(input.table))

            link.table <- ortho.names[ortho.name.idx,,drop=FALSE]

            output.table <- ortho.info[ortho.info.idx,]
            colnames(output.table) <- paste0(ortho.append, colnames(output.table))

            if(nrow(input.table) > nrow(output.table)){
                #if there is a many to one mapping, just replicate the one to match the many
                if(nrow(output.table) == 1){
                    padded.output <- matrix(rep(output.table, nrow(input.table)), byrow = TRUE, nrow = nrow(input.table))
                    colnames(padded.output) <- colnames(output.table)
                }else{
                    #otherwise, use NAs
                    output.padding <- matrix(NA, ncol = ncol(output.table), nrow = nrow(input.table)-nrow(output.table))
                    colnames(output.padding) <- colnames(output.table)
                    padded.output <- rbind(output.table, output.padding)
                }
                padded.input <- input.table
            }
            if(nrow(output.table) > nrow(input.table)){
                #if there is a many to one mapping, just replicate the one to match the many
                if(nrow(input.table) == 1){
                    padded.input <- matrix(rep(input.table, nrow(output.table)), byrow = TRUE, nrow = nrow(output.table))
                    colnames(padded.input) <- colnames(input.table)
                }else{
                    #otherwise, use NAs
                    input.padding <- matrix(NA, ncol = ncol(input.table), nrow = nrow(output.table)-nrow(input.table))
                    colnames(input.padding) <- colnames(input.table)
                    padded.input <- rbind(input.table, input.padding)
                }
                padded.output <- output.table
            }
            if(nrow(output.table) == nrow(input.table)){
                padded.input <- input.table
                padded.output <- output.table
            }

            merged.table <- cbind(padded.input, padded.output)
            return(merged.table)
        }

        #for(i in 1:length(found.names)){
        #    test <- merge_info(ortho.gene.name = found.names[i])
        #}

        merged.list <- lapply(found.names, merge_info)
        merged.table <- Reduce("rbind", merged.list)
        
        write.table(as.matrix(merged.table), file.name, sep = ",", quote = FALSE, row.names = FALSE)
    } #end case for file.name not already existing.

    return(merged.table)
}


