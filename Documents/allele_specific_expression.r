setwd("/Users/atyler/Documents/Projects/Klotho/klotho_analysis/Data/Human/allele-specific")
source("/Users/atyler/Documents/Projects/Klotho/klotho_analysis/Code/colors.from.values.R")
source("/Users/atyler/Documents/Projects/Klotho/klotho_analysis/Code/get.color.R")
cols <- read.table("phASER_WASP_GTEx_v8_matrix.gw_phased.txt", nrows = 1, header = TRUE, sep = "\t", comment.char = "!")

#I used grep -Ihnr ENSG00000133116 > klotho.txt
#to make a file that contains only the Klotho 
#transcripts. There are 31 lines all corresponding
#to transcript ENSG00000133116, ensembl version 7, 
#and different contigs? I'm not sure what the contigs 
#refer to. I'll have to look at the phASER documentation. 
#all have the same start and stop coordinates, but
#different contig names. Are these listing which 
#exons were used?

kl <- read.table("klotho.txt", header = FALSE,sep = "\t", fill = TRUE)
colnames(kl) <- colnames(cols)
split.alleles <- apply(kl[,5:ncol(kl)], 1, function(x) strsplit(x, "|", fixed = TRUE))
allele1 <- lapply(split.alleles, function(x) sapply(x, function(y) y[1]))
allele2 <- lapply(split.alleles, function(x) sapply(x, function(y) y[2]))

num.samples <- sapply(allele1, function(x) length(which(!is.na(x))))

#there are some extremely high numbers in here. Kidney?
#this will take some digging into...
boxplot(lapply(allele1, function(x) log10(as.numeric(x))))

#it actually looks as if all these numbers are 
#identical, so maybe we just take the top line.
big <- which(as.numeric(allele1[[1]]) > 100)
allele1[[1]][big]
kl[,big+4]

kl1 <- as.numeric(allele1[[1]])
kl2 <- as.numeric(allele2[[1]])
names(kl1) <- names(kl2) <- colnames(cols)[5:length(cols)]

#the following plot shows the alleles plotted against each
#other. It doesn't look as if haplotype has much of an effect
#on expression, which comports with what we've seen elsewhere.
plot(kl1, kl2)
abline(0,1)

enough.reads <- which(kl1 > 10)
ratios <- (kl1[enough.reads]+1)/(kl2[enough.reads]+1)
hist(ratios)


#tissues with big ratio differences have low read counts
par(mar = c(1,2))
plot(kl1[enough.reads], kl2[enough.reads], col = colors.from.values(ratios, col.scale = "blue", 
    grad.dir = "high"), pch = 16)
abline(0,1)


par(mfrow = c(1,2))
plot(kl1[enough.reads], ratios, xlab = "Allele 1 expression", ylab = "Ratio of allele 1 to allele2")
abline(v = 30, h = 7)
plot(kl2[enough.reads], ratios, xlab = "Allele 2 expression", ylab = "Ratio of allele 1 to allele2")
abline(v = 30, h = 7)


#there is one that kind of sticks out on the right. What is that?
#I think I have to get the version 8 sample names. Ugh.
#I don't think it's worth it anyway.
look.at <- intersect(which(kl1[enough.reads] > 30), which(ratios > 7))
cols[(enough.reads[look.at]+4)]
kl1[enough.reads[look.at]]
kl2[enough.reads[look.at]]
