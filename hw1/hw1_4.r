library("ggplot2")
g <- gene_expression$Gene.Activity

levels(gene_expression$Treatment)
a <- gene_expression[gene_expression$Treatment == "High Dose",]
b <- gene_expression[gene_expression$Treatment == "Control",]

# box plots
gene_expression$Treatment <- factor(gene_expression$Treatment)
my.bp <<- ggplot(data=gene_expression, aes(y=log10(Gene.Activity), x=Treatment, fill=Treatment))
my.bp <- my.bp + geom_boxplot() # Adds color
my.bp <- my.bp + ggtitle("Distribution of Gene Activity") # Adds a title
my.bp <- my.bp +  ylab("Gene Activity") + xlab("Treatment") # Adds kaveks
my.bp # displays the boxplots
  
# statistics
m <- mean(g)
med <- median(g)
variance <- var(g)
std <- sqrt(var(g))

