# Class-8-Lab

title: "Class 8 Lab"
author: "Divyanshu Kawankar (A16127402"
date: "10/25/2021"
output: html_document
---
title: "Class 8 Lab"
author: "Divyanshu Kawankar"
date: "10/25/2021"
output: pdf_document
---

---
title: "Week 8 Lab"
author: "Divyanshu Kawankar (A16127402)"
date: "10/24/2021"
output: pdf_document
---

### Questions 

Use the PCA Of UK Food data with the links given

#Data import

```{r}
url <- "https://tinyurl.com/UK-foods"
x <- read.csv(url)
```

#Question 1
 
Complete the following code to find out how many rows and columns are in x?

```{r}
dim(x)
```

#Check your data

Preview the first 6 Rows

```{r}
head(x,6)
```

Note how the minus indexing works

Note how the minus indexing works

```{r}
rownames(x) <- x[,1]
x <- x[,-1]
head(x)
```

what are the new dimensions
```{r}
dim(x)
```

alternative approach to correct row names
```{r}
x <- read.csv(url, row.names=1)
head(x)
```


#Q2. Which approach to solving the ‘row-names problem’ mentioned above do you prefer and why? Is one approach more robust than another under certain circumstances?

I prefer the approach where you set the first column as the argument of the table because if you use the code block "x <- x[,-1]" multiple times then you can lose some of the arguments you shouldn't use.

Spotting major differences and trends

```{r}
barplot(as.matrix(x), beside=T, col=rainbow(nrow(x)))
```

#Q3: Changing what optional argument in the above barplot() function results in the following plot? 

Setting the argument of "beside" to false would result in the plot given in the lab because it would list the data for each country on top of each other instead of besides. 

```{r}
barplot(as.matrix(x), beside=FALSE, col=rainbow(nrow(x)))
```

convert the given data to a pairwise plot 

```{r}
pairs(x, col=rainbow(10), pch=16)
```

#Q5: Generating all pairwise plots may help somewhat. Can you make sense of the following code and resulting figure? What does it mean if a given point lies on the diagonal for a given plot?

the following data plot tells you the trends and patterns between countries and their food consumption. A given point on the diagonal would mean that the consumption for that country follows the trend between countries but a spot between diagonals doesn't follow the trend for the countries.

#Q6. What is the main differences between N. Ireland and the other countries of the UK in terms of this data-set?

The difference between the countries in N. Ireland and the other countries in the UK is that N. Ireland has a larger spread of points than other countries, meaning that its citizens has the most varied diet compared to the other countries. 

```{r}
pca <- prcomp(t(x))
summary(pca)
```

Given the plot, try to compare PC1 and PC2

#Q7. Complete the code below to generate a plot of PC1 vs PC2. The second line adds text labels over the data points.

Plot PC1 vs PC2

```{r}
plot(pca$x[,1], pca$x[,2], xlab="PC1", ylab="PC2", xlim=c(-270,500))
text(pca$x[,1], pca$x[,2], colnames(x))
```

#Q8. Customize your plot so that the colors of the country names match the colors in our UK and Ireland map and table at start of this document.

```{r}
country_cols <- c("orange", "red", "blue", "green")
plot(pca$x[,1], pca$x[,2], xlab="PC1", ylab="PC2", xlim=c(-270,500))
text(pca$x[,1], pca$x[,2], colnames(x), col=country_cols)
```

Below we can use the square of pca$sdev , which stands for “standard deviation”, to calculate how much variation in the original data each PC accounts for.

```{r}
v <- round( pca$sdev^2/sum(pca$sdev^2) * 100 )
v
```

Or the second row here

```{r}
z <- summary(pca)
z$importance
```

Plot above summarized with eigen values

```{r}
barplot(v, xlab="Principal Component", ylab="Percent Variation")
```

We can also consider the influence of each variable using the loading scores. This can be seen in the table below.

Lets focus on PC1 as it accounts for > 90% of variance 

```{r}
par(mar=c(10, 3, 0.35, 0))
barplot( pca$rotation[,1], las=2 )
```

This allows us to see both the positive and negative scores for the food and drinks.

Now do the same with PC2 for question 2

#Q9: Generate a similar ‘loadings plot’ for PC2. What two food groups feature prominantely and what does PC2 maninly tell us about?

The 2 most prominently featured foods were fresh potatoes and soft drinks. This mainly tells us that consumption of these 2 foods is greatly varied for ireland compared to the other UK countries.

```{r}
par(mar=c(10, 3, 0.35, 0))
barplot( pca$rotation[,2], las=2 )
```

Another way to see this information is with a biplot

The inbuilt biplot() can be useful for small datasets

```{r}
biplot(pca)
```

Using PCA or related MDS plots to analyze RNA-seq results.

```{r}
url2 <- "https://tinyurl.com/expression-CSV"
rna.data <- read.csv(url2, row.names=1)
head(rna.data)
```

#Q10: How many genes and samples are in this data set?

```{r}
dim(rna.data)
```

Now we have to once again take the transpose of our data

```{r}
pca <- prcomp(t(rna.data), scale=TRUE)
```

Now, we need to creat a simple un polished plot of pc1 and pc2

```{r}
plot(pca$x[,1], pca$x[,2], xlab="PC1", ylab="PC2")
```

Examine the summary of how much variation in the original data each PC accounts for

```{r}
summary(pca)
```

We can see from this results that PC1 is where all the action is.
We can further prove this using a quick barplot summary

```{r}
plot(pca, main="Quick scree plot")
```

Try to calculate how much variation in the original data each PC accounts for.

Variance captured per PC

```{r}
pca.var <- pca$sdev^2
```

Percent variance is often more informative to look at

```{r}
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
pca.var.per
```

Now, we can generate our own scree plot

```{r}
barplot(pca.var.per, main="Scree Plot", 
        names.arg = paste0("PC", 1:10),
        xlab="Principal Component", ylab="Percent Variation")
```

Now, make the data more attractive

Make a vector of colors for wt and ko samples

```{r}
colvec <- colnames(rna.data)
colvec[grep("wt", colvec)] <- "red"
colvec[grep("ko", colvec)] <- "blue"

plot(pca$x[,1], pca$x[,2], col=colvec, pch=16,
     xlab=paste0("PC1 (", pca.var.per[1], "%)"),
     ylab=paste0("PC2 (", pca.var.per[2], "%)"))

text(pca$x[,1], pca$x[,2], labels = colnames(rna.data), pos=c(rep(4,5), rep(2,5)))
```

Create a data frame, and then create a ggplot

```{r}
library(ggplot2)

df <- as.data.frame(pca$x)

# Our first basic plot
ggplot(df) + 
  aes(PC1, PC2) + 
  geom_point()
```

Add a condition specific color and perhaps sample aesthetic labels

Add a 'wt' and 'ko' "condition" column

```{r}
df$samples <- colnames(rna.data) 
df$condition <- substr(colnames(rna.data),1,2)

p <- ggplot(df) + 
        aes(PC1, PC2, label=samples, col=condition) + 
        geom_label(show.legend = FALSE)
p
```

Refine it more

```{r}
p + labs(title="PCA of RNASeq Data",
       subtitle = "PC1 clealy seperates wild-type from knock-out samples",
       x=paste0("PC1 (", pca.var.per[1], "%)"),
       y=paste0("PC2 (", pca.var.per[2], "%)"),
       caption="BIMM143 example data") +
     theme_bw()
```
