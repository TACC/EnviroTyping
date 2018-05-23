library(tidyverse)
library(klaR)
library(cba)
library(cluster)

var.sel.80 <- read_rds("var.sel.80.rds")
var.sel.90 <- read_rds("var.sel.90.rds")
var.sel.95 <- read_rds("var.sel.95.rds")

df.95 <- as_data_frame(unlist(var.sel.95)) %>% 
    rownames_to_column("hyb") %>% 
    separate(hyb, into = c("hyb", "stat"), "\\.") %>% 
    dplyr::select(hyb,stat = value)
# %>% 
#     mutate_all(funs(as_factor(.)))
n_distinct(df.95$stat)
ggplot(df.95, aes(stat)) + 
    geom_histogram(bins = 112, stat = "count")
df.sig <- df.95 %>% group_by(stat) %>% summarize(count = n()) %>% filter(count >= 20)
mu.sig <- mean(df.sig$count); sig.sig <- sd(df.sig$count)
range <- c(mu.sig -  sig.sig, mu.sig +  sig.sig)
df.95.kmodes3w <- kmodes(df.95,3, weighted = TRUE)
df.95.kmodes3 <- kmodes(df.95,3)
df.95.kmodes6 <- kmodes(df.95,6)
df.95.kmodes10 <- kmodes(df.95,10)
df.95.kmodes10w <- kmodes(df.95,10, weighted = TRUE)
df.95.kmodes100w <- kmodes(df.95,50, weighted = TRUE)
df.95.clust <- cbind(df.95, clust = df.95.kmodes100w$cluster)
ggplot(df.95.clust, aes(clust)) + 
    geom_histogram(bins = 50)
df.95.filtered <- df.95.clust %>% filter(clust == 29)
n_distinct(df.95.filtered$hyb)

str(df.95.clust)
n_distinct(df.95.clust$stat)
k <- c(seq(1,1000, by = 100))
diff <- vector(mode = "numeric")
for ( i in k) {
    diff <- c(diff, sum(kmodes(df.95,i)$withindiff))
}
plot(diff, k)
df.95.factor <- df.95 %>%
    mutate_all(funs(as.numeric(as_factor(.))))
plot(jitter(as.matrix(df.95.factor)), col = df.95.kmodes10$cluster)
points(df.95.kmodes10w$modes,)

df.95.rock <- rockLink(df.95, 3)
