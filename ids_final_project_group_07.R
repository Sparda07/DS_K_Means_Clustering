
install.packages("tm")
install.packages("SnowballC")
install.packages("Matrix")
install.packages("cluster")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("uwot")
install.packages("factoextra")



suppressPackageStartupMessages({
  library(tm)
  library(SnowballC)
  library(Matrix)
  library(cluster)
  library(dplyr)
  library(ggplot2)
  library(uwot)
  library(factoextra)
})

set.seed(42)


df <- read.csv("ids_final_dataset_sample_group_07.csv",
               stringsAsFactors = FALSE)


cat("Total rows:", nrow(df), "\n")
print(table(df$source))
print(table(df$domain_label))


df$domain <- ifelse(df$domain_label == 0,
                    "Biomedical",
                    "Artificial Intelligence")


df$text <- paste(df$title, df$abstract, sep = " ")
df$text[is.na(df$text)] <- ""


corpus <- Corpus(VectorSource(df$text))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)


inspect(corpus[1:3])


dtm <- DocumentTermMatrix(corpus)

dtm <- removeSparseTerms(dtm, 0.99)

tfidf <- weightTfIdf(dtm)

tfidf_mat <- as.matrix(tfidf)

cat("TF-IDF Matrix Shape:", dim(tfidf_mat), "\n")


fviz_nbclust(tfidf_mat, kmeans, method = "wss") +
  ggtitle("Elbow Method for Choosing Number of Clusters")

ggsave("elbow_method_group_07.png",
       width = 8,
       height = 6)


k <- 4

km <- kmeans(tfidf_mat,
             centers = k,
             nstart = 25)

df$cluster <- as.factor(km$cluster)

cat("Cluster Count:\n")
print(table(df$cluster))


sil <- silhouette(km$cluster, dist(tfidf_mat))

cat("Average Silhouette Score:",
    mean(sil[, 3]),
    "\n")


umap_result <- umap(tfidf_mat,
                    n_neighbors = 15,
                    min_dist = 0.1,
                    n_components = 2,
                    metric = "cosine")

df$UMAP1 <- umap_result[, 1]
df$UMAP2 <- umap_result[, 2]


plot_cluster <- ggplot(df,
                       aes(x = UMAP1,
                           y = UMAP2,
                           color = cluster)) +
  geom_point(size = 2.5, alpha = 0.8) +
  theme_minimal() +
  labs(title = "UMAP Visualization by K-means Cluster",
       x = "UMAP Dimension 1",
       y = "UMAP Dimension 2",
       color = "Cluster")

print(plot_cluster)

ggsave("umap_cluster_plot_group_07.png",
       plot_cluster,
       width = 8,
       height = 6)


plot_domain <- ggplot(df,
                      aes(x = UMAP1,
                          y = UMAP2,
                          color = domain)) +
  geom_point(size = 2.5, alpha = 0.8) +
  theme_minimal() +
  labs(title = "UMAP Visualization by Research Domain",
       x = "UMAP Dimension 1",
       y = "UMAP Dimension 2",
       color = "Domain")

print(plot_domain)

ggsave("umap_domain_plot_group_07.png",
       plot_domain,
       width = 8,
       height = 6)


cluster_summary <- df %>%
  group_by(cluster, domain) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(cluster) %>%
  mutate(percentage = round((count / sum(count)) * 100, 2))

cat("Cluster Domain Summary:\n")
print(cluster_summary)

write.csv(cluster_summary,
          "cluster_domain_summary_group_07.csv",
          row.names = FALSE)

plot_percentage <- ggplot(cluster_summary,
                          aes(x = cluster,
                              y = percentage,
                              fill = domain)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  theme_minimal() +
  labs(title = "Biomedical vs AI Percentage in Each Cluster",
       x = "Cluster",
       y = "Percentage",
       fill = "Domain")

print(plot_percentage)

ggsave("cluster_domain_percentage_group_07.png",
       plot_percentage,
       width = 8,
       height = 6)


terms <- colnames(tfidf_mat)

for (i in 1:k) {
  
  cat("\n==============================\n")
  cat("Top Keywords for Cluster", i, "\n")
  cat("==============================\n")
  
  cluster_mat <- tfidf_mat[df$cluster == i, , drop = FALSE]
  
  word_scores <- colMeans(cluster_mat)
  
  top_terms <- names(sort(word_scores,
                          decreasing = TRUE)[1:10])
  
  print(top_terms)
}

keyword_list <- data.frame()

for (i in 1:k) {
  
  cluster_mat <- tfidf_mat[df$cluster == i, , drop = FALSE]
  
  word_scores <- colMeans(cluster_mat)
  
  top_words <- names(sort(word_scores,
                          decreasing = TRUE)[1:10])
  
  temp <- data.frame(
    cluster = i,
    top_keywords = paste(top_words, collapse = ", "),
    stringsAsFactors = FALSE
  )
  
  keyword_list <- rbind(keyword_list, temp)
}

write.csv(keyword_list,
          "cluster_top_keywords_group_07.csv",
          row.names = FALSE)

# ------------------------------------------------------------
# Step 10: Save Final Result Dataset
# ------------------------------------------------------------

write.csv(df,
          "final_clustered_dataset_group_07.csv",
          row.names = FALSE)

# ------------------------------------------------------------
# Final Message
# ------------------------------------------------------------

cat("\nProject analysis completed successfully.\n")
cat("Generated files:\n")
cat("1. final_clustered_dataset_group_07.csv\n")
cat("2. cluster_domain_summary_group_07.csv\n")
cat("3. cluster_top_keywords_group_07.csv\n")
cat("4. elbow_method_group_07.png\n")
cat("5. umap_cluster_plot_group_07.png\n")
cat("6. umap_domain_plot_group_07.png\n")
cat("7. cluster_domain_percentage_group_07.png\n")

