install.packages("rentrez")
install.packages("httr")
install.packages("xml2")
install.packages("dplyr")
install.packages("stringr")

library(rentrez)
library(httr)
library(xml2)
library(dplyr)
library(stringr)

library(rentrez)
library(httr)
library(xml2)
library(dplyr)
library(stringr)

get_pubmed_abstracts <- function(query, n = 100) {
  
  search_result <- entrez_search(
    db = "pubmed",
    term = query,
    retmax = n
  )
  
  ids <- search_result$ids
  
  records <- entrez_fetch(
    db = "pubmed",
    id = ids,
    rettype = "xml",
    parsed = FALSE
  )
  
  records_xml <- read_xml(records)
  
  articles <- xml_find_all(records_xml, ".//PubmedArticle")
  
  data <- data.frame()
  
  for (i in seq_along(articles)) {
    
    title <- xml_text(xml_find_first(articles[i], ".//ArticleTitle"))
    
    abstract <- paste(
      xml_text(xml_find_all(articles[i], ".//AbstractText")),
      collapse = " "
    )
    
    title <- str_squish(title)
    abstract <- str_squish(abstract)
    
    if (!is.na(title) && abstract != "") {
      temp <- data.frame(
        title = title,
        abstract = abstract,
        source = "PubMed",
        domain_label = 0,
        stringsAsFactors = FALSE
      )
      
      data <- rbind(data, temp)
    }
  }
  
  return(head(data, n))
}
get_arxiv_abstracts <- function(query, n = 100) {
  
  base_url <- "http://export.arxiv.org/api/query?"
  
  url <- paste0(
    base_url,
    "search_query=all:",
    URLencode(query),
    "&start=0&max_results=",
    n
  )
  
  response <- GET(url)
  
  content_xml <- read_xml(
    content(response, as = "text", encoding = "UTF-8")
  )
  
  ns <- xml_ns(content_xml)
  
  entries <- xml_find_all(content_xml, ".//d1:entry", ns)
  
  data <- data.frame()
  
  for (i in seq_along(entries)) {
    
    title <- xml_text(xml_find_first(entries[i], ".//d1:title", ns))
    abstract <- xml_text(xml_find_first(entries[i], ".//d1:summary", ns))
    
    title <- str_squish(title)
    abstract <- str_squish(abstract)
    
    if (!is.na(title) && abstract != "") {
      temp <- data.frame(
        title = title,
        abstract = abstract,
        source = "arXiv",
        domain_label = 1,
        stringsAsFactors = FALSE
      )
      
      data <- rbind(data, temp)
    }
  }
  
  return(head(data, n))
}



pubmed_data <- get_pubmed_abstracts(
  query = "cancer diagnosis medical imaging machine learning",
  n = 100
)

arxiv_data <- get_arxiv_abstracts(
  query = "artificial intelligence machine learning deep learning",
  n = 100
)


final_dataset <- rbind(pubmed_data, arxiv_data)


final_dataset <- final_dataset %>%
  distinct(abstract, .keep_all = TRUE)


final_dataset$id <- 1:nrow(final_dataset)


final_dataset <- final_dataset[, c("id", "title", "abstract", "source", "domain_label")]



write.csv(
  final_dataset,
  "ids_final_dataset_sample_group_07.csv",
  row.names = FALSE
)



print(head(final_dataset))

print(paste("Total abstracts collected:", nrow(final_dataset)))

print(table(final_dataset$source))

getwd()

dataset <- read.csv("ids_final_dataset_sample_group_07.csv")

head(dataset)
nrow(dataset)
colnames(dataset)
table(dataset$source)
table(dataset$domain_label)
