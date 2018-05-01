amzn <- read.csv(file="C:\\Users\\ASHOK SAI\\Desktop\\amzn.csv")
goog <- read.csv(file="C:\\Users\\ASHOK SAI\\Desktop\\google.csv")
amzn <- amzn[rowSums(is.na(amzn)) != ncol(amzn),]
goog <- goog[rowSums(is.na(goog)) != ncol(goog),]
str(amzn)
str(goog)
amzn <- amzn[rowSums(is.na(amzn)) == 0,]
goog <- goog[rowSums(is.na(goog)) == 0,]
str(amzn)
str(goog)


library(wordcloud)
library(plotrix)
library(qdap)
library(tm)
library(RWeka)
library(qdapDictionaries)
library(qdapRegex)
library(qdapTools)
library(tokenizers)




str(amzn)
str(goog)
amzn_pros <- amzn$pros
goog_pros <- goog$pros
amzn_cons <- amzn$cons
goog_cons <- goog$cons


freq_terms(goog_pros,10)
freq_terms(goog_cons,10)

freq_terms(amzn_pros,10)
freq_terms(amzn_cons,10)






qdap_clean <- function(x) {
  x <- replace_abbreviation(x)
  x <- replace_contraction(x)
  x <- replace_number(x)
  x <- replace_ordinal(x)
  x <- replace_symbol(x)
  x <- tolower(x)
  return(x)
}




tm_clean <- function(corpus) {
  tm_clean <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords,
                   c(stopwords("en"), "Google", "Amazon", "company"))
  return(corpus)
}




tokenizer <- function(x) 
  NGramTokenizer(x, Weka_control(min = 2, max = 2))




amzn_pros <- qdap_clean(amzn_pros)

amzn_cons <- qdap_clean(amzn_cons)

az_p_corp <- VCorpus(VectorSource(amzn_pros))

az_c_corp <- VCorpus(VectorSource(amzn_cons))

amzn_pros_corp <- tm_clean(az_p_corp)

amzn_cons_corp <- tm_clean(az_c_corp)








goog_pros <- qdap_clean(goog_pros)

goog_cons <- qdap_clean(goog_cons)

goog_p_corp <- VCorpus(VectorSource(goog_pros))

goog_c_corp <- VCorpus(VectorSource(goog_cons))

goog_pros_corp <- tm_clean(goog_p_corp)

goog_cons_corp <- tm_clean(goog_c_corp)











amzn_p_tdm <- TermDocumentMatrix(
  amzn_pros_corp, 
  control = list(tokenize = tokenizer)
)


amzn_p_tdm_m <- as.matrix(amzn_p_tdm)
amzn_p_tdm_m[1:5,1:5]

amzn_p_freq <- rowSums(amzn_p_tdm_m)


wordcloud(names(amzn_p_freq), amzn_p_freq, 
          max.words = 25, color = "blue")









amzn_c_tdm <- TermDocumentMatrix(
  amzn_cons_corp, 
  control = list(tokenize = tokenizer)
)

amzn_c_tdm_m <- as.matrix(amzn_c_tdm)

amzn_c_freq <- rowSums(amzn_c_tdm_m)

wordcloud(names(amzn_c_freq), amzn_c_freq, 
          max.words = 25, colors = "red")









amzn_c_tdm <- TermDocumentMatrix(
  amzn_cons_corp,
  control = list(tokenize = tokenizer)
)

amzn_c_tdm

amzn_c_tdm2 <- removeSparseTerms(amzn_c_tdm, .993)

hc <- hclust(dist(amzn_c_tdm2, method = "euclidean"), 
             method = "complete")

plot(hc)







amzn_p_tdm <- TermDocumentMatrix(
  amzn_pros_corp, 
  control = list(tokenize = tokenizer)
)

amzn_p_m <- as.matrix(amzn_p_tdm)

amzn_p_freq <- rowSums(amzn_p_m)

term_frequency <- sort(amzn_p_freq, decreasing = TRUE)

term_frequency[1:5]

findAssocs(amzn_p_tdm, "fast paced", 0.2)










goog_p_tdm <- TermDocumentMatrix(
  goog_pros_corp, 
  control = list(tokenize = tokenizer)
)

goog_p_tdm_m <- as.matrix(goog_p_tdm)

goog_p_freq <- rowSums(goog_p_tdm_m)

wordcloud(names(goog_p_freq), goog_p_freq, 
          max.words = 25, color = "blue")










goog_c_tdm <- TermDocumentMatrix(
  goog_cons_corp, 
  control = list(tokenize = tokenizer)
)

goog_c_tdm_m <- as.matrix(goog_c_tdm)

goog_c_freq <- rowSums(goog_c_tdm_m)

wordcloud(names(goog_c_freq), goog_c_freq, 
          max.words = 25, colors = "red")










goog_c_tdm <- TermDocumentMatrix(
  goog_cons_corp,
  control = list(tokenize = tokenizer)
)

goog_c_tdm

goog_c_tdm2 <- removeSparseTerms(goog_c_tdm, .993)

hc <- hclust(dist(goog_c_tdm2, method = "euclidean"), 
             method = "complete")

plot(hc)











goog_p_tdm <- TermDocumentMatrix(
  goog_pros_corp, 
  control = list(tokenize = tokenizer)
)

goog_p_m <- as.matrix(goog_p_tdm)

goog_p_freq <- rowSums(goog_p_m)

term_frequency <- sort(goog_p_freq, decreasing = TRUE)

term_frequency[1:5]

findAssocs(goog_p_tdm, "fast paced", 0.2)


















str(goog)
all_goog_corpus <- VCorpus(VectorSource(goog[,3:4]))


all_goog_corp <- tm_clean(all_goog_corpus)

all_tdm <- TermDocumentMatrix(all_goog_corp,control = list(tokenize = tokenizer))

colnames(all_tdm) <- c("Goog_Pros", "Goog_Cons")

all_m <- as.matrix(all_tdm)

comparison.cloud(all_m, 
                 colors = c("#F44336", "#2196f3"), 
                 max.words = 100)










str(amzn)
all_amzn_corpus <- VCorpus(VectorSource(amzn[,3:4]))


all_amzn_corp <- tm_clean(all_amzn_corpus)

all_tdm <- TermDocumentMatrix(all_amzn_corp,control = list(tokenize = tokenizer))

colnames(all_tdm) <- c("Amzn_Pros", "Amzn_Cons")

all_m <- as.matrix(all_tdm)

comparison.cloud(all_m, 
                 colors = c("#F44336", "#2196f3"), 
                 max.words = 100)

