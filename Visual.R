
amzn <- read.csv(file="C:\\Users\\ASHOK SAI\\Desktop\\amzn.csv")
goog <- read.csv(file="C:\\Users\\ASHOK SAI\\Desktop\\google.csv")
View(amzn)
str(amzn)
str(goog)
amzn_pros <- amzn$pros
goog_pros <- goog$pros

library(plotrix)
library(qdap)
library(tm)
library(RWeka)
library(qdapDictionaries)
library(qdapRegex)
library(qdapTools)
library(tokenizers)




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

pros <- data.frame(
  amzn_pros = c(as.character(amzn$pros), 'NULL'),
  goog_pros = as.character(goog$pros),
  stringsAsFactors = F
)

pros[is.na(pros)] <- "NULL"
pros <- qdap_clean(pros)

all_pros_corp <- VCorpus(VectorSource(pros))
all_pros_corp <- tm_clean(all_pros_corp)

all_tdm <- TermDocumentMatrix(
  all_pros_corp,
  control = list(tokenize = tokenizer)
)

all_tdm_m <- as.matrix(all_tdm)


sum(all_tdm_m)

sum(all_tdm_m[, 1] > 0 & all_tdm_m[, 2] > 0)


common_words <- subset(all_tdm_m, all_tdm_m[, 1] > 0 & all_tdm_m[, 2] > 0)

difference <- abs(common_words[, 1] - common_words[, 2])

common_words <- cbind(common_words, difference)

common_words <- common_words[order(common_words[, 3], decreasing = TRUE), ]

top15_df <- data.frame(x = common_words[1:15, 1], 
                       y = common_words[1:15, 2], 
                       labels = rownames(common_words[1:15, ]))

pyramid.plot(top15_df$x, top15_df$y, 
             labels = top15_df$labels, gap = 12, 
             top.labels = c("Amzn", "Pro Words", "Google"), 
             main = "Words in Common", unit = NULL)



cons <- data.frame(
  amzn_cons = c(as.character(amzn$cons), 'NULL'),
  goog_cons = as.character(goog$cons),
  stringsAsFactors = F
)
cons[is.na(cons)] <- "NULL"
cons <- qdap_clean(cons)

all_cons_corp <- VCorpus(VectorSource(cons))
all_cons_corp <- tm_clean(all_cons_corp)

all_tdm <- TermDocumentMatrix(
  all_cons_corp,
  control = list(tokenize = tokenizer)
)


all_tdm_m <- as.matrix(all_tdm)


common_words <- subset(
  all_tdm_m,
  all_tdm_m[, 1] > 0 & all_tdm_m[, 2] > 0
)

difference <- abs(common_words[, 1] - common_words[, 2])

common_words <- cbind(common_words, difference)

common_words <- common_words[order(common_words[, 3], decreasing = T), ]

top15_df <- data.frame(x = common_words[1:15, 1],
                       y = common_words[1:15, 2],
                       labels = rownames(common_words[1:15, ]))

pyramid.plot(top15_df$x, top15_df$y, labels = top15_df$labels, main = "Words in Common",
             top.labels = c("Amzn",
                            "Cons Words",
                            "Google"),
             gap = 12,
             unit = NULL
)

