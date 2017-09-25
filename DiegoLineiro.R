# Diego Lineiro
setwd("C:/Users/johnalva/Desktop")

dl <- read.csv("Diego.csv")
dl <- dl[,-1]
class(dl)
names(dl)
dll <- as.list(dl)


dll[2]

dlist <- lapply(as.list(dl), paste, collapse = " ")
dlist


corpus2 <- gsub(pattern = "\\W", replace = " ", dlist)
corpus2 <- gsub(pattern = "\\d", replace = " ", corpus2)
corpus2 <- tolower(corpus2)
corpus2 <- removeWords(corpus2, stopwords())
corpus2 <- removeWords(corpus2, stopwords("english"))
corpus2 <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", corpus2)
corpus2 <- stripWhitespace(corpus2)
corpus3 <- Corpus(VectorSource(corpus2))
corpus3
x11()
tdm <- TermDocumentMatrix(corpus3)
tdm # For this case have 670836 empty cells and 822384 non empty cells
# head(as.matrix(tdm)) # To see the frequency of word(term) vs Doc (Documents loaded)
# tail(as.matrix(tdm))
m <- as.matrix(tdm)
View(m)

# class(m)
colnames(m) <- c("1-Cloud.2Q17",
              "2-Interés.",
              "3-Mejorar contenido.",
              "4-Características.tecnológicas",
              "5-Comentarios")

comparison.cloud(m)
# mm <- data.frame(m)
# View(mm)
# rownames(mm)
# mb <- mm[order(mm$Blog,decreasing = TRUE),]
# mtb <- head(mb, 25)
# mtb <- mtb[order(mtb$Blog,decreasing = TRUE),]
# plot(mtb)
# ggplot(data = mtb, aes(x=Blog, y = rownames(mtb))) + geom_point()
# View(mtb)
# # View(mm)
# 
# library(tidytext)
# class(corpus2)
# # corpus2
# text_df <- data_frame(text = corpus2)
# # class(text_df)
# 
# tidy_corpus <- text_df %>%
#     unnest_tokens(word, text)
# 
# # gram
# text_df %>%
#     unnest_tokens(gram, text, token = "ngrams", n = 1)
# 
# # Bigrams
# text_df %>%
#     unnest_tokens(bigram, text, token = "ngrams", n = 2)
# 
# # Trigrams
# text_df %>%
#     unnest_tokens(trigram, text, token = "ngrams", n = 3)
# 
# # Fourgrams
# text_df %>%
#     unnest_tokens(fourthgram, text, token = "ngrams", n = 4)
# 
# text_df %>%
#     unnest_tokens(fourthgram, text, token = "ngrams", n = 5)
# 
# 
# # length(corpus3)
# # summary(corpus3)
# # meta(corpus3[[1]])
# # length(corpus3[[2]]$content)
# 
# 
