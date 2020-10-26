# install.packages('devtools')
# install.packages('wordcloud')
# install.packages('RColorBrewer')
# install.packages('readxl')
# install.packages('topicmodels')
# install.packages("xlsx")
# install.packages("stringr")
# install.packages("KoNLP")
# install.packages("RmecabKo")
# install.packages("remotes")
# install.packages("tm")
# install.packages("dplyr")
# install.packages("tidytext")
# install.packages("SentimentAnalysis")
# install.packages("tidyverse")
# install.packages("wordcloud2")
# install.packages("Rcpp")
# install.packages(c("htmlwidgets","htmltools","jsonlite","yaml","base64enc"))
#install.packages("corrplot")
# install.packages('igraph')




# install_github("lchiffon/wordcloud2")
# devtools::install_github("lchiffon/wordcloud2")

# remotes::install_github("mrchypark/multilinguer")
# install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
# install.packages("remotes")
# remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"), force=TRUE)

# library(reticulate)
# install_miniconda()
# library(devtools)
# devtools::install_github('haven-jeon/KoSpacing')
# library(KoSpacing)
# packageVersion("KoSpacing")
#install.packages('readxl')
#install.packages("ggraph")

#½Ã°¢È­
library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('gridExtra') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation

#ÆÄÀÏÀÐ±â, ÀüÃ³¸®
#library(KoNLP)
#useSejongDic()
#library(rJava)
library(readxl)
library(xlsx)
library(stringr)
library(devtools)
library(KoSpacing)
library(reticulate)
library(tm)	
library(dplyr)
library(tidytext)
library(tidyverse)
library(janeaustenr)
library(SentimentAnalysis)
library(wordcloud)
library(wordcloud2)
library(topicmodels)
require(devtools)
library(LDAvis)
library(Rmpfr)

library(ggplot2)
library('SnowballC') # text analysis
library('topicmodels') # text analysis
library('igraph') # visualisation
library('ggraph') # visualisation
library('babynames') # names

###################################################
###########################################################
set.seed(405)
setwd('C:\\Users\\knitwill\\Desktop\\±â»ç µ¥ÀÌÅÍ')
setwd('C:\\R\\rproject')

#µ¥ÀÌÅÍ ¼ÅÇÃ
dat_article<-read_excel("4¿ù±â»çÀüÃ³¸®.xlsx")
dat_article<-data_frame(dat_article)
rows<-sample(nrow(dat_article))
dat_article<-dat_article[rows,]
dat_article_LDA<-dat_article
dat_article<-dat_article[2:3]

#multiplot ÇÔ¼ö
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#µ¥ÀÌÅÍ ¿ä¾àÁ¤º¸
summary(jan_article)
glimpse(jan_article)

#stopwords list
sw <-readLines("sw.txt")
sw <-gsub("\t","",sw)

tidy_stopwords <- tibble(
  word = c(
    "°Í",
    "ÀÖ´Ù",
    "ÇÏ´Ù",
    "¾Ê´Ù",
    "µÇ´Ù",
    "³â",
    "¼ö",
    "À§ÇÏ´Ù",
    "µî",
    "ÀÏ","¿¬ÇÕ´º½º","Àç¹èÆ÷","Çß´Ù","À¸·Î","¿¡¼­","ÇÏ°í","ÇÏ´Â","±âÀÚ","ÀÖ´Â","ÇÑ´Ù","ÀÌ´Ù","»çÁø","±îÁö"
  ),
  lexicon = "tidyverse"
)


stopwords<-tibble(sw)

all_stop_words <- stop_words %>%
  bind_rows(stopwords,tidy_stopwords)

dat_article$context<-gsub("[[:digit:]]+","",dat_article$context)
dat_article$context<-gsub("[[:punct:]]+","",dat_article$context)
dat_article$context<-gsub("http[s]?://[[:alnum:].\\/]+", "", dat_article$context)
dat_article$context<-gsub("[[:digit:]]+","",dat_article$context)
dat_article$context<-gsub("\\s{2,}", "", dat_article$context)

dat_IT<-dat_article[dat_article$label=="IT",]
dat_eco<-dat_article[dat_article$label=="°æÁ¦",]
dat_soc<-dat_article[dat_article$label=="»çÈ¸",]
dat_world<-dat_article[dat_article$label=="¼¼°è",]
dat_pol<-dat_article[dat_article$label=="Á¤Ä¡",]

#ÇØ´ç ¿ùÀüÃ¼
t1<-dat_article %>% 
  unnest_tokens(word,context,"words")

t1 <- t1 %>%
  anti_join(all_stop_words, by = "word")

t1<-t1[nchar(t1$word)>=2,]

#ÇØ´ç¿ù IT
t1_it<-dat_IT %>% 
  unnest_tokens(word,context,"words")

t1_it <- t1_it %>%
  anti_join(all_stop_words, by = "word")

t1_it<-t1_it[nchar(t1_it$word)>=2,]

#1¿ù °æÁ¦
t1_eco<-dat_eco %>% 
  unnest_tokens(word,context,"words")

t1_eco <- t1_eco %>%
  anti_join(all_stop_words, by = "word")

t1_eco<-t1_eco[nchar(t1_eco$word)>=2,]


#1¿ù »çÈ¸
t1_soc<-dat_soc %>% 
  unnest_tokens(word,context,"words")

t1_soc <- t1_soc %>%
  anti_join(all_stop_words, by = "word")

t1_soc<-t1_soc[nchar(t1_soc$word)>=2,]

#1¿ù ¼¼°è
t1_world<-dat_world %>% 
  unnest_tokens(word,context,"words")

t1_world <- t1_world %>%
  anti_join(all_stop_words, by = "word")

t1_world<-t1_world[nchar(t1_world$word)>=2,]


#1¿ù Á¤Ä¡
t1_pol<-dat_pol %>% 
  unnest_tokens(word,context,"words")

t1_pol <- t1_pol %>%
  anti_join(all_stop_words, by = "word")

t1_pol<-t1_pol[nchar(t1_pol$word)>=2,]

#########################################################
#¿öµåÅ¬¶ó¿ìµå

#ÇØ´ç¿ùÀüÃ¼
tot<-t1$word
tot<-gsub(pattern='[^°¡-ÆR]',replacement="",x= tot)
tot<-gsub("http[s]?://[[:alnum:].\\/]+", "", tot)
tot<-gsub("[[:punct:]]+","",tot)
tot<-gsub("[[:digit:]]+","",tot)
tot<-gsub("\\s{2,}", " ", tot)
tot <- Filter(function(x){nchar(x)>=2}, tot)

wc<-sort(table(tot),decreasing = T)
wc_top<-head(wc,1000)
wc_top
letterCloud(wc_top,word='4¿ù')


#ÇØ´ç¿ù IT
tot<-t1_it$word
tot<-gsub(pattern='[^°¡-ÆR]',replacement="",x= tot)
tot<-gsub("http[s]?://[[:alnum:].\\/]+", "", tot)
tot<-gsub("[[:punct:]]+","",tot)
tot<-gsub("[[:digit:]]+","",tot)
tot<-gsub("\\s{2,}", " ", tot)
tot <- Filter(function(x){nchar(x)>=2}, tot)

wc<-sort(table(tot),decreasing = T)
wc_top<-head(wc,500)
wc_top
letterCloud(wc_top,word='IT')


#ÇØ´ç¿ù °æÁ¦
tot<-t1_eco$word
tot<-gsub(pattern='[^°¡-ÆR]',replacement="",x= tot)
tot<-gsub("http[s]?://[[:alnum:].\\/]+", "", tot)
tot<-gsub("[[:punct:]]+","",tot)
tot<-gsub("[[:digit:]]+","",tot)
tot<-gsub("\\s{2,}", " ", tot)
tot <- Filter(function(x){nchar(x)>=2}, tot)

wc<-sort(table(tot),decreasing = T)
wc_top<-head(wc,1000)
wc_top
letterCloud(wc_top,word='°æÁ¦')


#ÇØ´ç¿ù »çÈ¸
tot<-t1_soc$word
tot<-gsub(pattern='[^°¡-ÆR]',replacement="",x= tot)
tot<-gsub("http[s]?://[[:alnum:].\\/]+", "", tot)
tot<-gsub("[[:punct:]]+","",tot)
tot<-gsub("[[:digit:]]+","",tot)
tot<-gsub("\\s{2,}", " ", tot)
tot <- Filter(function(x){nchar(x)>=2}, tot)

wc<-sort(table(tot),decreasing = T)
wc_top<-head(wc,1000)
wc_top
letterCloud(wc_top,word='»çÈ¸')


#ÇØ´ç¿ù ¼¼°è
tot<-t1_world$word
tot<-gsub(pattern='[^°¡-ÆR]',replacement="",x= tot)
tot<-gsub("http[s]?://[[:alnum:].\\/]+", "", tot)
tot<-gsub("[[:punct:]]+","",tot)
tot<-gsub("[[:digit:]]+","",tot)
tot<-gsub("\\s{2,}", " ", tot)
tot <- Filter(function(x){nchar(x)>=2}, tot)


wc<-sort(table(tot),decreasing = T)
wc_top<-head(wc,1000)
wc_top
letterCloud(wc_top,word='¼¼°è')


#ÇØ´ç¿ù Á¤Ä¡
tot<-t1_pol$word
tot<-gsub(pattern='[^°¡-ÆR]',replacement="",x= tot)
tot<-gsub("http[s]?://[[:alnum:].\\/]+", "", tot)
tot<-gsub("[[:punct:]]+","",tot)
tot<-gsub("[[:digit:]]+","",tot)
tot<-gsub("\\s{2,}", " ", tot)
tot <- Filter(function(x){nchar(x)>=2}, tot)

wc<-sort(table(tot),decreasing = T)
wc_top<-head(wc,1000)
wc_top
letterCloud(wc_top,word='Á¤Ä¡')


#############################################################################
#ÀüÃ¼ ·¹ÀÌºí ºóµµ ±â¹Ý ´Ü¾î ½Ã°¢È­
#·¹ÀÌºí º° ´Ü¾î Ä«¿îÆ®ÇÏ±â
foo<-t1 %>%
  group_by(word, label) %>%
  count()

#´Ü¾îº°·Î ¸ð¾Æ¼­ ¼¼±â
bar <-t1 %>%
  group_by(word) %>%
  count() %>%
  rename(all=n)

#½Ã°¢È­
foo %>%
  left_join(bar, by = "word") %>%
  arrange(desc(all)) %>%
  head(80) %>%
  ungroup() %>%
  ggplot(aes(reorder(word, all, FUN = min), n, fill = label)) +
  ggtitle("4¿ù ±â»ç º»¹®")+
  #ggplot(aes(word, n)) +
  geom_col(position="dodge") +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(~ label) +
  theme(legend.position = "none",
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        axis.text.y = element_text(size=14,face='bold')
  )

#frequency ºÎ¿©ÇØ¼­ ´Ü¾î ½Ã°¢È­ ÇÏ±â
tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(label) %>%
  top_n(20, tf_idf) %>%
  ungroup() %>%  
  ggplot(aes(word, tf_idf, fill = label)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  theme(legend.position = "none") +
  facet_wrap(~ label, ncol = 3, scales = "free") +
  coord_flip() +
  labs(y = "TF-IDF values")




#·¹ÀÌºí°£ÀÇ »ó°ü °ü°è »ìÆìº¸±â
frequency <- t1 %>%
  count(label, word) %>%
  filter(n > 1e1) %>%
  group_by(label) %>%
  mutate(freq = n / sum(n)) %>% 
  select(-n) %>% 
  spread(label, freq)

frequency %>%
  select(-word) %>%
  cor(use="complete.obs", method="spearman") %>%
  corrplot(type="lower", method="number", diag=FALSE)


#################################################################################
#IT
foo<-t1_it %>%
  group_by(word, label) %>%
  count()


bar <-t1_it %>%
  group_by(word) %>%
  count() %>%
  rename(all=n)

foo %>%
  left_join(bar, by = "word") %>%
  arrange(desc(all)) %>%
  head(10) %>%
  ungroup() %>%
  ggplot(aes(reorder(word, all, FUN = min), n, fill = label)) +
  ggtitle("4¿ù IT")+
  #ggplot(aes(word, n)) +
  geom_col(position="dodge") +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(~ label) +
  theme(legend.position = "none",
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        axis.text.y = element_text(size=14,face='bold')
  )

#################################################################
#°æÁ¦

foo<-t1_eco %>%
  group_by(word, label) %>%
  count()


bar <-t1_eco %>%
  group_by(word) %>%
  count() %>%
  rename(all=n)

foo %>%
  left_join(bar, by = "word") %>%
  arrange(desc(all)) %>%
  head(10) %>%
  ungroup() %>%
  ggplot(aes(reorder(word, all, FUN = min), n, fill = label)) +
  ggtitle("4¿ù °æÁ¦")+
  #ggplot(aes(word, n)) +
  geom_col(position="dodge",fill="blue",alpha=0.3) +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(~ label) +
  theme(legend.position = "none",
        plot.title = element_text(color="black", size=15, face="bold.italic"),
        axis.text.y = element_text(size=14,face='bold')
        )

###############################################################################
#»çÈ¸
foo<-t1_soc %>%
  group_by(word, label) %>%
  count()


bar <-t1_soc %>%
  group_by(word) %>%
  count() %>%
  rename(all=n)

foo %>%
  left_join(bar, by = "word") %>%
  arrange(desc(all)) %>%
  head(10) %>%
  ungroup() %>%
  ggplot(aes(reorder(word, all, FUN = min), n, fill = label)) +
  ggtitle("4¿ù »çÈ¸")+
  #ggplot(aes(word, n)) +
  geom_col(position="dodge",,fill="green",alpha=0.3) +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(~ label) +
  theme(legend.position = "none",
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        axis.text.y = element_text(size=14,face='bold')
  )
##################################################################
#¼¼°è
foo<-t1_world %>%
  group_by(word, label) %>%
  count()


bar <-t1_world %>%
  group_by(word) %>%
  count() %>%
  rename(all=n)

foo %>%
  left_join(bar, by = "word") %>%
  arrange(desc(all)) %>%
  head(10) %>%
  ungroup() %>%
  ggplot(aes(reorder(word, all, FUN = min), n, fill = label)) +
  ggtitle("4¿ù ¼¼°è")+
  #ggplot(aes(word, n)) +
  geom_col(position="dodge",,fill="black",alpha=0.4) +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(~ label) +
  theme(legend.position = "none",
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        axis.text.y = element_text(size=14,face='bold')
  )
##########################################################
#Á¤Ä¡
foo<-t1_pol %>%
  group_by(word, label) %>%
  count()


bar <-t1_pol %>%
  group_by(word) %>%
  count() %>%
  rename(all=n)

foo %>%
  left_join(bar, by = "word") %>%
  arrange(desc(all)) %>%
  head(10) %>%
  ungroup() %>%
  ggplot(aes(reorder(word, all, FUN = min), n, fill = label)) +
  ggtitle("4¿ù Á¤Ä¡")+
  #ggplot(aes(word, n)) +
  geom_col(position="dodge",fill="orange",alpha=0.3) +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(~ label) +
  theme(legend.position = "none",
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        axis.text.y = element_text(size=14,face='bold')
  )

#######################################################################
#·¹ÀÌºíº° tfidf ´Ü¾î
frequency <-t1 %>%
  count(label, word)

tf_idf <- frequency %>%
  bind_tf_idf(word, label, n)

tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  top_n(30, tf_idf) %>%
  ggplot(aes(word, tf_idf, fill = label)) +
  geom_col() +
  labs(x = NULL, y = "TF-IDF values") +
  theme(legend.position = "top", axis.text.x  = element_text(angle=90, hjust=1, vjust=0.9,size=14, face="bold"))

################################################################################
#bigram tfidf
t2 <- dat_article %>% 
  select(label, context) %>% 
  unnest_tokens(bigram, context, token = "ngrams", n = 2)


bi_sep <- t2 %>%
  separate(bigram, c("word1", "word2"), sep = " ")


# for later
bigram_counts <- bi_sep %>%
  count(word1, word2, sort = TRUE)

t2 <- bi_sep %>%
  unite(bigram, word1, word2, sep = " ")

t2_tf_idf <- t2 %>%
  count(label, bigram) %>%
  bind_tf_idf(bigram, label, n) %>%
  arrange(desc(tf_idf))

t2_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%
  group_by(label) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%  
  ggplot(aes(bigram, tf_idf, fill = label)) +
  geom_col() +
  labs(x = NULL, y = "TF-IDF values") +
  theme(legend.position = "none",axis.text.y = element_text(size=14,face='bold')) +
  facet_wrap(~ label, ncol = 3, scales = "free") +
  coord_flip()


##################################################################################
#ÅäÇÈ¸ðµ¨¸µ ÁøÇàÇÏ±â
dat_article_LDA$context<-gsub("[[:digit:]]+","",dat_article_LDA$context)
dat_article_LDA$context<-gsub("[[:punct:]]+","",dat_article_LDA$context)
dat_article_LDA$context<-gsub("http[s]?://[[:alnum:].\\/]+", "", dat_article_LDA$context)
dat_article_LDA$context<-gsub("[[:digit:]]+","",dat_article_LDA$context)
dat_article_LDA$context<-gsub("\\s{2,}", "", dat_article_LDA$context)



lda_t1 <- dat_article_LDA %>%
  unnest_tokens(word,context)

lda_t1 <- lda_t1 %>%
  anti_join(all_stop_words, by = "word")

lda_t1<-lda_t1[nchar(lda_t1$word)>=2,]

freq<-lda_t1 %>%
  count(index,word)

#docTerm Matrix »ý¼º
lda_t1_tm <-cast_dtm(freq, index, word,n)
lda_t1_tm

inspect(lda_t1_tm[1:5,1:20])
lda_t1_tm <- LDA(lda_t1_tm, k=5, control=list(seed=1234))

lda_t1_topics<-tidy(lda_t1_tm, matrix='beta')
lda_t1_topics %>% sample_n(5)

#ÅäÇÈº° ½Ã°¢È­ ÇØº¸±â
lda_t1_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  theme(axis.text.x = element_text(angle = 90),axis.text.y = element_text(size=10,face='bold'))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 5) +
  coord_flip()


# ´Ü¾î°¡ ÁÖÁ¦ ¾îµð¼­ ´õ ºó¹øÇÏ°Ô µÇ´ÂÁö
p1 <- lda_t1_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log10(topic2 / topic1)) %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(5, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio, fill = log_ratio > 0)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Log ratio of beta in topic 2 / topic 1") +
  coord_flip()

p2 <- lda_t1_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic2 > .001 | topic3 > .001) %>%
  mutate(log_ratio = log10(topic3 / topic2)) %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(5, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio, fill = log_ratio > 0)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Log ratio of beta in topic 3 / topic 2") +
  coord_flip()

p3 <- lda_t1_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic4 > .001 | topic3 > .001) %>%
  mutate(log_ratio = log10(topic4 / topic3)) %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(5, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio, fill = log_ratio > 0)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Log ratio of beta in topic 4 / topic 3") +
  coord_flip()

p4 <- lda_t1_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic4 > .001 | topic5 > .001) %>%
  mutate(log_ratio = log10(topic5 / topic4)) %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(5, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio, fill = log_ratio > 0)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Log ratio of beta in topic 5 / topic 4") +
  coord_flip()

p5 <- lda_t1_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic5 > .001 | topic1 > .001) %>%
  mutate(log_ratio = log10(topic5 / topic4)) %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(5, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio, fill = log_ratio > 0)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Log ratio of beta in topic 5 / topic 1") +
  coord_flip()
layout <- matrix(c(1,2,3,4,5,6),2,3,byrow=TRUE)
multiplot(p1,p2,p3,p4,p5,layout = layout)
