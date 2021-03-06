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

#시각화
library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('gridExtra') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation

#파일읽기, 전처리
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
setwd('C:\\Users\\knitwill\\Desktop\\기사 데이터')
setwd('C:\\R\\rproject')

#데이터 셔플
dat_article<-read_excel("4월기사전처리.xlsx")
dat_article<-data_frame(dat_article)
rows<-sample(nrow(dat_article))
dat_article<-dat_article[rows,]
dat_article_LDA<-dat_article
dat_article<-dat_article[2:3]

#multiplot 함수
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

#데이터 요약정보
summary(jan_article)
glimpse(jan_article)

#stopwords list
sw <-readLines("sw.txt")
sw <-gsub("\t","",sw)

tidy_stopwords <- tibble(
  word = c(
    "것",
    "있다",
    "하다",
    "않다",
    "되다",
    "년",
    "수",
    "위하다",
    "등",
    "일","연합뉴스","재배포","했다","으로","에서","하고","하는","기자","있는","한다","이다","사진","까지"
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
dat_eco<-dat_article[dat_article$label=="경제",]
dat_soc<-dat_article[dat_article$label=="사회",]
dat_world<-dat_article[dat_article$label=="세계",]
dat_pol<-dat_article[dat_article$label=="정치",]

#해당 월전체
t1<-dat_article %>% 
  unnest_tokens(word,context,"words")

t1 <- t1 %>%
  anti_join(all_stop_words, by = "word")

t1<-t1[nchar(t1$word)>=2,]

#해당월 IT
t1_it<-dat_IT %>% 
  unnest_tokens(word,context,"words")

t1_it <- t1_it %>%
  anti_join(all_stop_words, by = "word")

t1_it<-t1_it[nchar(t1_it$word)>=2,]

#1월 경제
t1_eco<-dat_eco %>% 
  unnest_tokens(word,context,"words")

t1_eco <- t1_eco %>%
  anti_join(all_stop_words, by = "word")

t1_eco<-t1_eco[nchar(t1_eco$word)>=2,]


#1월 사회
t1_soc<-dat_soc %>% 
  unnest_tokens(word,context,"words")

t1_soc <- t1_soc %>%
  anti_join(all_stop_words, by = "word")

t1_soc<-t1_soc[nchar(t1_soc$word)>=2,]

#1월 세계
t1_world<-dat_world %>% 
  unnest_tokens(word,context,"words")

t1_world <- t1_world %>%
  anti_join(all_stop_words, by = "word")

t1_world<-t1_world[nchar(t1_world$word)>=2,]


#1월 정치
t1_pol<-dat_pol %>% 
  unnest_tokens(word,context,"words")

t1_pol <- t1_pol %>%
  anti_join(all_stop_words, by = "word")

t1_pol<-t1_pol[nchar(t1_pol$word)>=2,]

#########################################################
#워드클라우드

#해당월전체
tot<-t1$word
tot<-gsub(pattern='[^가-힣]',replacement="",x= tot)
tot<-gsub("http[s]?://[[:alnum:].\\/]+", "", tot)
tot<-gsub("[[:punct:]]+","",tot)
tot<-gsub("[[:digit:]]+","",tot)
tot<-gsub("\\s{2,}", " ", tot)
tot <- Filter(function(x){nchar(x)>=2}, tot)

wc<-sort(table(tot),decreasing = T)
wc_top<-head(wc,1000)
wc_top
letterCloud(wc_top,word='4월')


#해당월 IT
tot<-t1_it$word
tot<-gsub(pattern='[^가-힣]',replacement="",x= tot)
tot<-gsub("http[s]?://[[:alnum:].\\/]+", "", tot)
tot<-gsub("[[:punct:]]+","",tot)
tot<-gsub("[[:digit:]]+","",tot)
tot<-gsub("\\s{2,}", " ", tot)
tot <- Filter(function(x){nchar(x)>=2}, tot)

wc<-sort(table(tot),decreasing = T)
wc_top<-head(wc,500)
wc_top
letterCloud(wc_top,word='IT')


#해당월 경제
tot<-t1_eco$word
tot<-gsub(pattern='[^가-힣]',replacement="",x= tot)
tot<-gsub("http[s]?://[[:alnum:].\\/]+", "", tot)
tot<-gsub("[[:punct:]]+","",tot)
tot<-gsub("[[:digit:]]+","",tot)
tot<-gsub("\\s{2,}", " ", tot)
tot <- Filter(function(x){nchar(x)>=2}, tot)

wc<-sort(table(tot),decreasing = T)
wc_top<-head(wc,1000)
wc_top
letterCloud(wc_top,word='경제')


#해당월 사회
tot<-t1_soc$word
tot<-gsub(pattern='[^가-힣]',replacement="",x= tot)
tot<-gsub("http[s]?://[[:alnum:].\\/]+", "", tot)
tot<-gsub("[[:punct:]]+","",tot)
tot<-gsub("[[:digit:]]+","",tot)
tot<-gsub("\\s{2,}", " ", tot)
tot <- Filter(function(x){nchar(x)>=2}, tot)

wc<-sort(table(tot),decreasing = T)
wc_top<-head(wc,1000)
wc_top
letterCloud(wc_top,word='사회')


#해당월 세계
tot<-t1_world$word
tot<-gsub(pattern='[^가-힣]',replacement="",x= tot)
tot<-gsub("http[s]?://[[:alnum:].\\/]+", "", tot)
tot<-gsub("[[:punct:]]+","",tot)
tot<-gsub("[[:digit:]]+","",tot)
tot<-gsub("\\s{2,}", " ", tot)
tot <- Filter(function(x){nchar(x)>=2}, tot)


wc<-sort(table(tot),decreasing = T)
wc_top<-head(wc,1000)
wc_top
letterCloud(wc_top,word='세계')


#해당월 정치
tot<-t1_pol$word
tot<-gsub(pattern='[^가-힣]',replacement="",x= tot)
tot<-gsub("http[s]?://[[:alnum:].\\/]+", "", tot)
tot<-gsub("[[:punct:]]+","",tot)
tot<-gsub("[[:digit:]]+","",tot)
tot<-gsub("\\s{2,}", " ", tot)
tot <- Filter(function(x){nchar(x)>=2}, tot)

wc<-sort(table(tot),decreasing = T)
wc_top<-head(wc,1000)
wc_top
letterCloud(wc_top,word='정치')


#############################################################################
#전체 레이블 빈도 기반 단어 시각화
#레이블 별 단어 카운트하기
foo<-t1 %>%
  group_by(word, label) %>%
  count()

#단어별로 모아서 세기
bar <-t1 %>%
  group_by(word) %>%
  count() %>%
  rename(all=n)

#시각화
foo %>%
  left_join(bar, by = "word") %>%
  arrange(desc(all)) %>%
  head(80) %>%
  ungroup() %>%
  ggplot(aes(reorder(word, all, FUN = min), n, fill = label)) +
  ggtitle("4월 기사 본문")+
  #ggplot(aes(word, n)) +
  geom_col(position="dodge") +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(~ label) +
  theme(legend.position = "none",
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        axis.text.y = element_text(size=14,face='bold')
  )

#frequency 부여해서 단어 시각화 하기
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




#레이블간의 상관 관계 살펴보기
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
  ggtitle("4월 IT")+
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
#경제

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
  ggtitle("4월 경제")+
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
#사회
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
  ggtitle("4월 사회")+
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
#세계
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
  ggtitle("4월 세계")+
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
#정치
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
  ggtitle("4월 정치")+
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
#레이블별 tfidf 단어
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
#토픽모델링 진행하기
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

#docTerm Matrix 생성
lda_t1_tm <-cast_dtm(freq, index, word,n)
lda_t1_tm

inspect(lda_t1_tm[1:5,1:20])
lda_t1_tm <- LDA(lda_t1_tm, k=5, control=list(seed=1234))

lda_t1_topics<-tidy(lda_t1_tm, matrix='beta')
lda_t1_topics %>% sample_n(5)

#토픽별 시각화 해보기
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


# 단어가 주제 어디서 더 빈번하게 되는지
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
