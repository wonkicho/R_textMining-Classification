setwd('C:\\Users\\knitwill\\Desktop\\기사 데이터')
setwd('C:\\R\\rproject')
devtools::install_github("rstudio/tensorflow")
library(tensorflow)
install_tensorflow()

install.packages('ramify')
library('ramify')
devtools::install_github("rstudio/keras")
install.packages('readxl')
install.packages("keras")

library(keras)
install_keras()
library(readxl)
library(RmecabKo)
library(KoNLP)
useSejongDic()
library(rJava)
set.seed(123)

#data load
data<-read_excel('제목데이터전처리.xlsx')
rows<-sample(nrow(data))
data<-data[rows,]
data<-data[2:3]

sample_size<-floor(0.75*nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = sample_size)

train<-data[train_ind,]
test<-data[-train_ind,]

X_train <- train$title
y_train <- train$label

X_test <- test$title
y_test <- test$label

y_test


Y_train<-c()
for(i in 1:length(y_train)){
  if(y_train[i] == 1){
    Y_train[i] <- append(list(c(0,0,1)),Y_train[i])
  }else if(y_train[i]==0){
    Y_train[i] <-append(list(c(0,1,0)),Y_train[i])
  }else if(y_train[i]==-1){
    Y_train[i] <- append(list(c(1,0,1)),Y_train[i])
  }
}



max_len=50
batch_size <- 64
vocab_size<-22000
length(X_train)
length(y_train)
text_vectorization <- layer_text_vectorization(
  max_tokens = vocab_size,
  output_sequence_length = max_len
)

text_vectorization %>%
  adapt(X_train)

text_vectorization(matrix(X_train, ncol = 1))
get_vocabulary(text_vectorization)


input<-layer_input(shape = c(1), dtype = "string")


output <- input %>%
  text_vectorization() %>%
  layer_embedding(input_dim = vocab_size+1,128, input_length = max_len) %>%
  layer_lstm(128,return_sequences = TRUE, dropout = 0.5) %>%
  layer_batch_normalization()%>%
  layer_global_max_pooling_1d() %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 64,activation = 'relu') %>%
  layer_dropout(0.5) %>%
  layer_dense(units=3, activation='softmax')

model<-keras_model(input, output)
model %>% summary()

adam <-optimizer_adam(lr=0.01,decay=0.1,epsilon=1e-05)
rmsprop <- optimizer_rmsprop(lr=0.01,epsilon=1e-03,decay=0.1)
model %>% compile(
  loss = 'sparse_categorical_crossentropy', metrics= c('accuracy'), optimizer=adam
)

history <- model %>% fit(
  X_train,
  y_train,
  epochs = 20,
  batch_size = 64,
  validation_split = 0.1,
  verbose=1,
  shuffle=TRUE
  #callback_early_stopping(monitor='val_loss')
)

scores <- model %>% evaluate(
  X_test, y_test,
  batch_size = 64
)
plot(history)
X_test

predicted<-model %>% predict(X_test)

#1 긍정, 0 중립, -1 부정 // 1 긍정, 2 부정 0 중립
predicted<-argmax(predicted)
predicted
for(i in 1:length(predicted)){
  predicted[i]<-ifelse(predicted[i]==1,0,ifelse(predicted[i]==2,1,2))
}
predicted
paste(y_test,predicted)
t<-CrossTable(y_test,predicted)
CrossTable(y_test,predicted)

paste(X_test[1:20],y_test[1:20], predicted[1:20])

