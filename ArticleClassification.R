setwd('C:\\Users\\knitwill\\Desktop\\기사 데이터')
setwd('C:\\R\\rproject')
set.seed(123)
library(readxl)
library(keras)
install_keras(tensorflow = "gpu")
data<-read_excel('model_data.xlsx')
rows<-sample(nrow(data))
data<-data[rows,]
data<-data[1:2]

sample_size<-floor(0.75*nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = sample_size)
train_ind
train<-data[train_ind,]
test<-data[-train_ind,]

train



X_train <- train$context
y_train <- train$label

X_test <- test$context
y_test <- test$label

y_train<-ifelse(y_train == "IT", 0, ifelse(y_train=="경제",1 ,ifelse(y_train=="사회",2, ifelse(y_train=="세계",3,4))))
y_test<-ifelse(y_test == "IT", 0, ifelse(y_test=="경제",1 ,ifelse(y_test=="사회",2, ifelse(y_test=="세계",3,4))))

y_train<-to_categorical(y_train,5)
y_test<-to_categorical(y_test,5)


max_len=100
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
  layer_embedding(input_dim = vocab_size+1,256, input_length = max_len) %>%
  layer_lstm(256,return_sequences = TRUE, dropout = 0.5) %>%
  layer_batch_normalization()%>%
  layer_global_max_pooling_1d() %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 64,activation = 'relu') %>%
  layer_dropout(0.5) %>%
  layer_dense(units=5, activation='softmax')

output <- input %>%
  text_vectorization() %>%
  layer_embedding(input_dim = vocab_size+1,256, input_length = max_len) %>%
  layer_lstm(256,return_sequences = TRUE, dropout = 0.5) %>%
  layer_global_max_pooling_1d() %>%
  layer_dropout(0.5) %>%
  layer_dense(units=5, activation='softmax')

model<-keras_model(input, output)
model %>% summary()

adam <-optimizer_adam(lr=0.01, beta_1 = 0.9, beta_2=0.999,decay = 0.1,epsilon =1e-05)
rmsprop <- optimizer_rmsprop(lr=0.01,epsilon=1e-03,decay=0.1)
model %>% compile(
  loss = 'sparse_categorical_crossentropy', metrics= c('accuracy'), optimizer="adam"
)

history <- model %>% fit(
  X_train,
  y_train,
  epochs = 10,
  batch_size = 64,
  validation_split = 0.2,
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

test<-model %>% predict(X_test[1:10])


at<-argmax(test)
ayt<-argmax(y_test[1:10,])
paste(at,ayt)
