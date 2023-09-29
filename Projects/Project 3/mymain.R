#Libraries#################################################################################
library(glmnet)
library(text2vec)
library(tidyverse)
library(tm)
library(pROC)
library(slam)
library(stopwords)
library(stringr)
#Custom Functions (Only works for this project)############################################
#Creating DTM
customDTM <- function(x){
  #Setting the stop words
  stop_words = c("i", "me", "my", "myself", 
                 "we", "our", "ours", "ourselves", 
                 "you", "your", "yours", 
                 "their", "they", "his", "her", 
                 "she", "he", "a", "an", "and",
                 "is", "was", "are", "were", 
                 "him", "himself", "has", "have", 
                 "it", "its", "the", "us")
  it_train = itoken(x$review,
                    preprocessor = tolower, 
                    tokenizer = word_tokenizer)
  tmp.vocab = create_vocabulary(it_train, 
                                stopwords = stop_words, 
                                ngram = c(1L,4L))
  tmp.vocab = prune_vocabulary(tmp.vocab, term_count_min = 10,
                               doc_proportion_max = 0.5,
                               doc_proportion_min = 0.001)
  dtm  = create_dtm(it_train, vocab_vectorizer(tmp.vocab))
  
  return(dtm)
}
#Conditioning training vocab matrix for the model
conform <- function(dtm, vocab){
  fixed_vocab = matrix(nrow = nrow(dtm), ncol = length(vocab))
  colnames(fixed_vocab) <- vocab
  #Replacement for loop
  for(i in 1:ncol(fixed_vocab)){
    for(j in 1:ncol(dtm)){
      if(colnames(fixed_vocab)[i] == colnames(dtm)[j]){
        fixed_vocab[,i] = dtm[,vocab[i]]
      }
    }
  }
  #Filling NAs
  fixed_vocab[is.na(fixed_vocab)] <- 0
  
  return(fixed_vocab)
}
##Load Vocabulary and Training Data########################################################
myvocab <- scan(file = "myvocab.txt", what = character())
train = read.table("train.tsv", stringsAsFactors = FALSE, header = TRUE)
# removing specials characters from each review in train
train$review = gsub('<.*?>', ' ', train$review)
##Cleaning The Training Data###############################################################
set.seed(0243)
#Creating Initial DTM
dtm_train = customDTM(train)
#Shrinking DTM
fixed_vocab_train = conform(dtm_train, myvocab)
##Training the Model#######################################################################
fit = cv.glmnet(x = fixed_vocab_train, 
                y = train$sentiment, 
                alpha = 0, #Ridge
                family='binomial',
                nfolds = 5)
##Load the Test Data#######################################################################
test <- read.table("test.tsv", stringsAsFactors = FALSE, header = TRUE)
# removing specials characters from each review in test
test$review = gsub('<.*?>', ' ', test$review)
##Cleaning The Testing Data################################################################
#Creating Initial DTM
dtm_test = customDTM(test)
#Shrinking DTM
fixed_vocab_test = conform(dtm_test, myvocab)
##Predictions##############################################################################
#Doing the prediction
preds = predict(fit, fixed_vocab_test, type = 'response', s = fit$lambda.min)
#Creating the output
output = data.frame(id = test$id, prob = preds[,1])
#Exporting the Output
write.table(output, file = "mysubmission.txt", row.names = FALSE, sep='\t')