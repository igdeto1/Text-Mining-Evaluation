#.libPaths("c://Local/R/libs") # <- Poned esto si necesitáis cambiar el path de las librerías

# Including needed libraries
library(qdap)
library(XML)
library(tm)
library(splitstackshape)
library(caret)
library(devtools)
library(symantoAPI) #install_github("symanto-research/symantoAPI")
library(quanteda)

start.time <- Sys.time()

setwd("C:/Users/ignacio/Desktop/Universidad/Master Big Data/text mining/")

# Preparing parameters
n <- 10000     # Number of words in the vocabulary. Usually used 1000 or 10000
k <- 10        # Number of folds in cross-validation. Usually used 10
r <- 3        # Number of repeats in cross-validation. Usually used 3
N <- 10      # Number of tweets per author to average the handcrafter features
TOKEN <- "b9afdf15572e4a46995d16a941a018cd"
path_training <- "C:/Users/ignacio/Desktop/Universidad/Master Big Data/text mining/pan20-author-profiling-training-2020-02-23"	# Training path
path_test <- "C:/Users/ignacio/Desktop/Universidad/Master Big Data/text mining/test"			# Test path
lang <- "en"


# Auxiliar functions
# * GenerateVocabulary: Given a corpus (training set), obtains the n most frequent words
# * GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the bow representation

# GenerateVocabulary: Given a corpus (training set), obtains the n most frequent words
GenerateVocabulary <- function(path, n = 1000, lowcase = FALSE, punctuations = FALSE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE) {
  setwd(path)
  
  # Reading corpus list of files
  files = list.files(pattern="*.xml")
  
  # Reading files contents and concatenating into the corpus.raw variable
  corpus.raw <- NULL
  i <- 0
  for (file in files) {
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
    i <- i + 1
    if (verbose) print(paste(i, " ", file))
  }
  
  # Preprocessing the corpus
  corpus.preprocessed <- corpus.raw
  
  if (lowcase) {
    if (verbose) print("Tolower...")
    corpus.preprocessed <- tolower(corpus.preprocessed)
  }
  
  if (punctuations) {
    if (verbose) print("Removing punctuations...")
    corpus.preprocessed <- removePunctuation(corpus.preprocessed)
  }
  
  if (numbers) {
    if (verbose) print("Removing numbers...")
    corpus.preprocessed <- removeNumbers(corpus.preprocessed)
  }
  
  if (whitespaces) {
    if (verbose) print("Stripping whitestpaces...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  if (swlang!="")	{
    if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
  }
  
  if (swlist!="") {
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  
  # Generating the vocabulary as the n most frequent terms
  if (verbose) print("Generating frequency terms")
  corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
  if (verbose) plot(corpus.frequentterms)
  
  return (corpus.frequentterms)
}

getEmoIdx <-function(emo) {
  if (emo=="love") {
    return (1)
  } else if (emo=="joy") {
    return (2)
  } else if (emo=="anger") {
    return (3)
  } else if (emo=="sadness") {
    return (4)
  } else if (emo=="surprise") {
    return (5)
  }
}

# GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the bow representation
GenerateBoW <- function(path, vocabulary, n = 100000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", sentiment_features = FALSE, readable_features = TRUE, emotions_features = FALSE, BUA_features = FALSE, verbose = TRUE) {
  setwd(path)
  
  # Reading the truth file
  truth <- read.csv("truth.txt", sep=":", header=FALSE, stringsAsFactors = TRUE)
  truth <- truth[,c(1,4)]
  colnames(truth) <- c("author", "class")
  truth$class[truth$class == 0] <- "NO"
  truth$class[truth$class == 1] <- "YES"
  
  i <- 0
  bow <- NULL
  # Reading the list of files in the corpus
  files = list.files(pattern="*.xml")
  for (file in files) {
    # Obtaining truth information for the current author
    author <- gsub(".xml", "", file)
    myclass <- truth[truth$author==author,"class"]
    
    # Reading contents for the current author
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
    
    
    rawdata <- gsub("\"", "", txtdata)
    rawdata <- gsub("'", "", rawdata)
    rawdata <- gsub("\n", "", rawdata)
    rawdata <- gsub("\\\\", "", rawdata)
    
    #rawdata <- paste(rawdata, collapse = ' ')
    
    # Preprocessing the text
    if (lowcase) {
      txtdata <- tolower(txtdata)
    }
    
    if (punctuations) {
      txtdata <- removePunctuation(txtdata)
    }
    
    if (numbers) {
      txtdata <- removeNumbers(txtdata)
    }
    
    if (whitespaces) {
      txtdata <- stripWhitespace(txtdata)
    }
    
    # Building the vector space model. For each word in the vocabulary, it obtains the frequency of occurrence in the current author.
    line <- author
    freq <- freq_terms(txtdata, n)
    for (word in vocabulary$WORD) {
      thefreq <- 0
      if (length(freq[freq$WORD==word,"FREQ"])>0) {
        thefreq <- freq[freq$WORD==word,"FREQ"]
      }
      line <- paste(line, ",", thefreq, sep="")
    }
    
    
    
    # Adding handcrafted features by accessing Symanto API
    if (sentiment_features) {
      # Sentiment positive vs. negative
      avgSent <- 0
      for (iTweet in 1:N) {
        sentiment <- get_sentiment(text = rawdata[iTweet], language = lang, token = TOKEN)
        
        if (sentiment$sentiment == "positive") {
          avgSent <- avgSent + sentiment$probab
        } else {
          avgSent <- avgSent - sentiment$probab
        }
      }
      avgSent <- avgSent / N
      
      line <- paste(line, ",", avgSent)
    }
    if (readable_features) {
      # Readability positive vs. negative
      avgRe <- 0
      for (iTweet in 1:N) {
        readability<-textstat_readability(rawdata[iTweet], measure="Flesch")
        
        avgRe <- avgRe + readability$Flesch
      }
      
      avgRe <- avgRe / N
      
      line <- paste(line, ",", avgRe)
    }
    if (BUA_features) {
      # Personality emotional vs. rational
      #avgPersonality <- 0
      #for (iTweet in 1:N) {
      #  personality <- get_bua_personality(text = rawdata[iTweet], language = lang, token = TOKEN) 
      #  
      #  if (personality$personality_pred=="emotional") {
      #    avgPersonality <- avgPersonality + personality$personality_probab
      #  } else {
      #    avgPersonality <- avgPersonality - personality$personality_probab
      #  }
      #}
      #avgPersonality <- avgPersonality / N
      #line <- paste(line, ",", avgPersonality)
      
      # Action seeking yes/no
      #avgAction <- 0
      #for (iTweet in 1:N) {
      #  action <- get_bua_action(text = rawdata[iTweet], language = lang, token = TOKEN)  
      
      #  if (action$action_pred=="yes") {
      #    avgAction <- avgAction + action$action_probab
      #  } else {
      #    avgAction <- avgAction - action$action_probab
      #  }
      #} 
      #avgAction <- avgAction / N
      #line <- paste(line, ",", avgAction)
      
      # Fact-oriented yes/no
      #avgFact <- 0
      #for (iTweet in 1:N) {
      #  fact <- get_bua_fact(text = rawdata[iTweet], language = lang, token = TOKEN)  
      
      #  if (fact$fact_pred=="yes") {
      #    avgFact <- avgFact + fact$fact_probab
      #  } else {
      #    avgFact <- avgFact - fact$fact_probab
      #  }
      #} 
      #avgFact <- avgFact / N
      #line <- paste(line, ",", avgFact)
      
      # Information seeking yes/no
      #avgInfo <- 0
      #for (iTweet in 1:N) {
      #  info <- get_bua_info(text = rawdata[iTweet], language = lang, token = TOKEN)  
      #  
      #  if (info$info_pred=="yes") {
      #    avgInfo <- avgInfo + info$info_probab
      #  } else {
      #    avgInfo <- avgInfo - info$info_probab
      #  }
      #} 
      #avgInfo <- avgInfo / N
      #line <- paste(line, ",", avgInfo)
      
      # Self-revealing yes/no
      #avgSelf <- 0
      #for (iTweet in 1:N) {
      #  self <- get_bua_self(text = rawdata[iTweet], language = lang, token = TOKEN)  
      #  
      #  if (self$self_pred=="yes") {
      #    avgSelf <- avgSelf + self$self_probab
      #  } else {
      #    avgSelf <- avgSelf - self$self_probab
      #  }
      #} 
      #avgSelf <- avgSelf / N
      #line <- paste(line, ",", avgSelf)
      
      # All BUAs together
      avgPersonality <- 0
      avgInfo <- 0
      avgSelf <- 0
      avgFact <- 0
      avgAction <- 0
      for (iTweet in 1:N) {
        BUA <- get_bua(text = rawdata[iTweet], language = lang, token = TOKEN)
        
        if (BUA$personality_pred=="emotional") {
          avgPersonality <- avgPersonality + BUA$personality_probab
        } else {
          avgPersonality <- avgPersonality - BUA$personality_probab
        }
        
        if (BUA$self_pred=="yes") {
          avgSelf <- avgSelf + BUA$self_probab
        } else {
          avgSelf <- avgSelf - BUA$self_probab
        }
        
        if (BUA$info_pred=="yes") {
          avgInfo <- avgInfo + BUA$info_probab
        } else {
          avgInfo <- avgInfo - BUA$info_probab
        }
        
        if (BUA$action_pred=="yes") {
          avgAction <- avgAction + BUA$action_probab
        } else {
          avgAction <- avgAction - BUA$action_probab
        }
        
        if (BUA$fact_pred=="yes") {
          avgFact <- avgFact + BUA$fact_probab
        } else {
          avgFact <- avgFact - BUA$fact_probab
        }
      }
      avgPersonality <- avgPersonality / N
      avgInfo <- avgInfo / N
      avgSelf <- avgSelf / N
      avgFact <- avgFact / N
      avgAction <- avgAction / N
      
      line <- paste(line, ",", avgPersonality, ",", avgInfo, ",", avgSelf, ",", avgFact, ",", avgAction)
    }
    if (emotions_features) {
      avgEmo <- numeric(5)
      for (iTweet in 1:N) {
        emotion <- get_emotion(text = rawdata[iTweet], language = lang, token = TOKEN, all = TRUE)
        
        for (emo in c("love", "joy", "anger", "sadness", "surprise")) {
          if (emotion$emotion1==emo) {
            avgEmo[getEmoIdx(emo)] <- avgEmo[getEmoIdx(emo)] + emotion$probab1
          } else if (emotion$emotion2==emo) {
            avgEmo[getEmoIdx(emo)] <- avgEmo[getEmoIdx(emo)] + emotion$probab2
          } else if (emotion$emotion3==emo) {
            avgEmo[getEmoIdx(emo)] <- avgEmo[getEmoIdx(emo)] + emotion$probab3
          } else if (emotion$emotion4==emo) {
            avgEmo[getEmoIdx(emo)] <- avgEmo[getEmoIdx(emo)] + emotion$probab4
          } else if (emotion$emotion5==emo) {
            avgEmo[getEmoIdx(emo)] <- avgEmo[getEmoIdx(emo)] + emotion$probab5
          } else if (emotion$emotion6==emo) {
            avgEmo[getEmoIdx(emo)] <- avgEmo[getEmoIdx(emo)] + emotion$probab6
          }
        } 
      }
      avgEmo <- avgEmo / N
      
      line <- paste(line, ",", avgEmo[1], ",", avgEmo[2], ",", avgEmo[3], ",", avgEmo[4], ",", avgEmo[5])
    }
    
    # Concatenating the corresponding class
    line <- paste(myclass, ",", line, sep="")
    
    # New row in the vector space model matrix
    bow <- rbind(bow, line)
    i <- i + 1
    
    if (verbose) {
      print(paste(i, author, myclass))
    }
  }
  
  return (bow)
}



# GENERATE VOCABULARY
vocabulary <- GenerateVocabulary(paste(path_training, lang, sep="/" ), n, swlang=lang)#, swlist=c("url","user","hashtag"))

# GENDER IDENTIFICATION
#######################
# GENERATING THE BOW FOR THE TRAINING SET
bow_training <- GenerateBoW(paste(path_training, lang, sep="/"), vocabulary, sentiment_features = TRUE, readable_features = FALSE, emotions_features = TRUE)

# PREPARING THE VECTOR SPACE MODEL FOR THE TRAINING SET
training <- concat.split(bow_training, "V1", ",")
training <- cbind(training[,2], training[,4:ncol(training)])
names(training)[1] <- "theclass"



# Learning a SVM and evaluating it with k-fold cross-validation
train_control <- trainControl( method="repeatedcv", number = k , repeats = r)
model_SVM <- train( theclass~., data= training, trControl = train_control, method = "svmLinear")
print(model_SVM)
model_rf <- train( theclass~., data= training, trControl = train_control, method = "rf")
print(model_rf)

# Without handcrafted features
training_without <- training[,1:(n+1)]
train_control <- trainControl( method="repeatedcv", number = k , repeats = r)
model_SVM_without <- train( theclass~., data= training_without, trControl = train_control, method = "svmLinear")
print(model_SVM_without)



end.time <- Sys.time()

print(end.time - start.time)



# Learning a SVM with the whole training set and without evaluating it
#train_control <- trainControl(method="none")
#model_SVM <- train( theclass~., data= training, trControl = train_control, method = "svmLinear")

# GENERATING THE BOW FOR THE TEST SET
bow_test <- GenerateBoW(path_test, vocabulary, handcrafted = TRUE)

# Preparing the vector space model and truth for the test set
test <- concat.split(bow_test, "V1", ",")
truth <- unlist(test[,2])
test <- test[,4:ncol(test)]

# Predicting and evaluating the prediction
pred_SVM <- predict(model_SVM, test)
confusionMatrix(pred_SVM, truth)

