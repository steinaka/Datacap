source("utils.R")
library(tidytext)

locale <- "en_US"
folder <- "sample"
filePostfix <- if(folder == "sample") ".sample" else ""
grams <- 7
minFreq <- 2
verbose <- TRUE
debug <- TRUE

# load the corpus and tokenize it
if(missingVar("corpusTokenized",locale)){
  if(missingVar("corpusPreprocessed",locale)){
    if(missingVar("corpus",locale)){
      
      # load source data
      log("Loading data", folder, locale)
      twitter <- readLines(paste0("data/",folder,"/",locale,"/",locale,".twitter",filePostfix,".txt"))
      news <- readLines(paste0("data/",folder,"/",locale,"/",locale,".news",filePostfix,".txt"))
      blogs <- readLines(paste0("data/",folder,"/",locale,"/",locale,".blogs",filePostfix,".txt"))
      
      # combine into 1 big var
      corpus <- c(twitter, news, blogs)

      
      # remove old vars (memory management)
      cleanup(twitter)
      cleanup(news)
      cleanup(blogs)
      
      # cache file 
      storeVar(corpus, locale)
    }
    
    # preprocess raw text files into seperate lines
    log("Preprocessing corpus, # lines:", length(corpus))
    
    #load profanities
    
    # library(tm)
    # profanities <- readLines(paste0("data/cache/",locale,"/profanity.txt"))
    # copuspreclean<- removeWords(corpus, profanities)
    # 
    # corpusPreprocessed <- preProcessData(copuspreclean)
    
    corpusPreprocessed <- preProcessData(corpus)
    
    # cache preprocessed
    
    storeVar(corpusPreprocessed, locale)
    clean1up(corpus)
    # cleanup(copuspreclean)
  
    
  }
  
  
  # Tokenize all string
  log("Tokenizing corpus, # lines:", length(corpusPreprocessed))
  corpusTokenized <-  tokenize(corpusPreprocessed)
  
  
  corpusTokenizedbak <- corpusTokenized
  corpusTokenized <- corpusTokenizedbak
  
  # corpusTokenizedclean <-  removeWords(corpusTokenized$Value, profanities)
  
  profanities <- readLines(paste0("data/cache/",locale,"/profanity.txt"))
  
  library(tm)
  #first 2 lines for transformation to fit data type
  #tm functions to deal better with removing profanities
  
  corpusTokenized <-  iconv(corpusTokenized, to ="utf-8")
  corpusTokenized<-  SimpleCorpus(VectorSource(corpusTokenized))
  
  # Vcorpus keeps all symbols
  # corpusTokenized1 <-  VCorpus(VectorSource(corpusTokenized))
  
  corpusTokenized <- tm_map(corpusTokenized, content_transformer(tolower))
  corpusTokenized <- tm_map(corpusTokenized, stripWhitespace) 
  
  corpusTokenized <- tm_map(corpusTokenized, removePunctuation)
  
  dictCorpus <- corpusTokenized
  corpusTokenized <- tm_map(corpusTokenized, stemDocument)
  
  corpusTokenized <- tm_map(corpusTokenized, removeWords, profanities) 
  
  
  # tokenize the corpus
  myCorpusTokenized <- lapply(corpusTokenized, scan_tokenizer)
  
  #process again to remove symbols -WIP see if error pops up
  
  # myCorpusTokenized <- tm_map(myCorpusTokenized, removePunctuation)
  
  
  
  
  # stem complete each token vector- hang at this step. stopped. Taking a backup to try at next stage
  myTokensStemCompleted <- lapply(myCorpusTokenized, stemCompletion, dictCorpus)
  
  
  myCorpusTokenized1 <- myCorpusTokenized
  
  # concatenate tokens by document, create data frame
  myCorpusTokenizedorig <- data.frame(text = sapply(myCorpusTokenized, paste, collapse = " "), stringsAsFactors = FALSE)
  
  
  
  corpusPreprocessed <- preProcessData(myCorpusTokenizedorig)
  
  
  
  # corpusTokenized <- tm_map(corpusTokenized, stemDocument)
  # corpusTokenized <- tm_map(corpusTokenized, removeWords, profanities) 
  
  
  #corpus with meta data. How to transform to non meta data corpus
  
  #backup of Meta corpus
  # corpusTokenizedmetabak <-corpusTokenized
  #test of removal of meta data
  # utitizze tidy texxt
  
  # corpusTokenized1 <- data.frame(text=sapply(corpusTokenized, identity), 
                          # stringsAsFactors=F)
  # corpusTokenized2 <- unlist(corpusTokenized1)
  
  
  
  
  #reporcess dataframed corpus after removing stem profanity
  corpusTokenized <-  tokenize(corpusTokenized2)
  
  
  
  
  rmcorpusTokenized <- corpusTokenized5
  
  rm(corpusTokenized1)
  rm(corpusTokenized2)
  rm(corpusTokenized3)
  rm(corpusTokenized4)
  rm(corpusTokenized5)
  #----------------------
  
  # remove profanity
  # corpusTokenized <- profanityFilter(corpusTokenized, locale)
  
  # remove old vars (memory management)
  storeVar(corpusTokenized, locale)
  cleanup(corpusPreprocessed)
}

corpusTokenized <- myCorpusTokenized

# Here we should have tokenized data for creating the n-grams
log("Tokenized corpus, # lines:", length(corpusTokenized))


# create filal datastructure for all n-gram Lookup Tables
ngramLookupTables <- list()

# loop over all gram n's

for(gram in 1:grams){
  
  log("Creating ngram", gram)
  gramName <- paste0("gram",gram)
  postfix <- paste0(".",gramName)
  
  # Create or load the n-gram lookup (list of all unique n-gram lookups)
  if(missingVar("gramLookupTable",locale,postfix)){
    
    # Create or load the n-gram frequancy (list of all aggregated & sorted n-gram's )
    if(missingVar("gramFrequencyTable",locale,postfix)){
      
      # Create or load the n-gram table (list of all n-gram)
      if(missingVar("gramTable",locale,postfix)){
       
            log("Generating n-gram table",gram)
            gramTable <- transformNGram(corpusTokenized, gram)
            log("# rows", length(gramTable))
      
            storeVar(gramTable, locale, postfix)
      }
      
      log("Generating n-gram Frequency Table",gram)
      gramFrequencyTable <- frequencyTable(gramTable, gram)
      log("# rows", nrow(gramFrequencyTable))
  
      storeVar(gramFrequencyTable, locale, postfix)
      cleanup(gramTable)
    }
    
    log("Generating n-gram Lookup Table",gram)
    gramLookupTable <- lookupTable(gramFrequencyTable, minFreq)
    log("# rows", nrow(gramLookupTable))
      
    storeVar(gramLookupTable, locale, postfix)
    cleanup(gramFrequencyTable)
  }
      
  if(debug){
    gramLookupTableName <- paste0(gramName,"LookupTable")
    log("Assign lookuptable, ",gramLookupTableName)
    assign(gramLookupTableName, gramLookupTable, envir=globalenv())
  }
  
  log("Adding to ngramLookupTables list",gram)
  ngramLookupTables[[gram]] <- gramLookupTable
    
  cleanup(gramLookupTable)
}

log("Storing lookup tables",gram)
storeVar(ngramLookupTables, locale, force=TRUE)

ngramLookupTables1<- ngramLookupTables

storeVar(ngramLookupTables1, locale, force=TRUE)

