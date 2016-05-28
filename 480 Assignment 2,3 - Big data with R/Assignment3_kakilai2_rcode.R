#Homework3 kakilai2
#Code Reference: Code for lecture of section 3 on compass2g

#Q2
###############################################
#Read Data
###############################################

spamPath = "/home/student/container-data/RDataScience/SpamAssassinMessages"

Dir = list.files(path = paste(spamPath, "messages", "easy_ham",
                              sep = .Platform$file.sep))
fullDir = paste(spamPath, "messages","easy_ham",Dir,
                sep = .Platform$file.sep)

fileName = fullDir[1]
file = readLines(fileName)

dirNames = list.files(path = paste(spamPath, "messages", 
                                   sep = .Platform$file.sep))
length(list.files(paste(spamPath, "messages", dirNames, 
                        sep = .Platform$file.sep)))

# Get the number of files in each directory.
sapply(paste(spamPath, "messages", dirNames, 
             sep = .Platform$file.sep), 
       function(dir) length(list.files(dir)) )

fullDirNames = paste(spamPath, "messages", dirNames, 
                     sep = .Platform$file.sep)


###############################################
#Data Preparation
###############################################
# This function splits a message (msg) into its header and body.
splitMessage = function(msg) {
  splitPoint = match("", msg)
  header = msg[1:(splitPoint-1)]
  body = msg[ -(1:splitPoint) ]
  return(list(header = header, body = body))
}

# Wrap the boundary string matching and extraction steps into a single utility function.
getBoundary = function(header) {
  boundaryIdx = grep("boundary=", header)
  boundary = gsub('"', "", header[boundaryIdx])
  gsub(".*boundary= *([^;]*);?.*", "\\1", boundary)
}

# Now wrap the previous steps into a fundtion called dropAttach

dropAttach = function(body, boundary, drop){
  
  bString = paste("--", boundary, sep = "")
  bStringLocs = which(bString == body)
  
  # if there are fewer than 2 beginning boundary strings, 
  # there is on attachment to drop
  if ((length(bStringLocs) <= 1) | drop==FALSE) return(body)
  
  # do ending string processing
  eString = paste("--", boundary, "--", sep = "")
  eStringLoc = which(eString == body)
  
  if (length(eStringLoc) == 0) 
    return(body[ (bStringLocs[1] + 1) : (bStringLocs[2] - 1)])
  
  # typical case of well-formed email with attachments
  # grab contents between first two beginning boundary strings and 
  # add lines after ending boundary string
  n = length(body)
  if (eStringLoc < n) 
    return( body[ c( (bStringLocs[1] + 1) : (bStringLocs[2] - 1), 
                     ( (eStringLoc + 1) : n )) ] )
  
  return( body[ (bStringLocs[1] + 1) : (bStringLocs[2] - 1) ])
}

library(tm)
stopWords = stopwords()
cleanSW = tolower(gsub("[[:punct:]0-9[:blank:]]+", " ", stopWords))
SWords = unlist(strsplit(cleanSW, "[[:blank:]]+"))
SWords = SWords[ nchar(SWords) > 1 ]
stopWords = unique(SWords)

cleanText =
  function(msg)   {
    tolower(gsub("[[:punct:]0-9[:space:][:blank:]]+", " ", msg))
  }


findMsgWords = 
  function(msg, stopWords) {
    if(is.null(msg))
      return(character())
    
    words = unique(unlist(strsplit(cleanText(msg), "[[:blank:]\t]+")))
    
    # drop empty and 1 letter words
    words = words[ nchar(words) > 1]
    words = words[ !( words %in% stopWords) ]
    invisible(words)
  }

#Q6
my_findMsgWords = 
  function(msg, stopWords) {
    if(is.null(msg))
      return(character())
    
    words = unique(unlist(strsplit(cleanText(msg), "[[:blank:]\t]+")))
    
    # drop empty and 1 letter words
    words = words[ nchar(words) > 1]
    words = words[ !( words %in% stopWords) ]
    invisible(words)
    
    for (i in 1:length(words)){
      words[i] = wordStem(words[i], language = "en")
    }
    
    words = unique(words)
  }

#Q7 drop URL
mydropURL = function(body){
  boundaryIdx = grep("http", body)
  body[boundaryIdx] =gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "",body[boundaryIdx])
  body
}


processAllWords = function(dirName, stopWords)
{
  # read all files in the directory
  fileNames = list.files(dirName, full.names = TRUE)
  
  # drop files that are not email, i.e., cmds
  notEmail = grep("cmds$", fileNames)
  if ( length(notEmail) > 0) fileNames = fileNames[ - notEmail ]
  messages = lapply(fileNames, readLines, encoding = "latin1")
  
  # split header and body
  emailSplit = lapply(messages, splitMessage)
  
  # put body and header in own lists
  bodyList = lapply(emailSplit, function(msg) msg$body)
  headerList = lapply(emailSplit, function(msg) msg$header)
  rm(emailSplit)
  
  # dropURL
  bodyList = lapply(bodyList, mydropURL)
  
  # determine which messages have attachments
  hasAttach = sapply(headerList, function(header) {
    CTloc = grep("Content-Type", header)
    if (length(CTloc) == 0) return(0)
    multi = grep("multi", tolower(header[CTloc]))
    if (length(multi) == 0) return(0)
    multi
  })
  
  hasAttach = which(hasAttach > 0)
  # find boundary strings for messages with attachments
  boundaries = sapply(headerList[hasAttach], getBoundary)
  # drop attachments from message body
  bodyList[hasAttach] = mapply(dropAttach, bodyList[hasAttach],boundaries, SIMPLIFY = FALSE, drop = TRUE)
  #bodyList[hasAttach] = mapply(dropAttach, bodyList[hasAttach],boundaries, SIMPLIFY = FALSE, drop = FALSE)
  
  # extract words from body
  msgWordsList = lapply(bodyList, findMsgWords, stopWords)
  #msgWordsList = lapply(bodyList, my_findMsgWords, stopWords)
  invisible(msgWordsList)
}

# Obtain all the words which are not stopwords in the main bodies 
# of our email messages.
msgWordsList = lapply(fullDirNames, processAllWords, 
                      stopWords = stopWords) 

###############################################
# Start the classification algorithm
###############################################

# See how many messages we have in each directory.
numMsgs = sapply(msgWordsList, length)
numMsgs

# Define isSpam based on directory the message came from.
isSpam = rep(c(FALSE, FALSE, FALSE, TRUE, TRUE), numMsgs)

# Flatten the message words into a single list of lists of message words.
msgWordsList = unlist(msgWordsList, recursive = FALSE)


# Determine number of spam and ham messages for sampling.
numEmail = length(isSpam)
numSpam = sum(isSpam)
numHam = numEmail - numSpam

# Set a particular seed, so the results will be reproducible.
set.seed(418910)

# Take approximately 1/3 of the spam and ham messages as our test spam and ham messages.
testSpamIdx = sample(numSpam, size = floor(numSpam/3))
testHamIdx = sample(numHam, size = floor(numHam/3))

# select word lists for test messages.
# select word lists for training messages.
testMsgWords = c((msgWordsList[isSpam])[testSpamIdx],
                 (msgWordsList[!isSpam])[testHamIdx] )
trainMsgWords = c((msgWordsList[isSpam])[ - testSpamIdx], 
                  (msgWordsList[!isSpam])[ - testHamIdx])

# Create variables indicating which testing and training messages are spam and not.
testIsSpam = rep(c(TRUE, FALSE), 
                 c(length(testSpamIdx), length(testHamIdx)))
trainIsSpam = rep(c(TRUE, FALSE), 
                  c(numSpam - length(testSpamIdx), 
                    numHam - length(testHamIdx)))

# Obtain the bag of words from the training set, and see how many different words there are.
bow = unique(unlist(trainMsgWords))

length(bow)

# Create a vector for word counts for spam, and include the words.
spamWordCounts = rep(0, length(bow))

names(spamWordCounts) = bow

# Get the unique words from each spam message, count the messages each word appears in, 
# and assign the counts to the appropriate places in spamWordCounts.
tmp = lapply(trainMsgWords[trainIsSpam], unique)
tt = table( unlist(tmp) )
spamWordCounts[ names(tt) ] = tt

# creates an object that contains word probabilities given spam and ham,
# and log odds for presence and absence of words given spam or ham.

computeFreqs =
  function(wordsList, spam, bow = unique(unlist(wordsList)))
  {
    # create a matrix for spam, ham, and log odds
    wordTable = matrix(0.5, nrow = 4, ncol = length(bow), 
                       dimnames = list(c("spam", "ham", 
                                         "presentLogOdds", 
                                         "absentLogOdds"),  bow))
    
    # For each spam message, add 1/2 to counts for words in message
    counts.spam = table(unlist(lapply(wordsList[spam], unique)))
    wordTable["spam", names(counts.spam)] = counts.spam + .5
    
    # Similarly for ham messages
    counts.ham = table(unlist(lapply(wordsList[!spam], unique)))  
    wordTable["ham", names(counts.ham)] = counts.ham + .5  
    
    # Find the total number of spam and ham
    numSpam = sum(spam)
    numHam = length(spam) - numSpam
    
    # Prob(word|spam) and Prob(word | ham)
    wordTable["spam", ] = wordTable["spam", ]/(numSpam + .5)
    wordTable["ham", ] = wordTable["ham", ]/(numHam + .5)
    
    # log odds
    wordTable["presentLogOdds", ] = 
      log(wordTable["spam",]) - log(wordTable["ham", ])
    wordTable["absentLogOdds", ] = 
      log((1 - wordTable["spam", ])) - log((1 -wordTable["ham", ]))
    
    invisible(wordTable)
  }

# Obtain the probabilities and log odds for the training data.
trainTable = computeFreqs(trainMsgWords, trainIsSpam)


# combine the previous steps into a function that will compute a log 
# likelihood ratio statistic given words from a message and a message occurrence 
# frequency table for a training set.
computeMsgLLR = function(words, freqTable) 
{
  # Discards words not in training data.
  words = words[!is.na(match(words, colnames(freqTable)))]
  
  # Find which words are present
  present = colnames(freqTable) %in% words
  
  sum(freqTable["presentLogOdds", present]) +
    sum(freqTable["absentLogOdds", !present])
}


# Apply the function to each message in the test set.
testLLR = sapply(testMsgWords, computeMsgLLR, trainTable)


###############################################
# Computing type I error
###############################################
typeIErrorRate = 
  function(tau, llrVals, spam)
  {
    classify = llrVals > tau
    sum(classify & !spam)/sum(!spam)
  }

typeIErrorRate(0, testLLR,testIsSpam)


#Homework3 Q3

###############################################
#Preparing Sample Email
###############################################
indx = c(1:5, 15, 27, 68, 69, 329, 404, 427, 516, 852, 971)
fn = list.files(fullDirNames[1], full.names = TRUE)[indx]
sampleEmail = sapply(fn, readLines)  

###############################################
#split the message into header and body
###############################################

splitMessage = function(msg) {
  splitPoint = match("", msg)
  header = msg[1:(splitPoint-1)]
  body = msg[ -(1:splitPoint) ]
  return(list(header = header, body = body))
}
###############################################
#getBoundary function provided
###############################################

getBoundary = function(header) {
  boundaryIdx = grep("boundary=", header)
  boundary = gsub('"', "", header[boundaryIdx])
  gsub(".*boundary= *([^;]*);?.*", "\\1", boundary)
}

###############################################
# rewrite the getboundary on my own
###############################################

getboundary_rewrite = function(header){
  boundaryIdx = 0
  for (i in (1:length(header))){
    if (length(unlist(strsplit(header[i],'boundary=')))>1){
      boundaryIdx = i
    } 
  }
  if ( boundaryIdx == 0 ) {
    return ( character(0))
  }
  extract = unlist(strsplit(header[boundaryIdx],'boundary='))
  boundary = unlist(strsplit(extract[2],'"'))
  if (length(boundary) > 1 ){
    boundary = boundary[which.max(nchar(boundary))]
  }
  boundary
}


sampleSplit = lapply(sampleEmail, splitMessage)
headerList = lapply(sampleSplit, function(msg) msg$header)

###############################################
# Validating the function by comparing the output by the two functions: 
# getBoundary and getBoundary_rewrite based on sampleEmail
###############################################

boundary_list = c()
boundary_list_rewrite = c()
for (i in (1:length(headerList))){
  boundary = getBoundary(headerList[[i]]) 
  boundary_rewrite = getboundary_rewrite(headerList[[i]])
  boundary_list = c(boundary_list,boundary)
  boundary_list_rewrite = c(boundary_list_rewrite,boundary_rewrite)}


###############################################
# comparing the output of the two functions
###############################################
boundary_list
boundary_list_rewrite




