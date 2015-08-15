tokenize = function(x, insertLast=T) {
    temp = x
    temp = gsub("[^\\w\\.\\!\\?\\d\\%\\-\\$\\s]", "", temp, perl=T)
    
    temp = gsub("[\\.\\!\\?\\s]+$", "", temp, perl=T)
    
    if (insertLast) {
        temp = gsub("$", " </s>", temp, perl=T)
    }
    
    #temp = gsub("[\\.\\!\\?\\s]+$", " </s>", temp, perl=T)
    temp = gsub("[\\.\\!\\?]+", " </s> <s> ", temp, perl=T)
    temp = gsub("^", "<s> ", temp, perl=T)
      
    
    temp = gsub("\\d{1,2}:\\d{1,2}", " <time> ", temp, perl=T)
    temp = gsub("\\d{2,4}-\\d{2}-\\d{2,4}|\\d{2,4}/\\d{2}/\\d{2,4}", " <date> ", temp, perl=T)
    temp = gsub("\\d{3}-\\d{3}-\\d{4}|\\d{10}", " <phone> ", temp, perl=T)
    temp = gsub("\\$\\d+", " <dollars> ", temp, perl=T)
    temp = gsub("\\d+%", " <percent> ", temp, perl=T)
    temp = gsub("\\d+", " <number> ", temp, perl=T)
    temp = gsub('_+', ' ', temp, perl=T)
    temp = unlist(strsplit(temp, " "))
    
    temp = gsub("[\\.\\!\\?\\d\\%\\-\\$]", "", temp, perl=T)
    temp = temp[temp!=""]
    temp = tolower(temp)
    return (temp)
}



getPKN = function(numContexts, bigramsCount, count, contextCount, LAMBDA, d) {
    
    PCONTINUATION = numContexts / bigramsCount
    DISCOUNTEDCOUNT = (max(count - d, 0) / contextCount)
    PKN = DISCOUNTEDCOUNT +  (LAMBDA * PCONTINUATION)
    PKN
}

bestNextWordMemory = function(tokens, unigrams, bigrams)
{
    bigramsCount = nrow(bigrams)
    d = 0.5
        
    setkey(unigrams, V1, id)
    un = unigrams[unigrams$V1 == tokens[length(tokens)], list(id, count)]
    identifier = un$id
    
    contextCount = un$count
    setkey(bigrams, x, y)
    
    yAfterX = subset(bigrams, x == identifier, select=c('y', 'count'))
    possibleTokensCount = nrow(yAfterX)
    LAMBDA = (d / contextCount) * possibleTokensCount
    
    plan = merge(bigrams[bigrams$y %in% yAfterX$y, list(num = length(count)), by=c('y')], yAfterX, by=('y'))
    plan = plan[, list(y = paste0(y, collapse=','),  
                       PKN=getPKN(num[1], bigramsCount, count, contextCount[1], LAMBDA, d)), by=c('count', 'num')]
    
    nextWordsIds = unlist(strsplit(plan[plan$PKN == max(plan$PKN), y], ","))
    nextWords = unigrams[unigrams$id %in% nextWordsIds, ]$V1
    nextWords
}
    