library(RSQLite)
library(data.table)

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
    temp = gsub("\\$[\\d\\s]+", " <dollars> ", temp, perl=T)
    temp = gsub("[\\d\\s]+%", " <percent> ", temp, perl=T)
    temp = gsub("\\d+[\\W]+\\d+", " <number> ", temp, perl=T)
    temp = gsub('_+', ' ', temp, perl=T)
    temp = unlist(strsplit(temp, " "))
    
    temp = gsub("[\\.\\!\\?\\d\\%\\-\\$]", "", temp, perl=T)
    temp = temp[temp!=""]
    temp = tolower(temp)
    temp = iconv(temp)
    return (temp)
}

dbPreprocessed <- dbConnect(SQLite(), 'database/ngrams_preprocessed')
ngrams1 = data.table(dbGetQuery(dbPreprocessed, "SELECT * FROM unigrams"))
ngrams2 = data.table(dbGetQuery(dbPreprocessed, "SELECT * FROM bigrams"))
ngrams3 = data.table(dbGetQuery(dbPreprocessed, "SELECT * FROM trigrams"))
ngrams4 = data.table(dbGetQuery(dbPreprocessed, "SELECT * FROM quadgrams"))
ngrams5 = data.table(dbGetQuery(dbPreprocessed, "SELECT * FROM quinqgrams"))

#PKN("<s> I wanna be") =
# (max(count(<s> I wanna be) - d, 0))/ num (quadgrams)) + (d / num (quadgrams) * num(<s> I wanna .) * PKNLower (I wanna be)
# PKNLower (I wanna be) = 
# (max(num(. I wanna be)-d, 0) / num (quadgrams)) + (d / num (quadgrams)) * num(I wanna .) * PKNLower (wanna be)
# PKNLower (wanna be) = 
# (max(num(. wanna be)-d, 0) / num (trigrams)) + (d / num (trigrams)) * num(wanna .) * PKNLower (be)
# PKNLower (be) = (max(num(. be)-d, 0) / num (unigrams))

counter = 0

test = function()
{
    #print(getPknHigher(prepare("let me try this")))
    #print(getPknHigher(prepare("let me try")))
    #print(getPknHigher(prepare("we feel the need")))
    #print(getPknHigher(prepare("where is my")))
    #print(getPknHigher(prepare("touch this")))
    #print("I")
    #system.time(getPknHigher(prepare("I")))
    #print("<s>")
    #system.time(getPknHigher(prepare("")))
    #(max((5 - 0.5), 0) / 20100) + (0.5/20100) * 1 * 0.0001
}

getNextWords = function(sentence, num = 3)
{
    #print(sentence)
    tokens = prepare(sentence)
    nextwordsIds = data.table()
    
    #perform back off if needed amount of words was not found    
    while (nrow(nextwordsIds) < num && length(tokens) >= 1) {
        idsVector = setnames(data.table(getPknHigher(tokens)), c('id', 'pkn'))
        #if (nrow(nextwordsIds) > 0)
        #    idsVector$pkn = idsVector$pkn * (nextwordsIds[nrow(nextwordsIds),]$pkn/(10^4))
        nextwordsIds = rbind(nextwordsIds, idsVector[!(idsVector$id %in% nextwordsIds$id)])
        if (length(tokens) == 1) break;
        tokens = tokens[2:length(tokens)]
    }
    
    # back off to empty string
    if (nrow(nextwordsIds) < num) {
        idsVector = setnames(data.table(getPknHigher(prepare(""))), c('id', 'pkn'))
        nextwordsIds = rbind(nextwordsIds, idsVector[!(idsVector$id %in% nextwordsIds$id)])
    }
    nextwordsIds = nextwordsIds[1:num]  

    nextwordsIds$size = seq(from=10000, to=10, length.out=nrow(nextwordsIds))
    nextwordsIds = nextwordsIds[, list(id, size, rid = seq_along(id))]

    setkey(nextwordsIds, id)
    setkey(ngrams1, id)
    result = ngrams1[nextwordsIds]
    result = result[order(rid)]

    result
}

cacheLowerOrderNgrams = function()
{
    tableNgram2Exists = dbGetQuery(dbLower, "SELECT name FROM sqlite_master WHERE type='table' AND name='bigrams'")
    print(tableNgram2Exists)
    if (nrow(tableNgram2Exists) > 0) {
        ngrams2_lower = data.table(dbGetQuery(dbLower, "SELECT * FROM bigrams"))
    } else {
        ngrams_count = nrow(ngrams2)
        print(ngrams_count)
        ngrams2_lower = ngrams2[, list(lowerPkn = getPknLower(c(y), length(x))
        ), by = c('y')]
        
        print(nrow(ngrams2_lower))
        
        #ngrams2_lower = ngrams2_lower[, list(y, rid=seq_along(y))]
        print(head(ngrams2_lower))
        
        #print("start")
        #ngrams2_lower = ngrams2_lower[, 
        #                list(y,
        #                    lowerPkn = getPknLower(c(y), rid)
        #                ), by = c('y')
        #                ]
        #setkey(ngrams2_lower, c('y'))
        dbWriteTable(dbLower, 'bigrams', ngrams2_lower, overwrite=T)
    } 

    tableNgram3Exists = dbGetQuery(dbLower, "SELECT name FROM sqlite_master WHERE type='table' AND name='trigrams'")
    print(tableNgram3Exists)
    if (nrow(tableNgram3Exists) > 0) {
        ngrams3_lower = data.table(dbGetQuery(dbLower, "SELECT * FROM trigrams"))
    } else {
        ngrams_count = nrow(ngrams3)
        print(ngrams_count)
        ngrams3_lower = ngrams3[, list(lowerPkn = getPknLower(c(y, a), length(x), ngrams2_lower)), by = c('y', 'a')]

        print(nrow(ngrams3_lower))
        print(head(ngrams3_lower))
        
        #ngrams3_lower = ngrams3_lower[, list(y, a, rid=seq_along(y))]
        #ngrams3_lower = ngrams3_lower[, 
        #                list(
        #                    'lowerPkn' = getPknLower(c(y, a), rid, ngrams2_lower)
        #                ), by = c('y', 'a')
        #                ]
        #setkey(ngrams3_lower, c('y', 'a'))
        dbWriteTable(dbLower, 'trigrams', ngrams3_lower, overwrite=T)
    }

    tableNgram4Exists = dbGetQuery(dbLower, "SELECT name FROM sqlite_master WHERE type='table' AND name='quadgrams'")
    print(tableNgram4Exists)
    if (nrow(tableNgram4Exists) > 0) {
        ngrams4_lower = data.table(dbGetQuery(dbLower, "SELECT * FROM quadgrams"))
    } else {
        applicableset = ngrams4[, list(num = length(x)), by = c('y', 'a', 'b')]
        assign("counter", 0, envir = .GlobalEnv)
        
        downpkns = ngrams3_lower[ , , by=c('y', 'a')]
        setkey(downpkns, y, a)
        setkey(applicableset, a, b)
        applicableset = downpkns[applicableset]
        setnames(applicableset, c('a', 'b', 'pknDown', 'y', 'num'))
        
        context = ngrams3[, list(context_count = length(a)), by = c('x', 'y')]
        setnames(context, c('y', 'a', 'context_count'))
        setkey(context, y, a)
        setkey(applicableset, y, a, b)
        
        applicableset = context[applicableset]
                
        ngrams4_lower = applicableset[, list(
                lowerPkn = getPknLower(c(y, a, b), num, context_count, pknDown, ngrams2_lower, ngrams3_lower)
            ), by = c('y', 'a', 'b')]
        print(nrow(ngrams4_lower))
        print(head(ngrams4_lower))
        
        dbWriteTable(dbLower, 'quadgrams', ngrams4_lower, overwrite=T)
    }

    tableNgram5Exists = dbGetQuery(dbLower, "SELECT name FROM sqlite_master WHERE type='table' AND name='quinqgrams'")
    print(tableNgram5Exists)
    if (nrow(tableNgram5Exists)) {
        ngrams5_lower = data.table(dbGetQuery(dbLower, "SELECT * FROM quinqgrams"))
    } else {
        applicableset = ngrams5[, list(num = length(x)), by = list(y, a, b, c)]
        print(nrow(applicableset))
        assign("counter", 0, envir = .GlobalEnv)
        
        downpkns = ngrams4_lower[ , , by=list(y, a, b)]
        setkey(downpkns, y, a, b)
        setkey(applicableset, a, b, c)
        print(head(applicableset))
        applicableset = downpkns[applicableset]
        print(head(applicableset))
        setnames(applicableset, c('a', 'b', 'c', 'pknDown', 'y', 'num'))
        
        context = ngrams4[, list(context_count = length(b)), by = c('x', 'y', 'a')]
        setnames(context, c('y', 'a', 'b', 'context_count'))
        setkey(context, y, a, b)
        setkey(applicableset, y, a, b)
        
        applicableset = context[applicableset]
        print(nrow(applicableset))
        
        #applicableset = applicableset[1:10]
        
        ngrams5_lower = applicableset[, list(
            lowerPkn = getPknLower(c(y, a, b, c), num, context_count, pknDown, ngrams2_lower, ngrams3_lower, ngrams4_lower)
        ), by = c('y', 'a', 'b', 'c')]
        
        #ngrams5_lower = ngrams5[, list(
        #    lowerPkn = getPknLower(c(y, a, b, c), length(x), ngrams2_lower, ngrams3_lower, ngrams4_lower)
        #    ), by = c('y', 'a', 'b', 'c')]
        
        print(nrow(ngrams5_lower))
        print(head(ngrams5_lower))
        dbWriteTable(dbLower, 'quinqgrams', ngrams5_lower, overwrite=T)
    } 

}

getPknLower = function(tokens, main_context_count, context_count, pknDown, ngrams2_lower = NULL, ngrams3_lower=NULL, ngrams4_lower = NULL)
{
    order = length(tokens)
    
    if (get('counter') %% 100 == 0) {
        #print(order)
        #print(sprintf("current: %s order: %s", current, order))
        print(get('counter'))
        
        #current = current + 1
    }    
    assign('counter', get('counter') +1, envir = .GlobalEnv)
    
    #print(order)
    #print(current)

    #context_count = 0
    lower_pkn = 0
    ngrams_count = 0
    
    d = 0.75
    if (order == 4) {
        #main_context_count = nrow(ngrams5[ngrams5$y == tokens[1] & ngrams5$a == tokens[2] & ngrams5$b == tokens[3] & ngrams5$c == tokens[4]])
        #context_count = nrow(ngrams4[ngrams4$x == tokens[1] & ngrams4$y == tokens[2] & ngrams4$a == tokens[3]])
        lower_pkn = pknDown
        #if (!is.null(ngrams4_lower)) {
        #    lower_pkn = ngrams4_lower[ngrams4_lower$y == tokens[2] & ngrams4_lower$a == tokens[3] & ngrams4_lower$b == tokens[4], ]$lowerPkn
        #} else {
        #    lower_pkn = getPknLower(tokens[2:4])
        #}
        ngrams_count = nrow(ngrams5)
        #print(sprintf("tokens1 %s tokens2 %s tokens3 %s tokens4 %smain_context %s context_count %s lower_pkn %s pknDown %s ngrams5 %s", tokens[1], tokens[2], tokens[3], tokens[4], main_context_count, context_count, lower_pkn, pknDown, ngrams_count))
        
        
    } else if (order == 3) {
        #print('test')
        #main_context_count = nrow(ngrams4[ngrams4$y == tokens[1] & ngrams4$a == tokens[2] & ngrams4$b == tokens[3]])
        #context_count = nrow(ngrams3[ngrams3$x == tokens[1] & ngrams3$y == tokens[2]])
        lower_pkn = pknDown
        #if (!is.null(ngrams3_lower)) {
        #    lower_pkn = ngrams3_lower[ngrams3_lower$y == tokens[2] & ngrams3_lower$a == tokens[3], ]$lowerPkn
        #} else {
        #    lower_pkn = getPknLower(tokens[2:3])
        #}
        #print(sprintf("tokens1 %s tokens2 %s tokens3 %s main_context %s context_count %s lower_pkn %s pknDown %s", tokens[1], tokens[2], tokens[3], main_context_count, context_count, lower_pkn, pknDown))
        
        ngrams_count = nrow(ngrams4)
    } else if (order == 2) {
        #main_context_count = nrow(ngrams3[ngrams3$y == tokens[1] & ngrams3$a == tokens[2]])
        context_count = nrow(ngrams2[ngrams2$x == tokens[1]])
        ngrams_count = nrow(ngrams3)
        if (!is.null(ngrams2_lower)) {
            #print(head(ngrams2_lower[ngrams2_lower$y == tokens[2],]))
            lower_pkn = ngrams2_lower[ngrams2_lower$y == tokens[2], ]$lowerPkn
        } else {
            lower_pkn = getPknLower(tokens[2])
        }  
    } else if (order == 1) {
        #main_context_count = nrow(ngrams2[ngrams2$y == tokens[1]])
        context_count = nrow(ngrams1[ngrams1$x == tokens[1]])
        ngrams_count = nrow(ngrams2)
        lower_pkn = 0.0001
    }    
    result = max(main_context_count - d, 0) / ngrams_count + (d/ngrams_count) * context_count * lower_pkn
    #print(result)
    result
}

getLowerPknFromCache = function(tokens)
{
    
    
    order = length(tokens)
    if (order == 4) {
        result = ngrams5_lower[ngrams5_lower$y == tokens[1] & ngrams5_lower$a == tokens[2] & ngrams5_lower$b == tokens[3] & ngrams5_lower$c == tokens[4]]$lowerPkn
    } else if (order == 3) {
        result = ngrams4_lower[ngrams4_lower$y == tokens[1] & ngrams4_lower$a == tokens[2] & ngrams4_lower$b == tokens[3]]$lowerPkn
    } else if (order == 2) {
        result = ngrams3_lower[ngrams3_lower$y == tokens[1] & ngrams3_lower$a == tokens[2]]$lowerPkn
    } else if (order == 1) {
        result = ngrams3_lower[ngrams2_lower$y == tokens[1]]$lowerPkn
    }
    result
}

calculatePkn = function(count, d, ngrams_count, context_count, lowerPkn)
{
    if (get('counter') %% 100 == 0) {
        print(get('counter'))
    }    
    assign('counter', get('counter') +1, envir = .GlobalEnv)
    
    (max((count - d), 0) / ngrams_count) + 
        ((d/ngrams_count) * context_count * lowerPkn)
}

cacheNgrams = function()
{
    d = 0.75

    tableNgram5Exists = dbGetQuery(dbPreprocessed, "SELECT name FROM sqlite_master WHERE type='table' AND name='quinqgrams'")
    if (nrow(tableNgram5Exists) == 0) {
        ngrams_count = nrow(ngrams5)
        result = ngrams5[, list(c, count, context_count = length(c)), by = c('x', 'y', 'a', 'b')]
        
        print(nrow(result))
        assign('counter', 0, envir = .GlobalEnv)
        
        setkey(ngrams5_lower, y, a, b, c)
        setkey(result, y, a, b, c)
        result = ngrams5_lower[result]
        
        result = result[, 
                        list(
                            'pkn' = calculatePkn(count, d, ngrams_count, context_count, lowerPkn)
                        ), by = c('x', 'y', 'a', 'b', 'c')
                        ]
        dbWriteTable(dbPreprocessed, 'quinqgrams', result, overwrite=T)
    }
    
    tableNgram4Exists = dbGetQuery(dbPreprocessed, "SELECT name FROM sqlite_master WHERE type='table' AND name='quadgrams'")
    if (nrow(tableNgram4Exists) == 0) {
        ngrams_count = nrow(ngrams4)

        result = ngrams4[, list(b, count, context_count = length(b)), by = c('x', 'y', 'a')]
        print(nrow(result))
        assign('counter', 0, envir = .GlobalEnv)
        
        setkey(ngrams4_lower, y, a, b)
        setkey(result, y, a, b)
        result = ngrams4_lower[result]
        print(head(result))
        
        result = result[, 
                                list(
                                    'pkn' = calculatePkn(count, d, ngrams_count, context_count, lowerPkn)
                                ), by = c('x', 'y', 'a', 'b')
                                ]
        dbWriteTable(dbPreprocessed, 'quadgrams', result, overwrite=T)
    }
    
    tableNgram3Exists = dbGetQuery(dbPreprocessed, "SELECT name FROM sqlite_master WHERE type='table' AND name='trigrams'")
    if (nrow(tableNgram3Exists) == 0) {
        ngrams_count = nrow(ngrams3)
        
        result = ngrams3[, list(a, count, context_count = length(a)), by = c('x', 'y')]
        print(nrow(result))
        assign('counter', 0, envir = .GlobalEnv)
        print(nrow(result))
        
        setkey(ngrams3_lower, y, a)
        setkey(result, y, a)
        result = ngrams3_lower[result]
        print(head(result))

        result = result[, 
                        list(
                            'pkn' = calculatePkn(count, d, ngrams_count, context_count, lowerPkn)
                        ), by = c('x', 'y', 'a')
                        ]
        dbWriteTable(dbPreprocessed, 'trigrams', result, overwrite=T)
    } 
    
    tableNgram2Exists = dbGetQuery(dbPreprocessed, "SELECT name FROM sqlite_master WHERE type='table' AND name='bigrams'")
    if (nrow(tableNgram2Exists) == 0) {
        ngrams_count = nrow(ngrams2)

        result = ngrams2[, list(y, count, context_count = length(y)), by = list(x)]
        print(nrow(result))
        assign('counter', 0, envir = .GlobalEnv)
        print(nrow(result))
        
        setkey(ngrams2_lower, y)
        setkey(result, y)
        print(head(result))
        result = ngrams2_lower[result]
        print(head(result))

        result = result[, 
                        list(
                            'pkn' = calculatePkn(count, d, ngrams_count, context_count, lowerPkn)
                        ), by = c('x', 'y')
                        ]
        dbWriteTable(dbPreprocessed, 'bigrams', result, overwrite=T)
    }    
    
}

getPknHigher = function(tokens)
{
    #print(tokens)
    d = 0.75
    order = length(tokens)

    context_count = 0
    possible_words = 0
    ngrams_count = 0
    
    if (order == 4) {
        possible_words = ngrams5[ngrams5$x == tokens[1] & ngrams5$y == tokens[2] & ngrams5$a == tokens[3] & ngrams5$b == tokens[4]]
        #context_count = nrow(possible_words)
        #ngrams_count = nrow(ngrams4)
        #result = possible_words[, 
        #               list(
        #                   'pkn' = (max((count - d), 0) / ngrams_count) + 
        #                       ((d/ngrams_count) * context_count * getPknLower(c(y, a, b)))
        #               ), by = c('y', 'a', 'b')
        #               ]
        #print(possible_words)
        result = possible_words[order(pkn, decreasing=T), list(c, pkn)]
    } else if (order == 3) {
        possible_words = ngrams4[ngrams4$x == tokens[1] & ngrams4$y == tokens[2] & ngrams4$a == tokens[3]]
        #context_count = nrow(possible_words)
        #ngrams_count = nrow(ngrams4)
        #result = possible_words[, 
        #               list(
        #                   'pkn' = (max((count - d), 0) / ngrams_count) + 
        #                       ((d/ngrams_count) * context_count * getPknLower(c(y, a, b)))
        #               ), by = c('y', 'a', 'b')
        #               ]
        result = possible_words[order(pkn, decreasing=T), list(b, pkn)]
    } else if (order == 2) {
        possible_words = ngrams3[ngrams3$x == tokens[1] & ngrams3$y == tokens[2]]
        #context_count = nrow(possible_words)
        #ngrams_count = nrow(ngrams3)
        #result = possible_words[,
        #               list(
        #                   'pkn' = (max((count - d), 0) / ngrams_count) + 
        #                       ((d/ngrams_count) * context_count * getPknLower(c(y, a)))
        #               ), by = c('y', 'a')
        #               ]
        result = possible_words[order(pkn, decreasing=T), list(a, pkn)]
    } else if (order == 1) {
        possible_words = ngrams2[ngrams2$x == tokens[1]]
        #context_count = nrow(possible_words)
        #ngrams_count = nrow(ngrams2)
        #result = possible_words[, 
        #               list(
        #                   'pkn' = (max((count - d), 0) / ngrams_count) + 
        #                       ((d/ngrams_count) * context_count * getPknLower(c(y)))
        #               ), by = c('y')
        #               ]
        result = possible_words[order(pkn, decreasing=T), list(y, pkn)]
    }
    
    result
}




        
prepare = function(string)
{
    tkn = tokenize(string, F)
    tkn = data.table(tkn[((length(tkn)-min(4, length(tkn))) + 1):length(tkn)])
    tkn = tkn[, list(V1, rid = seq_along(V1))]

    result = ngrams1[ngrams1$V1 %in% tkn$V1]
    setkey(tkn, V1)
    setkey(result, V1)
    result = result[J(tkn)]
    result = result[order(rid)]$id
    result
}




getPKNRecursive = function(tokens)
{
    
    # 1. cut last three words
    # 2. find all possible next words in bigrams
    # 3 for every next word run PKNRecursive
    # 4 PKNRecursive calculates highest and then run lowerPKN
    # 5 lowerPKN stops on unigram
    
    #for (i in (length(tokens)-min(3, length(tokens)) + 1):length(tokens)) {
        
        #if (i == 3) {
    tokens = tokens[(length(tokens)-min(3, length(tokens)) + 1):length(tokens), id]
    tokensData = unigrams[unigrams$V1 %in% tokens]
            print(tokensData)
            #yAfterX = subset(quadgrams, x == identifier, select=c('y', 'count'))
        #}
        
        print(tokens)
    #}
    
    #unigrams[unigrams$V1 == tokens, list(id, count)]
    
    #quadgram[]
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
    print(identifier)
    
    contextCount = un$count
    setkey(bigrams, x, y)
    
    yAfterX = subset(bigrams, x == identifier, select=c('y', 'count'))
    print(bigrams[bigrams$x == identifier & bigrams$y == identifier])
    #print(bigrams[is.na(bigrams$y)])
    #print(yAfterX[is.na(yAfterX$y)])

    possibleTokensCount = nrow(yAfterX)
    LAMBDA = (d / contextCount) * possibleTokensCount
    
    testplan = bigrams[bigrams$y %in% yAfterX$y, list(num = length(count)), by=c('y')]
    
    
    plan = merge(bigrams[bigrams$y %in% yAfterX$y, list(num = length(count)), by=c('y')], yAfterX, by=('y'))
    
    plan = plan[, list(y = paste0(y, collapse=','),  
                       PKN=getPKN(num[1], bigramsCount, count, contextCount[1], LAMBDA, d)), by=c('count', 'num')]
    
    nextWordsIds = unlist(strsplit(plan[plan$PKN == max(plan$PKN), y], ","))
    #print(plan[plan$y == "NA"])
    print(plan[plan$PKN == max(plan$PKN)])
    print(max(plan$PKN))
    nextWords = unigrams[unigrams$id %in% nextWordsIds, ]$V1
    nextWords
}








bestNextWord = function(tokens, ngramsDbName)
{
    ngramsDb <- dbConnect(SQLite(), ngramsDbName)
    bigramsCount = dbGetQuery(ngramsDb, "SELECT count(*) as num FROM bigrams")$num
    #print(bigramsCount)
    d = 0.5
    if (length(tokens) == 1) {
        
        id = dbGetQuery(ngramsDb, sprintf("SELECT id, V1 FROM unigrams WHERE V1 = \"%s\"", tokens[1]))$id
        #print(id)
        query = sprintf("SELECT count FROM unigrams WHERE id = %s", id)
        #print(query)
        contextCount = dbGetQuery(ngramsDb, query)$count
        
        query = sprintf("SELECT y, count FROM bigrams WHERE x = %s", id)
        #print(query)
        yAfterX = dbGetQuery(ngramsDb, query)
        print(nrow(yAfterX))
        #nextPossibleTokens = $y
        
        #query = sprintf("SELECT id, V1 FROM unigrams WHERE id in (%s)", paste0(nextPossibleTokens, collapse=', '))
        #print(query)
        #nextTokens = dbGetQuery(ngramsDb, query)
        #print(nextTokens)
        
        result = data.table()
        
        query = sprintf("SELECT y, COUNT(*) as num FROM bigrams WHERE y in (%s) GROUP BY y", paste0(yAfterX$y, collapse=', '))
        #print(query)
        contexts = dbGetQuery(ngramsDb, query)
        print(nrow(contexts))
        #print(contexts[contexts$y %in% c(11951764, 11952923, 11949881, 11918466, 11939105, 11952302), ])
        
        singularContexts = contexts[contexts$num==1,]$y
        possibleTokensCount = nrow(yAfterX)
        singularPKN = getPKN(1, bigramsCount, 1, contextCount, possibleTokensCount, d)
        #for (i in 1:length(singularContexts)) {
        #    result = rbind(result, list('token' = singularContexts[i], 'pkn' = singularPKN))
        #}   
        #print(singularPKN)
        #result = rbind(result, list('token' = 0, 'pkn' = singularPKN))
        
        plan = merge(contexts, yAfterX)
        plan = aggregate(y~count+num, data=plan, FUN=function(x) {
            paste0(x)
        })
        
        
        
        #print(head(plan))
        #PKN = vector(mode = "numeric", length=nrow(plan))
        PKNs = data.table(y=character(), PKN=numeric(0), num=numeric(0), count=numeric(0))
        print(nrow(plan))
        for (i in 1:nrow(plan)) {
            currentPlan = plan[i,]
            #print(currentPlan)
            #print(bigramsCount)
            #print(contextCount)
            #print(possibleTokensCount)
            PKN = getPKN(currentPlan$num, bigramsCount, currentPlan$count, contextCount, possibleTokensCount, d)
            #print(PKN)
            PKNs = rbind(PKNs, list(currentPlan$y, PKN, currentPlan$num, currentPlan$count))
        }
        maxPkn = max(PKNs$PKN)
        print(unlist(PKNs[PKNs$PKN==max(PKNs$PKN),]$y))
        nextWordsIds = unlist(strsplit(unlist(PKNs[PKNs$PKN==max(PKNs$PKN),]$y), ","))
        nextWords = dbGetQuery(ngramsDb, sprintf("SELECT V1 FROM unigrams WHERE id in (%s)", nextWordsIds))$V1
        nextWords
        
        #nonSingularContexts = contexts[contexts$num!=1,]
        #plan = matrix(nrow = nrow(yAfterX), ncol=2)
        #for (i in 1:nrow(yAfterX)) {
        #    print(sprintf("%s out of %s", i, nrow(yAfterX)))
        
        #query = sprintf("SELECT SUM(count) as num FROM bigrams WHERE y = %s", nextPossibleTokens[i])
        #numContexts = dbGetQuery(ngramsDb, query)$num
        #    numContexts = contexts[contexts$y==yAfterX[i,]$y,]$num
        #print(numContexts);
        
        #query = sprintf("SELECT count FROM bigrams WHERE x = %s AND y = %s", id, nextPossibleTokens[i])
        #count = dbGetQuery(ngramsDb, query)$count
        #    count = yAfterX[i,]$count
        #print(count)
        
        #PKN = getPKN(numContexts, bigramsCount, count, contextCount, possibleTokensCount, d)
        #print(PKN)
        #    plan[i,1] = numContexts
        #    plan[i,2] = count
        #plan = rbind(plan, list('numContexts' = numContexts, 'count' = count))
        #}
        #print(plan)
        #colnames(result) = c('token', 'pkn')
        #tokenId = result[result$pkn==max(result$pkn),]$token[1]
        #print(tokenId)
        
        #nextTokens[nextTokens$id==tokenId,]$V1
    }
}
    