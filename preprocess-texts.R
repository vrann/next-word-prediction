source('ngram.R')
library(RSQLite)
library(data.table)

getSample = function(factor = 0.1) {
    library(data.table)
    twitter = readLines('en_US/en_US.twitter.txt')
    length(twitter)
    blog = readLines('en_US/en_US.blogs.txt')
    length(blog)
    news = readLines('en_US/en_US.news.txt')
    length(news)
    document = append(append(sample(twitter, factor*length(twitter)), sample(news, factor*length(news))), sample(blog, factor*length(blog)))
}

loadData = function()
{
    data.table(tokenize(getSample()))
}

createDatabase = function(tokens, tokensDbName, ngramsDbName)
{
    library(RSQLite)
    tokensDB <- dbConnect(SQLite(), tokensDbName)
    
    dbWriteTable(tokensDB, 'tokens', tokens, overwrite=T)
    dbGetQuery(tokensDB, "ALTER TABLE tokens ADD COLUMN count INTEGER DEFAULT 1")
    
    #tokens$count = rep(1, nrow(tokens))
    #unigrams = tokens[, list(count=sum(count)), by=c('V1')]
    #unigrams[, list(id = seq_along(V1))]
    #tokens = merge(tokens, unigrams, by=c('V1'))
    #print(head(tokens))
    
    dbGetQuery(tokensDB, "CREATE TABLE tokens_count (id INTEGER, V1 VARCHAR , count INTEGER)")
    dbGetQuery(tokensDB, "INSERT INTO tokens_count SELECT ROWID, V1, SUM(count) FROM tokens GROUP BY V1")
    data  = dbGetQuery(tokensDB, "SELECT * FROM tokens_count")

    ngramsDB <- dbConnect(SQLite(), ngramsDbName)
    dbWriteTable(ngramsDB, 'unigrams', data, overwrite=T)

    tokensIndexed = dbGetQuery(tokensDB, 
        "SELECT tokens.V1, tokens_count.count, tokens_count.id FROM tokens LEFT JOIN tokens_count ON tokens.V1 = tokens_count.V1")
    dbWriteTable(tokensDB, 'tokens_indexed', tokensIndexed, overwrite=T)

    indexSequence = dbGetQuery(tokensDB, "SELECT id, V1  FROM tokens_indexed")
    stopword = indexSequence[indexSequence$V1 == '</s>', ]$id[1]
    createNgrams(ngramsDB, indexSequence$id, stopword)
    
    #bigrams = dbGetQuery(tokensDB, "SELECT * FROM bigrams")
    
    #dbGetQuery(tokensDB, "ALTER TABLE bigrams ADD COLUMN count INTEGER DEFAULT 1")
    #dbGetQuery(tokensDB, "CREATE TABLE bigrams_count (id INTEGER, x INTEGER, y INTEGER, count INTEGER)")
    #dbGetQuery(tokensDB, "INSERT INTO bigrams_count SELECT ROWID, x, y, SUM(count) FROM bigrams GROUP BY x, y")
    #data  = dbGetQuery(tokensDB, "SELECT * FROM bigrams_count")
    #dbWriteTable(ngramsDB, 'bigrams', data, overwrite=T)
    #dbGetQuery(ngramsDB, "CREATE INDEX bigrams_index_x ON bigrams (x)")
    #dbGetQuery(ngramsDB, "CREATE INDEX bigrams_index_y ON bigrams (y)")
    
    #trigrams = dbGetQuery(tokensDB, "SELECT * FROM trigrams")
    #dbGetQuery(tokensDB, "ALTER TABLE trigrams ADD COLUMN count INTEGER DEFAULT 1")
    #dbGetQuery(tokensDB, "CREATE TABLE trigrams_count (id INTEGER, x INTEGER, y INTEGER, a INTEGER, count INTEGER)")
    #dbGetQuery(tokensDB, "INSERT INTO trigrams_count SELECT ROWID, x, y, a, SUM(count) FROM trigrams GROUP BY x, y, a")
    #data  = dbGetQuery(tokensDB, "SELECT * FROM trigrams_count")
    #dbWriteTable(ngramsDB, 'trigrams', data, overwrite=T)
    #dbGetQuery(ngramsDB, "CREATE INDEX trigrams_index_x ON trigrams (x)")
    #dbGetQuery(ngramsDB, "CREATE INDEX trigrams_index_y ON trigrams (y)")
    #dbGetQuery(ngramsDB, "CREATE INDEX trigrams_index_a ON trigrams (a)")
    
    #bigrams = dbGetQuery(tokensDB, "SELECT * FROM quadgrams")
    #dbGetQuery(tokensDB, "ALTER TABLE quadgrams ADD COLUMN count INTEGER DEFAULT 1")
    #dbGetQuery(tokensDB, "CREATE TABLE quadgrams_count (id INTEGER, x INTEGER, y INTEGER, a INTEGER, b INTEGER, count INTEGER)")
    #dbGetQuery(tokensDB, "INSERT INTO quadgrams_count SELECT ROWID, x, y, a, b, SUM(count) FROM quadgrams GROUP BY x, y, a, b")
    #data  = dbGetQuery(tokensDB, "SELECT * FROM quadgrams_count")
    #dbWriteTable(ngramsDB, 'quadgrams', data, overwrite=T)
    #dbGetQuery(ngramsDB, "CREATE INDEX quadgrams_index_x ON quadgrams (x)")
    #dbGetQuery(ngramsDB, "CREATE INDEX quadgrams_index_y ON quadgrams (y)")
    #dbGetQuery(ngramsDB, "CREATE INDEX quadgrams_index_a ON quadgrams (a)")
    #dbGetQuery(ngramsDB, "CREATE INDEX quadgrams_index_b ON quadgrams (b)")
    
    dbDisconnect(tokensDB)
    dbDisconnect(ngramsDB)
}

clearDatabase = function(ngramsDbName)
{
    db <- dbConnect(SQLite(), ngramsDbName)
    ids = dbGetQuery(db, "SELECT id FROM unigrams WHERE count=1")
    ids_string = paste0(ids$id, collapse=', ')
    dbGetQuery(db, 
               sprintf("DELETE FROM quadgrams WHERE (x in (%s)) OR (y in (%s)) OR (a in (%s)) or (b in (%s))", 
                       ids_string, ids_string, ids_string, ids_string))
    dbGetQuery(db, 
               sprintf("DELETE FROM trigrams WHERE (x in (%s)) OR (y in (%s)) OR (a in (%s))", 
                       ids_string, ids_string, ids_string))
    dbGetQuery(db, 
               sprintf("DELETE FROM bigrams WHERE (x in (%s)) OR (y in (%s))", 
                       ids_string, ids_string))
    dbGetQuery(db, 
               sprintf("DELETE FROM unigrams WHERE (id in (%s))", ids_string))   
}

copyTables  = function(fromDbName, toDbName) 
{
    fromDb <- dbConnect(SQLite(), fromDbName)
    toDb <- dbConnect(SQLite(), toDbName)
    
    data  = dbGetQuery(fromDb, "SELECT * FROM unigrams")
    dbWriteTable(toDb, 'unigrams', data, overwrite=T)
    
    data  = dbGetQuery(fromDb, "SELECT * FROM bigrams")
    dbWriteTable(toDb, 'bigrams', data, overwrite=T)    
    dbGetQuery(toDb, "CREATE INDEX bigrams_index_x ON bigrams (x)")
    dbGetQuery(toDb, "CREATE INDEX bigrams_index_y ON bigrams (y)")
    
    data  = dbGetQuery(fromDb, "SELECT * FROM trigrams")
    dbWriteTable(toDb, 'trigrams', data, overwrite=T)    
    dbGetQuery(toDb, "CREATE INDEX trigrams_index_x ON trigrams (x)")
    dbGetQuery(toDb, "CREATE INDEX trigrams_index_y ON trigrams (y)")
    dbGetQuery(toDb, "CREATE INDEX trigrams_index_a ON trigrams (a)")
    
    data  = dbGetQuery(fromDb, "SELECT * FROM quadgrams")
    dbWriteTable(toDb, 'quadgrams', data, overwrite=T)    
    dbGetQuery(toDb, "CREATE INDEX quadgrams_index_x ON quadgrams (x)")
    dbGetQuery(toDb, "CREATE INDEX quadgrams_index_y ON quadgrams (y)")
    dbGetQuery(toDb, "CREATE INDEX quadgrams_index_a ON quadgrams (a)")
    dbGetQuery(toDb, "CREATE INDEX quadgrams_index_b ON quadgrams (b)")
}

copyTablesNotIndexed = function(fromDbName, toDbName) 
{
    fromDb <- dbConnect(SQLite(), fromDbName)
    toDb <- dbConnect(SQLite(), toDbName)
    
    data  = dbGetQuery(fromDb, "SELECT * FROM unigrams")
    dbWriteTable(toDb, 'unigrams', data, overwrite=T)
    
    data  = dbGetQuery(fromDb, "SELECT * FROM bigrams")
    dbWriteTable(toDb, 'bigrams', data, overwrite=T)    
    
    data  = dbGetQuery(fromDb, "SELECT * FROM trigrams")
    dbWriteTable(toDb, 'trigrams', data, overwrite=T)    
    
    data  = dbGetQuery(fromDb, "SELECT * FROM quadgrams")
    dbWriteTable(toDb, 'quadgrams', data, overwrite=T)    
}


createNgrams = function(dbConnection, indexSequence, stopword) {
    print(stopword)
    bigrams = matrix(nrow = (length(indexSequence)-1), ncol=2)
    trigrams = matrix(nrow = (length(indexSequence)-2), ncol=3)
    quadgrams = matrix(nrow = (length(indexSequence)-3), ncol=4)
    quinqgrams = matrix(nrow = (length(indexSequence)-4), ncol=5)
    for (i in 1:(length(indexSequence)-4)) {
        if (indexSequence[i] == stopword)
            next
        quinqgrams[i, 1] = quadgrams[i,1] = trigrams[i,1] = bigrams[i,1] = indexSequence[i]
        if (indexSequence[i+1] == stopword)
            next
        quinqgrams[i, 2] = quadgrams[i,2] = trigrams[i,2] = bigrams[i,2] = indexSequence[i+1]
        if (indexSequence[i+2] == stopword)
            next
        quinqgrams[i, 3] = quadgrams[i,3] = trigrams[i,3] = indexSequence[i+2]
        if (indexSequence[i+3] == stopword)
            next
        quinqgrams[i, 4] = quadgrams[i,4] = indexSequence[i+3]
        if (indexSequence[i+4] == stopword)
            next
        quinqgrams[i,5] = indexSequence[i+4]
        if (i %% 10000 == 0) {
            print(i)
        }
    }
    
    bigrams_table = data.table(bigrams)
    setnames(bigrams_table, c('x', 'y'))
    bigrams_table = bigrams_table[!(is.na(bigrams_table$x))]
    bigrams_table = bigrams_table[!(is.na(bigrams_table$y))]                              
                                  
    print(head(bigrams_table))
    print(bigrams_table[(is.na(bigrams_table$x)) | (is.na(bigrams_table$y))])
    
    bigrams_table$count = rep(1, nrow(bigrams_table))
    bigrams_table = bigrams_table[, list(count=sum(count)), by=c('x', 'y')]
    bigrams_table = bigrams_table[bigrams_table$count > 1]

    dbWriteTable(dbConnection, 'bigrams', bigrams_table, overwrite=T)

    
    trigrams_table = data.table(trigrams)
    setnames(trigrams_table, c('x', 'y', 'a'))
    trigrams_table = trigrams_table[!(is.na(trigrams_table$x))]
    trigrams_table = trigrams_table[!(is.na(trigrams_table$y))]
    trigrams_table = trigrams_table[!(is.na(trigrams_table$a))]
    
    trigrams_table$count = rep(1, nrow(trigrams_table))
    trigrams_table = trigrams_table[, list(count=sum(count)), by=c('x', 'y', 'a')]
    trigrams_table = trigrams_table[trigrams_table$count > 1]

    dbWriteTable(dbConnection, 'trigrams', trigrams_table, overwrite=T)
    
    quadgrams_table = data.table(quadgrams)
    setnames(quadgrams_table, c('x', 'y', 'a', 'b'))
    quadgrams_table = quadgrams_table[!(is.na(quadgrams_table$x))]
    quadgrams_table = quadgrams_table[!(is.na(quadgrams_table$y))]
    quadgrams_table = quadgrams_table[!(is.na(quadgrams_table$a))]
    quadgrams_table = quadgrams_table[!(is.na(quadgrams_table$b))]

    quadgrams_table$count = rep(1, nrow(quadgrams_table))
    quadgrams_table = quadgrams_table[, list(count=sum(count)), by=c('x', 'y', 'a', 'b')]
    quadgrams_table = quadgrams_table[quadgrams_table$count > 1]

    dbWriteTable(dbConnection, 'quadgrams', quadgrams_table, overwrite=T)
    
    quinqgrams_table = data.table(quinqgrams)
    setnames(quinqgrams_table, c('x', 'y', 'a', 'b', 'c'))
    quinqgrams_table = quinqgrams_table[!(is.na(quinqgrams_table$x))]
    quinqgrams_table = quinqgrams_table[!(is.na(quinqgrams_table$y))]
    quinqgrams_table = quinqgrams_table[!(is.na(quinqgrams_table$a))]
    quinqgrams_table = quinqgrams_table[!(is.na(quinqgrams_table$b))]
    quinqgrams_table = quinqgrams_table[!(is.na(quinqgrams_table$c))]
    
    quinqgrams_table$count = rep(1, nrow(quinqgrams_table))
    quinqgrams_table = quinqgrams_table[, list(count=sum(count)), by=c('x', 'y', 'a', 'b', 'c')]
    quinqgrams_table = quinqgrams_table[quinqgrams_table$count > 1]

    dbWriteTable(dbConnection, 'quinqgrams', quinqgrams_table, overwrite=T)
}