#!/usr/bin/env r

problem22 <- function(){
        t <- sort(gsub("\"","",strsplit(readLines("names.txt"), ",")[[1]]))
        cat(sum(sapply(1:length(t),function(x){ x * sum(utf8ToInt(t[x]) - 64) })),"\n")
}

problem22()
