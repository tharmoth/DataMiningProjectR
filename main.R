# Title     : DataMining PreProcessing
# Objective : Exploring Dataset
# Created by: Tsuyu
# Created on: 10/15/2020

should_install <- FALSE
should_load_csv <- FALSE
should_save_csv <- FALSE
should_load_rdat <- FALSE
should_filter_lists <- TRUE
should_build_watchlist <- TRUE
should_fill_watchlist <- TRUE
should_build_new_watchlist <-  FALSE
should_build_rules <- FALSE
should_build_clusters <- TRUE

set.seed(321)

if(should_install){
  library(tidyverse)  # data manipulation
  library(cluster)    # clustering algorithms
  library(factoextra) # clustering algorithms & visualization
}

if(should_load_csv){
  print("Loading Lists from CSV")
  anime_list <- read.csv("AnimeList.csv")
  user_list <- read.csv("UserList.csv")
  ual_cols <- c(NA, NA, "NULL", "NULL", "NULL", "NULL", NA, "NULL", "NULL", "NULL", "NULL")
  time <- system.time(user_anime_list <- read.csv("animelists_filtered.csv", colClasses = ual_cols))
  #time <- system.time(user_anime_list <- read.csv("UserAnimeList.csv", colClasses = ual_cols))
  print(time)
}

if(should_save_csv){
  print("Saving Lists to RDA")
  save(user_anime_list, file="user_anime_list.Rda")
}

if(should_load_rdat){
  print("Loading Lists from RDA")
  time <- system.time(load("user_anime_list.Rda"))
  print(time)
}

if(should_filter_lists) {
  print("Filtering lists")
  anime_list_filtered <- anime_list[anime_list$popularity <= 10,]
  anime_list_filtered <- anime_list_filtered[anime_list_filtered$members > 1000,]
  anime_list_filtered <- anime_list_filtered["anime_id"]

  user_anime_list_filtered <- user_anime_list
  print(length(user_anime_list_filtered$username))
  user_anime_list_filtered <- user_anime_list[user_anime_list$anime_id %in% anime_list_filtered$anime_id,]
  user_anime_list_filtered <- user_anime_list_filtered[user_anime_list_filtered$my_status == 2,]
  print(length(user_anime_list_filtered$username))
  #user_anime_list_filtered <- user_anime_list_filtered[sample(nrow(user_anime_list_filtered), 10000, replace=TRUE),]
  user_anime_list_filtered <- user_anime_list_filtered[100000:200000,]
}

if(should_build_watchlist) {
  print("Building watchlist")
  watch_list <- data.frame("username" = unique(unlist(user_anime_list_filtered$username)))
  for (show in anime_list_filtered$anime_id){
    print(show)
    watch_list[[toString(show)]] <- FALSE
  }
}

if(should_fill_watchlist) {
  print("Filling watchlist")
  time <- system.time(
    for (i in 1:nrow(user_anime_list_filtered)){
      username <- user_anime_list_filtered[i,]$username
      anime_id <- user_anime_list_filtered[i,]$anime_id
      index <- match(anime_id, names(watch_list))

      watch_list[watch_list$username == username, index] <- TRUE

      if (i %% 10000 == 0) {
        print(i / nrow(user_anime_list_filtered) * 100)
      }
    }
  )
  print(time)
  str(watch_list)
}

if(should_build_new_watchlist){
  watch_list_new <- list()
  i <- 1
  for(user in unique(unlist(user_anime_list_filtered$username))) {
    #new_names <- list()
    #for(anime in user_anime_list_filtered[user_anime_list_filtered$username == user,]$anime_id) {
    #  new_names <- c(new_names, anime_list[anime_list$anime_id == anime,]$title)
    #}
    watch_list_new[[user]] <- user_anime_list_filtered[user_anime_list_filtered$username == user,]$anime_id
    i <- i + 1
  }
}

#if(should_build_rules) {
#  watch_list_new_sample <- watch_list_new[sample(length(watch_list_new), 100, replace=TRUE)]
#  ## coerce into transactions
#  trans1 <- as(watch_list_new_sample, "transactions")
#
#  ## analyze transactions
#  summary(trans1)
#  image(trans1)
#
#  grocery_rules <- apriori(trans1, parameter = list(support = 0.01, confidence = 0.5))
#  inspect(head(sort(grocery_rules, by = "confidence"), 10))
#  plot(grocery_rules)
#}

if(should_build_clusters){
  watch_list_named <- watch_list
  #new_names <- list()
  #for (i in names(watch_list)){
  # # print(i)
  #  if(i == "username") {
  #    new_names <- c(new_names, "username")
  #  } else {
  #    new_names <- c(new_names, anime_list[anime_list$anime_id == i,]$title)
  #  }
  #
  #}
  for (show in watch_list_named[,-1]) {
    print(i)
    print(length(watch_list_named[[i]] == TRUE))
    print(length(watch_list_named[[i]] == FALSE))
  }


  names(watch_list_named) <- new_names
  watch_list_named <- watch_list_named[sample(nrow(watch_list_named), 100, replace=TRUE),]
 # row.names(watch_list_named) <- watch_list_named$username

  k <- kmeans(watch_list_named[,-1], centers = 5, nstart = 3)
  print(k)
  fviz_cluster(k, data = watch_list_named[,-1])
}