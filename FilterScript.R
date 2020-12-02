# Title     : TODO
# Objective : TODO
# Created by: Scott Kersh
# Created on: 11/21/2020

should_load_csv <- TRUE
should_save_rdat <- FALSE
should_load_rdat <- FALSE
should_filter_lists <- TRUE
should_calc_mutual <- TRUE


if(should_load_csv){
  print("Loading Lists from CSV")
  anime_list <- read.csv("AnimeList.csv")
  user_list <- read.csv("UserList.csv")
  ual_cols <- c(NA, NA, "NULL", "NULL", "NULL", "NULL", NA, "NULL", "NULL", "NULL", "NULL")
  time <- system.time(user_anime_list <- read.csv("animelists_filtered.csv", colClasses = ual_cols))
  #time <- system.time(user_anime_list <- read.csv("UserAnimeList.csv", colClasses = ual_cols))
  #print(time)
}

if(should_save_rdat){
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
  # Only select the 100 most popular anime to speed up computation
  anime_list_filtered <- anime_list[anime_list$popularity <= 100,]
  anime_list_filtered <- anime_list_filtered[anime_list_filtered$members > 1000,]
  anime_list_filtered <- anime_list_filtered["anime_id"]

  # Get a sample of 100,000 users to speed up computation
  user_list_filtered <- user_list
  user_list_filtered <- user_list[sample(nrow(user_list), 100000, replace=TRUE),]

  user_anime_list_filtered <- user_anime_list
  print(length(user_anime_list_filtered$username))
  # Select the lines from our sampled users
  user_anime_list_filtered <- user_anime_list_filtered[user_anime_list_filtered$username %in% user_list_filtered$username,]
  # Select only the anime that meet our popularity requrements
  user_anime_list_filtered <- user_anime_list[user_anime_list$anime_id %in% anime_list_filtered$anime_id,]
  # Only select anime that have been set to completed
  user_anime_list_filtered <- user_anime_list_filtered[user_anime_list_filtered$my_status == 2,]
  print(length(user_anime_list_filtered$username))
}

# Get the distance between two anime by determining how their userbases corrolate, uses cosine simularity.
distance <- function (id1, id2) {
  mutual <- user_anime_list_filtered[user_anime_list_filtered$anime_id == id1 | user_anime_list_filtered$anime_id == id2,]
  combined <- sum(duplicated(mutual$username))
  unique_1 <- nrow(mutual[user_anime_list_filtered$anime_id == id1,])
  unique_2 <- nrow(mutual[user_anime_list_filtered$anime_id == id2,])
  distance <- combined / (sqrt(unique_1) * sqrt(unique_2)) # Cosine Distance

  # One Piece is broken for some reason still diagnosing
  if(is.nan(distance)) {
    distance <- 0
  } else if(anime_list[anime_list$anime_id == id1,]$title_english == "One Piece" | anime_list[anime_list$anime_id == id2,]$title_english == "One Piece" ) {
    print(mutual[user_anime_list_filtered$anime_id == id1,])
    cat("\nTotal Rows", nrow(mutual))
    cat("\nTotal Intersection", combined)
    cat("\nNumber 1", anime_list[anime_list$anime_id == id1,]$title_english, unique_1)
    cat("\nNumber 2", anime_list[anime_list$anime_id == id2,]$title_english, unique_2)
    cat("\nDistance:", distance)
    cat("\n")
  }
  return(distance)
}

# Calculates the distance between each show and stores them in a dendogram
if(should_calc_mutual){
  print("Calculating mutual distance")
  mat <- matrix(0, nrow = length(anime_list_filtered$anime_id), ncol = length(anime_list_filtered$anime_id))
  rownames(mat) <- replicate(length(anime_list_filtered$anime_id), 0)
  colnames(mat) <- replicate(length(anime_list_filtered$anime_id), 0)

  n <- length(anime_list_filtered$anime_id)*2
  distance_frame <- data.frame(show_1=character(n),
                               show_2=character(n),
                               distance=numeric(n),
                               stringsAsFactors=FALSE)

  k <- 1
  for (i in 2:length(anime_list_filtered$anime_id)-1) {
    for(j in (i+1):length(anime_list_filtered$anime_id)) {
      dist <- 1 - distance(anime_list_filtered$anime_id[i], anime_list_filtered$anime_id[j])
      mat[j, i] <- dist
      mat[i, j] <- dist

      rownames(mat)[j] <- anime_list[anime_list$anime_id == anime_list_filtered$anime_id[j],]$title_english
      colnames(mat)[j] <- anime_list[anime_list$anime_id == anime_list_filtered$anime_id[j],]$title_english
      rownames(mat)[i] <- anime_list[anime_list$anime_id == anime_list_filtered$anime_id[i],]$title_english
      colnames(mat)[i] <- anime_list[anime_list$anime_id == anime_list_filtered$anime_id[i],]$title_english

      distance_frame[k,] <- c(anime_list[anime_list$anime_id == anime_list_filtered$anime_id[i],]$title_english,
                                                     anime_list[anime_list$anime_id == anime_list_filtered$anime_id[j],]$title_english,
                                                     dist)
      k <- k + 1
      distance_frame[k,] <- c(anime_list[anime_list$anime_id == anime_list_filtered$anime_id[j],]$title_english,
                                               anime_list[anime_list$anime_id == anime_list_filtered$anime_id[i],]$title_english,
                                               dist)
      k <- k + 1
    }
  }
}

# Make clusters using built in R for testing!
# print(mat)
test <- as.dist(mat)
hc1 <- hclust(test, method = "complete")
plot(hc1)

print("Printing!")
write.csv(distance_frame, "anime_distance_100.csv", row.names = FALSE)