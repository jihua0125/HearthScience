setwd("~/Desktop/hearthstone/HearthScience/")
library("rjson")
library("dplyr")
json_file = "cards2.txt"
data <- fromJSON(file = json_file)
card_category = names(data)
not_empty = which(sapply(1:length(data), function(i){length(data[[i]])})>0)

card_category = card_category[not_empty]

data = lapply(not_empty, function(i){data[[i]]})
data1 = lapply(1:length(data), function(k) {lapply(data[[k]],
                                                   function(i) {lapply(i, function(j){
                                                     j = ifelse(is.null(j),NA,j)})})})

col_names = lapply(1:length(data1),
                   function(k) {
                     lapply(1:length(data1[[k]]),
                            function(i) {names(data1[[k]][[i]])})})

data2 = lapply(1:length(data1), 
           function(k) {
             lapply(1:length(data1[[k]]),
                    function(i) {
                      matrix(unlist(data1[[k]][[i]]), 
                             ncol = length(data1[[k]][[i]]), 
                             byrow = T)})})

for(k in 1:length(data2)){
  colnames(data2[[k]][[1]]) = col_names[[k]][[1]]
  data2[[k]][[1]] = data.frame(data2[[k]][[1]])
  for(i in 2:length(data2[[k]])){
    colnames(data2[[k]][[i]]) = col_names[[k]][[i]]
    data2[[k]][[i]] = data.frame(data2[[k]][[i]])
    data2[[k]][[i]] = bind_rows(data2[[k]][[i-1]],data2[[k]][[i]])
  }
  # assign(card_category[k], tbl_df(data2[[k]][[length(data2[[k]])]]))
}

final_data = card_category[1]
for (i in 2:length(data2)){
  final_data = bind_rows(get(final_data), get(card_category[i]))
}
write.csv(final_data, file = "final_data.csv")
# write.csv(Basic, file = "Basic.csv")
# write.csv(`Blackrock Mountain`, file = "Blackrock_Mountain.csv")
# write.csv(Classic, file = "Classic.csv")
# write.csv(`Goblins vs Gnomes`, file = "Goblins_Gnomes.csv")
# write.csv(`Hero Skins`, file = "Hero_skins.csv")
# write.csv(Naxxramas, file = "Naxxramas.csv")
# write.csv(Promotion, file = "Promotion.csv")
# write.csv(`The Grand Tournament`, file = "The_grand_tournament.csv")
# write.csv(`The League of Explorers`, file = "The_league_of_explorers.csv")
# write.csv(Reward, file = "Reward.csv")

