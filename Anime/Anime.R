#*****************************************************************
#*****************************************************************
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(ggcorrplot))
suppressPackageStartupMessages(library(dplyr))
library(splitstackshape)
library(gridExtra)
library(Matrix)
library(tidyverse)

dataset = data.frame(anime[c(-2)])

sum( is.na( dataset)) #[count no. of missing values]
dataset = na.omit(dataset)

glimpse(dataset)
# summary(dataset)

#*****************************************************************
#*****************************************************************

#*** GRAPHICAL ANALYSIS ***
# Visualize the relationship between the Episodes and Rating
gs <- ggplot( dataset ,aes(y = log(dataset$episodes) ,x = dataset$rating,color =rating, fill = rating)) # SETUP
gl <- gs + geom_count()+geom_smooth() # Adding Scatterplot
glb <- gl + labs(title ='ANIME ', y='LOGARITHM OF EPISODES', x='RATING');glb # Labeling



# Correlation
ggcorrplot(cor(dataset[c(-2,-3)]),method = 'circle',hc.order = T,
           type ='lower',colors = c('darkred','grey','cyan'),title = "CORRELATION MATRIX") #plotting


#*****************************************************************
#*****************************************************************

# *****ANIME TYPE  ANALYSIS*****

# Visualize the relationship between the Type and Episode

gs <- ggplot( dataset ,aes(x = dataset$type ,y = log(dataset$episodes),color =type, fill = type)) # SETUP
gl <- gs + geom_bar(stat = "identity") # Adding Barplot
glb <- gl + labs(title ='ANIME ', x='TYPE', y='LOGARITHM OF EPISODES');glb # Labeling

#*****************************************************************
# binary_rating vary by Anime type

# To see the distribution of the variable
dataset %>% filter(!is.na(rating)) %>% 
  ggplot(aes(rating, group = type)) +geom_density(aes(fill = type), alpha = .4) +xlim(0, 10)

#*****************************************************************
# Anime Distribution by TYPE(count)

# ONE HOT ENCODING ***
# Creating a new dataset for types
dataset_type = (dataset[1])

# METHOD-1 [ BASE ]
if(FALSE)
{
  # HOT-Encoding
  for(unique_value in unique(dataset$type))
  {
    dataset_type[paste("type", unique_value, sep = ".")] <- 
      ifelse(dataset$type == unique_value, 1, 0)
  }
  # Counting
  Type_of_anime=data.frame(c(sum(dataset_type$type.Movie == 1),
                             sum(dataset_type$type.TV == 1),
                             sum(dataset_type$type.OVA == 1),
                             sum(dataset_type$type.Special == 1),
                             sum(dataset_type$type.Music == 1),
                             sum(dataset_type$type.ONA == 1)))
  colnames(Type_of_anime) = "Count"
  
  names = data.frame(c("Movie","TV","OVA","Special","Music","ONA"))
  colnames(names) = "Type"
  Type_of_anime = cbind(names,Type_of_anime)
}
# METHOD-2 [ USING DPLYR ]
Type_of_anime_1= data.frame(dataset %>%group_by(type)%>%summarise(Count = n()))

# METHOD-3 [ USING table() ]
Type_of_anime_2 = as.data.frame(table(dataset$type))
colnames(Type_of_anime_2) =c("type",'Count')

# PLOTTING
plot_ly(data = Type_of_anime, labels = ~Type_of_anime$type,
        values = ~Type_of_anime$Count,type = 'pie')%>%
  layout(title = 'Anime Distribution by TYPE')
#*****************************************************************
# Anime(Type) Distribution by MEMBERS

# METHOD[ DPLYR ]
Type_of_anime_2 = dataset %>% group_by(type) %>% summarise( count_mem = sum(members))

Type_of_anime  = cbind(Type_of_anime,Type_of_anime_2$count_mem)
colnames(Type_of_anime) = c("Type","Count_type","Count_members")

# PLOTTING
plot_ly(data = Type_of_anime, labels = ~Type_of_anime$Type,
        values = ~Type_of_anime$Count_members,type = 'pie')%>%
  layout(title = 'Anime Distribution by MEMBERS')

#*****************************************************************
#*****************************************************************

# *****GENRE OF ANIME ANALYSIS*****

# Similar Anime By Genre
genre = as.data.frame(dataset$genre ,stringsAsFactors = F)

# Counting Unique genres[ 83 Unique genre in the data set ] 
unique_genre = (unique(unlist((as.data.frame
                    (tstrsplit(x = genre[,1],",",type.convert = T))))))
# Counting Total No. of Genres
genre_count = as.matrix(unlist(strsplit(x = genre[,1],","))
                                   ,colnames = "sep")
# Counting Table
gc_table = table(genre_count)
# Counting as Datafame
gc_df = as.data.frame(gc_table)

# PLOTTING
plot_ly(data = gc_df,x= gc_df$genre_count ,y=gc_df$Freq,
        name = "GENRE CHART",type = "bar")%>%
      layout(title = "GENRE CHART",yaxis = list(title = 'FREQUENCY'), 
             xaxis = list(title = "GENRE"))

#****************************************************************

# *** SPLITTING GENRE ***
# library(splitstackshape)

genre_split <- cSplit_e(dataset, "genre", ",", type = "character", fill = 0)
genre_split <- genre_split[-2]

#*****************************************************************
#*****************************************************************

# *** MISCELANEOUS TASKS ***

# 1)
# Movies Having sequence
movie_with_seq <- dataset %>% filter(episodes > 1 ,type == "Movie") %>% summarise( count = n())
# 2)
# Movies Having No Sequence
movie_with_seq <- dataset %>% filter(episodes == 1 ,type == "Movie") %>% summarise( count = n())

#3)
# Action Type Anime
anime_with_action <- genre_split %>%group_by(type)%>% filter(genre_Action == 1) %>% summarise( count = n())
# PLOTING
gg <- ggplot(anime_with_action ,aes(x =type, y=count, colour =type,fill = type)) + 
        geom_bar(stat = "identity") +
          labs(title = "ACTION TYPE ANALYSIS", x="TYPE OF ANIME", y ="COUNTING")
            theme_bw();gg

# 4)
# Mixed Type Anime[ COUNT ]
anime_with_mixed_genre <- genre_split %>%group_by(type)%>% filter(genre_Action == 1,genre_Romance == 1,
                                                        genre_Thriller ==1 ) %>% summarise( count = n())
# PLOTING
gg <- ggplot(anime_with_mixed_genre ,aes(x =type, y=count, colour =type,fill = type)) + 
  geom_bar(stat = "identity") +
  labs(title = "ACTION ROMANCE AND THRILLER", x="TYPE OF ANIME", y ="COUNTING")
theme_bw();gg

# Mixed Type Anime[ NAME ]
anime_exp <- genre_split %>% filter(genre_Action == 1,genre_Romance == 1,genre_Thriller ==1 ) %>%
                        select(anime_id)
# Combining Dataset By Anime_id
anime_name <- left_join(anime_exp,anime,by = "anime_id")%>%select(anime_id,name,type,rating,members)

#**********************************************************************************************************************************
#**********************************************************************************************************************************
#**********************************************************************************************************************************

# *** RATING DATASET ***

dataset_rating = data.frame(rating)

sum( is.na( dataset_rating)) #[count no. of missing values]
#dataset = na.omit(dataset)

glimpse(dataset)
summary(dataset)

# 1)
# People watched & Rated VS NOT-Rated, but watched
people_rated_watched <- rating %>%  filter( rating == -1) %>% summarise(count = n())
people_not_rated_watched <-rating %>%  filter( rating != -1) %>% summarise(count = n())
people_nm <-data.frame(c(people_not_rated_watched,people_rated_watched))
colnames(people_nm) <- c("Rated_&_watched","Not_Rated_&_Watched")

print(people_nm)

# 2)
# Average User Rating

average_user_rating <- rating %>% filter( rating != -1) %>% group_by(anime_id) %>% summarise( user_rating = mean(rating))
# Combining Dataset By Anime_id
rating_user <- left_join(average_user_rating,dataset, by ="anime_id") %>% select(anime_id,type,rating,user_rating)


#*****************************************************************
#*****************************************************************

# *** MIX MISCELANEOUS TASKS ***

# 1)
# User binary_rating vary by Anime type

# To see the distribution of the variable BY CRITICS RATING
gg_1 = rating_user %>% filter(!is.na(rating)) %>% 
  ggplot(aes(rating, group = type)) +geom_density(aes(fill = type), alpha = .4) +xlim(0, 10)

# To see the distribution of the variable BY USER RATING
rating_user_1 = na.omit(rating_user)
gg_2 = rating_user_1 %>% filter(!is.na(user_rating)) %>% 
  ggplot(aes(user_rating, group = type)) +geom_density(aes(fill = type), alpha = .4) +xlim(0, 10)

# PLOTING IN ONE SPACE
#library(gridExtra)
grid.arrange(gg_1, gg_2, nrow=2)

# 2)
# 

#**********************************************************************************************************************************
#**********************************************************************************************************************************
#**********************************************************************************************************************************

# *** RECOMMENDATION SYSTEM ***
if(!"recommenderlab" %in% rownames(installed.packages())){install.packages("recommenderlab")}
library(recommenderlab)
set.seed(1)

# <<<<<<<<<<<<<< anime.csv >>>>>>>>>>>>>>>
# Only anime_ ID AND Genres
recomm_genre = genre_split[c(-2,-3,-4,-5)]
# Adding Anime Names for searching
search_anime_1 = left_join(recomm_genre,anime[,c(1,2)],by = "anime_id")
search_anime_2 = search_anime_1%>%select(anime_id,name)
search_anime = cbind(search_anime_2,search_anime_1[c(-1,-45)])

sum( is.na( search_anime ))
# <<<<<<<<<<<<<< rating.csv >>>>>>>>>>>>>>
# Remove rows that are not rated
rating_1 <-rating %>%  filter( rating != -1)

# converting rating into likes & dis-likes
# If[ rating => 5  == like]
binary_rating_likes <-rating_1 %>% filter(rating > 5)
binary_rating_likes$rating = 1
# If[ rating < 5 == dis-like]
binary_rating_dislikes <-rating_1 %>% filter(rating <= 5)
binary_rating_dislikes$rating = -1
# Combine Row-wise
binary_rating <- rbind(binary_rating_likes,binary_rating_dislikes)
colnames(binary_rating)[3] <- "user_prespective"

sum( is.na( binary_rating ))

# Creating User Profile

# removing least important genre
gc_df_user <- gc_df %>% filter(Freq <= 33) %>% select(genre_count)
search_anime <- search_anime %>% select(-genre_Dementia,-genre_Josei,-`genre_Martial Arts`
                                        ,-genre_Military,-genre_Parody,-genre_Police,
                                        -genre_Psychological,-genre_Samurai,-genre_Seinen
                                        ,-genre_Shoujo,-genre_Space,-`genre_Super Power`
                                        ,-genre_Thriller,-genre_Vampire,-genre_Yaoi)

# Creating Profile
user_profile <- left_join(binary_rating,search_anime, by ="anime_id")

# Removing NA values
sum(is.na(user_profile))
user_profile <- na.omit(user_profile)


# 
#   # The User-Based Collaborative Filtering Approach
#   library(data.table)
#   #Create binary_rating matrix. Rows = userId, Columns = movieId
#   rating_mat <- dcast(binary_rating,anime_id~user_id,value.var = "user_prespective")
# 
#   library(tidyverse)
#   df %>%
#     gather(key, val, value:rating) %>%
#     unite(cond, key, condition) %>%
#     spread(cond, val)


# result <- user_profile[93:250,] #Fifth user's profile
# #Calculate Jaccard distance between user profile and all movies
# library(proxy)
# simlicity_result <- dist(result[,c(-2,-4)], method ='Jaccard' )
# 
# simlicity_result <- as.data.frame(as.matrix(simlicity_result[1:12403]))
# rows <- which(simlicity_result == min(simlicity_result))
# #Recommended movies
# View(user_profile[rows,4])

dimension_names <- list(user_id = sort(unique(rating_1[1:10000,]$user_id)),
                            anime_id = sort(unique(rating_1[1:10000,]$anime_id)))

rat_1 <- rating_1 %>% group_by(user_id) %>% mutate(user_id_gp = row_number())

ratingmat <- (spread(rating_1[1:3500000,],anime_id,rating) %>% select(-user_id))

dim(ratingmat)








