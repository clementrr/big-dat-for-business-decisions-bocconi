###########################################
##### BIG DATA FOR BUSINESS DECISIONS #####
############ cod. 20564 ###################
########## BOCCONI UNIVERSITY #############
###########################################

###################################
########### LAB 5 #################
######### WEB SCRAPING ############
###################################

# WHAT DOES WEB SCRAPING MEAN? --------------------------------------------

# Data and information on the web is growing exponentially. 
# One of the first and primary source of information for us today is Google.
# This means that information is stored in some way on a remote web page.
# The only thing limiting you from using it is the ability to access it. 
# And that's where web scraping kicks in! 
# 
# Most of the data available over the web is not readily available. 
# It is present in an unstructured format (HTML format) and is not downloadable. 
# Therefore, it requires knowledge & expertise to use this data to eventually build a useful model.
# 
# 
# 1. What is Web Scraping?
#   
# Web scraping is a technique for converting the data present in unstructured 
# format (HTML tags) over the web to the structured format which can easily 
# be accessed and used. This means that you are going to build a 
# data.frame or a corpus at the end of the day.
# 
# 2. Getting some data from Seeking Alpha
# 
# Other reasons why one would scrape data could be: 
# - Scraping movie rating data to create movie recommendation engines.
# - Scraping text data from Wikipedia and other sources for making NLP-based 
#   systems or training deep learning models for tasks like topic recognition 
#   from the given text.
# - Scraping labeled image data from websites like Google, Flickr, etc to 
#   train image classification models.
# - Scraping data from social media sites like Facebook and Twitter for 
#   performing tasks Sentiment analysis, opinion mining, etc.
# - Scraping user reviews and feedbacks from e-commerce sites like Amazon, etc.


library(data.table)
library(rvest)
library(stringr)
library(quanteda)


ticker = "AM"
url = paste0( "https://seekingalpha.com/symbol/", 
              ticker, "/earnings/transcripts?s=", tolower(ticker) )

N = 10
id_transcripts = data.table( id_conf = 1L:N,
                             conf_ref = rep( NA_character_, N ),
                             page = rep( NA_character_, N ) )
for ( i_trans in 1L:10L ) {
  
  print( i_trans )
  node_to_get = paste0( "#headlines_transcripts > div > ul > li:nth-child(",
                        i_trans, ")" )
  link_to_get = paste0( "#headlines_transcripts > div > ul > li:nth-child(",
                        i_trans, ") > div.content > div > a")
  current_id = read_html( url ) %>% 
    html_nodes( node_to_get ) %>% 
    html_text()
  
  current_link = read_html( url ) %>% 
    html_nodes( link_to_get ) %>% 
    html_attr("href")
  current_link = paste0( "https://seekingalpha.com", 
                         current_link, 
                         "?part=single" )
  
  id_transcripts[ i_trans, `:=` ( conf_ref = current_id,
                                  page = current_link ) ]
  if ( i_trans == N ) {
    id_transcripts[]
  }
}


participants_all = data.table( id = NA_integer_, 
                               mmddyyyy = NA_character_, 
                               qtr = NA_character_, 
                               name = NA_character_,
                               role = NA_character_ )
trans_corpus = data.table()

for ( i_link in 1L:nrow( id_transcripts ) ) {
  
  print( i_link )
  trans_link = id_transcripts[ i_link, page ]
  
  current_text = read_html(trans_link) %>% 
    html_nodes("#a-body") %>% 
    html_text()
  
  dates = str_extract( current_text, "^.*?\\n" )
  qtr = str_extract(dates, "Q\\d" )
  mmddyyyy = str_extract( dates, "(?<=Call\\s)\\w+\\s\\d+\\,\\s\\d+")
  current_text = str_replace_all( current_text, "\\n", " #999# ")
  temp = str_extract( current_text, 
                      "(?<=\\#\\d\\d\\d\\#\\s).*?(?=\\#\\d+\\#\\sOperator\\s\\#\\d+\\#\\s)")
  if ( !is.na( temp ) ) {
    temp = str_split( temp, "\\s\\#\\d+\\#\\s" )
    temp = as.data.table( temp )
    temp = temp[ 2L:nrow( temp ) ]
    temp = temp[ !str_detect( V1, "Conference Call Participants" ) ]
    participants = as.data.table( str_split( temp$V1, "\\s\\-\\s", simplify = TRUE ) )
    setnames( participants, names(participants), c( "name", "role" ) )
    participants[ , `:=` (name = str_trim( name ),
                          role = str_trim( role ) ) ]
    
    participants[ , `:=` ( id = i_link, mmddyyyy = mmddyyyy, qtr = qtr ) ]
    setcolorder( participants, c( "id", "mmddyyyy", "qtr", "name", "role" ) )
  } else  {
    participants = data.table( id = i_link, 
                               mmddyyyy = NA_character_, 
                               qtr = NA_character_, 
                               name = NA_character_,
                               role = NA_character_ )
    
  }
  participants_all = rbindlist( list( participants_all, participants ) )
  if ( i_link == 1 ) {
    participants_all = na.omit( participants_all )
  }
  
  if ( i_link == 1 ) {
    trans_corpus = corpus( current_text )
  } else {
    current_corpus = corpus( current_text )
    trans_corpus = trans_corpus + current_corpus  
  }
  
  docvars( trans_corpus, "id" ) = id_transcripts[ i_link, id_conf ]
  docvars( trans_corpus, "conf_ref" ) = id_transcripts[ i_link, conf_ref ]
  docvars( trans_corpus, "page" ) = trans_link
  
}


mydfm = dfm( trans_corpus )



