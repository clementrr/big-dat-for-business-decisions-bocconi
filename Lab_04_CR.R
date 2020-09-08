###########################################
##### BIG DATA FOR BUSINESS DECISIONS #####
############ cod. 20564 ###################
########## BOCCONI UNIVERSITY #############
###########################################

###################################
########### LAB 3 #################
##### ANALYZING TEXTUAL DATA ######
###################################



# PACKAGES FOR NLP AND QUANTEDA -------------------------------------------

# In order to analyze text data, R has several packages available. 
# In this Lab, we focus on one of the most popular one: quanteda. quanteda
# stands for QUantitative ANalysis of TExtual DAta. This package is developed
# by Ken Benoit from LSE and contributors. quanteda is fully-featured and 
# allows the user to easily perform natural language processing tasks.
# It offers an extensive documentation and is regularly updated. 
# quanteda is most useful for preparing data that can then be further analyzed 
# using unsupervised/supervised machine learning or other techniques. 
# A combination with tidyverse leads to a more transparent code structure 
# and offers a mere variety of useful areas that could not be addressed 
# within the limited time of the workshop (e.g., scaling models, 
# part-of-speech (POS) tagging, named entities, word embeddings, etc.).
# 
# There are also similar R packages such as "tm", "tidytext", and "koRpus". 
# tm has simpler grammer but slightly fewer features, tidytext is very 
# closely integrated with dplyr and well-documented, and koRpus is good 
# for tasks such as part-of-speech (POS) tagging).


# USING QUANTEDA ----------------------------------------------------------

# Most analyses in quanteda require three steps:
#   
# 1. IMPORT THE DATA 
# the data that we usually use for text analysis is 
# available in text formats (e.g., .txt or .csv files). Sometimes we also have
# to start from the much more complicated .pdf format.
# 
# 2. BUILD A CORPUS
# After reading in the data, we need to generate a corpus. 
# A corpus is a type of dataset that is used in text analysis. 
# It contains a "collection of text or speech material that has been brought 
# together according to a certain set of predetermined criteria" 
# (Shmelova et al. 2019, p. 33). 
# These criteria are usually set by the researchers and are in concordance 
# with the guiding question. For instance, if you are interested in analyzing 
# speeches in the UN General Debate, these predetermined criteria are the 
# time and scope conditions of these debates (speeches by countries at 
# different points in time).
# 
# 3. CALCULATE A DOCUMENT-FEATURE-MATRIX or DFM
# Another essential component for text analysis is an object called 
# Document-Feature-Matrix (DFM). Some refer to this as document-term matrix (DTM). 
# These two terms are synonyms but quanteda refers to a DFM whereas others 
# will refer to DTM. It describes how frequently terms occur in the corpus 
# by counting single terms.
# To generate a DFM, we first split the text into its single terms (tokens). 
# We then count how frequently each term (token) occurs in each document.
# The result will be an object with N rows corresponding to the number of 
# documents in the sample and P columns corresponding to the number of 
# tokens that quanteda found in the corpus when converting it to a DFM.
# We will see how simple it is to move from a corpus to a DFM.
# 
# 
# 
# IMPORTANT CONCEPTS TO REMEMBER ------------------------------------------
# 
# 1. A corpus is positional (string of words) and a DFM is non-positional 
# (bag of words). Put differently, the order of the words matters in a corpus 
# whereas a DFM does not have information on the position of words.
# 
# 2. A token is each individual word in a text (but it could also be a sentence, 
# paragraph, or character). This is why we call creating a "bag of words" also 
# tokenizing text. In a nutshell, a DFM is a very efficient way of organizing 
# the frequency of features/tokens but does not contain any information on 
# their position. In our example, the features of a text are represented by 
# the columns of a DFM and aggregate the frequency of each token.
# 
# 3. In most projects you want one corpus to contain all your data and 
# generate many DFMs from that according to specific needs.
# 
# 4. The rows of a DFM can contain any unit on which you can aggregate documents. 
# It may also well be more fine-grained with sub-documents or more aggregated 
# with a larger collection of documents.
# 
# 5. The columns of a DFM are any unit on which you can aggregate features. 
# Features are extracted from the texts and quantitatively measurable. 
# Features can be words, parts of the text, content categories, word counts, etc. 



# LOAD ALL THE REQUIRED PACKAGES ------------------------------------------

library( data.table )
library( readtext )
library( quanteda )
library( topicmodels )
library( ggplot2 )
library( gghighlight )


# IMPORT THE DATA ---------------------------------------------------------

# let's load the data which comes from the United Nations
# you can find the original transcripts here
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/0TJX8Y

UNGDspeeches <- readtext(file = "Lab_04/UN_data/",
                         docvarsfrom = "filenames",
                         docvarnames = c("country", "session", "year"),
                         dvsep = "_",
                         encoding = "UTF-8"
)

class( UNGDspeeches )


# CREATING THE CORPUS -----------------------------------------------------

# we want to convert the imported files into a formal object for
# textual analysis: the corpus
# to do that, we the function corpus() from quanteda

UNGDcorpus <- corpus( UNGDspeeches )

# let's visualize the structure for the first 10 docs
summary( UNGDcorpus, 10 )

# how many documents do we have? 
N <- ndoc( UNGDcorpus )
N

# Assigns a unique identifier to each text called "id_doc"
docvars( UNGDcorpus, "id_doc" ) <- sprintf( "%04d", 1L:N )

# let's see that now we have one more docvar...
summary( UNGDcorpus, 10 )



# TOKENIZATION AND PREPROCESSING ------------------------------------------

# Tokenization is the process of splitting a given textual document
# in its components given by words or tokens.
# In order to do this, we use the quanteda function tokens()
# Let' try it with a very simple example

example = c( "Star formation is the process by which dense regions within 
             molecular clouds in interstellar space, sometimes referred to as 
             \"stellar nurseries\" or \"star-forming regions\", collapse and 
             form stars. As a branch of astronomy, star formation includes the 
             study of the interstellar medium (ISM) and giant molecular clouds 
             (GMC) as precursors to the star formation process, and the study 
             of protostars and young stellar objects as its immediate products. 
             It is closely related to planet formation, another branch of 
             astronomy. Star formation theory, as well as accounting for the 
             formation of a single star, must also account for the statistics 
             of binary stars and the initial mass function. Most stars do not 
             form in isolation but as part of a group of stars referred as 
             star clusters or stellar associations." )

tokens( example )

# As you can see, we retain all the tokens including punctuation and symbols
# We might want to get rid of those and it's very simple. 

example_tok = tokens( example, 
                      remove_numbers = TRUE,
                      remove_punct = TRUE,
                      remove_symbols = TRUE,
                      remove_twitter = TRUE,
                      remove_url = TRUE,
                      remove_hyphens = TRUE )

# Some might ask: what if we wanted to keep track of the senteces? 
# This is your choice and really stems upon what you have in mind.
# It could be the case that the whole document about star formation is the unit
# of analysis and maybe the document about world povery is another one.
# To show you the difference, we can convert our example into a corpus and you 
# will see that we will only record one document

example_corpus = corpus( example )
summary( example_corpus )

# you can see though that this document is made up by 5 sentences. 
# Maybe we are interested in keeping track of them in well separated unit
# of analysis so we can do this trick
# We can reshape the corpus we just created and split it by sentences.
# We will use the function corpus_reshape()

example_corpus_sent = corpus_reshape( example_corpus, to = "sentences" )
summary( example_corpus_sent ) 

# The reason why I am showing this is to make you understand that the 
# preprocessing step is fundamental. For instance, if we were to define
# a corpus based on the sentences, removing the punctuation from our example
# would have jeopardize the whole analysis. 


# Let's now go back to our UN data
# In this case, we want the corpus to record individual document as
# unit of analysis so we don't have to perform any reshape. 
# We want to do some preprocessing as we did before.

# Create tokens but don't print this object! 
UNGD_tokens <- tokens( UNGDcorpus,
                       remove_numbers = TRUE,
                       remove_punct = TRUE,
                       remove_symbols = TRUE,
                       remove_twitter = TRUE,
                       remove_url = TRUE,
                       remove_hyphens = TRUE,
                       include_docvars = TRUE )

# Since the pre-1994 documents were scanned with OCR scanners, several 
# tokens with combinations of digits and characters were introduced.
# Clean tokens created by OCR
UNGD_tokens <- tokens_select( UNGD_tokens,
                              c("[\\d-]", "[[:punct:]]", "^.{1,2}$"),
                              selection = "remove",
                              valuetype = "regex",
                              verbose = TRUE )



# CREATING THE DOCUMENT-FEATURE MATRIX (DFM) ------------------------------

# Now it's time to create a quantitative object which allows us to 
# run analsyses. To do this, we use dfm()
# We also want to force every character to be lowercase and to just have
# the root of the words through stemming.
# Also, we want to remove all the common english words called "stopwords"

stopwords()

UNGD_dfm <- dfm( UNGD_tokens,
                 tolower = TRUE,
                 stem = TRUE,
                 remove = stopwords("english") )

class( UNGD_dfm )

# What we have now is an object which contains documents in rows and 
# features (tokens/words) in columns. Since we didn't apply any sort
# of threshold, this object contains all the set of features in our corpus.
# What if we wanted to remove very uncommon words or maybe too common ones?
# 
# This procedure is called "trimming" and this example we want to remove
# words that appear less than 10% and more than 90%. 
# This is a rather conservative approach but since we have a relatively
# large corpus, we just stick with it
UNGD_dfm_trim <- dfm_trim( UNGD_dfm,
                           # min 7.5%
                           min_docfreq = 0.01,
                           #  max 90%
                           max_docfreq = 0.90,
                           docfreq_type = "prop" ) 

class( UNGD_dfm_trim )

# let's visualize the first 10 observations (i.e. docs) and the 
# first 10 features (i.e. tokens)
head( UNGD_dfm_trim, n = 10, nf = 10 )

# the percentage of sparsity reflects the proportions of zeroes wrt 
# the total occurrences
# As we can imagine, this percentage tends to increase with the number of 
# features
head( UNGD_dfm_trim, n = 10, nf = 100 ) 
head( UNGD_dfm_trim, n = 10, nf = 1000 )
head( UNGD_dfm_trim, n = 10, nf = 10000 )


# TEXT CLASSIFICATION -----------------------------------------------------

# With a dfm object, we are now ready to run some analysis
# For this tutorial, we are going to adopt the dictionary based approach
# Dictionaries contain lists of words that correspond to different categories. 
# If we apply a dictionary approach, we count how often words that are 
# associated with different categories are represented in each document. 
# These dictionaries help us to classify (or categorize) the speeches based 
# on the frequency of the words that they contain. 
# 
# There are plenty of dictionaries available for free.
# Popular dictionaries are sentiment dictionaries 
# (such as Bing, Afinn or LIWC) or LexiCoder.
# 
# To analyze political speeches, we use the “LexiCoder Policy Agenda” 
# dictionary that can be accessed here in a ".lcd" format. 
# The “LexiCoder Policy Agenda” dictionary captures major topics from the 
# comparative Policy Agenda project and is currently available in 
# Dutch and English. 
# 
# You can find the dictionary directly in quanteda as follows:
# quanteda::data_dictionary_LSD2015
# 
# If you are interested in the topics in the corpus, you can download
# the Lexicoder Topic Dictionary here:
# http://www.lexicoder.com/ltdjun2013/
# 
# we then read it through quanteda as follows: 
LTD_topics = dictionary( file = "Lab_04/dictionaries/policy_agendas_english.lcd" )

# let's have a look at the different topics
names( LTD_topics )

# of course, in each topic we will have a set of tokens which identify 
# and characterize that specific topic
# this gives the length of each sub-dictionary
sapply( LTD_topics, length )

# the standard procedure when running dictionary-based analysis is to 
# further preprocess the dfm to retain only the tokens defined in the 
# dictionary one is using. 
# 
# the dfm() function has indeed an argument "dictionary" in which we can specify
# the word list we want.

UNGD_wl = dfm( UNGD_dfm_trim, 
               groups = "country", 
               dictionary = LTD_topics )

UNGD_wl_dt = convert( UNGD_wl, to = "data.frame")
setDT( UNGD_wl_dt )
setnames( UNGD_wl_dt, "document", "country" )

# topics of interest
topics = c( "country", "macroeconomics", "civil_rights", "althcare",
            "agriculture", "forestry", "labour", "immigration", 
            "education", "environment", "energy", "culture", "defence" )

prop_topics = melt( UNGD_wl_dt[ , mget( topics ) ], 
                    id.vars = "country", 
                    variable.name = "topic", 
                    value.name = "weight" )

prop_topics[ , `:=` (weight_prop = weight / sum(weight ),
                     topic = as.factor( topic ) ), 
             by = country ]

ggplot(prop_topics, 
       aes( country, weight_prop, color = topic, fill = topic ) ) +
  geom_bar(stat = "identity") +
  ggtitle("Distribution of topics in the UN General Debate corpus") +
  xlab("") +
  ylab("Topic share (%)") +
  theme(legend.position = "right", 
        axis.text.x = element_text( angle = 90, vjust = .2, size = 6),
        axis.ticks.x = element_blank()) 

countries = c( "ITA", "USA", "GBR", "DEN", "RUS", "CHN", 
               "FRA", "GER", "SPA" )

ggplot(prop_topics[ country %in% countries ], 
       aes( country, weight_prop, color = topic, fill = topic ) ) +
  geom_bar(stat = "identity") +
  ggtitle("Distribution of topics in the UN General Debate corpus") +
  xlab("") +
  ylab("Topic share (%)") +
  theme(legend.position = "right", 
        axis.text.x = element_text( angle = 90, vjust = .2, size = 6),
        axis.ticks.x = element_blank()) 



# TOPIC MODELING ----------------------------------------------------------

# When the categories are unknown we are moving in the unsupervised 
# framework.
# In this section we introduce a very famous approach to topic modeling called
# Latent Dirichlet Allocation (LDA). 
# Topic models find patterns of words that appear together and group them 
# into topics. The researcher decides on the number of topics and the 
# algorithm then discovers the main topics of the texts without prior 
# information, training sets or human annotations.
# 
# Before running the model, we extract a subset of the original corpus
# This is because of time constraints. The LDA can be a computationally
# intensive model and running it on over 8000 documents can be too much
# for the purpose of this Lab. 
# Since we already did a bit of pre-processing over the original corpus,
# we would like to directly subset the DFM object. Luckily, quanteda
# has a function that does that: dfm_subset(). We just use the 
# docvars we created to slice down the dfm.

UNGD_dfm_subset <- dfm_subset( UNGD_dfm_trim, year >= 2017 )
set.seed( 1970 )
UNGD_dfm_subset <- dfm_sample( UNGD_dfm_trim, size = 50 )

# original number of docs
ndoc(UNGD_dfm_trim)
# what we have now
ndoc(UNGD_dfm_subset)

# To run the LDA, we will use the package "topicmodels"

# we specify the number of topics 
n_topics = 15

# we also need to convert our trimmed DFM to a proper topicmoodels object
dfmLDA <- convert( UNGD_dfm_subset, to = "topicmodels" )

# then we run the LDA with the function LDA()
LDAmodel <- LDA( dfmLDA, k = n_topics )

gamma = as.data.table( LDAmodel@gamma )
doc_vars = docvars( UNGD_dfm_subset )
gamma[ , country := doc_vars$country]
gamma[ , year := doc_vars$year ]
gamma_molten = melt( gamma, 
                     id.vars = c( "country", "year" ),
                     value.name = "gamma",
                     variable.name = "topic",
                     measure.vars = paste0("V", 1:n_topics) )
gamma_molten[ , `:=` (gamma_prop = gamma / sum(gamma ),
                     topic = as.factor( topic ) ), 
             by = .(country) ]

ggplot(gamma_molten, 
       aes( country, gamma_prop, color = topic, fill = topic ) ) +
  geom_bar(stat = "identity") + 
  ggtitle("Distribution of topics in the UN General Debate corpus") +
  xlab("") +
  ylab("Topic share (%)") +
  theme(legend.position = "right", 
        axis.text.x = element_text( angle = 90, vjust = .2, size = 6),
        axis.ticks.x = element_blank()) 



# END OF SCRIPT