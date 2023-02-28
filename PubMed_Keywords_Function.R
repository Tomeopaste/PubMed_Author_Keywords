#############################################################################
#####  		                 PubMed Search Function                 	#####
##### 	   Returns keywords, with counts, from most recent pubs  		#####
#############################################################################
#
## This function :
#   1. loads dplyr and easyPubMed
#   2. searches for a name supplied to the argument "searchName" and retrieves 
#        PubMed records for that author. Use format: 
#        "LastName, FirstName MI [AU]"
#	2.1.  Additional modifications to the recommended format will allow more
#			specificity in searching where necessary to disambiguate authors
#			with the same name.	
#   3. parses the return and turn it into a useable format (i.e., data.frame)
#   4. filters out only the publications that have keywords associated with them
#   5. pulls keywords from the PI's most recent pubs & reformat slightly; will 
#        return keywords from the number of pubs supplied to argument nToReturn
#   6. counts the keywords and list them in order of appearance w/ count
#   7. outputs a tibble of the keywords and counts
#
#	NOTES
#	1. Basic modifications are readily implementatble to return information 
#		other than keywords. The functions used from easyPubMed pull down 
#		abstracts or dates, for example. 
#	2. Depending on the use case, cutting "stop words" (both standard 
#		english and the PubMed stop words included with easyPubMed) is 
#		a good idea. (I should probably build this into the funtion as an
#		optional argument to turn on/off.)


PM_Search_By_Author_KW <- function(searchName, nToReturn){ 
	#Arguments are searchName = the query to use in the search of PubMed
	#	The input is a string in quotes
	#And nToReturn = the number of pubs use in subsequent operations
	#	The input is an integer
   require("dplyr"); require("easyPubMed") #load necessary packages
   #
   # Get all PMIDs for the author
   T_PI_entrezID <- get_pubmed_ids(as.character(searchName))
   # Pull all data associated with the PMIDs (authors, pub date, abstracts, etc)
   PI_abstracts_xml <- fetch_pubmed_data(T_PI_entrezID, format = "xml")
   PI_List <- articles_to_list(PI_abstracts_xml)
   # Convert list items to data.frame type/structure
   PI_df <- do.call(rbind, lapply(PI_List, article_to_df, 
                                  max_chars = -1,
                                  getAuthors = TRUE, 
                                  getKeywords = TRUE))
   #Cut duplicate rows
   PI_df <- PI_df %>% distinct(title, .keep_all = TRUE)
   #Create proper date field
   PI_df$date <- as.POSIXct(paste0(PI_df$year, "-", 
                                   PI_df$month, "-",
                                   PI_df$day))
   # Filter out pubs w/o keywords and select  nToReturn most recent pubs 
   PI_top_KW <- PI_df %>% filter(keywords != "<NA>") %>% 
      top_n(nToReturn, date)
   # Convert semicolon separators between kws to commas
   PI_top_KW$keywords <- stringr::str_replace_all(PI_top_KW$keywords, ";", ",")
   # turn this into a format we can do calculations with, i.e., one keyword/row
   PI_kws <- data.frame(kws = strsplit(paste(PI_top_KW$keywords,
                                             sep = ",",
                                             collapse = ""), ","))
   names(PI_kws) <- "keywords" #Rename the column header
   PI_kws$keywords <- tolower(PI_kws$keywords) # standardize all to lowercase
   PI_kws %>% 
      group_by(keywords) %>% 
      count() %>% 
      arrange(-n) %>% 
      print(n = Inf)
}

