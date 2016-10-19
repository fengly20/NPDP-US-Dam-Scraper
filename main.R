library( 'rvest' )
library( 'plyr' )

# -------------
# construct the urls
dam_count <- as.character( 1 : 85228 )
dam_count <- unlist( lapply( dam_count, function( dam ) {
  char_count <- nchar( dam )
  
  if ( char_count == 1 ) { dam_string <- paste0( '0000', dam ) }
  if ( char_count == 2 ) { dam_string <- paste0( '000', dam ) }
  if ( char_count == 3 ) { dam_string <- paste0( '00', dam ) }
  if ( char_count == 4 ) { dam_string <- paste0( '0', dam ) }
  if ( char_count == 5 ) { dam_string <- dam } 
  
  return( dam_string )
} ) )
url_list <- paste0( 'http://npdp.stanford.edu/data_access/international_dams_view.php?editid1=NPDPUSA00', 
                    dam_count, 
                    '#' )
# ---------------
# define some data fram col names 
ori_col_names <- c( "Alternative Dam Name", "Dam Type", "Dam Height (m)", 
                    "Dam Length(m)", "Hazard Class", 
                    "Geographic Coordinates (degrees)(latitude, longitude)", 
                    "Emergency Action Plan", "Main Purpose", "Other Purposes", 
                    "Country", "State/Province", "County", "River", 
                    "Nearest Downstream City", "Distance To City (km)", 
                    "Population at Risk", "Year Completed", "Year Modified", 
                    "Normal Reservoir Storage (m3)", "Max Reservoir Storage (m3)", 
                    "Crest Elevation(m)", "Dam Base Width(m)", "Foundation Type", 
                    "Core Type", "Volume (m3)", "Material", "Dam Designer", "Federal Regulatory Agency", 
                    "Inspection Frequencyper year", "State Regulated", 
                    "State Regulatory Agency", "Mean Annual Energy", "Electric Capacity" )
new_col_names <- c( 'Dam Name', "Alternative Dam Name", "Dam Type", "Dam Height (m)", 
                    "Dam Length(m)", "Hazard Class",
                    "Emergency Action Plan", "Main Purpose", "Other Purposes", 
                    "Country", "State/Province", "County", "River", 
                    "Nearest Downstream City", "Distance To City (km)", 
                    "Population at Risk", "Year Completed", "Year Modified", 
                    "Normal Reservoir Storage (m3)", "Max Reservoir Storage (m3)", 
                    "Crest Elevation(m)", "Dam Base Width(m)", "Foundation Type", 
                    "Core Type", "Volume (m3)", "Material", "Dam Designer", "Federal Regulatory Agency", 
                    "Inspection Frequencyper year", "State Regulated", 
                    "State Regulatory Agency", "Mean Annual Energy", "Electric Capacity", 
                    "lat", "lon" )

# -------------------------
# the scrapping starts from here 

# create empty df for data storage 
dam_df <- data.frame( matrix( NA, 1, 35 ) )
colnames( dam_df ) <- new_col_names 

for ( url in url_list ) { 
  
  print( url )
  
  html_file <- read_html( url )
  
  # for the html_file, extract the dam name and the dam table 
  # extract dam name 
  titles <- html_nodes( html_file, 'title' )
  dam_name <- html_text( titles )
  dam_name <- gsub( '\r', '', dam_name, fixed = T )
  dam_name <- gsub( '\n', '', dam_name, fixed = T )
  dam_name <- gsub( '[ ]{2,}', '', dam_name )
  dam_name <- unlist( strsplit( dam_name, split = ': ' ) )[ 2 ]
  
  # then extract the dam info table  
  # only last table we care about
  tables <- html_nodes( html_file, "table" )
  
  # third table 
  table3 <- html_table( tables[ 3 ], fill = TRUE )[[ 1 ]]
  
  # extract table col names 
  data_line <- table3[ , 2 ]
  data_line <- gsub( pattern = 'Â', '', data_line, fixed = T )
  data_line <- gsub( '\r', '', data_line, fixed = T )
  data_line <- gsub( '\n', '', data_line, fixed = T )
  data_line <- gsub( '[ ]{2,}', '', data_line )
  
  # make the coordinates column into two columns: lat, lon
  gc <- data_line[ 6 ]
  data_line <- data_line[ -6 ]
  gc <- unlist( strsplit( gc, split = ', ' ) )
  data_line <- c( data_line , gc[ 1 : 2 ] )
  
  # add the dam name 
  data_line <- c( dam_name, data_line )
  
  # construct the data frame 
  df <- data.frame( t( data_line ) ) 
  colnames( df ) <- new_col_names
  
  # append to the master dam list 
  dam_df <- rbind( dam_df, df )
  
}

# remove the first empty row 
dam_df <- dam_df[ -1, ]

# save as csv 
write.csv( dam_df, 'yourpath/us_dams.csv', row.names = F )
