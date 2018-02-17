# 0 Prepare R

# 0.1 Set libraries
library(tidyr)
library(dplyr)
library(magrittr)
library(XML)

# 0.2 Set working directory
setwd('C:/Users/Guillaume/Documents/Box Covers')

# 0.3 Set options
options(stringsAsFactors = FALSE)




# 1 Acquire Data from thegamesdb.net
# 1.1 Prepare list of covers to download
df.ps1_list = 
	xmlParse("http://thegamesdb.net/api/PlatformGames.php?platform=sony+playstation") %>%
	xmlToList() %>% #transform the xml file to a list
	data.table::rbindlist(fill=TRUE) #transform the list to a data frame


# 1.2 Iterate through the list of covers and download them
for(i in 1:nrow(df.ps1_list)){

	if(!is.na(df.ps1_list$thumb[i])){
		download.file(
			paste0('http://thegamesdb.net/banners/_gameviewcache/',df.ps1_list$thumb[i]), #source of the covers, taken in banner form to simplify the computations
			paste0('.\\Cover2\\', df.ps1_list$id[i], '.jpg'), #where the cover is downloaded
			mode = 'wb' #binary download (needed in windows)
		)
	}

	Sys.sleep(2)

}	

# 1.3 Crop the covers in the left and the bottom to remove the Playstation and other companies' watermarks
# Using Imagemagick & command line
# FOR /R %i IN (*.jpg) DO (convert "%i" -gravity east -crop 85x100%  +repage -gravity north -crop 100x82%   ./../CoverCropped/%~ni%~xi)