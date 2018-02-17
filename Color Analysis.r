###########################################
############### 0 Prepare R ###############
###########################################

# 0.1 Set libraries
library(tidyr)
library(dplyr)
library(magrittr)
library(jpeg)
library(ggplot2)
library(colorspace)
library(reshape2)
library(locfit)
library(ggimage)
library(Rtsne)
library(XML)

# 0.2 Set working directory
setwd('C:/Users/Guillaume/Documents/Box Covers')

# 0.3 Set options
options(stringsAsFactors = FALSE)

# 0.4 list all previously downloaded covers files
list.cover = list.files('.\\CoverCropped')

# 0.5 Create Hilbert Function
# From: https://www.r-bloggers.com/going-bananas-with-hilbert/
hilbert = function(m,n,r) {
  for (i in 1:n)
  {
    tmp=cbind(t(m), m+nrow(m)^2)
    m=rbind(tmp, (2*nrow(m))^r-tmp[nrow(m):1,]+1)
  }
  melt(m) %>% plyr::rename(c("Var1" = "x", "Var2" = "y", "value"="order")) %>% arrange(order)
}
  
# 0.6 Initialize an Hilbert curve to simplify a 2d surface in a single line
# Idea taken from https://mathematica.stackexchange.com/questions/87588/how-to-sort-colors-properly
df.hilbert = hilbert(m=matrix(1), n=5, r=2)














########################################################################
############### 1 Acquire data for all downloaded covers ###############
########################################################################

# 1.1 Initialize the data frame for the game genres and the game details
df.gamedetails = data.frame()
df.gamegenres = data.frame()

# 1.2 Iterate through all downloaded covers
for(i in 1:length(list.cover)){ 


# 1.2.1 Acquire the XML for the cover ID
xmlextract = 
	list.cover[i] %>%
	gsub('\\.jpg$', '', .)  %>% #Remove .jpg from the filename
	paste0('http://thegamesdb.net/api/GetGame.php?id=', .) %>% 
	xmlParse()


# 1.2.2 Put game details in a data frame 
df.gamedetails = 
	data.frame(
		id = 			xpathSApply(xmlextract, '/Data/Game/id', 	xmlValue) %>% {if(length(.)==0){NA}else{.}},						
		GameTitle = 	xpathSApply(xmlextract, '//GameTitle', 		xmlValue) %>% {if(length(.)==0){NA}else{.}},                        
		ReleaseDate = 	xpathSApply(xmlextract, '//ReleaseDate', 	xmlValue) %>% {if(length(.)==0){NA}else{as.Date(., '%m/%d/%Y')}},   
		Overview = 		xpathSApply(xmlextract, '//Overview', 		xmlValue) %>% {if(length(.)==0){NA}else{.}},                        
		ESRB = 			xpathSApply(xmlextract, '//ESRB', 			xmlValue) %>% {if(length(.)==0){NA}else{.}},                        
		Publisher = 	xpathSApply(xmlextract, '//Publisher', 		xmlValue) %>% {if(length(.)==0){NA}else{.}},                        
		Developer = 	xpathSApply(xmlextract, '//Developer', 		xmlValue) %>% {if(length(.)==0){NA}else{.}},                        
		Rating = 		xpathSApply(xmlextract, '//Rating', 		xmlValue) %>% {if(length(.)==0){NA}else{.}}                         
	) %>%
	bind_rows(df.gamedetails, .) #Append to the data frame

# 1.2.3 There can be multiple genres for a game, so these are put in another data frame	
df.gamegenres = 
	data.frame(
		id = xpathSApply(xmlextract, '/Data/Game/id', xmlValue)  %>% {if(length(.)==0){NA}else{.}},
		genre = xpathSApply(xmlextract, '//genre', xmlValue)  %>% {if(length(.)==0){NA}else{.}}
	) %>%
	bind_rows(df.gamegenres, .)
	
	print(i)
	Sys.sleep(3)
}















####################################################
############### 2 Import Covers in R ###############
####################################################

# 2.1 Initialize the data frame for TSNE analysis
df.colors = expand.grid(Red = 0:3, Green = 0:3, Blue = 0:3)

# 2.2 Get all genres 
list.genres = 	
	df.gamegenres$genre %>%
	unique() %>%
	{ifelse(is.na(.), 'Other', .)}

# 2.3 Initialize the data frame for HSV analysis by genre
df.colors_genre = 
	expand.grid(Red = 0:63, Green = 0:63, Blue = 0:63, genre = list.genres) %>%
	mutate(Tot_Avg_Pixels = 0)	
	

# 2.4 Iterate through all covers
for(covername in list.cover){

	# 2.4.1 Import the image in R
	image.jpeg = 
		paste0('.\\CoverCropped\\', covername) %>%
		readJPEG()
		
	# 2.4.2 Get the proportions of colors for all images
	df.colors = 
		data.frame( #Get each color as a column from the data frame
			Red = (image.jpeg[,,1]*255/64) %>% as.vector %>% floor,
			Green = (image.jpeg[,,2]*255/64) %>% as.vector %>% floor,
			Blue = (image.jpeg[,,3]*255/64) %>% as.vector %>% floor
		) %>%
		group_by(Red, Green, Blue) %>%
		summarise(Avg_Pixels = n()/(dim(image.jpeg)[1] * dim(image.jpeg)[2])) %>% #proportion of pixels by color
		right_join(df.colors) %>% #merge to global data frame 
		mutate(!!covername := ifelse(is.na(Avg_Pixels), 0, Avg_Pixels)) %>% #filename as variable name
		select(-Avg_Pixels)
		
	# 2.4.3 Get the colors proportion and join it to that game genres
	df.Avg_Colors = 
		data.frame( #Get each color as a column from the data frame
			Red = (image.jpeg[,,1]*255/4) %>% as.vector %>% floor,
			Green = (image.jpeg[,,2]*255/4) %>% as.vector %>% floor,
			Blue = (image.jpeg[,,3]*255/4) %>% as.vector %>% floor,
			id = gsub('\\.jpg', '', covername)
		) %>%
		group_by(Red, Green, Blue, id) %>%
		summarise(Avg_Pixels = n()/(dim(image.jpeg)[1] * dim(image.jpeg)[2])) %>% 
		left_join(df.gamegenres) %>%
		select(-id) #merge colors to the game genres

	# 2.4.4 merge and add pixels to the global data frame 
	df.colors_genre = 
		df.Avg_Colors %>%
		right_join(df.colors_genre) %>% #merge to the global data frame 
		mutate(Tot_Avg_Pixels = Tot_Avg_Pixels + ifelse(is.na(Avg_Pixels), 0, Avg_Pixels)) %>% #Add pixels to the total pixels
		select(-Avg_Pixels)

}	

# 2.5 Extract Hue, Saturation and Value from the genre data frame
df.colors_genre2 =  
	df.colors_genre %>%
	mutate(
		Hue = rgb2hsv(Red, Green, Blue, maxColorValue = 63)[1,],
		Saturation = rgb2hsv(Red, Green, Blue, maxColorValue = 63)[2,],
		Value = rgb2hsv(Red, Green, Blue, maxColorValue = 63)[3,],
		color = rgb(Red/63, Green/63, Blue/63)
	)

# 2.6 Count the number of games by genre
table_genres = table(df.gamegenres$genre)



	
	
	
	
	
	
	
	
	
	
	
##############################################
############### 3 Hue Analysis ###############
##############################################

# 3.1 Estimate the Hue density function for all the covers combined
model.AllDataDensityfit_Hue = 
	locfit(
		~lp(Hue),
		data = df.colors_genre2 %>% filter(Saturation > 0.2, Value > 0.2), 
		weights = Tot_Avg_Pixels, 
		xlim = c(0,1)
	)

# 3.2 Put the estimated density function in a data frame format
df.AllDataDensity_Hue = 
	data.frame(Hue = (0:800)/800) %>%
	mutate(densityfit_Hue = predict(model.AllDataDensityfit_Hue, Hue))
	
# 3.3 Initialize the data frame for the estimated Hue density function covers by genre
df.DataDensity_Hue.Appended = data.frame()
	
# 3.4 Iterate through all genres
for(genre_select in names(table_genres)[table_genres>20]){

	# 3.4.1 Estimate the Hue density function for the genre
	model.DataDensityfit_Hue = 
		locfit(
			~lp(Hue),
			data = df.colors_genre2 %>% filter(genre == genre_select, Saturation > 0.2, Value > 0.2),
			weights = Tot_Avg_Pixels, 
			xlim = c(0,1)
		)

	# 3.4.2 Merge the Hilbert curve to the color data frame
	df.hilbert2_Hue = 
		df.colors_genre2 %>% 
		filter(genre == genre_select) %>% #Filter by genre
		filter(Tot_Avg_Pixels > 0) %>% # Filter colors that are not used
		filter(Saturation > 0.2) %>% #filter unsaturated whitish colors
		filter(Value > 0.2) %>% #filter colors that are too dark
		group_by(Hue) %>%
		mutate(x = round(1 + Saturation * max(df.hilbert$x-1)), y = round(1 + Value * max(df.hilbert$y-1))) %>%
		left_join(df.hilbert, by = c('x', 'y'))

	# 3.4.3 Create the data frame to sample from for the density plot
	df.Color_to_sample_Hue =
		df.hilbert2_Hue %>%
		select(Hue, Hue, color, Tot_Avg_Pixels, order) %>%
		group_by(Hue) %>%
		nest()
		



	# 3.4.4 Create the density plot data frame
	df.DataDensity_Hue = 
		data.frame(Hue = (0:400)/400) %>% # 400 pixel wide
		mutate(densityfit_Hue = predict(model.DataDensityfit_Hue, Hue)) %>%
		rowwise() %>%
		mutate(
			Closest_Hue = df.Color_to_sample_Hue$Hue[which.min(abs(df.Color_to_sample_Hue$Hue-Hue))],
			height = round(densityfit_Hue * 20) # Around 100 pixel high
		) %>%
		left_join(df.Color_to_sample_Hue, by = c('Closest_Hue' = 'Hue')) %>%
		group_by(Hue) %>%
		mutate(Sample = purrr::map2(data, height, sample_n, weight = Tot_Avg_Pixels, replace = TRUE)) %>% #sample in each nest
		unnest(Sample) %>%
		group_by(Hue) %>%
		mutate(order2 = rank(order, ties.method = 'first')/20 )

	# 3.4.5 Append the density data frame for the combined visualization
	df.DataDensity_Hue.Appended = 
		df.DataDensity_Hue %>% 
		mutate(
			Type = 'Hue',
			Genre = genre_select
		) %>%
		bind_rows(df.DataDensity_Hue.Appended)

	# 3.4.6 Graph the Hue	
	ggplot()+
		geom_raster(data = df.DataDensity_Hue, aes(x = Hue, y = order2, fill = color))+ #Density of the genre
		geom_line(data = df.AllDataDensity_Hue, aes(x = Hue, y = densityfit_Hue), linetype = 'twodash', color = 'black', size = 1)+ #Density of all genres in a line
		scale_fill_identity(guide=FALSE) + #direct color from the data base
		ylab("Density")+
		coord_fixed(ratio = 1/20) + 
		scale_y_continuous(expand = c(0,0), limits = c(0,10))+
		scale_x_continuous(expand = c(0,0), breaks = (0:5)/5)+
		theme(
			axis.text.y = element_blank(),
			axis.ticks.y = element_blank(),
			panel.grid = element_line(linetype = 'dashed'),
			panel.border = element_rect(color = 'black', fill = NA)
		)
		
	# 3.4.7 Save the Graph in a folder 	
	ggsave(paste0('DensityPlotHue', genre_select, '.png'), width = 885/96, height = 455/96, dpi = 96, path = '.\\Density Plot')
}	
	
	



	
	
	
	
	
	
	
	
	
	
	
#####################################################
############### 4 Saturation Analysis ###############
#####################################################

# 4.1 Estimate the Saturation density function for all the covers combined
model.AllDataDensityfit_Saturation = 
	locfit(
		~lp(Saturation),
		data = df.colors_genre2,
		weights = Tot_Avg_Pixels, xlim = c(0,1)
	)

# 4.2 Put the estimated density function in a data frame format
df.AllDataDensity_Saturation = 
	data.frame(Saturation = (0:800)/800) %>%
	mutate(densityfit_Saturation = predict(model.AllDataDensityfit_Saturation, Saturation))

# 4.3 Initialize the data frame for the estimated Hue density function covers by genre
df.DataDensity_Saturation.Appended = data.frame()
	
# 4.4 Iterate through all genres
for(genre_select in names(table_genres)[table_genres>20]){

	# 4.4.1 Estimate the Hue density function for the genre
	model.DataDensityfit_Saturation = 
		locfit(
			~lp(Saturation),
			data = df.colors_genre2 %>% filter(genre == genre_select),
			weights = Tot_Avg_Pixels, 
			xlim = c(0,1)
		)

	# 4.4.2 Merge the Hilbert curve to the color data frame
	df.hilbert2_Saturation = 
		df.colors_genre2 %>% 
		filter(genre == genre_select) %>% #Filter by genre
		filter(Tot_Avg_Pixels > 0) %>% # Filter colors that are not used
		group_by(Saturation) %>%
		mutate(x = round(1 + Hue * max(df.hilbert$x-1)), y = round(1 + Value * max(df.hilbert$y-1))) %>%
		left_join(df.hilbert, by = c('x', 'y'))

	# 4.4.3 Create the data frame to sample from for the density plot
	df.Color_to_sample_Saturation =
		df.hilbert2_Saturation %>%
		select(Saturation, Saturation, color, Tot_Avg_Pixels, order) %>%
		group_by(Saturation) %>%
		nest()	
		


	# 4.4.4 Create the density plot data frame
	df.DataDensity_Saturation = 
		data.frame(Saturation = (0:400)/400) %>% # 400 pixel wide
		mutate(densityfit_Saturation = predict(model.DataDensityfit_Saturation, Saturation)) %>%
		rowwise() %>%
		mutate(
			Closest_Saturation = df.Color_to_sample_Saturation$Saturation[which.min(abs(df.Color_to_sample_Saturation$Saturation-Saturation))],
			height = round(densityfit_Saturation * 20)# Around 100 pixel high
		) %>%
		left_join(df.Color_to_sample_Saturation, by = c('Closest_Saturation' = 'Saturation')) %>%
		group_by(Saturation) %>%
		mutate(Sample = purrr::map2(data, height, sample_n, weight = Tot_Avg_Pixels, replace = TRUE)) %>%
		unnest(Sample) %>%
		group_by(Saturation) %>%
		mutate(order2 = rank(order, ties.method = 'first')/20 )

	# 4.4.5 Append the density data frame for the combined visualization
	df.DataDensity_Saturation.Appended = 
		df.DataDensity_Saturation %>% 
		mutate(
			Type = 'Saturation',
			Genre = genre_select
		) %>%
		bind_rows(df.DataDensity_Saturation.Appended)		
		
	# 4.4.6 Graph the Saturation	
	ggplot()+
		geom_raster(data = df.DataDensity_Saturation, aes(x = Saturation, y = order2, fill = color))+ #Density of the genre
		geom_line(data = df.AllDataDensity_Saturation, aes(x = Saturation, y = densityfit_Saturation), linetype = 'twodash', color = 'black', size = 1)+ #Density of all genres in a line
		scale_fill_identity(guide=FALSE) + #direct color from the data base
		ylab("Density")+
		coord_fixed(ratio = 1/20) + 
		scale_y_continuous(expand = c(0,0), limits = c(0,10))+
		scale_x_continuous(expand = c(0,0), breaks = (0:5)/5)+
		theme(
			axis.text.y = element_blank(),
			axis.ticks.y = element_blank(),
			panel.grid = element_line(linetype = 'dashed'),
			panel.border = element_rect(color = 'black', fill = NA)
		)

	# 4.4.7 Save the Graph in a folder 		
	ggsave(paste0('DensityPlotSaturation', genre_select, '.png'), width = 885/96, height = 455/96, dpi = 96, path = '.\\Density Plot')
}










#############################################################
############### 5 Value (Brightness) Analysis ###############
#############################################################

# 5.1 Estimate the Saturation density function for all the covers combined
model.AllDataDensityfit_Value = 
	locfit(
		~lp(Value), 
		data = df.colors_genre2,
		weights = Tot_Avg_Pixels, 
		xlim = c(0,1)
	)

# 5.2 Put the estimated density function in a data frame format
df.AllDataDensity_Value = 
	data.frame(Value = (0:800)/800) %>%
	mutate(densityfit_Value = predict(model.AllDataDensityfit_Value, Value))

# 5.3 Initialize the data frame for the estimated Hue density function covers by genre
df.DataDensity_Value.Appended = data.frame()	

# 5.4 Iterate through all genres
for(genre_select in names(table_genres)[table_genres>20]){ #only genres with more than 20 games will be analyzed

	# 5.4.1 Estimate the Value density function for the genre
	model.DataDensityfit_Value = 
		locfit(
			~lp(Value),
			data = df.colors_genre2 %>% filter(genre == genre_select),
			weights = Tot_Avg_Pixels,
			xlim = c(0,1)
		)

	# 5.4.2 Merge the Hilbert curve to the color data frame
	df.hilbert2_Value = 
		df.colors_genre2 %>% 
		filter(genre == genre_select) %>% #Filter by genre
		filter(Tot_Avg_Pixels > 0) %>% # Filter colors that are not used
		group_by(Value) %>%
		mutate(x = round(1 + Hue * max(df.hilbert$x-1)), y = round(1 + Saturation * max(df.hilbert$y-1))) %>%
		left_join(df.hilbert, by = c('x', 'y'))

	# 5.4.3 Create the data frame to sample from for the density plot	
	df.Color_to_sample_Value =
		df.hilbert2_Value %>%
		select(Value, Value, color, Tot_Avg_Pixels, order) %>%
		group_by(Value) %>%
		nest()	
		

	# 5.4.4 Create the density plot data frame
	df.DataDensity_Value = 
		data.frame(Value = (0:400)/400) %>% # 400 pixel wide
		mutate(densityfit_Value = predict(model.DataDensityfit_Value, Value)) %>%
		rowwise() %>%
		mutate(
			Closest_Value = df.Color_to_sample_Value$Value[which.min(abs(df.Color_to_sample_Value$Value-Value))],
			height = round(densityfit_Value * 20)# Around 100 pixel high maxmimum
		) %>%
		left_join(df.Color_to_sample_Value, by = c('Closest_Value' = 'Value')) %>%
		group_by(Value) %>%
		mutate(Sample = purrr::map2(data, height, sample_n, weight = Tot_Avg_Pixels, replace = TRUE)) %>%
		unnest(Sample) %>%
		group_by(Value) %>%
		mutate(order2 = rank(order, ties.method = 'first')/20 )

	# 5.4.5 Append the density data frame for the combined visualization
	df.DataDensity_Value.Appended = 
		df.DataDensity_Value %>% 
		mutate(
			Type = 'Value (Brightness)',
			Genre = genre_select
			) %>%
		bind_rows(df.DataDensity_Value.Appended)	

	# 5.4.6 Graph the Value		
	ggplot()+
		geom_raster(data = df.DataDensity_Value, aes(x = Value, y = order2, fill = color))+ #Density of the genre
		geom_line(data = df.AllDataDensity_Value, aes(x = Value, y = densityfit_Value), linetype = 'twodash', color = 'grey40', size = 1)+ #Density of all genres in a line
		scale_fill_identity(guide=FALSE) + #direct color from the data base
		ylab("Density")+
		coord_fixed(ratio = 1/20) + 
		scale_y_continuous(expand = c(0,0), limits = c(0,10))+
		scale_x_continuous(expand = c(0,0), breaks = (0:5)/5)+
		theme(
			axis.text.y = element_blank(),
			axis.ticks.y = element_blank(),
			panel.grid = element_line(linetype = 'dashed'),
			panel.border = element_rect(color = 'black', fill = NA)
		)

	# 5.4.7 Save the Graph in a folder 			
	ggsave(paste0('DensityPlotValue', genre_select, '.png'), width = 885/96, height = 455/96, dpi = 96, path = '.\\Density Plot')
}














#################################################################
############### 6 Hue Saturation Value altogether ###############
#################################################################

# 6.1 Graph all genres and graphs together
ggplot()+
	geom_raster(data = df.DataDensity_Hue.Appended, aes(x = Hue, y = order2, fill = color)) + 
	geom_raster(data = df.DataDensity_Saturation.Appended, aes(x = Saturation, y = order2, fill = color))+
	geom_raster(data = df.DataDensity_Value.Appended, aes(x = Value, y = order2, fill = color)) + 
	geom_line(data = df.AllDataDensity_Hue %>% mutate(Type = 'Hue'), aes(x = Hue, y = densityfit_Hue), linetype = 'twodash', color = 'black', size = 1)+
	geom_line(data = df.AllDataDensity_Saturation %>% mutate(Type = 'Saturation'), aes(x = Saturation, y = densityfit_Saturation), linetype = 'twodash', color = 'black', size = 1)+
	geom_line(data = df.AllDataDensity_Value %>% mutate(Type = 'Value (Brightness)'), aes(x = Value, y = densityfit_Value), linetype = 'twodash', color = 'grey40', size = 1)+
	scale_fill_identity(guide=FALSE) + 
	facet_grid(Genre ~ Type)+
	ylab("Density")+
	coord_fixed(ratio = 1/20) + 
	scale_y_continuous(expand = c(0,0), limits = c(0,10))+
	scale_x_continuous(expand = c(0,0), breaks = (0:5)/5, name = 'Amount')+
	ggtitle('HSV Density Plots\nfor PlayStation1 Game Covers\nUsing Sampled Colors ')+
	theme(
		plot.title = element_text(size=13, face="bold.italic"),
		axis.text.y = element_blank(),
		axis.ticks.y = element_blank(),
		panel.grid = element_line(linetype = 'dashed'),
		panel.border = element_rect(color = 'black', fill = NA)
	)
	
# 6.2 Save the Graph in a file 	
ggsave('DensityPlotAll.png', width = 885/96*1.55, height = 455/96*6.05, dpi = 96, limitsize = FALSE)	

# 6.3 Iterate through all genres
for(genre_select in names(table_genres)[table_genres>20]){ #only genres with more than 20 games will be analyzed

# 6.3.1 Graph all genres and graphs together
ggplot()+
	geom_raster(data = df.DataDensity_Hue.Appended %>% filter(Genre == genre_select), aes(x = Hue, y = order2, fill = color)) + 
	geom_raster(data = df.DataDensity_Saturation.Appended %>% filter(Genre == genre_select), aes(x = Saturation, y = order2, fill = color))+
	geom_raster(data = df.DataDensity_Value.Appended %>% filter(Genre == genre_select), aes(x = Value, y = order2, fill = color)) + 
	geom_line(data = df.AllDataDensity_Hue %>% mutate(Type = 'Hue'), aes(x = Hue, y = densityfit_Hue), linetype = 'twodash', color = 'black', size = 1)+
	geom_line(data = df.AllDataDensity_Saturation %>% mutate(Type = 'Saturation'), aes(x = Saturation, y = densityfit_Saturation), linetype = 'twodash', color = 'black', size = 1)+
	geom_line(data = df.AllDataDensity_Value %>% mutate(Type = 'Value (Brightness)'), aes(x = Value, y = densityfit_Value), linetype = 'twodash', color = 'grey40', size = 1)+
	scale_fill_identity(guide=FALSE) + 
	facet_grid(Type ~ .)+
	ylab("Density")+
	coord_fixed(ratio = 1/20) + 
	scale_y_continuous(expand = c(0,0), limits = c(0,10))+
	scale_x_continuous(expand = c(0,0), breaks = (0:5)/5, name = 'Amount')+
	ggtitle(paste0('HSV Density Plots\nfor PlayStation1 ', genre_select,' Games\nUsing Sampled Colors from their Covers'))+
	theme(
		plot.title = element_text(size=13, face="bold.italic"),
		axis.text.y = element_blank(),
		axis.ticks.y = element_blank(),
		panel.grid = element_line(linetype = 'dashed'),
		panel.border = element_rect(color = 'black', fill = NA)
	)
	
# 6.3.2 Save the Graph in a file 	
ggsave(paste0('DensityPlotAll', genre_select, '.png'), width = 885/96*1.1, height = 455/96*3.1, dpi = 96, limitsize = FALSE)	
}

##############################################################
############### 7 TSNE Representation by genre ###############
##############################################################

# 7.1 Run the TSNE algorithm
tsne_out = 
	df.colors[,-1:-3] %>%	#Remove the colors form the data frame
	{t(. + runif(ncol(.))/10000000)} %>% #add little variations because equal values crash TSNE
	Rtsne(pca_center = FALSE) # TSNE algorithm

# 7.2 Put the TSNE result in a data frame
df.tsne = 
	data.frame(tsne_out$Y) %>% 
	mutate(
		name = colnames(df.colors)[-1:-3], #re-add the file names
		id = gsub('\\.jpg', '', name)
	) %>%
	inner_join(df.gamegenres) %>%
	arrange(-as.numeric(id)) %>% #more popular games have low ids most often
	mutate(
		imagename = paste0('./CoverCropped/', name),
		imagepath = factor(imagename, levels = unique(imagename)) 
	)

# 7.3 Iterate through all genres
for(genre_select in names(table_genres)[table_genres>20]){ #only genres with more than 20 games will be analyzed

	# 7.3.1 Graph the results for all genres
	ggplot() + 
		geom_image(data = df.tsne %>% filter(genre == genre_select), aes(x = X1, y = X2, image = imagepath),size=.1)+
		theme_void()
		
	# 7.3.2 Save the Graph in a file 	
	ggsave(paste0('./Tsne Plot/tsne_',genre_select ,'.png'), width = 8.72, height = 8, dpi = 96, limitsize = FALSE)			
}

# 7.4.1 Graph the results for all games together
ggplot(df.tsne, aes(x = X1, y = X2)) + 
	geom_image(data = df.tsne, aes(x = X1, y = X2, image = imagepath),size=.1)+
	theme_void()
	
# 7.4.2 Save the Graph in a file 
ggsave(paste0('./Tsne Plot/tsne_ALL.png'), width = 8.72, height = 8, dpi = 96, limitsize = FALSE)	

# 7.5.1 Much bigger version
ggplot() + 
	geom_image(data = df.tsne, aes(x = X1, y = X2, image = imagepath),size=.02)+
	theme_void()
		
# 7.5.2 Save the Graph in a file 		
ggsave(paste0('./Tsne Plot/tsne_ALL_BIG.png'), width = 8.72*10, height = 8*10, dpi = 96, limitsize = FALSE)	
	