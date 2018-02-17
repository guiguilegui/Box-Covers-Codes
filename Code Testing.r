library(tidyr)
library(dplyr)
library(RSelenium)
library(magrittr)
library(XML)
library(jpeg)
library(ggplot2)
library(colorspace)
library(ggimage)
library(Rtsne)
# library(class)
library(kknn)

options(stringsAsFactors = FALSE)
setwd('C:/Users/Guillaume/Documents/Box Covers')

cprof <- getChromeProfile("C:\\Users\\Guillaume\\AppData\\Local\\Google\\Chrome\\User Data", "Default")
rD <- rsDriver(extraCapabilities = cprof, port = 4444L)

remDr <- rD$client

df.artwork = data.frame()

for(i in 1:28){
	paste0("http://game-oldies.com/play-retro-games/sony-playstation?qt-brows=&page=", i) %>%
	remDr$navigate()
	Sys.sleep(30)
	page_tree.Current = 
		remDr$getPageSource()[[1]] %>%
		htmlParse()
		
	df.artwork = 
		data.frame(
			Name = xpathSApply(page_tree.Current,'//div[@id="quicktabs-tabpage-browse_games_qt-0"]//td[@class="views-field views-field-field-logo"]/div/a', xmlValue),
			LogoUrl = 			
				xpathSApply(
					page_tree.Current,
					'//div[@id="quicktabs-tabpage-browse_games_qt-0"]//td[@class="views-field views-field-field-logo"]',
					function(x) {
						if (xpathSApply(x, "boolean(.//img)")) {
							xpathSApply(x, ".//img", xmlGetAttr, 'src', xmlValue)
						} else {
							NA
						}
					}
				),
			CoverUrl = 			
				xpathSApply(
					page_tree.Current,
					'//div[@id="quicktabs-tabpage-browse_games_qt-0"]//td[@class="views-field views-field-field-packshot views-align-center"]',
					function(x) {
						if (xpathSApply(x, "boolean(.//img)")) {
							xpathSApply(x, ".//img", xmlGetAttr, 'src', xmlValue)
						} else {
							NA
						}
					}
				),
			TitleUrl = 
				xpathSApply(
					page_tree.Current,
					'//div[@id="quicktabs-tabpage-browse_games_qt-0"]//td[@class="views-field views-field-field-title views-align-center"]',
					function(x) {
						if (xpathSApply(x, "boolean(.//img)")) {
							xpathSApply(x, ".//img", xmlGetAttr, 'src', xmlValue)
						} else {
							NA
						}
					}
				),
			IngameUrl = 			
				xpathSApply(
					page_tree.Current,
					'//div[@id="quicktabs-tabpage-browse_games_qt-0"]//td[@class="views-field views-field-field-ingame views-align-center"]',
					function(x) {
						if (xpathSApply(x, "boolean(.//img)")) {
							xpathSApply(x, ".//img", xmlGetAttr, 'src', xmlValue)
						} else {
							NA
						}
					}
				)
		) %>%
		bind_rows(df.artwork, .)
}

# for(i in 1:nrow(df.artwork)){
for(i in 1:nrow(df.artwork)){
	if(!is.na(df.artwork$LogoUrl[i]) )	download.file(df.artwork$LogoUrl[i], 	paste0('.\\Logo\\', df.artwork$Name[i], '_logo.jpg'), mode = 'wb')
	Sys.sleep(1)
	if(!is.na(df.artwork$CoverUrl[i]))	download.file(df.artwork$CoverUrl[i], 	paste0('.\\Cover\\', df.artwork$Name[i], '_cover.jpg'), mode = 'wb')
	Sys.sleep(1)
	if(!is.na(df.artwork$TitleUrl[i]))	download.file(df.artwork$TitleUrl[i], 	paste0('.\\Title\\', df.artwork$Name[i], '_title.png'), mode = 'wb')
	Sys.sleep(1)
	if(!is.na(df.artwork$IngameUrl[i])) download.file(df.artwork$IngameUrl[i], 	paste0('.\\Ingame\\', df.artwork$Name[i], '_ingame.png'), mode = 'wb')
	Sys.sleep(10)
}

df.ps1_list = 
	xmlParse("http://thegamesdb.net/api/PlatformGames.php?platform=sony+playstation") %>%
	xmlToList() %>%
	data.table::rbindlist(fill=TRUE)


for(i in 1464:nrow(df.ps1_list)){

	if(!is.na(df.ps1_list$thumb[i]))	download.file(paste0('http://thegamesdb.net/banners/_gameviewcache/',df.ps1_list$thumb[i]), 	paste0('.\\Cover2\\', df.ps1_list$id[i], '.jpg'), mode = 'wb')

	Sys.sleep(2)

}	

# FOR /R %i IN (*.jpg) DO (convert "%i" -gravity east -crop 85x100%  +repage -gravity north -crop 100x82%   ./../CoverCropped/%~ni%~xi)





		

list.covercropped = list.files('.\\CoverCropped')

df.gamedetails = data.frame()
df.gamegenres = data.frame()

# for(i in 1:length(list.covercropped)){
for(i in 1:length(list.covercropped)){

xmlextract = 
	list.covercropped[i] %>%
	gsub('\\.jpg$', '', .) %>%
	paste0('http://thegamesdb.net/api/GetGame.php?id=', .) %>%
	xmlParse()

df.gamedetails = 
	data.frame(
		id = xpathSApply(xmlextract, '/Data/Game/id', xmlValue) %>% {if(length(.)==0){NA}else{.}},
		GameTitle = xpathSApply(xmlextract, '//GameTitle', xmlValue) %>% {if(length(.)==0){NA}else{.}},
		ReleaseDate = xpathSApply(xmlextract, '//ReleaseDate', xmlValue) %>% {if(length(.)==0){NA}else{as.Date(., '%m/%d/%Y')}},
		Overview = xpathSApply(xmlextract, '//Overview', xmlValue) %>% {if(length(.)==0){NA}else{.}},
		ESRB = xpathSApply(xmlextract, '//ESRB', xmlValue) %>% {if(length(.)==0){NA}else{.}},
		Publisher = xpathSApply(xmlextract, '//Publisher', xmlValue) %>% {if(length(.)==0){NA}else{.}},
		Developer = xpathSApply(xmlextract, '//Developer', xmlValue) %>% {if(length(.)==0){NA}else{.}},
		Rating = xpathSApply(xmlextract, '//Rating', xmlValue) %>% {if(length(.)==0){NA}else{.}}
	) %>%
	bind_rows(df.gamedetails, .)

df.gamegenres = 
	data.frame(
		id = xpathSApply(xmlextract, '/Data/Game/id', xmlValue),
		genre = xpathSApply(xmlextract, '//genre', xmlValue)  %>% {if(length(.)==0){NA}else{.}}
	) %>%
	bind_rows(df.gamegenres, .)
	
	print(i)
	Sys.sleep(3)
}







	
# df.image = 
	# data.frame(
		# Red = image.jpeg[,,1] %>% as.vector,
		# Green = image.jpeg[,,2] %>% as.vector,
		# Blue = image.jpeg[,,3] %>% as.vector,
		# Hue = image.hsv[1,],
		# Saturation = image.hsv[2,],
		# Value = image.hsv[3,]
	# ) %>%
	# mutate(color = rgb(Red, Green, Blue)) %>%
	# group_by(color, Red, Green, Blue, Hue, Saturation, Value) %>%
	# summarise(Count = n()) 



list.cover = list.files('.\\CoverCropped')
df.colors = expand.grid(Red = 0:3, Green = 0:3, Blue = 0:3)

list.genres = 	
	df.gamegenres$genre %>%
	unique() %>%
	{ifelse(is.na(.), 'Other', .)}
	
df.colors_genre = 
	expand.grid(Red = 0:31, Green = 0:31, Blue = 0:31, genre = list.genres) %>%
	mutate(Tot_Avg_Pixels = 0)
	

	
for(covername in list.cover){
# for(covername in list.cover[1:200]){
	image.jpeg = 
		paste0('.\\CoverCropped\\', covername) %>%
		readJPEG()
		
	# image.hsv = rgb2hsv(image.jpeg[,,1] %>% as.vector,image.jpeg[,,2] %>% as.vector,image.jpeg[,,3] %>% as.vector)	
	
	df.colors = 
		data.frame(
			Red = (image.jpeg[,,1]*255/64) %>% as.vector %>% floor,
			Green = (image.jpeg[,,2]*255/64) %>% as.vector %>% floor,
			Blue = (image.jpeg[,,3]*255/64) %>% as.vector %>% floor
		) %>%
		group_by(Red, Green, Blue) %>%
		summarise(count = n()) %>%
		ungroup() %>%
		mutate(prop = count/sum(count)) %>%
		right_join(df.colors) %>%
		mutate(!!covername := ifelse(is.na(prop), 0, prop)) %>%
		select(-count, -prop)
		
	df.Avg_Colors = 
		data.frame(
			Red = (image.jpeg[,,1]*255/8) %>% as.vector %>% floor,
			Green = (image.jpeg[,,2]*255/8) %>% as.vector %>% floor,
			Blue = (image.jpeg[,,3]*255/8) %>% as.vector %>% floor,
			id = gsub('\\.jpg', '', covername)
		) %>%
		group_by(Red, Green, Blue, id) %>%
		summarise(Avg_Pixels = n()/(dim(image.jpeg)[1] * dim(image.jpeg)[2])) %>% 
		left_join(df.gamegenres) %>%
		select(-id)
		
	df.colors_genre = 
		df.Avg_Colors %>%
		right_join(df.colors_genre) %>%
		# head()
		# rowwise() %>%
		mutate(Tot_Avg_Pixels = Tot_Avg_Pixels + ifelse(is.na(Avg_Pixels), 0, Avg_Pixels)) %>%
		select(-Avg_Pixels)

}	


df.colors_genre2 =  
	df.colors_genre %>%
	mutate(
		Hue = rgb2hsv(Red, Green, Blue, maxColorValue = 31)[1,],
		Saturation = rgb2hsv(Red, Green, Blue, maxColorValue = 31)[2,],
		Value = rgb2hsv(Red, Green, Blue, maxColorValue = 31)[3,],
		color = rgb(Red/31, Green/31, Blue/31)
	)

library(reshape2)
# library(fuzzyjoin)
# library(logspline)
library(locfit)


#https://www.r-bloggers.com/going-bananas-with-hilbert/
hilbert = function(m,n,r) {
  for (i in 1:n)
  {
    tmp=cbind(t(m), m+nrow(m)^2)
    m=rbind(tmp, (2*nrow(m))^r-tmp[nrow(m):1,]+1)
  }
  melt(m) %>% plyr::rename(c("Var1" = "x", "Var2" = "y", "value"="order")) %>% arrange(order)
}
  
  
# max.Value = max(df.colors_genre2$Value)
# max.Saturation = max(df.colors_genre2$Saturation)
# max.Hue = max(df.colors_genre2$Hue)  

max.Value = 1
max.Saturation = 1
max.Hue = 1
  
# genre_select = 'Sports'

df.hilbert = hilbert(m=matrix(1), n=5, r=2)

table_genres = table(df.gamegenres$genre)

model.AllDataDensityfit_Value = locfit(~lp(Value), data = df.colors_genre2, weights = Tot_Avg_Pixels, xlim = c(0,1))
df.AllDataDensity_Value = 
	data.frame(Value = (0:800)/800) %>%
	mutate(densityfit_Value = predict(model.AllDataDensityfit_Value, Value))
	
for(genre_select in names(table_genres)[table_genres>20]){
nb.genre = sum(df.gamegenres$genre == genre_select, na.rm = TRUE)
	
model.DataDensityfit_Value = locfit(~lp(Value), data = df.colors_genre2 %>% filter(genre == genre_select), weights = Tot_Avg_Pixels, xlim = c(0,1))

df.hilbert2_Value = 
	df.colors_genre2 %>% 
	filter(genre == genre_select) %>%
	filter(Tot_Avg_Pixels > 0) %>%
	# head(7) %>%
	group_by(Value) %>%
	mutate(Avg_Pixels_Value = sum(Tot_Avg_Pixels) / nb.genre) %>% 
	mutate(x = round(1 + Hue/max.Hue * max(df.hilbert$x-1)), y = round(1 + Saturation/max.Saturation * max(df.hilbert$y-1))) %>%
	left_join(df.hilbert, by = c('x', 'y'))# %>% 
	# group_by(x.x, y.x) %>%
	# filter(rank(order, ties.method = 'first') == 1) %>%
	# ungroup() %>%
	# select(-x.x, -y.x, -x.y, -y.y)


df.Color_to_sample_Value =
	df.hilbert2_Value %>%
	select(Value, Value, color, Tot_Avg_Pixels, order) %>%
	group_by(Value) %>%
	# mutate(graph_height = floor(Avg_Pixels_Value/max(df.hilbert2$Avg_Pixels_Value) * 200)) %>%
	nest()	
	



df.DataDensity_Value = 
	data.frame(Value = (0:400)/400) %>%
	mutate(densityfit_Value = predict(model.DataDensityfit_Value, Value)) %>%
	rowwise() %>%
	mutate(
		Closest_Value = df.Color_to_sample_Value$Value[which.min(abs(df.Color_to_sample_Value$Value-Value))],
		height = floor(densityfit_Value * 20)
	) %>%
	left_join(df.Color_to_sample_Value, by = c('Closest_Value' = 'Value')) %>%
	group_by(Value) %>%
	mutate(Sample = purrr::map2(data, height, sample_n, weight = Tot_Avg_Pixels, replace = TRUE)) %>%
	unnest(Sample) %>%
	group_by(Value) %>%
	mutate(order2 = rank(order, ties.method = 'first')/20 )

		
ggplot()+
	geom_raster(data = df.DataDensity_Value, aes(x = Value, y = order2, fill = color))+
	geom_line(data = df.AllDataDensity_Value, aes(x = Value, y = densityfit_Value), linetype = 'twodash', color = 'grey40', size = 1)+
	# scale_color_continuous(
	scale_fill_identity(guide=FALSE) +
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
	
# ggsave(paste0('DensityPlotValue', genre_select, '.png'), dpi = 72)
ggsave(paste0('DensityPlotValue', genre_select, '.png'), width = 885/96, height = 455/96, dpi = 96, path = '.\\Density Plot')
}

model.AllDataDensityfit_Saturation = locfit(~lp(Saturation), data = df.colors_genre2, weights = Tot_Avg_Pixels, xlim = c(0,1))
df.AllDataDensity_Saturation = 
	data.frame(Saturation = (0:800)/800) %>%
	mutate(densityfit_Saturation = predict(model.AllDataDensityfit_Saturation, Saturation))
	
for(genre_select in names(table_genres)[table_genres>20]){
nb.genre = sum(df.gamegenres$genre == genre_select, na.rm = TRUE)
	
model.DataDensityfit_Saturation = locfit(~lp(Saturation), data = df.colors_genre2 %>% filter(genre == genre_select), weights = Tot_Avg_Pixels, xlim = c(0,1))

df.hilbert2_Saturation = 
	df.colors_genre2 %>% 
	filter(genre == genre_select) %>%
	filter(Tot_Avg_Pixels > 0) %>%
	# head(7) %>%
	group_by(Saturation) %>%
	mutate(Avg_Pixels_Saturation = sum(Tot_Avg_Pixels) / nb.genre) %>% 
	mutate(x = round(1 + Hue/max.Hue * max(df.hilbert$x-1)), y = round(1 + Value/max.Value * max(df.hilbert$y-1))) %>%
	left_join(df.hilbert, by = c('x', 'y'))# %>% 
	# group_by(x.x, y.x) %>%
	# filter(rank(order, ties.method = 'first') == 1) %>%
	# ungroup() %>%
	# select(-x.x, -y.x, -x.y, -y.y)


df.Color_to_sample_Saturation =
	df.hilbert2_Saturation %>%
	select(Saturation, Saturation, color, Tot_Avg_Pixels, order) %>%
	group_by(Saturation) %>%
	nest()	
	



df.DataDensity_Saturation = 
	data.frame(Saturation = (0:400)/400) %>%
	mutate(densityfit_Saturation = predict(model.DataDensityfit_Saturation, Saturation)) %>%
	rowwise() %>%
	mutate(
		Closest_Saturation = df.Color_to_sample_Saturation$Saturation[which.min(abs(df.Color_to_sample_Saturation$Saturation-Saturation))],
		height = floor(densityfit_Saturation * 40)
	) %>%
	left_join(df.Color_to_sample_Saturation, by = c('Closest_Saturation' = 'Saturation')) %>%
	group_by(Saturation) %>%
	mutate(Sample = purrr::map2(data, height, sample_n, weight = Tot_Avg_Pixels, replace = TRUE)) %>%
	unnest(Sample) %>%
	group_by(Saturation) %>%
	mutate(order2 = rank(order, ties.method = 'first')/40 )

		
ggplot()+
	geom_raster(data = df.DataDensity_Saturation, aes(x = Saturation, y = order2, fill = color))+
	geom_line(data = df.AllDataDensity_Saturation, aes(x = Saturation, y = densityfit_Saturation), linetype = 'twodash', color = 'grey10', size = 1)+
	# scale_color_continuous(
	scale_fill_identity(guide=FALSE) +
	ylab("Density")+
	coord_fixed(ratio = 1/10) + 
	scale_y_continuous(expand = c(0,0), limits = c(0,5))+
	scale_x_continuous(expand = c(0,0), breaks = (0:5)/5)+
	theme(
		axis.text.y = element_blank(),
		axis.ticks.y = element_blank(),
		panel.grid = element_line(linetype = 'dashed'),
		panel.border = element_rect(color = 'black', fill = NA)
	)
	
ggsave(paste0('DensityPlotSaturation', genre_select, '.png'), width = 885/96, height = 455/96, dpi = 96, path = '.\\Density Plot')
}


model.AllDataDensityfit_Hue = locfit(~lp(Hue), data = df.colors_genre2 %>% filter(Saturation > 0.2, Value > 0.2), weights = Tot_Avg_Pixels, xlim = c(0,1))
df.AllDataDensity_Hue = 
	data.frame(Hue = (0:800)/800) %>%
	mutate(densityfit_Hue = predict(model.AllDataDensityfit_Hue, Hue))
	
for(genre_select in names(table_genres)[table_genres>20]){
nb.genre = sum(df.gamegenres$genre == genre_select, na.rm = TRUE)
	
model.DataDensityfit_Hue = locfit(~lp(Hue), data = df.colors_genre2 %>% filter(genre == genre_select, Saturation > 0.2, Value > 0.2), weights = Tot_Avg_Pixels, xlim = c(0,1))

df.hilbert2_Hue = 
	df.colors_genre2 %>% 
	filter(genre == genre_select) %>%
	filter(Tot_Avg_Pixels > 0) %>%
	filter( Saturation > 0.2) %>%
	filter( Value > 0.2) %>%
	
	# head(7) %>%
	group_by(Hue) %>%
	mutate(Avg_Pixels_Hue = sum(Tot_Avg_Pixels) / nb.genre) %>% 
	mutate(x = round(1 + Saturation/max.Saturation * max(df.hilbert$x-1)), y = round(1 + Value/max.Value * max(df.hilbert$y-1))) %>%
	left_join(df.hilbert, by = c('x', 'y'))# %>% 
	# group_by(x.x, y.x) %>%
	# filter(rank(order, ties.method = 'first') == 1) %>%
	# ungroup() %>%
	# select(-x.x, -y.x, -x.y, -y.y)


df.Color_to_sample_Hue =
	df.hilbert2_Hue %>%
	select(Hue, Hue, color, Tot_Avg_Pixels, order) %>%
	group_by(Hue) %>%
	# mutate(graph_height = floor(Avg_Pixels_Hue/max(df.hilbert2$Avg_Pixels_Hue) * 200)) %>%
	nest()	
	



df.DataDensity_Hue = 
	data.frame(Hue = (0:400)/400) %>%
	mutate(densityfit_Hue = predict(model.DataDensityfit_Hue, Hue)) %>%
	rowwise() %>%
	mutate(
		Closest_Hue = df.Color_to_sample_Hue$Hue[which.min(abs(df.Color_to_sample_Hue$Hue-Hue))],
		height = floor(densityfit_Hue * 20)
	) %>%
	left_join(df.Color_to_sample_Hue, by = c('Closest_Hue' = 'Hue')) %>%
	group_by(Hue) %>%
	mutate(Sample = purrr::map2(data, height, sample_n, weight = Tot_Avg_Pixels, replace = TRUE)) %>%
	unnest(Sample) %>%
	group_by(Hue) %>%
	mutate(order2 = rank(order, ties.method = 'first')/20 )

		
ggplot()+
	geom_raster(data = df.DataDensity_Hue, aes(x = Hue, y = order2, fill = color))+
	geom_line(data = df.AllDataDensity_Hue, aes(x = Hue, y = densityfit_Hue), linetype = 'twodash', color = 'grey40', size = 1)+
	# scale_color_continuous(
	scale_fill_identity(guide=FALSE) +
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
	
# ggsave(paste0('DensityPlotHue', genre_select, '.png'), dpi = 72)
ggsave(paste0('DensityPlotHue', genre_select, '.png'), width = 885/96, height = 455/96, dpi = 96, path = '.\\Density Plot')
}

	
	
df.colors_genre2 %>% filter(Value == max)
df.colors_genre2 %>% filter(genre == 'Strategy') %>% summarise(total = sum(Tot_Avg_Pixels))
	
	
ggplot(
	df.colors_genre2 %>% 
	# filter(genre %in% c('Horror', 'Platform', 'Puzzle', 'Strategy', 'Role-Playing', 'Adventure', 'Fighting', 'Shooter', 'Racing', 'Sports', 'Action')) %>%
	filter(genre == c('Horror')) %>%
	group_by(Value, genre) %>% 
	summarise(Tot_Avg_Pixels = sum(Tot_Avg_Pixels))
) + 
	# geom_density(aes(x = Value, color = genre, weight = Tot_Avg_Pixels, fill = Value), adjust = 0.05)+
	# geom_density(aes(x = Value, color = genre, weight = Tot_Avg_Pixels, fill = Value), adjust = 0.2, kernel = 'epanechnikov')+
	geom_density(aes(x = Value, color = genre, weight = Tot_Avg_Pixels, fill = Value), adjust =0.4, kernel = 'rectangular')+
	facet_grid(genre ~ ., scales = "free_y")
	
df.colors_genre2 %>% 
filter(genre == c('Horror')) %>%
# group_by(Value, genre) %>%
# summarise(Tot_Avg_Pixels = sum(Tot_Avg_Pixels)) %>% 

with(df.colors_genre2, locfit(Value, weights = Tot_Avg_Pixels))
fit = locfit(~lp(Value), data = df.colors_genre2, weights = Tot_Avg_Pixels, xlim = c(0,1)); plot(fit)
 
# plot(0:400, predict(fit,(0:400)/400*0.004))
 # , lbound = 0, ubound = 0.004)
	
ggplot(
	df.colors_genre2 %>% 
	filter(genre == 'Horror') %>%
	group_by(Value, Hue) %>% 
	summarise(Tot_Avg_Pixels = sum(Tot_Avg_Pixels))
) + 
	geom_point(aes(x = Value, y = Hue, size = Tot_Avg_Pixels)) +
	geom_density_2d(aes(x = Value, y = Hue, weight = Tot_Avg_Pixels))
	
# Horror

	geom_density_2d()
	
ggplot(
	df.colors_genre2 %>% 
	filter(genre %in% c('Horror', 'Platform', 'Puzzle', 'Strategy', 'Role-Playing', 'Adventure', 'Fighting', 'Shooter', 'Racing', 'Sports', 'Action')) %>%
	# group_by(genre) %>%
	# sample_n(2200, replace = TRUE, weight = Tot_Avg_Pixels)
	sample_n(2200, replace = FALSE, weight = Tot_Avg_Pixels)
)+
geom_point(aes(color = rgb(Red/63, Green/63, Blue/63), x = Value, y = Saturation))+
scale_colour_identity(guide=FALSE)

iris_matrix <- as.matrix(iris_unique[,1:4])
set.seed(42) # Set a seed if you want reproducible results
tsne_out = 
	df.colors[,-1:-3] %>%
	{t(. + runif(ncol(.))/10000000)} %>%
	Rtsne(pca_center = FALSE) # Run TSNE

# Show the objects in the 2D tsne representation
# plot(tsne_out$Y,col=iris_unique$Species)

df.gamegenres_wide = 
	df.gamegenres %>%
	mutate(value = 'Y') %>%
	spread(genre, value, fill = 'N')
	table(df.gamegenres$genre) %>%

df.tsne = 
	data.frame(tsne_out$Y) %>% 
	mutate(
		name = colnames(df.colors)[-1:-3],
		id = gsub('\\.jpg', '', name)
	) %>%
	inner_join(df.gamedetails) %>%
	mutate(ESRB = ifelse(is.na(ESRB), 'NA', ESRB)) %>%
	left_join(df.gamegenres_wide) #%>%
	# select(-`<NA>`)

	table(df.gamegenres$genre) %>% sort
df.tsne_gamegenre = 
	data.frame(tsne_out$Y) %>% 
	mutate(
		name = colnames(df.colors)[-1:-3],
		id = gsub('\\.jpg', '', name)
	) %>%
	left_join(df.gamegenres) %>%
	mutate(
		genre = ifelse(genre %in% c('Platform', 'Puzzle', 'Strategy', 'Role-Playing', 'Adventure', 'Fighting', 'Shooter', 'Racing', 'Sports', 'Action'), genre, 'Other')
	) %>%
	filter(genre != 'Other') %>%
	distinct() %>%
	group_by(id) %>%
	mutate(W = 1/n())
	
df.tsne_gamegenre$genre = as.factor(df.tsne_gamegenre$genre)

 test <- expand.grid(X1=seq(min(df.tsne$X1-1), max(df.tsne$X1+1),
                           by=0.5),
                     X2=seq(min(df.tsne$X2-1), max(df.tsne$X2+1), 
                           by=0.5))

						   
# train.model = train.kknn(genre~X1 + X2, df.tsne_gamegenre, kmax = 100, kernel = c("rectangular", "triangular", "epanechnikov", "gaussian", "rank", "optimal"))
model = kknn(genre~X1 + X2, df.tsne_gamegenre, test, k = 100)
test %<>% mutate(class = model$fitted.values)
	
											   
ggplot() + 
	geom_raster(data = test, aes(x=X1, y=X2, fill=class))+
	# scale_color_discrete(guide = FALSE) + 
	geom_point(data = df.tsne_gamegenre, aes(x = X1, y = X2, fill = genre), pch = 21) +
	theme_bw()
	
ggplot(df.tsne %>% sample_n(400), aes(x = X1, y = X2)) + 
# ggplot(df.tsne, aes(x = X1, y = X2)) + 
geom_image(aes(image = paste0('./CoverCropped/', name)),size=.05)+
theme_bw()

as.matrix(df.gamegenres_wide[,-1]) %>% colSums

 
 test <- expand.grid(x=seq(min(df.tsne$X1-1), max(df.tsne$X1+1),
                           by=0.5),
                     y=seq(min(df.tsne$X2-1), max(df.tsne$X2+1), 
                           by=0.5))

 classif <- knn(tsne_out$Y, test, df.tsne$ESRB, k = 7)
 prob <- attr(classif, "prob")
 
test %<>% mutate(class = classif)
										   
ggplot() + 
	geom_point(data = test, aes(x=x, y=y, col=classif),size=1)+
	geom_point(data = df.tsne, aes(x = X1, y = X2, fill = ESRB), pch = 21) +
	theme_bw()


	
ggplot() + 
	# geom_point(data = test, aes(x=x, y=y, col=classif),size=1)+
	geom_point(data = df.tsne, aes(x = X1, y = X2, fill = as.numeric(Rating)), pch = 21) +
	scale_fill_continuous(low = 'red', high = 'green')+
	theme_bw()
	
ggplot() + 
	# geom_point(data = test, aes(x=x, y=y, col=classif),size=1)+
	geom_point(data = df.tsne %>% filter(`<NA>` != 'Y'), aes(x = X1, y = X2, fill = Sports), pch = 21) +
	theme_bw()
		
ggplot() + 
	# geom_point(data = test, aes(x=x, y=y, col=classif),size=1)+
	geom_point(data = df.tsne, aes(x = X1, y = X2, fill = gsub('(\\S+)\\b.*', '\\1', tolower(Publisher))), pch = 21) +
	scale_fill_discrete(guide = FALSE)+
	theme_bw()
	
gsub('(\\S+)\\b.*', '\\1', tolower(unique(df.tsne$Publisher[1:10])), perl=TRUE)
geom_segment(data = df.colorvectors %>% filter(abs(get(PC_a)) > 0.08|abs(get(PC_b)) > 0.08), aes_string(x = 0, y = 0, xend = PC_a, yend = PC_b, color = "color"), size = 2,  arrow = arrow(length = unit(0.03, "npc"))) + 
scale_colour_identity(guide=FALSE)+

PCA = 
	as.matrix(df.colors[,-1:-3]) %>%
	t() %>%
	prcomp(center = TRUE)

df.colorvectors = 
	data.frame(t(t(PCA$rotation) * PCA$sdev * length(PCA$sdev)^0.5 * 0.4 ), color = rgb(df.colors$Red/31, df.colors$Green/31, df.colors$Blue/31))
df.points = 
	data.frame(t(t(PCA$x) / PCA$sdev / length(PCA$sdev)^0.5), name = colnames(df.colors)[-1:-3] )
	
PC_a = "PC2"	
PC_b = "PC11"	
	
	
	#https://stats.stackexchange.com/questions/276645/arrows-of-underlying-variables-in-pca-biplot-in-r
	
# ggplot(df.points %>% sample_n(nrow(df.points)), aes_string(x = PC_a, y = PC_b)) + 
ggplot(df.points %>% sample_n(150), aes_string(x = PC_a, y = PC_b)) + 
# geom_point() + 
# geom_text(aes(label=name))+
# geom_image(aes(image = paste0('./Cover2/', name)))+
geom_image(aes(image = paste0('./CoverCropped/', name)),size=.1)+
geom_segment(data = df.colorvectors %>% filter(abs(get(PC_a)) > 0.08|abs(get(PC_b)) > 0.08), aes_string(x = 0, y = 0, xend = PC_a, yend = PC_b, color = "color"), size = 2,  arrow = arrow(length = unit(0.03, "npc"))) + 
scale_colour_identity(guide=FALSE)+
theme_bw()



ggplot()+
geom_segment(df.colorvectors %>% filter(abs(get(PC_a)) > 0.005|abs(get(PC_b)) > 0.005), aes(x = 0, y = 0, xend = PC1*0.5, yend = PC6*0.5, color = color)) + 
scale_colour_identity(guide=FALSE)

correlations = as.data.frame(cor(USArrests, pca1$x))

ggplot(df.image %>% arrange(Value))+
	geom_point(aes(x = Hue, y = Saturation, color = color), size = 0.5)+
	scale_colour_identity(guide=FALSE)+
	theme_bw()
	
ggplot(df.image)+
	geom_point(aes(x = Blue, y = Green, color = color))+
	scale_colour_identity(guide=FALSE)+
	theme_bw()
	
ggplot(df.image %>% arrange(Saturation))+
	geom_point(aes(x = Hue, y = Value, color = color), size = 0.5)+
	scale_colour_identity(guide=FALSE)+
	theme_bw()
		
with(df.image, {
    scatterplot3d(Red,   # x axis
                  Green,     # y axis
                  Blue,    # z axis
                  color,
				  angle = 40,
				  pch = 16,
				  cex.symbols = 0.5)
 })		
 
 with(df.image, {
    scatterplot3d(Hue,   # x axis
                  Saturation,     # y axis
                  Value,    # z axis
                  color,
				  angle = 130,
				  pch = 16,
				  cex.symbols = 0.5)
 })		
 
 
prcomp