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
df.hilbert = hilbert(m=matrix(1), n=4, r=2)














########################################################################
############### 1 Acquire data for all downloaded covers ###############
########################################################################

# 1.1 Import the image in R
image.jpeg = readJPEG('.\\CoverCropped\\511.jpg')
	
# 1.2 Get the proportions of colors for all images
df.colors = 
	data.frame( #Get each color as a column from the data frame
		Red = (image.jpeg[,,1]) %>% as.vector,
		Green = (image.jpeg[,,2]) %>% as.vector,
		Blue = (image.jpeg[,,3]) %>% as.vector
	) %>%
	group_by(Red, Green, Blue) %>%
	summarise(Avg_Pixels = n()/(dim(image.jpeg)[1] * dim(image.jpeg)[2])) %>% #proportion of pixels by color
	mutate(
		Hue = rgb2hsv(Red, Green, Blue, maxColorValue = 1)[1,],
		Saturation = rgb2hsv(Red, Green, Blue, maxColorValue = 1)[2,],
		Value = rgb2hsv(Red, Green, Blue, maxColorValue = 1)[3,],
		color = rgb(Red, Green, Blue)
	)


	
	
	
	
	
	
	
	
	
	
	
	
	
##################################################
############### 2 3d Visualization ###############
##################################################

# 3.1 RGB 3d plot
filter(df.colors, Avg_Pixels > 0.00002) %>% 
with({
    scatterplot3d(Red,   # x axis
                  Green,     # y axis
                  Blue,    # z axis
                  color,
				  grid = FALSE,
				  angle = 30,
				  pch = 16,
				  cex.symbols = 0.5)
 })

 # 3.2 HSV 3d plot
filter(df.colors, Avg_Pixels > 0.00002) %>% 
with({
scatterplot3d(Hue,   # x axis
			  Saturation,     # y axis
			  Value,    # z axis
			  color,
			  angle = 120,
			  pch = 16,
			  cex.symbols = 0.5)
})	





########################################################
############### 3 Hue Density Estimation ###############
########################################################

# 3.1 Estimate the Hue density function
model.DataDensityfit_Hue = 
	locfit(
		~lp(Hue),
		data = df.colors %>% filter(Saturation > 0.1, Value > 0.1),
		weights = Avg_Pixels, 
		xlim = c(0,1)
	)

# 3.2 Merge the Hilbert curve to the color data frame
df.hilbert2_Hue = 
	df.colors %>% 
	filter(Saturation > 0.1) %>% #filter unsaturated whitish colors
	filter(Value > 0.1) %>% #filter colors that are too dark
	group_by(Hue) %>%
	mutate(x = round(1 + Saturation * max(df.hilbert$x-1)), y = round(1 + Value * max(df.hilbert$y-1))) %>%
	left_join(df.hilbert, by = c('x', 'y'))

# 3.3 Create the data frame to sample from for the density plot
df.Color_to_sample_Hue =
	df.hilbert2_Hue %>%
	select(Hue, color, Avg_Pixels, order) %>%
	group_by(Hue) %>%
	nest()
	
	
# 3.4 Create the density plot data frame
df.DataDensity_Hue = 
	data.frame(Hue = (0:400)/400) %>% # 400 pixel wide
	mutate(densityfit_Hue = predict(model.DataDensityfit_Hue, Hue)) %>%
	rowwise() %>%
	mutate(
		Closest_Hue = df.Color_to_sample_Hue$Hue[which.min(abs(df.Color_to_sample_Hue$Hue-Hue))],
		height = floor(densityfit_Hue * 20) # Around 100 pixel high
	) %>%
	left_join(df.Color_to_sample_Hue, by = c('Closest_Hue' = 'Hue')) %>%
	group_by(Hue) %>%
	mutate(Sample = purrr::map2(data, height, sample_n, weight = Avg_Pixels, replace = TRUE)) %>% #sample in each nest
	unnest(Sample) %>%
	group_by(Hue) %>%
	mutate(order2 = rank(order, ties.method = 'first')/20, Dimension = 'Hue')


	
	
	
	
	
	
	
	
	
###############################################################
############### 4 Saturation Density Estimation ###############
###############################################################


# 4.1 Estimate the Saturation density function for the genre
model.DataDensityfit_Saturation = 
	locfit(
		~lp(Saturation),
		data = df.colors ,
		# data = df.colors %>% filter(Value > 0.1) ,
		weights = Avg_Pixels, 
		xlim = c(0,1)
	)

# 4.2 Merge the Hilbert curve to the color data frame
df.hilbert2_Saturation = 
	df.colors %>% 
	group_by(Saturation) %>%
	mutate(x = round(1 + Value * max(df.hilbert$x-1)), y = round(1 + Hue * max(df.hilbert$y-1))) %>%
	left_join(df.hilbert, by = c('x', 'y'))

# 4.3 Create the data frame to sample from for the density plot
df.Color_to_sample_Saturation =
	df.hilbert2_Saturation %>%
	select(Saturation, color, Avg_Pixels, order) %>%
	group_by(Saturation) %>%
	nest()
	
	
# 4.4 Create the density plot data frame
df.DataDensity_Saturation = 
	data.frame(Saturation = (0:400)/400) %>% # 400 pixel wide
	mutate(densityfit_Saturation = predict(model.DataDensityfit_Saturation, Saturation)) %>%
	rowwise() %>%
	mutate(
		Closest_Saturation = df.Color_to_sample_Saturation$Saturation[which.min(abs(df.Color_to_sample_Saturation$Saturation-Saturation))],
		height = floor(densityfit_Saturation * 20) # Around 100 pixel high
	) %>%
	left_join(df.Color_to_sample_Saturation, by = c('Closest_Saturation' = 'Saturation')) %>%
	group_by(Saturation) %>%
	mutate(Sample = purrr::map2(data, height, sample_n, weight = Avg_Pixels, replace = TRUE)) %>% #sample in each nest
	unnest(Sample) %>%
	group_by(Saturation) %>%
	mutate(order2 = rank(order, ties.method = 'first')/20 , Dimension = 'Saturation')

	
	
	
	
	
	
	
	
	
	
	
	
	
	
#######################################################################
############### 5 Value (Brightness) Density Estimation ###############
#######################################################################


# 5.1 Estimate the Value density function for the genre
model.DataDensityfit_Value = 
	locfit(
		~lp(Value),
		data = df.colors,
		weights = Avg_Pixels, 
		xlim = c(0,1)
	)

# 5.2 Merge the Hilbert curve to the color data frame
df.hilbert2_Value = 
	df.colors %>% 
	group_by(Value) %>%
	mutate(x = round(1 + Hue * max(df.hilbert$x-1)), y = round(1 + Saturation * max(df.hilbert$y-1))) %>%
	left_join(df.hilbert, by = c('x', 'y'))

# 5.3 Create the data frame to sample from for the density plot
df.Color_to_sample_Value =
	df.hilbert2_Value %>%
	select(Value, color, Avg_Pixels, order) %>%
	group_by(Value) %>%
	nest()
	
	
# 5.4 Create the density plot data frame
df.DataDensity_Value = 
	data.frame(Value = (0:400)/400) %>% # 400 pixel wide
	mutate(densityfit_Value = predict(model.DataDensityfit_Value, Value)) %>%
	rowwise() %>%
	mutate(
		Closest_Value = df.Color_to_sample_Value$Value[which.min(abs(df.Color_to_sample_Value$Value-Value))],
		height = floor(densityfit_Value * 20) # Around 100 pixel high
	) %>%
	left_join(df.Color_to_sample_Value, by = c('Closest_Value' = 'Value')) %>%
	group_by(Value) %>%
	mutate(Sample = purrr::map2(data, height, sample_n, weight = Avg_Pixels, replace = TRUE)) %>% #sample in each nest
	unnest(Sample) %>%
	group_by(Value) %>%
	mutate(order2 = rank(order, ties.method = 'first')/20, Dimension = 'Value (Brightness)')



	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
##################################################
############### 6 HSV Density Plot ###############
##################################################

#6.1 Graph Hue Saturation and Value together	
ggplot()+
	geom_raster(data = df.DataDensity_Hue, aes(x = Hue, y = order2, fill = color))+ #Density of the Hue
	geom_raster(data = df.DataDensity_Saturation, aes(x = Saturation, y = order2, fill = color))+ #Density of the Saturation
	geom_raster(data = df.DataDensity_Value, aes(x = Value, y = order2, fill = color))+ #Density of the Value (Brightness)
	facet_grid(Dimension ~ .)+
	scale_fill_identity(guide=FALSE) + #direct color from the data base
	coord_fixed(ratio = 1/10) + 
	scale_y_continuous(expand = c(0,0), limits = c(0,5), name = 'Density')+
	scale_x_continuous(expand = c(0,0), breaks = (0:5)/5, name = 'Amount')+
	ggtitle('Hue, Saturation & Value of the box art for\n"Castlevania: Symphony of the Night (US)"')+
	theme(
		plot.title = element_text(size=13, face="bold.italic"),
		axis.text.y = element_blank(),
		axis.ticks.y = element_blank(),
		axis.line = element_blank(),
		panel.grid = element_line(linetype = 'dashed'),
		panel.border = element_blank()
	)
	