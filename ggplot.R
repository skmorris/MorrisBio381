# ------------------------------------------------------
# ggplot
# 17 May 2021
# Sarah K. Morris
# ------------------------------------------------------
# Template for ggplot components
#####################################################
# p1 <- ggplot(data = <DATA>,
#              mapping = aes(<MAPPIMG>))+
#       <GEOM_FUNCTION(mapping = aes(<MAPPING>),
#               stat=<STAT>,
#               position=<POSITION>) +
#       <COORDINATE_FUNCTION> +
#       <FACET_FUNCTION>
#   print(p1)
#   ggsave(plot = p1,
#          filename = "MyPlot",
#          width = 5,
#          height = 3,
#          units = "in",
#          device = "pdf"

# Preliminaries
library(ggplot2)
library(ggthemes)
library(patchwork)
library(TeachingDemos)
char2seed("doubling time")

d <- mpg # use built in data set on car performance
str(d)
table(d$fl)

# fast plotting with qplot -----------------------------------

#basic histogram
qplot(x=d$hwy) # highway gas mileage
qplot(x=d$hwy, fill=I("tan"), color=I("black"))

my_histo <- function(x_var, fil_col="goldenrod"){
  qplot(x=x_var, color=I("black"), fill=I(fil_col))}

my_histo(d$hwy)
my_histo(d$hwy,"thistle")

# basic density plot
qplot(x=d$hwy,geom="density") # smoothes overirregularities and estimates smooth density

# basic scatter plot
qplot(x=d$displ, # engine displacement on x-axis
      y=d$hwy,
      geom=c("smooth","point"))

# basic scatterplot with linear regression line
qplot(x=d$displ, 
      y=d$hwy,
      geom=c("smooth","point"),method="lm")

# basic boxplot
qplot(x=d$fl, y=d$cty, geom="boxplot", fill=I("tan"))
# horizontal lines represents median, 25% & 75% quartiles

# basic barplot ("long format")
qplot(x=d$fl,geom="bar",fill=I("tan"))

# common mistake = don't forget Identity ("I") function
qplot(x=d$fl,geom="bar",fill=("tan")) # this is weird

# bar plot with specified counts or means ("short format")
x_treatment = c("Control","Low","High")
y_response = c(12,2.5,22.9) # means
qplot(x=x_treatment, y=y_response, geom="col",fill=I(c("grey20","grey50","grey90")))

# basic curves and functions
my_vec <- seq(1,100,by=0.1)
head(my_vec)

my_fun <- function(x) sin(x) + 0.1*x
qplot(x=my_vec, y=sin(my_vec), geom="line") #evaluating function over range of 1 to 100
qplot(x=my_vec, y=dgamma(my_vec, shape=5, scale = 3),
      geom="line") # d function gives us probability density of a distribution 
qplot(x=my_vec, y=my_fun(my_vec), geom="line")

# Themes & fonts -----------------------------------
p1 <- ggplot(data=d, # d from built in mpg dataset
             mapping = aes(x=displ, y=cty)) +
      geom_point()
print(p1)

p1 + theme_classic()
p1 + theme_linedraw()
p1 + theme_dark()
p1 + theme_base()
p1 + theme_void()
p1 + theme_economist() # from ggthemes package
p1 + theme_bw() # gotelli's fav
p1 + theme_grey() # default

# Major theme modifications -----------------------------------

p1 + theme_classic(base_size=15)
p1 + theme_classic(base_family = "serif")

# defaults: theme_grey, base_size = 16, base_family="Helvetica"
# (grey not great and 16 a bit too small for publication-quality figures)

# default font families (Mac): Times, Ariel, Monaco, Courier, Helvetica, serif, sans

# use coordinate_flip to invert entire plot
p2 <- ggplot(data=d, mapping=aes(x=fl,fill=fl)) + geom_bar()
print(p2)
p2 + coord_flip()

# Minor theme modifications -----------------------------------

p1 <- ggplot(data=d, mapping=aes(x=displ,y=cty)) + 
  geom_point(size=7,shape=21,color="black",fill="steelblue") +
  labs(title="My graph title here",
       subtitle="An extended subtitle that will print below the main title",
       x="My x axis label",
       y="My y axis label") +
  xlim(0,4) + ylim(0,20)
print(p1)

#############################################################################
################### PART 2 ##################################################
############## multiple plots and aesthetics ################################
#############################################################################

char2seed("crocus")

g1 <- ggplot(data=d, mapping=aes(x=displ,y=cty)) + 
  geom_point() + 
  geom_smooth()
print(g1)

g2 <- ggplot(data=d,
             mapping=aes(x=fl,fill=I("tomato"),color=I("black"))) + # fl = fuel types
  geom_bar(stat="count") + 
  theme(legend.position="none") # removes legend
print(g2)

g3 <- ggplot(data=d,
             mapping=aes(x=displ,fill=I("royalblue"),color=I("black"))) + 
  geom_histogram()
print(g3)

g4 <- ggplot(data=d, # fl = fuel categories
             mapping=aes(x=fl,y=cty,fill=fl)) + 
  geom_boxplot() + 
  theme(legend.position="none")
print(g4)

# place two plots horizontally
g1 + g2

# place 3 plots vertically
g1 + g2 + g3 + plot_layout(ncol=1)

# change relative area of each plot
g1 + g2 + plot_layout(ncol=1,heights=c(2,1))

g1 + g2 + plot_layout(ncol=2,widths=c(1,2))

# add a spacer plot (under construction)
g1 + plot_spacer() + g2 
# great for if you wanted to add something else in photoshop post-R

# use nested layouts
g1 + {
  g2 + {
    g3 +
      g4 +
      plot_layout(ncol=1)
  }
} +
  plot_layout(ncol=1)
# nick doesn't understand how this works. lolz.

# - operator for subtrack placement
g1 + g2 - g3 + plot_layout(ncol=1) # minus sign puts plot 3 on bottom

# / and | for intuitive layouts
(g1 | g2 | g3)/g4 # first three in a row OVER g4

(g1 | g2)/(g3 | g4)

# Add title, etc. to a patchwork
g1 + g2 + plot_annotation('This is a title', caption = 'made with patchwork')

# Change styling of patchwork elements
g1 + g2 +
  plot_annotation(
    title = 'This is a title',
    caption = 'made with patchwork',
    theme = theme(plot.title = element_text(size = 16))
  )

# Add tags to plots
g1 / (g2 | g3) +
  plot_annotation(tag_levels = 'A') # great for publications

# swapping axes, orientation:
g3a <- g3 + scale_x_reverse() # goes from high to low across x-axis now
g3a
g3b <- g3 + scale_y_reverse() 
g3c <- g3 + scale_x_reverse() + scale_y_reverse()

(g3 | g3a)/(g3b | g3c)

#coordinate flip = flips x & y
(g3 + coord_flip() | g3a + coord_flip())/(g3b + coord_flip() | g3c + coord_flip())

ggsave(filename="MyPlot.pdf",plot=g3, device="pdf",width=20,height=20,units="cm",dpi=300)


### AESTHETIC MAPPINGS:
# mapping of a discrete variable to point color
m1 <- ggplot(data=mpg, mapping=(aes(x=displ,y=cty,color=class))) + 
  geom_point(size=3) 
print(m1)

# mapping of a discrete variable to point shape (<= 6)
m1 <- ggplot(data=mpg, mapping=(aes(x=displ,y=cty,shape=class))) + 
  geom_point(size=3) 
print(m1)

# mapping of a DISCRETE variable to point size (not advised) # b/c size assignments arbitrary
m1 <- ggplot(data=mpg, mapping=(aes(x=displ,y=cty,size=class))) + 
  geom_point() 
print(m1)

# mapping a continuous variable to point size 
m1 <- ggplot(data=mpg, mapping=(aes(x=displ,y=cty,size=hwy))) + 
  geom_point() 
print(m1)

# mapping a continuous variable to point color
m1 <- ggplot(data=mpg, mapping=(aes(x=displ,y=cty,color=hwy))) + 
  geom_point(size=5) 
print(m1)

#########################################################################
# GETTING MORE COMPLEX
# mapping two variables to different aesthetics
m1 <- ggplot(data=mpg, mapping=(aes(x=displ,y=cty,shape=class,color=hwy))) + 
  geom_point(size=5) 
print(m1)

# use shape for smaller number of categories
m1 <- ggplot(data=mpg, mapping=(aes(x=displ,y=cty,shape=drv,color=fl))) + 
  geom_point(size=5) 
print(m1)
# didn't exceed 6 categories

# use all 3 (size, shape, color) to indicate 5 attributes!
m1 <- ggplot(data=mpg, mapping=(aes(x=displ,y=cty,shape=drv,color=fl,size=hwy))) + 
  geom_point()
print(m1)

# mapping a variable to the same aesthetic in two different geoms
m1 <- ggplot(data=mpg, mapping=(aes(x=displ,y=cty,color=drv))) + 
  geom_point(size=2) + geom_smooth(method="lm")
print(m1)
# plotting 2 geoms: point and smooth to the same aestetics (color)

#####################################
###### FACETINGC#####################
#####################################

# basic faceting with variables split by row, column, or both
m1 <- ggplot(data=mpg, mapping=(aes(x=displ,y=cty))) + # y-axis = city mileage, x-axis = displacement
  geom_point() 
m1 +  facet_grid(class~fl)   # rows~columns  
# rows represent class (vehicle) types
# columns represent fuel types
# scatterplot repeated with each combo of class and fuel type
# great for visualizing patterns, seeing which combos have no data, etc.

m1 + facet_grid(class~fl, scales="free_y")
# allows y-axis to fit data

m1 + facet_grid(class~fl, scales="free")
# both axes free to fit data

m1 + facet_grid(.~class)
# . = all x's same 
# class = collapse fuel types and just look at classes

m1 + facet_grid(class~.)
# same graph as before but instead of column display, row display 

# use facet wrap when variables are not crossed
# algorithm maximizes 'squareness' of graphs

m1 + facet_grid(.~class)
m1 +  facet_wrap(~class)     

m1 + facet_wrap(~class + fl) # add another grouping variable

m1 + facet_wrap(~class + fl, drop=FALSE) # doesn't drop the empties
m1 + facet_grid(class~fl) # contrast grid v. wrap

# use facet with other aesthetic mapping within rows or columns

m1 <- ggplot(data=mpg, mapping=(aes(x=displ,y=cty,color=drv))) + 
  geom_point() 
m1 + facet_grid(.~class)

# easy to switch to other geoms
# best fit regression line
m1 <- ggplot(data=mpg, mapping=(aes(x=displ,y=cty,color=drv))) + 
  geom_smooth(se=FALSE,method="lm") # se = standard error (gray CI bar around line)
m1 + facet_grid(.~class)

# fitting with boxplots over a continuous variable
m1 <- ggplot(data=mpg, mapping=(aes(x=displ,y=cty))) + 
  geom_boxplot()
m1 + facet_grid(.~class)
# boxplots placed over the average displacement

# add a group and fill mapping for subgroups
m1 <- ggplot(data=mpg, mapping=(aes(x=displ,y=cty,group=drv,fill=drv))) + 
  geom_boxplot()
m1 + facet_grid(.~class)

####### MORE DETAIL ON AESTHETIC MAPPINGS #####################################

# standard plot with all data
p1 <- ggplot(data=d,mapping=aes(x=displ,y=hwy)) +
  geom_point() + geom_smooth()
print(p1)

# break out the drive types (note what group affects
p1 <- ggplot(data=d,mapping=aes(x=displ,y=hwy, group=drv)) +
  geom_point() + geom_smooth()
print(p1)

# break out the drive types (note what color affects
p1 <- ggplot(data=d,mapping=aes(x=displ,y=hwy, color=drv)) +
  geom_point() + geom_smooth()
print(p1)

# break out the drive types (note what fill affects
p1 <- ggplot(data=d,mapping=aes(x=displ,y=hwy, fill=drv)) +
  geom_point() + geom_smooth()
print(p1)

# use both if you want points, lines, and confidence intervals colored
p1 <- ggplot(data=d,mapping=aes(x=displ,y=hwy, color=drv, fill=drv)) +
  geom_point() + geom_smooth()
print(p1)

# now use aesthetic mappings within each geom to over-ride defaults
# subset the data frame to pull out what you need

p1 <- ggplot(data=d,mapping=aes(x=displ,y=hwy,col=drv)) +
  geom_point(data=d[d$drv=="4",]) + geom_smooth()
print(p1)

# instead of subsetting, just map an aesthetic
p1 <- ggplot(data=d,mapping=aes(x=displ,y=hwy)) +
  geom_point(mapping=aes(color=drv)) + geom_smooth()
print(p1)

# Conversely, map the smoother, but not the points
p1 <- ggplot(data=d,mapping=aes(x=displ,y=hwy)) +
  geom_point() + geom_smooth(mapping=aes(color=drv))
print(p1)

# also, subset in the first layer to eliminate some data entirely
# instead of subsetting, just map an aesthetic
p1 <- ggplot(data=d[d$drv!="4",],mapping=aes(x=displ,y=hwy)) +
  geom_point(mapping=aes(color=drv)) + geom_smooth()
print(p1)

#############################################################################
################### PART 3 ##################################################
##################### Color #################################################
#############################################################################

# install.packages("remotes")
library(remotes)
# remotes::install_github("clauswilke/colorblindr") 

# update all of the recommended packages when prompted
# if an error is generated, re-start RStudio and re-run this line of code

# install.packages("colorspace")
# install.packages("cowplot")
# install.packages("wesanderson")
# install.packages("cggsci")

library(ggplot2)
library(ggthemes)
library(patchwork)
library(colorblindr) # requires colorspace
library(cowplot)
library(colorspace) 
library(wesanderson)
library(ggsci)
library(TeachingDemos)

char2seed("Dark Star")
d <- mpg

# use to plot the counts of rows for a categorical variable
table(d$drv)

p1 <- ggplot(d,aes(x=drv)) + geom_bar(color="black",fill="cornsilk")
print(p1)

# aesthetic mapping gives multiple groups for each bar
p1 <- ggplot(d,aes(x=drv,fill=fl)) + geom_bar()
print(p1)

# stacked, but need to adjust color transparency, which is "alpha"
# alpha = 0 - completely transparent; alpha = 1 - completely opaque
p1 <- ggplot(d,aes(x=drv,fill=fl)) + geom_bar(alpha = 0.3, position="identity") # identity means that each group starts at 0/bottom
print(p1)

# better to use position = fill for stacking, but with equivalent height
p1 <- ggplot(d,aes(x=drv,fill=fl)) + geom_bar(position="fill")
print(p1) #proportion, not count

# best to use position = dodge for multiple bars
p1 <- ggplot(d,aes(x=drv,fill=fl)) + geom_bar(position="dodge",color="black",size=1)
print(p1) # still not great, bars aren't uniform width

# more typical "bar plot" has heights as the values themselves
dTiny <- tapply(X=d$hwy,INDEX=as.factor(d$fl),FUN=mean) #calculate the means
dTiny <- data.frame(hwy=dTiny) # create a single-column data frame
dTiny <- cbind(fl=row.names(dTiny),dTiny) 

p2 <- ggplot(dTiny, aes(x=fl,y=hwy,fill=fl)) +
  geom_col()
print(p2) # standard bar plot with means = height of the bar

#### BETTER than bar plot:
#### Use a box plot instead of standard "means" bars!
# basic boxplot is simple and more informative
# gives a sense of spread
p1 <- ggplot(d,aes(x=fl,y=hwy,fill=fl)) +
  geom_boxplot()
print(p1)

# now overlay the raw data
p1 <- ggplot(d,aes(x=fl,y=hwy)) +
  geom_boxplot(fill="thistle",outlier.shape=NA) + 
  geom_point(position=position_jitter(width=0.1,height=0.7),color="grey60",size=2)
# position jitter allows for multiple points with same value to be visible

print(p1)

####################################################################
## 1. Aesthetics ###################################################
## Colors that are attractive - large geoms (fills) - pale colors - small geoms(lines,points) - bright colors
## Colors that highlight elements - pale, grey to de-emphasize - bright or saturated colors to emphasize
## Colors that are visible to the color blind
## Colors that convert well to black and white
####################################################################
# 2. Information content
# A. Discrete scale
###colors to group similar treatments
### neutral colors (black,grey,white) to indicate control groups
### Symbolic colors (heat=red, cool = blue, photosynthesis/growth=green, oligotrophic=blue, eutrophic=brown, infected=red)
### Colors that map to chemical stains or gels, or even colors of organisms
# B. Continuous scale
### monochromatic (differing shades of 1 color)
### 2 tone chromatic scale (from color x to color y)
### 3 tone divergent scale (from color x through color y to color z)
# C. Use color information within and between graphs
####################################################################
# 3. show color names, hex in base R
####################################################################
# 4. show color schemes in colorbrewer
####################################################################
# hue - wavelength of visible light
# saturation - intensity, vibrancy
# lightness - black to white
# red, blue, green
# named colors in R

# use color brewer website to play around with  diff palettes 
# https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3

my_cols <- c("thistle","tomato","cornsilk","cyan","chocolate")

my_cols <- c('#ca0020','#f4a582','#92c5de', '#0571b0')

demoplot(my_cols,"map") 

demoplot(my_cols,"bar")

demoplot(my_cols,"scatter")

demoplot(my_cols,"heatmap")

demoplot(my_cols,"spine")

demoplot(my_cols,"perspective")

my_r_col <- c("red","brown","cyan","green")
demoplot(my_r_col,"pie")

##################################################################

# gray function versus gray colors

# built in greys (0 = black, 100 = white)
my_greys <- c("grey20","grey50","grey80")
demoplot(my_greys,"bar")

my_greys2 <- grey(seq(from=0.1,to=0.9,length.out=10))   
# gray/grey function takes values from 0 & 1 and gives you the specified # of colors (in hex codes)
demoplot(my_greys2,"heatmap")

# converting color plots to black and white
p1 <- ggplot(d,aes(x=as.factor(cyl),y=cty,fill=as.factor(cyl))) + geom_boxplot() 
plot(p1)
# default colors look identical in black white
p1_des<- colorblindr::edit_colors(p1, desaturate)
plot(p1_des)
# doesn't look great - default ggplot settings only change hue, not saturation or brightness so when you desaturate, they all look identical

# custom colors not pretty, but convert ok to bw
p2 <- p1 + scale_fill_manual(values=c("red","blue","green","yellow"))
plot(p2)

p2_des<- colorblindr::edit_colors(p2, desaturate)
plot(p2_des)

x1 <- rnorm(n=100,mean=0)
x2 <- rnorm(n=100,mean=2.7)
dFrame <- data.frame(v1=c(x1,x2))
lab <- rep(c("Control","Treatment"),each=100)
dFrame <- cbind(dFrame,lab)
str(dFrame)

h1 <- ggplot(dFrame,aes(x=v1,fill=lab))
h1 + geom_histogram(position="identity",alpha=0.5,color="black") 


######## COLOR CUSTOMIZATIONS IN GGPLOTS ##############
# --------- discrete classification
# scale_fill_manual() for boxplots,bars
# scale_color_manual() for points, lines

# boxplot no color
p_fil <- ggplot(d,aes(x=as.factor(cyl),y=cty))
p_fil + geom_boxplot()

# boxplot default ggplot fill
p_fil <- ggplot(d,aes(x=as.factor(cyl),y=cty,fill=as.factor(cyl))) + geom_boxplot()
plot(p_fil) # not giving us any new info

# create custom color palette
my_cols <- c("red","brown","blue","orange")

# boxplot with custom colors for fill
p_fil + scale_fill_manual(values=my_cols)
# same deal as above, but we chose the colors

# scatterplot with no color
p_col <- ggplot(d,aes(x=displ,y=cty))
p_col + geom_point(size=3)

# scatterplot default ggplot colors
p_col <- ggplot(d,aes(x=displ,y=cty,col=as.factor(cyl))) + geom_point(size=3)
plot(p_col)

# scatterplot with custom colors for point color
p_col + scale_color_manual(values=my_cols)

# ------- continuous classification (color gradient)------------------

# default color gradient
p_grad <- ggplot(d,aes(x=displ,y=cty,col=hwy)) + geom_point(size=3)
plot(p_grad) # highway mileage is a continuous variable, color mapped to it

# custom sequential gradient (2-colors)
p_grad + scale_color_gradient(low="green", high="red")

# custom diverging gradient (3-colors)
mid <- median(d$hwy)
p_grad + scale_color_gradient2(midpoint=mid,
                               low="blue",
                               mid="white",
                               high="red")

# custom diverging gradient (n # of colors)
p_grad + scale_color_gradientn(colors=c("blue","green","yellow","purple","orange"))

########################################################################
# PALETTE TOUR
# ----------------------------------------------------------------------

library(wesanderson)
print(wes_palettes)

demoplot(wes_palettes$GrandBudapest2,"bar")

demoplot(wes_palettes[[2]][1:3],"spine")

my_cols <- wes_palettes$GrandBudapest2[1:4]
p_fil + scale_fill_manual(values=my_cols)

library(RColorBrewer)
display.brewer.all() # shows the palettes available

display.brewer.all(colorblindFriendly=TRUE)

demoplot(brewer.pal(4,"Accent"),"bar")

demoplot(brewer.pal(11,"Spectral"),"heatmap")

my_cols <- c("grey75",brewer.pal(3,"Blues"))
p_fil + scale_fill_manual(values=my_cols)

# nice for seeing hex values!
library(scales)
show_col(my_cols)

#### Making a heat map
xVar <- 1:30
yVar <- 1:5
myData <- expand.grid(xVar=xVar,yVar=yVar) # expand.grid() creates df from all combos
head(myData)

zVar <- myData$xVar + myData$yVar + 2*rnorm(n=150)
myData <- cbind(myData,zVar)
head(myData)

# default gradient colors in ggplot
p4 <- ggplot(myData,aes(x=xVar,y=yVar,fill=zVar)) +
  geom_tile() # produces heatmap
print(p4)

# user defined divergent palette
p4 + scale_fill_gradient2(midpoint=19,low="brown",mid=grey(0.8),high="darkblue")

# viridis scale (built into base R)
p4  + scale_fill_viridis_c()

# options viridis, cividis, magma, inferno, plasma
p4 + scale_fill_viridis_c(option="inferno")

#desaturated viridis
p4 <- p4 + geom_tile() + scale_fill_viridis_c() 
p4des<-edit_colors(p4, desaturate)
ggdraw(p4des)

####################################################################
####################################################################
?`ggsci-package`
vignette("ggsci")
# example from the vignette:
data("diamonds")

ggplot(subset(diamonds, carat >= 2.2),
       aes(x = table, y = price, colour = cut)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", alpha = 0.1, size = 1, span = 1) +
  theme_bw() + scale_color_rickandmorty()

ggplot(subset(diamonds, carat > 2.2 & depth > 55 & depth < 70),
       aes(x = depth, fill = cut)) +
  geom_histogram(colour = "black", binwidth = 1, position = "dodge") +
  theme_bw() + scale_fill_rickandmorty()

library(ggsci)
library(scales)
mypal <- pal_rickandmorty("schwifty", alpha = 0.9)(12)
show_col(mypal)
