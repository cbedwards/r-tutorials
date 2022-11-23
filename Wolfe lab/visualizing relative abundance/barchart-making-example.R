## Step 0: save the sheet with the key data into a CSV file -- less likely for R to get grumpy.
##   NOTE: You want your data organized so that there is a single row of column names, followed by all the data.
##   Don't organize with nested levels of column names - that complicates things. So re-organize as needed (see example)
##   OTHER NOTE: R *doesn't* want column names that start with numbers. relabel to something other than that (or add an "x" or some other letter in front)
## 
## Step 0.1: R needs to know where the data is in relation to the "working directory"
## if you're not familiar with this, one easy option is to save your data and this script in the same folder
##   Then you can use the menu: Session -> Set Working Directory -> To Source File Location
##   That will set your R script to the right folder. In fact, it does this by running a line in the Console
##   For ease/consistency, you can copy-paste that line into this script, so that now
##   every time you run the code again, you don't have to point and click (unless you're on a new computer with different folders)
##  #Here's mine
setwd("I:/Dropbox/academia/resources/R scripts - do they live here/Wolfe barchart prototype")
## 
## Step 0.2: install the necessary R packages if you don't have them on this computer. 
## Tools -> Install Packages
## We'll want "tidyverse" for data manipulation and ggplot
## I'll also use viridis and paletteer for colors for plot color choices
## 
## Okay, actual code:

## Libraries
library(tidyverse)
library(viridis)
library(paletteer)

## read in our data file, assuming the file is in our current working directory

dat = read.csv("Sample Community Data Set.csv")

## Our plotting function wants "long" data, with a single row for every unique piece of
## information. But in the lab we tend to have a separate column for the abundance of each species. Let's fix that.
## We'll use pivot_longer, which is super powerful but also complicated. Decent tutorial here: https://www.statology.org/pivot_wider-r/

dat = dat %>% 
  pivot_longer(cols = c(BC9, BW86, BW87, BW96), #the names of our abundance columns
               names_to = "species", #column name to store the identities of `cols`
               values_to = "log.abundance" #column name for the values
  )


#now to actually make the plot. ggplot makes figures in layers, and can store them as 
#variables, which means you can easily use them later / add layers later. 
#I usually label my plot `gp` for "gg plot", so I'll do that here

gp = ggplot(dat, aes(x = Replicate, y = log.abundance, fill = species)) + #list
  facet_wrap(~Fungi.Treatment) +  
  geom_col()
## what happened?
## Well, we stored the plot in a variable. Call the variable to make the computer plot it
gp

## that should have worked!


## okay, but the sizes of fonts etc are silly, at least for saving
## In ggplot you manipulate these things with the theme() function
## and it's pretty esoteric until you get used to it, BUT it's very easy to copy-paste
## the same formatting choices for all your plots
## Here's the one I usually start with
theme.mine = theme(plot.title = element_text(size = rel(3.5)),
                   axis.text = element_text(size = rel(3.5)),
                   axis.title = element_text(size=rel(3.5)),
                   legend.text = element_text(size=rel(2.5)),
                   legend.title = element_text(size = rel(3.2)),
                   strip.text = element_text(size = rel(3)),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank()
)

gp + theme.mine

## There we go! This will look very large on your viewscreen, but is about right for saving
## Speaking of which, let's save this plot

ggsave(filename = "example-plot.pdf", 
       plot = gp + theme.mine, 
       width = 8, height = 12)
## note that ggsave() is pretty smart. If you give the filename a .tif or .jpg extension instead of .pdf
## it automatically saves the correct filetype. PDF is better for journals and to give to Ben for illustrator
## while jpg / tif are often better for throwing into MS office documents.
## You can change the size/shape with the width and height (I think it defaults to being in inches?)


## okay, let's clean up our plot a bit, though.
## It has no title, and the x and y labels are based on column names, which might or might not be what we want.
## We'll also explicitly add the theme to gp, rather than continuing to tack it on.

gp = gp + 
  xlab("Experimental Replicate")+ #you can just use an empty string (ie "") if you want to x label
  ylab("Abundance (log10 scale)")+
  ggtitle("EXPERIMENT NAME GOES HERE")+
  theme.mine
gp
#okay, but I kinda hate the colors. A few options. First, we could use a pre-made palette
# If I don't care much about specifics, I usually use the functions provided in the `viridis` package, which is
# designed to handle color blindness and grayscale printing well. 

gp + 
  scale_fill_viridis(discrete=TRUE)

## viridis has 8 palette options: type ?viridis into the console to see them all

## here's with the "turbo" option
gp + 
  scale_fill_viridis(discrete = TRUE, option="turbo")

## but viridis isn't necessarily PLEASING, just reliable contrast for lots of people/contexts.
## 
## For tons more premade palette options, we could check out the "paleteer" package
##    https://www.rdocumentation.org/packages/paletteer/versions/1.5.0
## 
## There are an ungodly number of palette options here (see the above, or type palettes_d_names into the consol for the full list). Some examples:
gp + 
  scale_fill_paletteer_d("nord::frost")
gp + 
  scale_fill_paletteer_d("beyonce::X30")
## etc. Go nuts! (but I will remind you that spending 20 hours choosing a palette is probably not a great use of time.

##finally, we can specify our colors individually
## Again, TONS of options to choose from. As a starting point for color names: https://cpb-us-east-1-juc1ugur1qwqqqo4.stackpathdns.com/sites.ucsc.edu/dist/d/276/files/2015/10/colorbynames.png

gp + 
  scale_fill_manual(limits = c("BC9", "BW86", "BW87", "BW96"), #name of the species
                    values = c("dodgerblue", "lightcoral", "lightgoldenrod1", "sienna4")
  )

# Note: we might come up with a standardized set of colors for species in the lab...
## okay, let's put it all together and save a "final" figure
gfin = gp +   scale_fill_paletteer_d("beyonce::X30")
ggsave(filename = "final plot abs abundance.pdf", 
       plot = gfin,
       width = 8, height = 12)


### Relative abundance ------------------
# We might also want relative abundance!
# We'll save this figure as a different name.
# Probably the simplest thing to do is to make a new column with proportional abundance. 
# Note that we want to back-transform from our log scale for this!!

# First, data frame with the total number of individuals in each unit:
dat.abund = dat %>% 
  group_by(Fungi.Treatment, Replicate) %>%  #which columns define our individual units?
  summarize(tot.abund = sum(10^log.abundance)) %>% 
  ungroup
# next, merge that back in
dat = merge(dat, dat.abund)
## now we have a column with the total abundance across species in that unit (the same number across species)
## We just need to divide individual abundances by that (after un-log-10-ing)
dat$rel.abundance = (10^dat$log.abundance) / dat$tot.abund

gp2 = ggplot(dat, aes(x = Replicate, y = rel.abundance, fill = species)) + #list
  geom_col()+
  facet_wrap(~Fungi.Treatment) +
  ggtitle("relative abundances") + 
  ylab("relative abundance")+
  theme.mine + 
  scale_fill_manual(limits = c("BC9", "BW86", "BW87", "BW96"), #name of the species
                    values = c("dodgerblue", "lightcoral", "lightgoldenrod1", "sienna4")
  )
gp2

ggsave(filename = "final plot relative abundance.pdf", 
       plot = gp2,
       width = 10, height = 12)

