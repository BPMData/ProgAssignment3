library(tidyverse)
library(ggplot2)
library(swirl)
swirl()

# https://www.coursera.org/learn/data-analysis-r/quiz/fmyHH/hands-on-activity-visualizing-data-with-ggplot2/attempt

data(palmerpenguins)

library(palmerpenguins)

data(penguins)

View(penguins)


# Scatterplot to show relationship between body mass & flipper size

ggplot(data = penguins) + geom_point(mapping = aes(x = flipper_length_mm, y=body_mass_g))

# This also works, and is a more normal syntax

ggplot(data = penguins, mapping = aes(x=flipper_length_mm, y=body_mass_g)) + geom_point()

ggplot(data = penguins, mapping = aes(x=flipper_length_mm, y=body_mass_g)) +
      geom_point() +
      facet_wrap(~species)

ggplot(data = penguins, mapping = aes(x=flipper_length_mm, y=body_mass_g)) +
      geom_point(aes(color = species)) +
      facet_wrap(~species)

ggplot(data = penguins, mapping = aes(x=flipper_length_mm, y=body_mass_g)) +
      geom_point(aes(color = species)) +
      scale_color_manual(values = c("plum", "salmon", "navajowhite2")) +
      facet_wrap(~species)

colors()

ggplot(data = penguins, mapping = aes(x=flipper_length_mm, y=body_mass_g)) +
      geom_point(aes(color = species)) +
      scale_color_manual(values = c("plum", "salmon", "navajowhite2")) +
      facet_wrap(~species) + scale_color_brewer(palette = "Blues")

# Color_brewer overrides the specific scale_color_manual settings

ggplot(data = penguins, mapping = aes(x=bill_length_mm, y=bill_depth_mm)) + geom_point()

# https://www.coursera.org/learn/data-analysis-r/supplement/cwdaL/common-problems-when-visualizing-in-r

# Hands-On Activity: Using ggplot


setwd("C:\\Users\\corma\\OneDrive\\Documents\\R\\Google")

hotel_bookings <- read.csv("hotel_bookings.csv", stringsAsFactors = FALSE)

str(hotel_bookings)

colnames(hotel_bookings)

library(ggplot2)

ggplot(data = hotel_bookings) + geom_point(mapping = aes(x = lead_time, y = children))

ggplot(data = hotel_bookings) + geom_point(mapping = aes(x = stays_in_weekend_nights, y = children))

# Back to the lesson

# https://www.coursera.org/learn/data-analysis-r/lecture/A7ESc/enhancing-visualizations-in-r

?ggplot

ggplot(data = penguins) + geom_point(mapping = aes(x = flipper_length_mm, y=body_mass_g))

install.packages("ragg")
library(ragg)

?ggplot

?par

points()
?points

colors()

?colors

as.list(colors())

?palette

palette()

as.list(palette())

palette()

colors()


ggplot(data = penguins) + geom_point(mapping = aes(x = flipper_length_mm, y=body_mass_g, color = species))



ggplot(data = penguins) + geom_point(mapping = aes(x = flipper_length_mm, y=body_mass_g, color = species, shape = species))


ggplot(data = penguins) + geom_point(mapping = aes(x = flipper_length_mm, y=body_mass_g, color = species, size = species))

# The argument "size" is terrible, never use this lol.


ggplot(data = penguins) + geom_point(mapping = aes(x = flipper_length_mm, y=body_mass_g, alpha = species, shape = species), color = "purple")


# Code inside aes() function = change parameters for that specific variable(s).
 
# Code outside the aes() function = change a global parameter for that plot.

ggplot(data = penguins) + geom_smooth(mapping = aes(x = flipper_length_mm, y=body_mass_g, alpha = species, shape = species), color = "purple")

# Changing your geom is the largest, most impactful change you can make to your plot.
ggplot(data = penguins) + geom_smooth(mapping = aes(x = flipper_length_mm, y=body_mass_g), color = "purple")

# Combining geoms

ggplot(data = penguins) + geom_smooth(mapping = 
            aes(x = flipper_length_mm, y=body_mass_g), color = "purple") + geom_point(mapping = 
            aes(x=flipper_length_mm, y = body_mass_g, color = species))

# For soem reason, adding certain parameters immediately breaks up your chart

ggplot(data = penguins) + geom_smooth(mapping = 
            aes(x = flipper_length_mm, y=body_mass_g,# Next is the command that splits the plot
                shape = species), color = "purple") + geom_point(mapping = 
            aes(x=flipper_length_mm, y = body_mass_g, color = species))

# Linetype, the 2d equivalent of pch
ggplot(data = penguins) + geom_smooth(mapping = 
            aes(x = flipper_length_mm, y=body_mass_g, linetype = species))

ggplot(data = penguins) + geom_smooth(mapping = 
             aes(x = flipper_length_mm, y=body_mass_g), color = "purple") + 
      geom_point(mapping = aes(x=flipper_length_mm, y = body_mass_g, 
                               color = species))

ggplot(data = penguins) + geom_point(mapping = aes(x = flipper_length_mm, y=body_mass_g, color = species), pch = 11)

ggplot(data = penguins) + 
      geom_smooth(mapping = aes(x = flipper_length_mm, y=body_mass_g, linetype = species), color = "purple") + 
      geom_point(mapping = aes(x=flipper_length_mm, y = body_mass_g, color = species), pch = 11)

# The pch argument needs to go outside the aes() argument, not inside it, but inside the geom_point argument.

# Jitter

ggplot(data = penguins) + geom_point(mapping = aes(x = flipper_length_mm, y=body_mass_g, color = species), pch = 11)
 
# vs

ggplot(data = penguins) + geom_jitter(mapping = aes(x = flipper_length_mm, y=body_mass_g, color = species), pch = 11)

data(diamonds)

ggplot(data = diamonds) + 
      geom_bar(mapping = aes(x=cut, color = cut))

# Interesting that putting the # at the beginning 
# breaks the function call
ggplot(data = diamonds) 
      + geom_bar(mapping = aes(x=cut))

# This does NOT work as expected
ggplot(data = diamonds) + 
      geom_bar(mapping = aes(x=cut, color = cut))

# This DOES work as intended. Use fill = , not color =, with geom_bar

ggplot(data = diamonds) + 
      geom_bar(mapping = aes(x=cut, fill = cut))


# Using both fill and color to make borders around your bars:

ggplot(data = diamonds) + 
      geom_bar(mapping = aes(x=cut, fill = cut), color = "black")

# Use fill, color and size to make the borders thicker

ggplot(data = diamonds) + 
      geom_bar(mapping = aes(x=cut, fill = cut), color = "black", size = 2)

ggplot(data = diamonds) + 
      geom_bar(mapping = aes(x = cut, fill = cut, color = cut), size = 1.5) +
      scale_color_manual(values = rep("black", 5))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut, color = cut), size = 1.5) +
  scale_color_manual(values = rep("black", 5))

ggplot(data = diamonds) + 
      geom_bar(mapping = aes(x=cut, fill = cut))

ggplot(data = diamonds) + 
      geom_bar(mapping = aes(x=cut, fill = cut))

# Specify the color of the bars manually

ggplot(data = diamonds) + 
      geom_bar(mapping = aes(x=cut, fill = cut), color = "black", size = 1.5) +
      scale_fill_manual(values = c("red","orange","yellow","blue", "green"))

ggplot(data = diamonds) + 
      geom_bar(mapping = aes(x=cut, fill = cut), color = "black", size = 1.5) +
      scale_fill_manual(values = c("red","orange","yellow","blue", "green"))

# Using a gradient instead - won't work because it's a categorical variable (cut) and not a continous variable

ggplot(data = diamonds) + 
      geom_bar(mapping = aes(x=cut, fill = cut), color = "black", size = 1.5) +
      scale_fill_gradient(low = "white", high = "red")

# Setting color to be a different variable than cut makes a stacked bar chart
ggplot(data = diamonds) + 
      geom_bar(mapping = aes(x = cut, fill = clarity))

# Choose a different color palette for our stacked bar chart
ggplot(data = diamonds) + 
      geom_bar(mapping = aes(x = cut, fill = clarity)) +
      scale_fill_brewer(palette = "Set1")

??palette

?palette

install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()
?display.brewer.all

# check out http://colorbrewer.org.
library(ggplot2)
ggplot(data = diamonds) + 
      geom_bar(mapping = aes(x = cut, fill = clarity)) +
      scale_fill_brewer(palette = "Set2")

install.packages("wesanderson")
library(wesanderson)

# Cuter color palettes... but it's not working....
ggplot(data = diamonds) + 
      geom_bar(mapping = aes(x = cut, fill = clarity)) + 
      scale_color_manual(values = wes_palette(5, name = "FantasticFox1"))

?wes_palette
wes_palette("FantasticFox1")
ggplot(data = diamonds) + 
      geom_bar(mapping = aes(x = cut, fill = clarity)) + 
      col=wes_palette(5, name = "FantasticFox1")
barplot(c(1,2,3))

# I got it! That was annoying
# http://www.sthda.com/english/wiki/colors-in-r

ggplot(data = diamonds) + 
      geom_bar(mapping = aes(x = cut, fill = clarity)) + 
      scale_fill_manual(values = wes_palette(8, name = "FantasticFox1", type = "continuous"))

gplot(data = diamonds) + 
      geom_bar(mapping = aes(x = cut, fill = clarity)) + 
      scale_fill_manual(values = wes_palette(8, name = "Zissou1", type = "continuous"))

wesplot <- ggplot(data = diamonds) + 
      geom_bar(mapping = aes(x = cut, fill = clarity)) + 
      scale_fill_manual(values = wes_palette(8, name = "Zissou1", type = "continuous"))

# https://www.coursera.org/learn/data-analysis-r/supplement/06qO1/smoothing

# FACETS TIME

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity)) +
      scale_fill_brewer(palette = "Set2") +
      facet_wrap(~color)

# facet_wrap is for single variable facets

ggplot(data = penguins) + geom_point(mapping = aes(x=flipper_length_mm, y=body_mass_g, color = species)) +
      facet_wrap(~species)

ggplot(data = diamonds) + geom_bar(mapping = aes(x = color, fill = clarity)) +
      scale_fill_brewer(palette = "Set2") +
      facet_wrap(~cut)

# facet_grid is for faceting by two variables

ggplot(data = penguins) + 
      geom_point(mapping = aes(x=flipper_length_mm, 
                               y=body_mass_g, 
                               color = species)) +
      facet_grid(sex~species)

# It's a little more intuitive IMO to flip the variables
ggplot(data = penguins) + 
      geom_point(mapping = aes(x=flipper_length_mm, 
                               y=body_mass_g, 
                               color = species)) +
      facet_grid(species~sex)

# facet_grid on one variable is the same as facet_wrap as far as I can see
ggplot(data = penguins) + 
      geom_point(mapping = aes(x=flipper_length_mm, 
                               y=body_mass_g, 
                               color = species)) +
      facet_grid(~sex)

wrap1 <- ggplot(data = penguins) + 
      geom_point(mapping = aes(x=flipper_length_mm, 
                               y=body_mass_g, 
                               color = species)) +
      facet_wrap(~sex) + ggtitle("facet_wrap(~sex)")

wrap2 <- ggplot(data = penguins) + 
      geom_point(mapping = aes(x=flipper_length_mm, 
                               y=body_mass_g, 
                               color = species)) +
      facet_grid(~sex) + ggtitle("facet_grid(~sex)")

identical(wrap1, wrap2)

# Lesson_3_aesthetics

setwd("C:\\Users\\corma\\OneDrive\\Documents\\R\\Google")

hotel_bookings <- read.csv("hotel_bookings.csv", stringsAsFactors = FALSE)

ggplot(data = hotel_bookings) + geom_bar(mapping = aes(x = distribution_channel))

# Most bookings are TA/TO, overwhelmingly so
# Stands for Travel Agent/Tour Operator

ggplot(data = hotel_bookings) + geom_bar(mapping = aes(x = distribution_channel,
                                                       fill = deposit_type))

ggplot(data = hotel_bookings) + geom_bar(mapping = aes(x = distribution_channel,
                                                       fill = market_segment))

ggplot(data = hotel_bookings) + geom_bar(mapping = aes(x = distribution_channel,
                                                       fill = market_segment)) +
      facet_wrap(~deposit_type)

# Rotate the labels so they don't overlap with each other and become impossible to read

ggplot(data = hotel_bookings) + geom_bar(mapping = aes(x = distribution_channel,
                                                       fill = market_segment)) +
      facet_wrap(~deposit_type) +
      theme(axis.text.x = element_text(angle = 45))

ggplot(data = hotel_bookings) + geom_bar(mapping = aes(x = distribution_channel,
                                                       fill = deposit_type)) +
      facet_wrap(~market_segment) +
      theme(axis.text.x = element_text(angle = 45))

# Or use facet_grid

ggplot(data = hotel_bookings) + geom_bar(mapping = aes(x = distribution_channel,
                                                       fill = market_segment)) +
      facet_grid(~deposit_type) +
      theme(axis.text.x = element_text(angle = 45))

ggplot(data = hotel_bookings) + geom_bar(mapping = aes(x = distribution_channel,
                                                       fill = market_segment)) +
      facet_wrap(~deposit_type~market_segment) +
      theme(axis.text.x = element_text(angle = 45))

# Lesson 3, Filters

colnames(hotel_bookings)

ggplot(data = hotel_bookings) +
      geom_point(mapping = aes(x = lead_time, y = children))


library(wesanderson)
library(ggplot2)
library(tidyverse)

ggplot(data = hotel_bookings) +
      geom_bar(mapping = aes(x = hotel, fill = market_segment))

ggplot(data = hotel_bookings) +
      geom_bar(mapping = aes(x = hotel)) +
      facet_wrap(~market_segment)


# You don't need that $...
onlineta_city_hotels1 <- filter(hotel_bookings, hotel =="City Hotel" &
                                     hotel_bookings$market_segment == "Online TA")

onlineta_city_hotels2 <- filter(hotel_bookings, hotel =="City Hotel" &
                                     market_segment == "Online TA")


identical(onlineta_city_hotels1, onlineta_city_hotels2)

# Now with piping

onlineta_city_hotels_piping <- hotel_bookings %>% 
      filter(hotel=="City Hotel") %>% 
      filter(market_segment == "Online TA")

identical(onlineta_city_hotels1,onlineta_city_hotels_piping)


ggplot(data = onlineta_city_hotels) + 
      geom_point(mapping = aes(x = lead_time, y = children))

head(onlineta_city_hotels$lead_time,1)


# Annotations

# gg title vs labs(title = )

ggplot(data = penguins) + 
      geom_point(mapping = aes(x=flipper_length_mm, 
                               y=body_mass_g, 
                               color = species)) +
      facet_wrap(~sex) + ggtitle("facet_wrap(~sex)")

ggplot(data = penguins) + 
      geom_point(mapping = aes(x=flipper_length_mm, 
                               y=body_mass_g, 
                               color = species)) +
      facet_wrap(~sex) + labs(title = "facet_wrap(~sex)")

?ggtitle

?labs

# basically labs is better because in one function call you can do what would otherwise 
# require a ggtitle(), xlab() and ylab() call.

ggplot(data = penguins) + 
      geom_point(mapping = aes(x=flipper_length_mm, 
                               y=body_mass_g, 
                               color = species)) +
      facet_wrap(~sex) + ggtitle("facet_wrap(~sex)", subtitle = "Subtitle Test")

ggplot(data = penguins) + 
      geom_point(mapping = aes(x=flipper_length_mm, y = body_mass_g, color = species)) + 
      labs(title = "Palmer Penguins: Body Mass vs Flipper Length", subtitle = "Sample",
      x = "Flipper Length (MM)", y = "Body Mass (g)", 
      caption = "Data collected by Dr. Kristen Gorman")

# Let's center our title cuz it looks bad like this

ggplot(data = penguins) + 
      geom_point(mapping = aes(x=flipper_length_mm, y = body_mass_g, color = species)) + 
      labs(title = "Palmer Penguins: Body Mass vs Flipper Length", subtitle = "Sample",
           x = "Flipper Length (MM)", y = "Body Mass (g)", 
           caption = "Data collected by Dr. Kristen Gorman") + 
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)) 

# Now let's focus on actually annotating the chart itself

ggplot(data = penguins) + 
      geom_point(mapping = aes(x=flipper_length_mm, y = body_mass_g, color = species)) + 
      labs(title = "Palmer Penguins: Body Mass vs Flipper Length", subtitle = "Sample",
           x = "Flipper Length (MM)", y = "Body Mass (g)", 
           caption = "Data collected by Dr. Kristen Gorman") + 
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)) +
      annotate("text", x=220, y=4000, label= "The Gentoos are the largest",
               color = "red",
               fontface = "bold",
               size = 4.5, 
               angle = 25)

# Shortening the visible code using variables

pengies <- ggplot(data = penguins) + 
      geom_point(mapping = aes(x=flipper_length_mm, y = body_mass_g, color = species)) + 
      labs(title = "Palmer Penguins: Body Mass vs Flipper Length", subtitle = "Sample",
           x = "Flipper Length (MM)", y = "Body Mass (g)", 
           caption = "Data collected by Dr. Kristen Gorman") + 
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)) 

pengies

pengies + annotate("text", x=220, y=4000, label= "The Gentoos are the largest",
                         color = "red",
                         fontface = "bold",
                         size = 4.5, 
                         angle = 25)
      
pengiesplot <- pengies + annotate("text", x=220, y=4000, label= "The Gentoos are the largest",
                                  color = "red",
                                  fontface = "bold",
                                  size = 4.5, 
                                  angle = 25)

# Saving our work
?ggsave

ggsave(filename = "Palmer Penguins Plot w. Labels, Created by ggsave", device = "jpeg",
        # plot = default, path = default)
) 

#whoops you need to include a filetype in the filename apparently

ggsave(filename = "Palmer Penguins Plot w. Labels, Created by ggsave.jpg", device = "jpeg",
       # plot = default, path = default)
) 

# Interesting how different this is from the one created by manually selecting export...


# If you don't want to save the last plot plotted, I'm assuming you need to save the plot as a variable? Let's test it.

ggsave(plot = wesplot, filename = "Diamonds by Count and Clarity, Stacked Bars, Zissou1 Wesplot Palette.jpg", device = "jpeg",
      # path = default)
) 

# Yep, that was the trick!

# Saving multiple plots as a multiple-page pdf

# Create some plots
library(ggplot2)
myplot1 <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) + 
      geom_point()
myplot2 <- ggplot(iris, aes(Species, Sepal.Length)) + 
      geom_boxplot()

# Print plots to a pdf file
pdf("Test Multi-Page PDF Printing using pdf inside RStudio.pdf")
print(myplot1)     # Plot 1 --> in the first page of PDF
print(myplot2)     # Plot 2 ---> in the second page of the PDF
dev.off() 

# Hey, it worked!
pdf("Cute Graphs.pdf")
print(pengiesplot)     # Plot 1 --> in the first page of PDF
print(wesplot)     # Plot 2 ---> in the second page of the PDF
dev.off() 

# Note this command will overrwrite files with an identical filename with no warning!

# Lesson 4 - Annotations & Saving
hotel_bookings %>% count(is_repeated_guest)
hotel_bookings %>% count(customer_type)

ggplot(data = hotel_bookings) + 
      geom_bar(mapping = aes(x = market_segment, fill = customer_type)) + 
      facet_wrap(~hotel) +
      theme(axis.text.x = element_text(angle = 45, size = 8)) + 
      scale_fill_manual(values = wes_palette(4, name = "Zissou1", type = "continuous"))

# Zissou1 actually has 5 colors so I shouldn't need type = "continuous" but let's see

ggplot(data = hotel_bookings) + 
      geom_bar(mapping = aes(x = market_segment, fill = customer_type)) + 
      facet_wrap(~hotel) +
      theme(axis.text.x = element_text(angle = 45, size = 8)) + 
      scale_fill_manual(values = wes_palette(4, name = "Zissou1"))

# Looks better with type = continous imo. So let's add annotations to that one:

weshotel <- ggplot(data = hotel_bookings) + 
      geom_bar(mapping = aes(x = market_segment, fill = customer_type)) + 
      facet_wrap(~hotel) +
      theme(axis.text.x = element_text(angle = 45, size = 8)) + 
      scale_fill_manual(name = "Customer Type", values = wes_palette(4, name = "Zissou1", type = "continuous"))
#  If you want to change the title of the legend, do it in the scale_fill_manual function.



weshotel + 
      labs(title = "Customer Bookings by Hotel Type and Customer Type")

# Here's how to insert a line break

weshotel + 
      labs(title = "Customer Bookings by\n Hotel Type and Customer Type",
           x = "Market Segment", 
           y = "Total Customer Bookings") + # Let's center our title
      theme(plot.title = element_text(hjust = 0.5))


mindate <- min(hotel_bookings$arrival_date_year)
maxdate <- max(hotel_bookings$arrival_date_year)

# ChatGPT fixed it!

hotel_bookings %>% 
      pull(arrival_date_year) %>%  min()

# However, the way piping works, you can't then just add %>%  max(), so piping
# is inefficient in this case.

# Unless you use range...

hotel_bookings %>% pull(arrival_date_year) %>%  range()


weshotel + 
      labs(title = "Customer Bookings by\n Hotel Type and Customer Type",
           x = "Market Segment", 
           y = "Total Customer Bookings",
           subtitle = paste0("Data from ",mindate," to ",maxdate)) + # Let's center our title
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# Change subtitle to a caption

weshotel + 
      labs(title = "Customer Bookings by\n Hotel Type and Customer Type",
           x = "Market Segment", 
           y = "Total Customer Bookings",
           caption = paste0("Data from ",mindate," to ",maxdate)) + # Let's center our title
      theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

# Saving

ggsave("Hotel Booking Chart by Hotel and Customer Type.jpg", device = "jpeg")

# The x-axis subtitles are obnoxious, let's fix them:

weshotel2 <- ggplot(data = hotel_bookings) + 
      geom_bar(mapping = aes(x = market_segment, fill = customer_type)) + 
      facet_wrap(~hotel) +
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(t=10))) + 
      scale_fill_manual(name = "Customer Type", values = wes_palette(4, name = "Zissou1", type = "continuous"))

weshotel2
# that's a really weird way to have to specify the margins but okay. Actually not that weird, that t = 10 thing wasn't need, just put 10.

# Wait the margins thing is fucking me up here. Here's the way to create fixed axis 2:

weshotel2 <- ggplot(data = hotel_bookings) + 
      geom_bar(mapping = aes(x = market_segment, fill = customer_type)) + 
      facet_wrap(~hotel) +
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10))) + 
      scale_fill_manual(name = "Customer Type", values = wes_palette(4, name = "Zissou1", type = "continuous"))

?margin
weshotel2 + 
      labs(title = "Customer Bookings by\n Hotel Type and Customer Type",
           x = "Market Segment", 
           y = "Total Customer Bookings",
           caption = paste0("Data from ",mindate," to ",maxdate)) + # Let's center our title
      theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

ggsave("Hotel Booking Chart by Hotel and Customer Type, Fixed X-Axis 2.jpg", device = "jpeg")

# What the heck did I do for fixed x-axis 1 that looked so much better? Why does that look so bad?

# Does the margin = margin(t=10) thing really matter?
weshotel2 <- ggplot(data = hotel_bookings) + 
      geom_bar(mapping = aes(x = market_segment, fill = customer_type)) + 
      facet_wrap(~hotel) +
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(t=10))) + 
      scale_fill_manual(name = "Customer Type", values = wes_palette(4, name = "Zissou1", type = "continuous"))

weshotel2


axistest <- weshotel2 + 
      labs(title = "Customer Bookings by\n Hotel Type and Customer Type",
           x = "Market Segment", 
           y = "Total Customer Bookings",
           caption = paste0("Data from ",mindate," to ",maxdate)) + # Let's center our title
      theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

ggsave(plot = axistest, filename = "Axis Test 5.jpg", device = "jpeg")

# It's making them all look bad - honestly wonder what happens if I restart R Studio

# What if I set margin = margin(10)?
weshotel2 <- ggplot(data = hotel_bookings) + 
      geom_bar(mapping = aes(x = market_segment, fill = customer_type)) + 
      facet_wrap(~hotel) +
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10))) + 
      scale_fill_manual(name = "Customer Type", values = wes_palette(4, name = "Zissou1", type = "continuous"))

weshotel2


axistest <- weshotel2 + 
      labs(title = "Customer Bookings by\n Hotel Type and Customer Type",
           x = "Market Segment", 
           y = "Total Customer Bookings",
           caption = paste0("Data from ",mindate," to ",maxdate)) + # Let's center our title
      theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

ggsave(plot = axistest, filename = "Axis Test 6.jpg", device = "jpeg")

# It's still making the scale of the legend way too big, I don't know why it's doing this now.


# Start from the top
ggplot(data = hotel_bookings) + 
      geom_bar(mapping = aes(x = market_segment, fill = customer_type)) + 
      facet_wrap(~hotel) +
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10))) + 
      scale_fill_manual(name = "Customer Type", values = wes_palette(4, name = "Zissou1", type = "continuous")) +
      labs(title = "Customer Bookings by\n Hotel Type and Customer Type 2",
           x = "Market Segment", 
           y = "Total Customer Bookings",
           caption = paste0("Data from ",mindate," to ",maxdate, " / Legend Title Size 8, Text 6")) + # Let's center our title
      theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5),
            # Fixing the size of that cursed Legend/Scale
            legend.text=element_text(size = 6), legend.title=element_text(size = 8))

ggsave(filename = "Test 8.jpg", device = "jpeg")

ggsave(filename = "NAILED IT!.jpg", device = "jpeg")
?scale_fill_manual

?margin

ggsave(filename = "Rectangle 16 by 8.jpg", device = "jpeg",
       width = 16,
       height = 8)

# Wow in the bigger version the tiny legend looks awful, what if we took that out
ggplot(data = hotel_bookings) + 
      geom_bar(mapping = aes(x = market_segment, fill = customer_type)) + 
      facet_wrap(~hotel) +
      theme(axis.text.x = element_text(angle = 45, size = 6, margin = margin(10))) + 
      scale_fill_manual(name = "Customer Type", values = wes_palette(4, name = "Zissou1", type = "continuous")) +
      labs(title = "Customer Bookings by\n Hotel Type and Customer Type 2",
           x = "Market Segment", 
           y = "Total Customer Bookings",
           caption = paste0("Data from ",mindate," to ",maxdate, " / Legend Title Size 8, Text 6")) + # Let's center our title
      theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

ggsave(filename = "Rectangle 16 by 8, default legend size.jpg", device = "jpeg",
       width = 16,
       height = 8)
# Still looks silly and it was fine the way it was.

ggplot(data = diamonds) + 
      
      geom_bar(mapping = aes(x = cut, fill = clarity))

data(diamonds)
