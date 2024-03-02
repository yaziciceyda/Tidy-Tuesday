library(tidyverse)
library(ggtext)
library(ggimage)
library(sysfonts)
library(showtext)
library(cropcircles)

#import fonts for plot
sysfonts::font_add_google("Cabin Sketch")
showtext::showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2022, week = 33)

characters_data <- tuesdata$characters
myers_briggs <- tuesdata$myers_briggs
psych_stats <- tuesdata$psych_stats


# The code is adapted from Tanya Shapiro

##coord radar hack---- use to remove last gridline, custom with ggpronto object
#https://stackoverflow.com/questions/36579767/add-unit-labels-to-radar-plot-and-remove-outer-ring-ggplot2-spider-web-plot-co
coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  
  #dirty
  rename_data <- function(coord, data) {
    if (coord$theta == "y") {
      plyr::rename(data, c("y" = "theta", "x" = "r"), warn_missing = FALSE)
    } else {
      plyr::rename(data, c("y" = "r", "x" = "theta"), warn_missing = FALSE)
    }
  }
  theta_rescale <- function(coord, x, scale_details) {
    rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
    rotate(scales::rescale(x, c(0, 2 * pi), scale_details$theta.range))
  }
  
  r_rescale <- function(coord, x, scale_details) {
    scales::rescale(x, c(0, 0.4), scale_details$r.range)
  }
  
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE,
          render_bg = function(self, scale_details, theme) {
            scale_details <- rename_data(self, scale_details)
            
            theta <- if (length(scale_details$theta.major) > 0)
              theta_rescale(self, scale_details$theta.major, scale_details)
            thetamin <- if (length(scale_details$theta.minor) > 0)
              theta_rescale(self, scale_details$theta.minor, scale_details)
            thetafine <- seq(0, 2 * pi, length.out = 100)
            
            rfine <- c(r_rescale(self, scale_details$r.major, scale_details))
            
            # This gets the proper theme element for theta and r grid lines:
            #   panel.grid.major.x or .y
            majortheta <- paste("panel.grid.major.", self$theta, sep = "")
            minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
            majorr     <- paste("panel.grid.major.", self$r,     sep = "")
            
            ggplot2:::ggname("grill", grid::grobTree(
              ggplot2:::element_render(theme, "panel.background"),
              if (length(theta) > 0) ggplot2:::element_render(
                theme, majortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
                y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
                id.lengths = rep(2, length(theta)),
                default.units = "native"
              ),
              if (length(thetamin) > 0) ggplot2:::element_render(
                theme, minortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
                y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
                id.lengths = rep(2, length(thetamin)),
                default.units = "native"
              ),
              
              ggplot2:::element_render(
                theme, majorr, name = "radius",
                x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
                y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
                id.lengths = rep(length(thetafine), length(rfine)),
                default.units = "native"
              )
            ))
          })
}
#Data ----

dd <- pych2 %>%
  filter(uni_name == "Sherlock",
         question %in% c("loyal/traitorous",
                         "animalistic/human",
                         "cruel/kind",
                         "high IQ/low IQ",
                         "jock/nerd", 
                         "conventional/creative"))  

characters <- c("Sherlock Holmes", "Dr. John Watson", "Mycroft Holmes",
               "Mrs. Hudson", "D.I. Greg Lestrade", "Molly Hooper")

traits <- c("loyal", "human", "kind", "high IQ", "nerd", "cruel", "conventional")


data_sherlock <- dd %>%
    filter(char_name %in% char_name & personality %in% traits)|>
  arrange(char_name, personality)|>
  #add image paths, images taken from Google Search and saved locally
  #add custom labels for x axis, use these later with geom_richtext
  mutate(label=paste0("<span style='color:black;font-size:18pt;'>",
  toupper(personality),"</span><br><span style='color:black;font-size:18pt;'>",
  round(avg_rating),"</span>")
  ) %>%
  left_join(characters_data, by = c("char_name" = "name")) %>%
  mutate(image_cropped = circle_crop(image_link))

#create factor for characters to order them in our facet plot based on 
# *subjective* importance (otherwise defaults to alphabetical order)
data_sherlock$char_name <- factor(data_sherlock$char_name, 
                                  levels = c("Sherlock Holmes", 
                                             "Dr. John Watson", 
                                             "Mycroft Holmes",
                                             "Mrs. Hudson", 
                                             "D.I. Greg Lestrade", 
                                             "Molly Hooper"))


#grid lines for x axis, use geom_line to add back in
line <- data.frame(x=rep(traits,2),y=c(rep(0, length(traits)),
                                       rep(100, length(traits))))



# PLOT
p <-  ggplot(data_sherlock, aes(y = avg_rating, x  =personality,
                                group = char_name))+
  geom_polygon(fill = "black", color = "black", alpha = 0.35)+
  geom_point(color = "black", size = 3)+
  geom_line(data = line, mapping = aes(x = x, y = y, group = x),
            color = "white", alpha = 0.5) +
  geom_point(inherit.aes = FALSE, data = data.frame(x = traits,
                                  y = rep(100,length(traits))),
             mapping = aes(x = x, y = y),
             shape = 21, fill = "#1D3540", color = "black", size = 3)+
  geom_image(aes(x = 1, y = -40, image = image_cropped), size = 0.3)+
  geom_richtext(aes(label = label, y = 130), family = "Cabin Sketch", 
                colour = "black",
                label.color = NA, fill = NA) +
  facet_wrap(~char_name, ncol = 4)+
  scale_y_continuous(limits = c(-40,130), breaks = c(0, 20, 40, 60, 80, 100))+
  coord_radar()+
  labs(title = "\n",
       subtitle = "\n",
       x = "",
       y = "",
       tag = "SHERLOCK\nHOLMES",
       caption = "Data Source: Open-Source Psychometrics Project | TidyTuesday 2022 - Week 33 | Prepared by: @Cyd_yzc")+
  theme_minimal()+
  theme(plot.background=element_rect(fill = "#d0c4b4",color = NA),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(color = "black", face = "bold", 
                                  size = 28, family = "Cabin Sketch"),
        axis.text = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid = element_line(color="grey20"),
        plot.margin = margin(t = 30, b = 30, r = 30, l = 30),
        plot.caption = element_text(color="black",size = 17, 
                                  hjust = 1, family = "Cabin Sketch"),
        text = element_text(color = "black"),
        plot.tag.position = c(0.75, 0.3),
        plot.tag = element_text(family = "Cabin Sketch", size = 100, face = "bold")
  )

p

# Save the Plot

ggsave("Week33_2022.png", p, width = 27, height = 15, dpi = 72)

