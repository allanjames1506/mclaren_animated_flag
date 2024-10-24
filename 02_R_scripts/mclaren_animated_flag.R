# 1 Inspiration----
# https://github.com/aschinchon/spinning-pins/blob/master/spinning_pins.R

# 2 Libraries----
library(tidyverse)
library(data.table)
library(gganimate)
library(scales)
library(ggtext)
library(showtext)
library(emojifont)
library(extrafont)
library(here)
library(png)
library(ggimage)
library(ggfx)
library(here)
library(magick)

# 3 Fonts----
font_add_google("Rubik","rubik")
font_add_google("Alfa Slab One", "alfa")
font_add_google("Calistoga", "vegas")
font_add_google("Rye", "rye")
font_add_google("Nosifer", "nosifer")
font_add_google("Creepster", "creepster")
showtext_auto()
showtext_opts(dpi = 320)

# sysfonts::font_add(family = "Font Awesome 6 Brands",
#                    regular = "./00_raw_data/fonts/otfs/Font Awesome 6 Brands-Regular-400.otf")

# font_add('fa-brands', "./00_raw_data/fonts/otfs/Font Awesome 6 Brands-Regular-400.otf")
# font_add('fa-reg', "./00_raw_data/fonts/otfs/Font Awesome 6 Free-Regular-400.otf")
# font_add('fa-solid', "./00_raw_data/fonts/otfs/Font Awesome 6 Free-Solid-400.otf")

# loadfonts()
# ft <- fonttable()
# 
# ft[grepl("Awesome", ft$FontName), c("FullName", "FamilyName", 'FontName')]

# load.fontawesome()
# fa <- fontawesome(c('fa-github', 'fa-weibo', 'fa-twitter', 'fa-android', 'fa-coffee'))
# fa_github <- fontawesome('fa-github')

# 4 Import McLaren logo----
# 84 dots for McLaren logo derived using WebPlotDigitizer https://apps.automeris.io/wpd/
grid_mclaren_simple <- read.csv('./00_raw_data/mclaren_logo_dots.csv') %>% 
  select(2:3) %>% 
  rename(lon = x, lat = y)

# basic plot for check
p_mclaren <- grid_mclaren_simple %>% 
  ggplot(aes(lon, lat)) + 
  geom_point(color = '#FF8000') 

# rescale coordinates to align with coordinates for gridded spinning pins
grid_mclaren_simple_rescale <- grid_mclaren_simple %>% 
  mutate(x = rescale(lon, to = c(1, 20)),
         y = rescale(lat, to = c(1, 20))) %>% 
  select(x, y) 

# basic plot for check of rescaled McLaren coordinates
p_mclaren_rescale <- grid_mclaren_simple_rescale %>% 
  ggplot(aes(x, y)) + 
  geom_point(color = '#FF8000')

# 5 Patterns----
# *5.1 1st pattern----
# 3rd pattern - use this - wave pattern
# n_points is the number of points for the mclaren logo
n_points  <- 84
closeness <- 2*pi/n_points
speed     <- 2*pi/n_points
v_angles <- seq(0, 0, length.out = n_points)

# This function creates a grid of vectors (coordinates and angle)
# using a initial vector of angles adding factor f each iteration
create_grid <- function(n, a, f) {
  lapply(seq_len(n), function (x) {a+f*(x-1)}) %>% 
    do.call("rbind", .) %>% 
    melt(varnames=c('x', 'y'), value.name="angle")
}

# This is what makes to spin the pins - inspect the structure of the output -> df 
lapply(1:(n_points+1), function(x) {
  create_grid(n_points, 
              v_angles+(x-1)*speed,
              closeness)}) %>% 
  as.list(.) %>% 
  rbindlist(idcol="frame") -> df

# for checking original spinning pins animation
# Plot pins using frame as transition time
# ggplot(df) +
#   geom_spoke(aes(x=x, y=y, angle = angle), radius = 1) +
#   geom_point(aes(x+cos(angle), y+sin(angle)), size=2) +
#   theme_void() + 
#   coord_fixed() +
#   transition_time(time=frame)

# save original spinning pins animation
# anim_save("choose_a_name.gif")


# **5.1.1 McLaren dots in grid----

# images/icons for halloween animation
jack_o_lantern_img <- here("00_raw_data", "lantern_48.png")
bat_img <- here("00_raw_data", "bat.png")
pumpkin_img <- here("00_raw_data", "pumpkin_48.png")
ghost_img <- here("00_raw_data", "ghost_512.png")

# x variable from the square pattern
df_x <- df %>% 
  select(x) %>% 
  rename(original_x = x)

# x variable from the rescaled McLaren logo coordinates
new_df_x <- grid_mclaren_simple_rescale %>% 
  select(x) %>% 
  rename(new_x = x) %>% 
  slice(1:83)

# https://stackoverflow.com/questions/66434941/make-a-column-based-a-repetitive-numbers-that-follows-another-column
# transpose new McLaren x-cordinates to same length as square pattern
df_x_rep <- df_x %>%
  mutate(new_col = rep(list(new_df_x), n())) %>% 
  unnest(new_col) %>% 
  slice(1:599760)

# y variable
# y variable from the square pattern
df_y <- df %>% 
  select(y) %>% 
  rename(original_y = y)

# different structure for y - stepwise, single coordinates repeated n times
# https://stackoverflow.com/questions/2894775/repeat-each-row-of-data-frame-the-number-of-times-specified-in-a-column
new_df_y <- grid_mclaren_simple_rescale %>%
  select(y) %>% 
  mutate(freq = 84) %>% 
  slice(rep(seq_len(n()), freq)) %>% 
  select(-freq)

# then repeat previous step n times
df_y_rep <- purrr::map_dfr(seq_len(85), ~new_df_y)

# bind new x and y's
new_xy <- df_x_rep %>% 
  bind_cols(df_y_rep) %>% 
  select(new_x, y) %>% 
  rename(x = new_x)

# reclaim frame and angle variables from original square pattern
df_frame_angle <- df %>% 
  select(frame, angle)

# bind frame, angle variables to new x,y variables
# add row numbers in case needed later
df_mclaren <- new_xy %>% 
  bind_cols(df_frame_angle) %>% 
  select(frame, x, y, angle) %>% 
  mutate(row = row_number())

# obtain just the coordinates for the McLaren data points
# this data set could be used for input to ggplot/animate
mclaren_points_yes <- df_mclaren %>%
  inner_join(grid_mclaren_simple_rescale, by = c('x', 'y'), relationship = "many-to-many") %>% 
  mutate(mclaren_point = 'yes') %>% 
  filter(!duplicated(row)) %>% 
  #mutate(label = sample(fa, 9690, replace = T)) %>%
  mutate(label = jack_o_lantern_img) 

mclaren_points_yes_random_rows <- mclaren_points_yes %>% 
  sample_n(100) %>% 
  mutate(label = bat_img)

mclaren_points_yes_random_rows_ghost <- mclaren_points_yes %>% 
  sample_n(50) %>% 
  mutate(label = ghost_img)

# obtain coordinates that are not needed to show the McLaren logo
mclaren_points_no <- df_mclaren %>% anti_join(mclaren_points_yes, by = 'row') %>% 
  mutate(mclaren_point = 'no')

# bind back the McLaren data points yes and no
mclaren_points_yes_no <- mclaren_points_no %>% 
  bind_rows(mclaren_points_yes) %>% 
  arrange(row)

# **5.1.2 Animate McLaren pins----
# plot and animate
mclaren_pins_animate <- mclaren_points_yes %>% 
  ggplot() +
  geom_spoke(aes(x=x, y=y, angle = angle), radius = 1, colour = '#FF8000') +
  geom_point(aes(x+cos(angle), y+sin(angle)), size=3, colour = '#FF8000') +
  #geom_text(aes(x+cos(angle), y+sin(angle), label = label), family = 'fontawesome-webfont') +
  theme_void() +
  darklyplot::theme_dark2() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.caption = element_text(size = 14, family = "alfa",hjust = 0.5, colour = '#B6BABD')) +
  coord_fixed() +
  transition_time(time=frame) +
  #shadow_wake(0.15) +
  labs(caption = "design by hey-jay")

# animate
animate(mclaren_pins_animate, fps=10)

# animation save
anim_save("./04_gifs/mclaren_pins_animate.gif", height = 372, width = 538, units = "px")

# **5.1.3 Animate McLaren Halloween pins----
mclaren_pins_animate_halloween <- mclaren_points_yes %>% 
  ggplot() +
  geom_spoke(aes(x=x, y=y, angle = angle), radius = 1, colour = '#FF8000') +
  geom_image(aes(x+cos(angle), y+sin(angle), image = label)) +
  geom_image(data = mclaren_points_yes_random_rows, aes(x+cos(angle), y+sin(angle), image = label), colour = '#FF8000', size=0.2) +
  geom_image(data = mclaren_points_yes_random_rows_ghost, aes(x+cos(angle), y+sin(angle), image = label), colour = 'white', size=0.1) +
  theme_void() +
  darklyplot::theme_dark2() +
  theme(legend.position = "none",
        plot.margin = unit(c(50, 0, 20, 0), "pt"),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 48, family = "creepster",hjust = 0.5, colour = '#52E252'),
        plot.caption = element_text(size = 24, family = "creepster",hjust = 0.5, colour = '#B6BABD')) +
  coord_fixed() +
  transition_time(time=frame) +
  #shadow_wake(0.15) +
  labs(title = "Happy Halloween", caption = "design by hey-jay")

# animate
animate(mclaren_pins_animate_halloween, fps=5)

# animation save
anim_save("./04_gifs/mclaren_pins_animate_halloween.gif", height = 372, width = 538, units = "px")

# *5.2 2nd pattern----
# 2nd pattern 
# n_points is the number of points for the mclaren logo
n_points  <- 10
closeness <- 0
speed     <- 2*pi/n_points
v_angles <- seq(0, by=pi/2, length.out = n_points)

# This function creates a grid of vectors (coordinates and angle)
# using a initial vector of angles adding factor f each iteration
create_grid <- function(n, a, f) {
  lapply(seq_len(n), function (x) {a+f*(x-1)}) %>% 
    do.call("rbind", .) %>% 
    melt(varnames=c('x', 'y'), value.name="angle")
}

# This is what makes to spin the pins - inspect the structure of the output -> df 
lapply(1:(n_points+1), function(x) {
  create_grid(n_points, 
              v_angles+(x-1)*speed,
              closeness)}) %>% 
  as.list(.) %>% 
  rbindlist(idcol="frame") -> df

# images/icons for icons rotating
lando_logo <- image_read(here("00_raw_data", "lando_logo512.png")) %>% 
  image_crop(geometry = "-10") %>% 
  image_scale("x960") %>% 
  image_write(here("00_raw_data", "lando_logo_magick.png"), format = "png", quality = 75)

print(lando_logo)

max_logo <- image_read(here("00_raw_data", "max_logo2.png")) %>% 
  image_crop(geometry = "360x360+50") %>% 
  image_scale("x960") %>% 
  image_modulate(brightness = 80, saturation = 120, hue = 200) %>% 
  image_write(here("00_raw_data", "max_logo_magick.png"), format = "png", quality = 75)

print(max_logo)

max_logo_svg <- image_read(here("00_raw_data", "max-verstappen.svg")) %>% 
  image_crop(geometry = "450x350+100") %>%
  #image_convert("png") %>% 
  #image_fill('steelblue', fuzz = 0) %>% 
  #image_colorize(opacity = 40, color = 'white') %>%
  image_modulate(brightness = 80, saturation = 120, hue = 200) 
#%>% 
  image_write(here("00_raw_data", "max_logo_magick2.svg"), format = "svg", quality = 75)

print(max_logo_svg)

df <- df %>% 
  mutate(label = case_when(y %% 2 == 0 ~ lando_logo,
                           TRUE ~ max_logo)) 

# **5.2.1 McLaren dots in grid----

# x variable from the square pattern
# df_x <- df %>% 
#   select(x) %>% 
#   rename(original_x = x)

# new x - stepwise, single coordinates repeated n times
# https://stackoverflow.com/questions/2894775/repeat-each-row-of-data-frame-the-number-of-times-specified-in-a-column
# for n points = 83, freq = 83
new_df_x <- grid_mclaren_simple_rescale %>%
  #slice(1:10) %>% 
  select(x) %>% 
  mutate(freq = 83) %>% 
  slice(rep(seq_len(n()), freq)) %>% 
  select(-freq) 
#%>% slice(1:6889)

# for n points = 83, seq_len(83)
# then repeat previous step n times
df_x_rep <- purrr::map_dfr(seq_len(83), ~new_df_x)

# y variable from the rescaled McLaren logo coordinates
new_df_y <- grid_mclaren_simple_rescale %>%
  #slice(1:10) %>%
  select(y) %>% 
  rename(new_y = y)

# y variable from the square pattern
df_y <- df %>%
  select(y) %>%
  rename(original_y = y)

# https://stackoverflow.com/questions/66434941/make-a-column-based-a-repetitive-numbers-that-follows-another-column
# transpose new McLaren x-cordinates to same length as square pattern
df_y_rep <- df_y %>%
  mutate(new_col = rep(list(new_df_y), n())) %>% 
  unnest(new_col) %>% 
  slice(1:578676)

# bind new x and y's
new_xy <- df_x_rep %>% 
  bind_cols(df_y_rep) %>% 
  select(x, new_y) %>% 
  rename(y = new_y)

# reclaim frame and angle variables from original square pattern
df_frame_angle <- df %>% 
  select(frame, angle)

# bind frame, angle variables to new x,y variables
# add row numbers in case needed later
df_mclaren <- new_xy %>% 
  bind_cols(df_frame_angle) %>% 
  select(frame, x, y, angle) %>% 
  mutate(row = row_number())

# obtain just the coordinates for the McLaren data points
# this data set could be used for input to ggplot/animate
mclaren_points_yes <- df_mclaren %>%
  inner_join(grid_mclaren_simple_rescale, by = c('x', 'y'), relationship = "many-to-many") %>% 
  mutate(mclaren_point = 'yes') %>% 
  filter(!duplicated(row)) 

# **5.2.2 Animate McLaren pins----
# plot and animate
mclaren_pins_animate <- df %>% 
  ggplot() +
  #geom_spoke(aes(x=x, y=y, angle = angle), radius = 1, colour = '#FF8000') +
  geom_image(aes(x+cos(angle), y+sin(angle), image = label)) +
  #geom_point(aes(x+cos(angle), y+sin(angle)), size=3, colour = '#FF8000') +
  #geom_text(aes(x+cos(angle), y+sin(angle), label = label), family = 'fontawesome-webfont') +
  theme_void() +
  darklyplot::theme_dark2() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.caption = element_text(size = 14, family = "alfa",hjust = 0.5, colour = '#B6BABD')) +
  coord_fixed() +
  transition_time(time=frame) +
  #shadow_wake(0.15) +
  labs(caption = "design by hey-jay")

# animate
animate(mclaren_pins_animate, fps=10)

# animation save
anim_save("./04_gifs/mclaren_pins_animate_lando_max.gif", height = 372, width = 538, units = "px")

  



# 8 original script----
# 1st pattern
n_points  <- 10
closeness <- 2*pi/n_points
speed     <- 2*pi/n_points
v_angles <- seq(0, 2*pi, length.out = n_points)

# 2nd pattern
n_points  <- 83
closeness <- 0
speed     <- 2*pi/n_points
v_angles <- seq(0, by=pi/2, length.out = n_points)

# 3rd pattern - use this
n_points  <- 10
closeness <- 2*pi/n_points
speed     <- 2*pi/n_points
v_angles <- seq(0, 0, length.out = n_points)

# 4th pattern
n_points  <- 10
closeness <- pi/4
speed     <- 2*pi/n_points
v_angles <- seq(0, by=pi/4, length.out = n_points)

# This function creates a grid of vectors (coordinates and angle)
# using a initial vector of angles adding factor f each iteration
create_grid <- function(n, a, f) {
  lapply(seq_len(n), function (x) {a+f*(x-1)}) %>% 
    do.call("rbind", .) %>% 
    melt(varnames=c('x', 'y'), value.name="angle")
}

# This is what makes to spin the pins 
lapply(1:(n_points+1), function(x) {
  create_grid(n_points, 
              v_angles+(x-1)*speed,
              closeness)}) %>% 
  as.list(.) %>% 
  rbindlist(idcol="frame") -> df

# Plot pins using frame as transition time
ggplot(df) +
  geom_spoke(aes(x=x, y=y, angle = angle), radius = 1) +
  geom_point(aes(x+cos(angle), y+sin(angle)), size=2) +
  theme_void() + 
  coord_fixed() +
  transition_time(time=frame)

# Do you want to save it?
anim_save("choose_a_name.gif")
