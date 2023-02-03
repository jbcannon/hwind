# Functions for applying HWIND model

## Example:

```
library(tidyverse)
devtools::github_install('jbcannon/hwind')
library(hwind)

# Figure out the leaf area index of the pine stand
LAI = pred_LAI_longleaf(Dq=30, TPH=150, Hdom=30, age=30)

# Simulate a wind profile
# At landfall, sustained winds ~70 ms
# longlef LAI calculated above
# canopy height = 30 m
wp = wind_profile(ws = 20, LAI = LAI, ch = 30, plot = TRUE)

# get air density for Hurricane Michael at landfall
rho = air_dens(T_C=25, RH=90, P_mbar=1000)
print(rho)

# Can use standard value of 1.225 kg m-3
#rho = 1.225

# Making up data representing a tree
tree = data.frame(z = 1:30,
                  crown = c(rep(0.3, 20),
                  seq(0.3,4,length.out=5),
                  seq(4,0.3,length.out=5)))
head(tree)
# view tree
barplot(tree$crown, horiz = TRUE, asp=1)

# Create your own dynamic drag coefficient
def_Cd = function(ws) {
  sapply(ws, function(i) {
    if(is.na(i)) return(0)
    if(i < 4) return(0.75) else{
      if(i> 20) return(0.25) else {
        return(-0.5/16*i + 0.875)}
    }})}

df = wp %>% left_join(tree) %>%
  mutate(crown, ifelse(is.na(crown),0,crown)) %>%
  mutate(drag_kN = drag(crown, ws, Cd=def_Cd)/1000) %>%
  mutate(torque_kNm = drag_kN * (z-0.5))

fig_a = wp %>% ggplot(aes(y=z, x=ws)) +
  geom_line() +
  theme_bw() +
  labs(y='height (m)', x='windspeed (m/s)');fig_a

fig_b = tree %>% ggplot(aes(x=z, y=crown)) +
  geom_area() + coord_flip()+
  theme(aspect.ratio = 1) +
  theme_bw() +
  labs(y = 'crown area (m2)', x = 'height(m)')

scale = 5
fig_c = ggplot(wp, aes(x=z, y=ws)) +
  geom_line(color='black', linetype='dashed') +
  geom_line(data=tree, aes(x=z, y=crown*scale),size=1, color='dark green') +
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name="crown area (m2")) +
  theme_bw() +
  labs(x='height (m)', y='windspeed (m/s)');fig_c

fig_d = df %>% ggplot(aes(y=torque_kNm, x = z)) +
  geom_line() +
  geom_point() +
  labs(y='torque (kN-m)', x = 'height (m)') +
  theme_bw(); fig_d

df2 = tibble(ws=0:40,
       torque_kNm = sapply(0:40, function(ws) {
          wp = wind_profile(ws = ws, LAI = LAI, ch = 30, plot = FALSE)
          out = wp %>% left_join(tree) %>% suppressMessages() %>%
            mutate(crown, ifelse(is.na(crown),0,crown)) %>%
            mutate(drag_kN = drag(crown, ws, Cd=def_Cd)/1000) %>%
            mutate(torque_kNm = drag_kN * z) %>%
            pull(torque_kNm) %>% sum(na.rm=TRUE)
          return(out)}))

ws_fig = df2 %>% ggplot(aes(x=ws, y=torque_kNm)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(y='total torque (kN-m)', x='windspeed (m/s)') +
  geom_hline(yintercept = 150, color='red', linetype='dashed');ws_fig

print(model_fig)
print(ws_fig)
model_fig = ggpubr::ggarrange(fig_a, fig_b, fig_c, fig_d, labels = letters[1:4]);model_fig
ggsave('model_fig.png', model_fig, dpi=600, height=6, width=6)
ggsave('ws_fig.png', ws_fig, dpi=600, height=4, width=4)
```