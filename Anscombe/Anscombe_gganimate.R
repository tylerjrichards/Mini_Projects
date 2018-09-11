library(devtools)
library(gganimate)
library(dplyr)
data(anscombe)
anscombe
install.packages("reshape")
library(reshape)
melt(anscombe)
anscombe

anscombe.1 <- data.frame(x = anscombe[["x1"]], y = anscombe[["y1"]], Set = "Anscombe Set 1")
anscombe.2 <- data.frame(x = anscombe[["x2"]], y = anscombe[["y2"]], Set = "Anscombe Set 2")
anscombe.3 <- data.frame(x = anscombe[["x3"]], y = anscombe[["y3"]], Set = "Anscombe Set 3")
anscombe.4 <- data.frame(x = anscombe[["x4"]], y = anscombe[["y4"]], Set = "Anscombe Set 4")

anscombe.data <- rbind(anscombe.1, anscombe.2, anscombe.3, anscombe.4)

gg <- ggplot(anscombe.data, aes(x = x, y = y)) + 
  geom_point(color = "black") + 
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE, data = anscombe.data) +
  labs(title = "Anscombe's Quartet") +
  transition_states(Set,
                    transition_length = 2,
                    state_length = 1)
gg
