brown_birds_lines <- c("brown bear", "red bird", "yellow duck", "blue horse")

for (i in 2:length(brown_birds_lines) - 1) {
  print(paste(brown_birds_lines[i], ",", brown_birds_lines[i], ", what do you see? I see a", brown_birds_lines[i + 1], "looking at me."))
}
