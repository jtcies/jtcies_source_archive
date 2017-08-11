library('devtools')
library('ggthemr')
library('extrafont')
library('hrbrthemes')
loadfonts()

jtc_colors <- c("#4f4f4f", "#6b7a8f", "#f7882f", "#F7C331", "#dcc7aa",
                '#212121')

jtc_theme <- define_palette(swatch = jtc_colors, 
    gradient = c(lower = jtc_colors[1L], upper = jtc_colors[2L]),
    text = c("#4f4f4f", "#6b7a8f"),
    background = "#ffffff")

ggthemr(jtc_theme, layout = 'clean', text_size = 14)

jtc <- theme(text=element_text(family = 'Open Sans'),
             axis.title.y = element_text(hjust = 1, vjust = 2),
             axis.title.x = element_text(hjust = 1),
             plot.title = element_text(hjust = 0))
