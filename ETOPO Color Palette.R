require(scales)

cpt <- read_table2( "~/R-Studio/Projects/Shiny/Bathos/ETOPO1 color scheme.cpt",
                    col_names = FALSE,
                    col_types = "diii",
                    n_max = 44 )
colnames( cpt ) <- c( "level", "R", "G", "B" )
cpt$R <- toupper( as.hexmode( cpt$R ) )
cpt$G <- toupper( as.hexmode( cpt$G ) )
cpt$B <- toupper( as.hexmode( cpt$B ) )
cpt$RGB <- paste( "#",cpt$R, cpt$G, cpt$B, sep = "" )

palette <- list( cmin = cpt$level[1] / 1000, cmax = cpt$level[44] / 1000,
                 colorscale = list( rescale( cpt$level, to = c(0,1)), cpt$RGB ) )

saveRDS( palette, "~/R-Studio/Projects/Shiny/Bathos/colors.RDS")
