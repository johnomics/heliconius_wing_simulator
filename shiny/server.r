library(shiny)
library(grid)

#load coordinates of wings and elements
coords <- read.delim("melpomene_pattern_coordinates.tsv")

wingPatternElement<-function(x, y, col, fill) {
    polygonGrob(x, y, gp=gpar(col=col, fill=fill), default.units="native")
}

blackForewing<-function() {
    wingPatternElement(coords$L_FW_X, coords$L_FW_Y, "black", "black")
}

blackHindwing<-function() {
    wingPatternElement(coords$L_HW_X, coords$L_HW_Y, "black", "black")
}

whiteForewingPatches<-function() {
    gList(
        wingPatternElement(coords$L_N1_X, coords$L_N1_Y, rgb(1,1,0.6,1), rgb(1,1,0.6,1)),
        wingPatternElement(coords$L_N2_X, coords$L_N2_Y, rgb(1,1,0.6,1), rgb(1,1,0.6,1)),
        wingPatternElement(coords$L_N3_X, coords$L_N3_Y, rgb(1,1,0.6,1), rgb(1,1,0.6,1)),
        wingPatternElement(coords$L_N4_X, coords$L_N4_Y, rgb(1,1,0.6,1), rgb(1,1,0.6,1))
    )
}

redForewingPatches<-function() {
    gList(
        wingPatternElement(coords$L_BandN1_X, coords$L_BandN1_Y, rgb(1,0.2,0,1), rgb(1,0.2,0,1)),
        wingPatternElement(coords$L_BandN2_X, coords$L_BandN2_Y, rgb(1,0.2,0,1), rgb(1,0.2,0,1)),
        wingPatternElement(coords$L_BandN3_X, coords$L_BandN3_Y, rgb(1,0.2,0,1), rgb(1,0.2,0,1)),
        wingPatternElement(coords$L_BandN4_X, coords$L_BandN4_Y, rgb(1,0.2,0,1), rgb(1,0.2,0,1))
    )
}

redForewingBand<-function() {
    wingPatternElement(coords$L_Band_X, coords$L_Band_Y, rgb(1,0.2,0,1), rgb(1,0.2,0,1))
}

DennisForewingPatches<-function() {
    gList(
        wingPatternElement(coords$L_Fwden1_X, coords$L_Fwden1_Y, rgb(1,0.2,0,1), rgb(1,0.2,0,1)),
        wingPatternElement(coords$L_Fwden2_X, coords$L_Fwden2_Y, rgb(1,0.2,0,1), rgb(1,0.2,0,1)),
        wingPatternElement(coords$L_Fwden3_X, coords$L_Fwden3_Y, rgb(1,0.2,0,1), rgb(1,0.2,0,1)),
        wingPatternElement(coords$L_Fwden4_X, coords$L_Fwden4_Y, rgb(1,0.2,0,1), rgb(1,0.2,0,1))
    )
}

DennisHindwingBand<-function() {
    wingPatternElement(coords$L_Hwden_X, coords$L_Hwden_Y, rgb(1,0.2,0,1), rgb(1,0.2,0,1))
}

raysForewingPatches<-function() {
    gList(
        wingPatternElement(coords$L_ray1_X, coords$L_ray1_Y, rgb(1,0.2,0,1), rgb(1,0.2,0,1)),
        wingPatternElement(coords$L_ray2_X, coords$L_ray2_Y, rgb(1,0.2,0,1), rgb(1,0.2,0,1)),
        wingPatternElement(coords$L_ray3_X, coords$L_ray3_Y, rgb(1,0.2,0,1), rgb(1,0.2,0,1)),
        wingPatternElement(coords$L_ray4_X, coords$L_ray4_Y, rgb(1,0.2,0,1), rgb(1,0.2,0,1)),
        wingPatternElement(coords$L_ray5_X, coords$L_ray5_Y, rgb(1,0.2,0,1), rgb(1,0.2,0,1)),
        wingPatternElement(coords$L_ray6_X, coords$L_ray6_Y, rgb(1,0.2,0,1), rgb(1,0.2,0,1))
    )
}

yellowHindwingBand<-function() {
    wingPatternElement(coords$L_Yb_X, coords$L_Yb_Y, rgb(1,1,0.4,1), rgb(1,1,0.4,1))
}

shinyServer(function(input, output) {
    
    melpomene<-reactive({

        wing<-gList(blackForewing(),blackHindwing())

        if (input$N) wing<-gList(wing, whiteForewingPatches())

        if (input$B) {
          if (input$N) wing<-gList(wing, redForewingPatches()) else wing<-gList(wing, redForewingBand())
        }
  
        if (input$D) wing<-gList(wing, DennisForewingPatches(), DennisHindwingBand())

        if (input$R) wing<-gList(wing, raysForewingPatches())
  
        if (input$Y) wing<-gList(wing, yellowHindwingBand())

        wing
    })

    output$wing<-renderPlot({
        grid.newpage()
        pushViewport(viewport(0.5,0.49,xscale=c(0,250),yscale=c(250,0)))
        grid.draw(melpomene())
    }, width=600,height=600)
})