library(shiny)
library(grid)
library(yaml)

# Heliconius colours
hmel.col<-c(black  = "black",
            red    = rgb(1,0.2,0,1),
            yellow = rgb(1,1,0.4,1),
            white  = rgb(1,1,0.6,1)
           )

# load coordinates of wings and elements
coords<-yaml.load_file("melpomene_pattern_coordinates.yml")

wingPatternElement<-function(element) {
    e<-coords[[element]]
    polygonGrob(e$Xval,
                e$Yval,
                default.units="native",
                gp=gpar(col=hmel.col[e$col], fill=hmel.col[e$col])
    )
}

whiteForewingPatches<-function() {
    gList(
        wingPatternElement("whiteForewingPatch1"),
        wingPatternElement("whiteForewingPatch2"),
        wingPatternElement("whiteForewingPatch3"),
        wingPatternElement("whiteForewingPatch4")
    )
}

redForewingPatches<-function() {
    gList(
        wingPatternElement("redForewingPatch1"),
        wingPatternElement("redForewingPatch2"),
        wingPatternElement("redForewingPatch3"),
        wingPatternElement("redForewingPatch4")
    )
}

DennisForewingPatches<-function() {
    gList(
        wingPatternElement("DennisForewingPatch1"),
        wingPatternElement("DennisForewingPatch2"),
        wingPatternElement("DennisForewingPatch3"),
        wingPatternElement("DennisForewingPatch4")
    )
}

raysForewingPatches<-function() {
    gList(
        wingPatternElement("raysForewingPatch1"),
        wingPatternElement("raysForewingPatch2"),
        wingPatternElement("raysForewingPatch3"),
        wingPatternElement("raysForewingPatch4"),
        wingPatternElement("raysForewingPatch5"),
        wingPatternElement("raysForewingPatch6")
    )
}

shinyServer(function(input, output) {
    
    melpomene<-reactive({

        wing<-gList(wingPatternElement("blackForewing"),
                    wingPatternElement("blackHindwing"))

        if (input$N) wing<-gList(wing, whiteForewingPatches())
        if (input$B) {
          if (input$N) wing<-gList(wing, redForewingPatches()) else wing<-gList(wing, wingPatternElement("redForewingBand"))
        }
        if (input$D) wing<-gList(wing, DennisForewingPatches(), wingPatternElement("DennisHindwingBand"))
        if (input$R) wing<-gList(wing, raysForewingPatches())
        if (input$Y) wing<-gList(wing, wingPatternElement("yellowHindwingBand"))

        wing
    })

    output$wing<-renderPlot({
        grid.newpage()
        pushViewport(viewport(0.5,0.49,xscale=c(0,250),yscale=c(250,0)))
        grid.draw(melpomene())
    }, width=600,height=600)
})