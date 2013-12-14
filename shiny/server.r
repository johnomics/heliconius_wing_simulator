library(shiny)
library(grid)

#load coordinates of wings and elements
coords <- read.delim("melpomene_pattern_coordinates.tsv")

shinyServer(function(input, output) {
    
    melpomene<-reactive({

        wing<-gList(blackForewing=polygonGrob(coords$L_FW_X, coords$L_FW_Y, gp=gpar(col="black", fill="black"), default.units="native"),
                    blackHindwing=polygonGrob(coords$L_HW_X, coords$L_HW_Y, gp=gpar(col="black", fill="black"), default.units="native"))

        if (input$N) {
            wing<-gList(wing,
                    whiteForewingPatch1=polygonGrob(coords$L_N1_X, coords$L_N1_Y, gp=gpar(col=rgb(1,1,0.6,1), fill=rgb(1,1,0.6,1)), default.units="native"),
                    whiteForewingPatch2=polygonGrob(coords$L_N2_X, coords$L_N2_Y, gp=gpar(col=rgb(1,1,0.6,1), fill=rgb(1,1,0.6,1)), default.units="native"),
                    whiteForewingPatch3=polygonGrob(coords$L_N3_X, coords$L_N3_Y, gp=gpar(col=rgb(1,1,0.6,1), fill=rgb(1,1,0.6,1)), default.units="native"),
                    whiteForewingPatch4=polygonGrob(coords$L_N4_X, coords$L_N4_Y, gp=gpar(col=rgb(1,1,0.6,1), fill=rgb(1,1,0.6,1)), default.units="native")
                  )
        }

        if (input$B) {
          if (input$N) {
            wing<-gList(wing,
                      redForewingPatch1=polygonGrob(coords$L_BandN1_X, coords$L_BandN1_Y, gp=gpar(col=rgb(1,0.2,0.1), fill=rgb(1,0.2,0,1)), default.units="native"),
                      redForewingPatch2=polygonGrob(coords$L_BandN2_X, coords$L_BandN2_Y, gp=gpar(col=rgb(1,0.2,0.1), fill=rgb(1,0.2,0,1)), default.units="native"),
                      redForewingPatch3=polygonGrob(coords$L_BandN3_X, coords$L_BandN3_Y, gp=gpar(col=rgb(1,0.2,0.1), fill=rgb(1,0.2,0,1)), default.units="native"),
                      redForewingPatch4=polygonGrob(coords$L_BandN4_X, coords$L_BandN4_Y, gp=gpar(col=rgb(1,0.2,0.1), fill=rgb(1,0.2,0,1)), default.units="native")
                  )
          } else {
            wing<-gList(wing,redForewingBand=polygonGrob(coords$L_Band_X, coords$L_Band_Y, gp=gpar(col=rgb(1,0.2,0,1), fill=rgb(1,0.2,0,1)), default.units="native"))
          }
        }
  
        if (input$D) {
            wing<-gList(wing,
                    DennisForewingPatch1=polygonGrob(coords$L_Fwden1_X, coords$L_Fwden1_Y, gp=gpar(col=rgb(1,0.2,0,1), fill=rgb(1,0.2,0,1)), default.units="native"),
                    DennisForewingPatch2=polygonGrob(coords$L_Fwden2_X, coords$L_Fwden2_Y, gp=gpar(col=rgb(1,0.2,0,1), fill=rgb(1,0.2,0,1)), default.units="native"),
                    DennisForewingPatch3=polygonGrob(coords$L_Fwden3_X, coords$L_Fwden3_Y, gp=gpar(col=rgb(1,0.2,0,1), fill=rgb(1,0.2,0,1)), default.units="native"),
                    DennisForewingPatch4=polygonGrob(coords$L_Fwden4_X, coords$L_Fwden4_Y, gp=gpar(col=rgb(1,0.2,0,1), fill=rgb(1,0.2,0,1)), default.units="native"),
                    DennisHindwingBand=polygonGrob(coords$L_Hwden_X, coords$L_Hwden_Y, gp=gpar(col=rgb(1,0.2,0,1), fill=rgb(1,0.2,0.1)), default.units="native")
                )
        }

        if (input$R) {
            wing<-gList(wing,
                RaysForewingPatch1=polygonGrob(coords$L_ray1_X, coords$L_ray1_Y, gp=gpar(col=rgb(1,0.2,0,1), fill=rgb(1,0.2,0,1)), default.units="native"),
                RaysForewingPatch2=polygonGrob(coords$L_ray2_X, coords$L_ray2_Y, gp=gpar(col=rgb(1,0.2,0,1), fill=rgb(1,0.2,0,1)), default.units="native"),
                RaysForewingPatch3=polygonGrob(coords$L_ray3_X, coords$L_ray3_Y, gp=gpar(col=rgb(1,0.2,0,1), fill=rgb(1,0.2,0,1)), default.units="native"),
                RaysForewingPatch4=polygonGrob(coords$L_ray4_X, coords$L_ray4_Y, gp=gpar(col=rgb(1,0.2,0,1), fill=rgb(1,0.2,0,1)), default.units="native"),
                RaysForewingPatch5=polygonGrob(coords$L_ray5_X, coords$L_ray5_Y, gp=gpar(col=rgb(1,0.2,0,1), fill=rgb(1,0.2,0,1)), default.units="native"),
                RaysForewingPatch6=polygonGrob(coords$L_ray6_X, coords$L_ray6_Y, gp=gpar(col=rgb(1,0.2,0,1), fill=rgb(1,0.2,0,1)), default.units="native")
                )
        }
  
        if (input$Y) {
            wing<-gList(wing,
                    polygonGrob(coords$L_Yb_X, coords$L_Yb_Y, gp=gpar(col=rgb(1,1,0.4,1), fill=rgb(1,1,0.4,1)), default.units="native")
                )
          }

        wing
    })

    output$wing<-renderPlot({
        grid.newpage()
        pushViewport(viewport(0.5,0.49,xscale=c(0,250),yscale=c(250,0)))
        grid.draw(melpomene())
    }, width=600,height=600)
})



