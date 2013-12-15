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
sim.data<-yaml.load_file("heliconius_wing_simulator.yml")

max.chrom.l<-0
for (i in 1:length(sim.data$Chromosomes)) {
    if (sim.data$Chromosomes[[i]]$length > max.chrom.l) max.chrom.l <- sim.data$Chromosomes[[i]]$length
}

chromosomeGrob<-function(chrnum, chrom) {
    chrtext<-chrnum
    if (chrnum == "Z") chrnum <- 21
    gList(
        linesGrob(
            c(chrnum*10-5, chrnum*10-5),
            c(max.chrom.l*0.1,chrom$length+max.chrom.l*0.1),
            default.units="native",
            gp=gpar(lwd=10,lineend="round")
        ),
        textGrob(
            chrtext, chrnum*10-5, 0,
            default.units="native",
            just=c("centre","top"),
            gp=gpar(fontsize=18)
        )
    )
}

geneGrob<-function(g,input) {
    g.on<-FALSE
    for (l in g$loci) {
        if (input[[l]]) {
            g.on<-TRUE
        }
    }

    g.fontface<-"plain"
    g.col<-"black"
    if (g.on) {
        g.fontface<-"bold"
        g.col<-"red"
    }
    gList(
        textGrob(g$name,
                 g$chromosome*10-3,
                 g$position+max.chrom.l*0.1,
                 default.units="native",
                 just=c("left","centre"),
                 gp=gpar(fontface=g.fontface,col=g.col)
        ),
        linesGrob(c(g$chromosome*10-6,g$chromosome*10-4),
                  c(g$position+max.chrom.l*0.1,g$position+max.chrom.l*0.1),
                  default.units="native",
                  gp=gpar(col=g.col,lwd=4)
        )
    )
}

wingPatternElement<-function(element, fill=TRUE) {
    e<-sim.data$wingPatternElements[[element]]
    if (fill) fillcol<-hmel.col[e$col] else fillcol<-"white"
    polygonGrob(e$Xval,
                e$Yval,
                default.units="native",
                gp=gpar(col=hmel.col[e$col], fill=fillcol)
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
    
    karyotype<-reactive({
        genome<-gList()
        for (i in 1:length(sim.data$Chromosomes)) {
            genome<-gList(genome, chromosomeGrob(i, sim.data$Chromosomes[[i]]))
        }
        
        for (g in sim.data$Genes) {
            genome<-gList(genome, geneGrob(g,input))
        }

        genome
    })
    
    melpomene<-reactive({

        wing<-gList(wingPatternElement("blackForewing"),
                    wingPatternElement("blackHindwing"))

        if (input$N) wing<-gList(wing, whiteForewingPatches())
        if (input$B) {
          if (input$N) wing<-gList(wing, redForewingPatches()) else wing<-gList(wing, wingPatternElement("redForewingBand"))
        }
        if (input$D) wing<-gList(wing, DennisForewingPatches(), wingPatternElement("DennisHindwingBand"))
        if (input$R) wing<-gList(wing, raysForewingPatches())
        if (input$Yb) wing<-gList(wing, wingPatternElement("yellowHindwingBand"))

        wing
    })

    output$karyotype<-renderPlot({
        grid.newpage()
        pushViewport(viewport(0.5,0.49,xscale=c(0,length(sim.data$Chromosomes)*10),yscale=c(max.chrom.l*1.1,0)))
        grid.draw(karyotype())
    },width=900,height=300)

    output$wing<-renderPlot({
        grid.newpage()
        pushViewport(viewport(0.5,0.49,xscale=c(0,250),yscale=c(250,0)))
        grid.draw(melpomene())
    }, width=400,height=400)
})