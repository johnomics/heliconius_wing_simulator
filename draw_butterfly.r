

#function to draw a melpomene, takes as input the coordinates file, and the genotypes. Currently does 5 loci
melpomene <- function(coords,B,D,R,Yb,N){
  attach(coords)
  plot(0,0,cex = 0, ylim = c(250,0), xlim = c(0,250), xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
  
  #make black wings
  polygon(L_FW_X, L_FW_Y, col = "black", border = "black")
  polygon(L_HW_X, L_HW_Y, col = "black", border = "black")

  #patterns
  if (N == "N"){
    polygon(L_N1_X, L_N1_Y, col = rgb(1,1,0.6,1), border = rgb(1,1,0.6,1))
    polygon(L_N2_X, L_N2_Y, col = rgb(1,1,0.6,1), border = rgb(1,1,0.6,1))
    polygon(L_N3_X, L_N3_Y, col = rgb(1,1,0.6,1), border = rgb(1,1,0.6,1))
    polygon(L_N4_X, L_N4_Y, col = rgb(1,1,0.6,1), border = rgb(1,1,0.6,1))
    }

  if (B == "B"){
    if (N == "N"){
      polygon(L_BandN1_X, L_BandN1_Y, col = rgb(1,0.2,0,1), border = rgb(1,0.2,0,1))
      polygon(L_BandN2_X, L_BandN2_Y, col = rgb(1,0.2,0,1), border = rgb(1,0.2,0,1))
      polygon(L_BandN3_X, L_BandN3_Y, col = rgb(1,0.2,0,1), border = rgb(1,0.2,0,1))
      polygon(L_BandN4_X, L_BandN4_Y, col = rgb(1,0.2,0,1), border = rgb(1,0.2,0,1))
      } else {
    polygon(L_Band_X, L_Band_Y, col = rgb(1,0.2,0,1), border = rgb(1,0.2,0,1))
      }
    }
  
  if (D == "D"){
    polygon(L_Fwden1_X, L_Fwden1_Y, col = rgb(1,0.2,0,1), border = rgb(1,0.2,0,1))
    polygon(L_Fwden2_X, L_Fwden2_Y, col = rgb(1,0.2,0,1), border = rgb(1,0.2,0,1))
    polygon(L_Fwden3_X, L_Fwden3_Y, col = rgb(1,0.2,0,1), border = rgb(1,0.2,0,1))
    polygon(L_Fwden4_X, L_Fwden4_Y, col = rgb(1,0.2,0,1), border = rgb(1,0.2,0,1))
    polygon(L_Hwden_X, L_Hwden_Y, col = rgb(1,0.2,0,1), border = rgb(1,0.2,0,1))
    }
  
  if (R == "R") {
    polygon(L_ray1_X, L_ray1_Y, col = rgb(1,0.2,0,1), border = rgb(1,0.2,0,1))
    polygon(L_ray2_X, L_ray2_Y, col = rgb(1,0.2,0,1), border = rgb(1,0.2,0,1))
    polygon(L_ray3_X, L_ray3_Y, col = rgb(1,0.2,0,1), border = rgb(1,0.2,0,1))
    polygon(L_ray4_X, L_ray4_Y, col = rgb(1,0.2,0,1), border = rgb(1,0.2,0,1))
    polygon(L_ray5_X, L_ray5_Y, col = rgb(1,0.2,0,1), border = rgb(1,0.2,0,1))
    polygon(L_ray6_X, L_ray6_Y, col = rgb(1,0.2,0,1), border = rgb(1,0.2,0,1))
    }
  
  if (Yb == "Yb") {polygon(L_Yb_X, L_Yb_Y, col = rgb(1,1,0.4,1), border = rgb(1,1,0.4,1))
    }
  
  detach(coords)
  }


#load coordinates of wings and elements
melp_coords <- read.delim("~/Images/Butterflies/Heliconius_drawing_code/pattern_coordinates.csv", sep = "\t")


#define genotypes
B <- "B" # B = band, b = None
D <- "d" # D = Dennis, d = none
R <- "r" # R = rays, r = none
N <- "n" # N = FW_white, n = none
Yb <- "Yb" # "Yb" = yellow bar, "yb" = none

#draw patterns
melpomene(melp_coords,B,D,R,Yb,N)




