# formulas for cable
setwd("C:/Data/Cable Design")

# call the specific gravity data from the csv file in the current directory
SG_Table <- read.csv(file = "SG_List.csv", header = TRUE, sep = ",")
SG_Table$Item <- as.character(SG_Table$Item)
SG_Table$Description <- as.character(SG_Table$Description)
SG_Table <- unique(SG_Table[,1:3])

# To call a specific gravity from the data.frame SG_Table
# find specific gravity from the table
# print "Compound" "Not Found" if error
FindSG <- function(Compound) {
        # print(Compound)
        SG_actual <- SG_Table[SG_Table$Item == Compound,3]
        if (!Compound %in% SG_Table$Item) {
                print("INVALID Compound - COMPOUND NOT FOUND")
                0
        } else {
        SG_Table[SG_Table$Item == Compound,3]
}}

# calculate the gap area between three circles

AreaBetw3Circle <- function(Dia1,Dia2,Dia3) {
        r1 <- Dia1/2; r2 <- Dia2/2; r3 <- Dia3/2
        aa <- r2+r3; bb <- r1+r3; cc <- r1+r2
        At <- .25 * ((aa+bb+cc)*(aa+bb-cc)*(aa-bb+cc)*(-aa+bb+cc))^.5
        alpha <- acos((bb^2+cc^2-aa^2)/(2*bb*cc))
        beta <- acos((aa^2+cc^2-bb^2)/(2*aa*cc))
        gamma <- acos((aa^2+bb^2-cc^2)/(2*bb*aa))
        areaA <- alpha / 2 * r1^2
        areaB <- beta / 2 * r2^2
        areaC <- gamma / 2 * r3^2
        At - areaA - areaB - areaC
}

# Import Kfactor Table from http://www.cmcorporation.com/cable-and-shielding-information/k-factor-chart
# Cable Diameter Factors (K) for Multi-Conductor Cables
# The overall diameter (before tape or drains) of the cable may be estimated by multiplying the diameter
# of one of the identical components by the K factor shown below. Accurate only with lay-ups indicated.
# 1 Number of uniform size conductors
# 5 Layup description
# 6 Unilay      conductor diameter (d) * factor = cable diameter
# 7 Concentric  conductor diameter (d) * factor = cable diameter
# 8 Pairs       conductor diameter (d) * factor = cable diameter
# added diameter center of the conductor to the center of the cable to determine conductor length
# 2 Unilay      Cable Axis to center of outer conductor * 2
# 3 Pairs       Cable Axis to center of outer conductor * 2
# 4 Concentric  Cable Axis to center of outer conductor * 2
# Weight calculation factors
# 9  added Area of Outside Interstice:             d^2 * factor    = Area
# 10 added Diameter to Fit Outside Interstice:     d * factor      = conductor diameter
# 11 added Area of the Center Interstice:          d^2 * factor    = Area
# 12 added Diameter to Fit Center Interstice:      d * factor      = filler or conductor diameter

KFactors <- read.csv(file = "KFactorTable.csv", header = TRUE, sep = ",")
KFactors$Layup <- as.character(KFactors$Layup)

# fix the following 3 functions to include compound input and a single common dataframe output
# df(OD, ID, Wall, lb_ft, Compound, SG)

# weight per foot from OD and ID
lb_ft_OD_ID <- function(OD,ID, Compound=483866) {
        SG <- FindSG(Compound)
        Area <- (OD^2-ID^2)/4*pi
        lb_ft <- (OD^2-ID^2)/4*pi*SG*12*.0361273
        Wall <- (OD - ID)/2
        df <- round(data.frame(OD, ID, Wall, lb_ft, Compound, SG, Area),4)
        print(df)
}

# weight per foot from OD and Wall
lb_ft_OD_Wall <- function(OD,Wall,Compound=483866) {
        SG <- FindSG(Compound)
        lb_ft <- (OD^2-(OD - 2*Wall)^2)/4*pi*SG*12*.0361273
        ID <- OD - Wall*2
        Area <- (OD^2-ID^2)/4*pi
        df <- round(data.frame(OD, ID, Wall, lb_ft, Compound, SG, Area),4)
        print(df)
}

# average wall from weight per foot and OD
Average_Wall <- function(Weight, OD, Compound=483866) {
        SG <- FindSG(Compound)
        Wall <- (OD - ((-4 / pi * Weight / SG /12 / .0361273) + OD^2)^0.5) / 2
        lb_ft <- Weight
        Area <- lb_ft / (pi * SG * 12 * .0361273)
        ID <- OD - 2 * Wall
        df <- round(data.frame(OD, ID, Wall, lb_ft, Compound, SG, Area),4)
        print(df)
}

# Outer Layer with braid and tape calculator
# Diameter Total = Dia_Total
# Diameter of 1st Pass = Dia_1st_Pass
# Enable Specific Gravity Lookup from Material = Compound
# Specific Gravity Outer Layer = SG_Outer
# Tape Over 1st Pass Thickness = Tape1_Thickness
# 2nd Tape Over 1st Pass Thickness = Tape2_Thickness
# Braid Over 1st Pass Wire Diameter = Braid_Wire_Dia
# Braid % Coverage Over 1st Pass = Braid_%_Coverage
# multi-line function example, last line will print

# need to sub Compound for SG_Outer, then use FindSG() to recall specific gravity
# need to build dataframe for output, repeat all inputs and solution, then function spits out the dataframe

Outer_Pass_Weight <- function(Dia_Total = 3.000, Dia_1st_Pass = 2.000, Compound = 483866, Tape1_Thickness = .0015, Tape2_Thickness = .010,
                            Braid_Wire_Dia = 0.0126, Braid_Percent_Coverage = 0.88) {
        Area <- (Dia_Total^2- Dia_1st_Pass^2) * pi / 4 - ((Tape1_Thickness + Tape2_Thickness + 2 * Braid_Wire_Dia)
                                                          * pi * Dia_1st_Pass * Braid_Percent_Coverage)
        SG_Outer <- FindSG(Compound)
        Outer_Lb_Ft <- 0.0361273 * SG_Outer *12 * Area
        df <- round(data.frame(Dia_Total, Dia_1st_Pass, Compound, Tape1_Thickness, Tape2_Thickness,
                   Braid_Wire_Dia, Braid_Percent_Coverage, Area, Outer_Lb_Ft),4)
        print(df)
}

# conductor length in a multi-conductor cable
# conductors that lay in a different distance from the center of the cable have a different length
# LayLength = Lay Length
# Dia_of_Conductor_center = diameter of the conductor(s) center inside the cable
# Length of the conductor = (LL^2 + (pi * 2 * r)^2)^0.5
# where r is the distance from the center of the conductor to the cable axis

Cable_Loss <- function(LayLength, Dia_of_Conductor_Center) {
        LayLength/((LayLength^2 + (pi * Dia_of_Conductor_Center)^2)^0.5)
}

# cable loss using modified diameter tables (Factor - 1 diameter)
# need to re label the columns and develop a type to choose
# don't know how to do that exactly - but have to input 2 3 or 4 for column number
# wires <- as.numeric(unlist(strsplit(x, ",")))
# if(interactive()) cable()

cable_style = data.frame(
        Index= c("",2:4),
        Style = c('Number_of_Conductors_or_Pairs', '2 = Unilay_Singles', '3 = Unilay_Pairs', '4 = Concentric _Singles'))
cable_style$Index <- as.integer(cable_style$Index)
cable_style$Style <- as.character(cable_style$Style)
print("Choose index (2, 3, or 4) of type of cable, pair or single and cable lay for 'Style'")
print(cable_style$Style)

# need error message for unexpected values.......

cable <- function(wires, style) {
        layup <- KFactors[wires,5]
        center_distance_style <- style
        if (style == 2) {
                cable_diameter_style <- 6
        } else if (style == 3) {
                cable_diameter_style <- 8
        } else if (style == 4) {
                cable_diameter_style <- 7
        } else
                print("ERROR - CHOOSE 2, 3 OR 4 FOR STYLE")
        cable_description <- paste(c(wires, " Number of Pairs or Singles ", cable_style[style,2]))
        cabledf <- data.frame(wires, cable_style[style,2], layup, cable_diameter_style, center_distance_style)
        cabledf
}


# center_to_center= data.frame( 
#        No_Wires=c(1:33), 
#        Single_Unilay=c(0,1,1.155,1.414,1.701,2,2,2.305,2.613,2.828,2.924,3.030,3.195,3.236,3.521,3.553,
#                   3.842,3.820,3.864,4.458,4.179,4.179,4.458,4.494,4.666,4.856,4.879,5.044,5.065,
#                   5.126,5.363,5.383,5.442),
#        Pairs=c(1,2.2,2.48,2.91,3.43,3.91,4.26,4.49,5,5.35,5.51,5.51,5.96,6.03,6.5,6.56,7,
#                      7.04,7.07,7.6,7.6,8.06,8.12,8.41,8.41,8.72,8.76,9.03,9.07,9.17,9.56,9.6,9.69),
#        Single_Concentric = c(0,1,1.155,1.414,1.701,2,2,2.305,2.613,3,3,3.155,3.414,3.414,3.701,3.701,
#                       4,4,4,4.305,4.305,4.613,4.613,5,5.155,5.155,5.155,5.414,5.414,5.414,
#                       5.701,5.701,5.701))

# error message on unexpected entry
# df output style
# references may be out of place
# description of program is missing


Cable_Loss2 <- function(wires, Type, wire_dia, LayLength) {
        #print(type)
        #print("Choose index of type of conductor, pair or single and cable lay for 'type'")
        LayLength/((LayLength^2 + (pi * center_to_center[wires,Type] * wire_dia)^2)^0.5)
}
# formula for ground length is for up to 6 conductors, Calculation is for the length of one Lay Length of
# a ground nested in a valley
# wires = number of conductors
# wire_dia = conductor diameter
# gnd_dia = diameter of ground
# LayLength = Length of one twist of the conductors
# df output
# reference to values
# sufficient value limitations?

Gnd_Length <- function(wires, wire_dia, gnd_dia, LayLength) {
        if(wires<=1 | wires>=7) warning("Number of conductors > 0 and within the first layer max of [1+6]")
        height <- ((wire_dia/2 + gnd_dia/2)^2 - gnd_dia^2)^0.5
        height2 <- wire_dia/2 * tan((90-180/wires)/180 * pi)
        (LayLength^2 + (pi * (height2+height))^2)^0.5
}


# Weight per foot of the inner layer compound

# inputs
# # cable factor table
# Inner(Layer) Diameter = 		Target_Inner_Diameter
# Number of Conductors = 		No_Conductors
# Conductor Diameter = 			Conductor_Diameter
# Specific Gravity = 			SGinner
# Lay Length = 				Lay_Length
# Tape Thickness before 1st Jacket = 	Tape1_Thickness
# Tape2 Thickness before 1st Jacket = 	Tape2_Thickness
# Braid Wire diameter before 1st Jacket= Braid_Wire_Diameter
# Braid Wire coverage before 1st Jacket= Braid_Wire_Coverage
# Ground Conductor Diameter1 = 		Gnd_Diameter1
# Ground Conductor Diameter2 = 		Gnd_Diameter2
# Ground Conductor Diameter3 = 		Gnd_Diameter3
# Ground Conductor Diameter4 = 		Gnd_Diameter4
# Filler Diameter = 			Filler_Diameter
# Number of Fillers = 			No_Fillers
# Filler Diameter2 = 			Filler_Diameter2
# Number of Fillers2 = 			No_Fillers2
# "Number_of_Conductors_or_Pairs" "2 = Unilay_Singles"           
# "3 = Unilay_Pairs"              "4 = Concentric _Singles"

inner_layer <- function(Target_Inner_Diameter = 2.050, No_Conductors = 3, LayStyle = 2, Conductor_Diameter = .625,
                        Compound = "483866", Lay_Length = 30.5, Tape1_Thickness = .010,
                        Tape2_Thickness = 0.000, Braid_Wire_Diameter = 0.000, Braid_Wire_Coverage = 0.000,
                        Gnd_Diameter1 = 0.25, Gnd_Diameter2 = 0.25, Gnd_Diameter3 = 0.25,
                        Gnd_Diameter4 = 0.000, Filler_Diameter = 0.000, No_Fillers = 0.000,
                        Filler_Diameter2 = 0.000, No_Fillers2 = 0.000){
        
SGinner <- FindSG(Compound)
cabledf <- cable(No_Conductors,LayStyle)

#Table determined outputs - use the k factor table to find dimensions

Dia_of_Conductors <- KFactors[KFactors$No_Wires == No_Conductors, cabledf[1,4]] * Conductor_Diameter
Area_Interstices <- KFactors[KFactors$No_Wires == No_Conductors,9] * Conductor_Diameter^2 * No_Conductors
Dia_Outside_Interstice <- KFactors[KFactors$No_Wires == No_Conductors,10] * Conductor_Diameter
Area_Internal_Interstice <- KFactors[KFactors$No_Wires == No_Conductors,11] * Conductor_Diameter^2
DiaMaxInterstice <- KFactors[KFactors$No_Wires == No_Conductors,12] * Conductor_Diameter

# Calculate Areas of Components

AreaTargDiam <- pi * Target_Inner_Diameter^2 / 4

GndConductorArea <- pi / 4 * (Gnd_Diameter1^2 + Gnd_Diameter2^2 + Gnd_Diameter3^2 + Gnd_Diameter4^2)

FillerArea <- pi * Filler_Diameter^2 / 4 * No_Fillers + pi * Filler_Diameter2^2 / 4 * No_Fillers2

Cond_Area <- pi * Conductor_Diameter^2 / 4 * No_Conductors

TapeBraidArea <- (Tape1_Thickness + Tape2_Thickness + Braid_Wire_Diameter * 2) * Braid_Wire_Coverage * pi * Dia_of_Conductors

# Calculate the area between the grounds and the conductors, this function is called elsewhere for use

GapArea1 <- AreaBetw3Circle(Conductor_Diameter,Conductor_Diameter,Gnd_Diameter1)
GapArea2 <- AreaBetw3Circle(Conductor_Diameter,Conductor_Diameter,Gnd_Diameter2)
GapArea3 <- AreaBetw3Circle(Conductor_Diameter,Conductor_Diameter,Gnd_Diameter3)
GapArea4 <- AreaBetw3Circle(Conductor_Diameter,Conductor_Diameter,Gnd_Diameter4)
# print("Gap Area 1"); print(GapArea1)
GapArea <- GapArea1 + GapArea2 + GapArea3 + GapArea4
# print("Gap Area"); print(GapArea)
AreaSum <- AreaTargDiam - GndConductorArea - FillerArea - Cond_Area - TapeBraidArea - GapArea
# print("Area");print(AreaSum)

# Calculate the Area above the conductor diameter
# Calculate the Area below the conductor diameter

Dia_of_Conductors <- Dia_of_Conductors + 2 * (Tape1_Thickness + Tape2_Thickness)
AreaAbove <- (Target_Inner_Diameter^2- Dia_of_Conductors^2)/4*pi
AreaBelow <- AreaSum - AreaAbove

# Calculate if filler package is too large for available area
# Calculate if the grounds are above the conductor diameter 

fill_Gnd_Area <- FillerArea + GndConductorArea
fill_percent <- round((1-(Area_Interstices - fill_Gnd_Area)/Area_Interstices)*100, 1)
# print(paste(round(fill_Gnd_Area,3), round(Area_Interstices,3), round(FillerArea,3), round(GndConductorArea,3)))
fill_Gnd_Area <- round(fill_Gnd_Area,4)
Area_Interstices <- round(Area_Interstices,4)

if (Area_Interstices > fill_Gnd_Area) {
        print(paste(fill_Gnd_Area, Area_Interstices, "some left to fill", fill_percent))
}       else
                print(paste(fill_percent, "% error - too much filler and grounds planned"))
d1 <- Dia_Outside_Interstice
if (Gnd_Diameter1 > d1 | Gnd_Diameter2 > d1 | Gnd_Diameter3 > d1 | Gnd_Diameter4 > d1) {
        print(paste(Dia_Outside_Interstice, "ERROR - maximum valley diameter exceeded"))
}

# Calculate Length of 12 inches of Cable

TwistingLength <- (Lay_Length^2+(pi*(Dia_of_Conductors - Conductor_Diameter))^2)^.5 * 12 / Lay_Length

# Calculate the volume and the weight of the compound in cable per 1 foot of cable in pounds
# Apply twisting length to area below the conductor diameter
# store results in data.frame

lb_ft_inner <- (AreaBelow * TwistingLength + AreaAbove * 12 ) * SGinner * 0.03612729
dfinner <- data.frame(lb_ft_inner, fill_percent, Target_Inner_Diameter, Conductor_Diameter,
                 Compound, Lay_Length, Tape1_Thickness,
                 Tape2_Thickness, Braid_Wire_Diameter, Braid_Wire_Coverage,
                 Gnd_Diameter1, Gnd_Diameter2, Gnd_Diameter3,
                 Gnd_Diameter4, Filler_Diameter, No_Fillers,
                 Filler_Diameter2, No_Fillers2, Dia_of_Conductors, Dia_Outside_Interstice,AreaSum, AreaBelow)
dfinner
}
