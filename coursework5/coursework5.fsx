(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 5: Working with data: intro to type providers and charting

  ------------------------------------
  Name: Vladyslav Kopylash
  TUT Student ID: vlkopy
  ------------------------------------


  Answer the questions below.  You answers to questions 1--6 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by October 29, 2016.
*)

// 1) The following three different fuel consumption units for vehicles are in use:
//    * litres per 100 km
//    * miles per imperial gallon (in use in the UK)
//    * miles per US gallon (in use in the US)
// 1.a) Define the units in terms of units of measure.
[<Measure>] type lp100km
[<Measure>] type impg
[<Measure>] type usmpg
// 1.b) Define 2 functions that convert the appropriate US and imperial mpg values to
//      litres per 100 km. 
let convertImpgToLp100km (impg:float<impg>) : float<lp100km>  = 100.0<lp100km> * 4.54609<impg> / 1.609344 / impg
convertImpgToLp100km 40.0<impg>
convertImpgToLp100km 11.0<impg>

let convertUSmpgToLp100km (usmpg:float<usmpg>) : float<lp100km>  = 100.0<lp100km> * 3.785411784<usmpg> / 1.609344 / usmpg
convertUSmpgToLp100km 4.0<usmpg>

// 1.c) Define a function that converts litres per 100 km of appropriate fuel to
//      CO2 emissions g per km.
[<Measure>] type gpkm
let convertLp100kmToCO2emissionForPetrol (lph:float<lp100km>) : float<gpkm> = 23.2<gpkm> / 1.0<lp100km> * lph
convertLp100kmToCO2emissionForPetrol 10.0<lp100km>

let convertLp100kmToCO2emissionForDiesel (lph:float<lp100km>) : float<gpkm> = 26.5<gpkm> / 1.0<lp100km> * lph
convertLp100kmToCO2emissionForDiesel 10.0<lp100km>

// 2) Get the fuel consumption data
// 2.a) in imperial MPG (miles per imperial gallon) of at least 20 vehicles from
// http://carfueldata.direct.gov.uk/search-by-fuel-economy.aspx
// Save the data in file called imperial.csv

// 2.b) Get the fuel consumption data of at least 20 cars in US MPG (miles per US gallon) from
// https://www.fueleconomy.gov/feg/download.shtml
// save the data in file called us.csv

// 3) Load the imperial.csv and us.csv files using FSharp.Data.CsvProvider<>
#r @"packages\FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"
open FSharp.Data

type IMPGProvider = FSharp.Data.CsvProvider<"imperial.csv">
let impgData = IMPGProvider.Load("imperial.csv")


type USMPGProvider = FSharp.Data.CsvProvider<"us.csv">
let usmpgData = USMPGProvider.Load("us.csv")


// 4) Write a function to convert the appropriate mpg data into
//    litres per 100 km using the functions defined in Q1.
let IMPGRows = impgData.Rows 
let convertAndFilterIMPGRows rows = rows |> Seq.map (fun (r:IMPGProvider.Row) -> (r.``Fuel type``, convertImpgToLp100km(r.``Imperial combined`` * 1.0<impg>), r.CO2))

let convertedIMPGData = convertAndFilterIMPGRows IMPGRows |> Seq.sortBy (fun (_, consumption, _) -> consumption)


let USMPGRows = usmpgData.Rows
let convertAndFilterUSMPGRows rows = rows |> Seq.map (fun (r:USMPGProvider.Row) -> (r.Fuel, convertUSmpgToLp100km(r.``Cmb MPG`` * 1.0<usmpg>), r.``Comb CO2``))

let convertedUSMPGData = convertAndFilterUSMPGRows USMPGRows |> Seq.sortBy (fun (_, consumption, _) -> consumption)
// 5) Display the converted data in an appropriate chart (select the type that is most 
//    appropriate for displaying the data).

//To make it more interesting I decided to compare CO2 emission 
//depending on fuel consumption expressed in litres per 100 km
//for different types of fuel

#load @"packages\FSharp.Charting.0.90.14\FSharp.Charting.fsx"
open FSharp.Charting

let IMPGPetrolCO2Emission = convertedIMPGData |> Seq.filter (fun (fuel,_,_) -> fuel = "Petrol") |> Seq.map (fun (_,consumption, emission) -> (consumption, emission))
let IMPGDieselCO2Emission = convertedIMPGData |> Seq.filter (fun (fuel,_,_) -> fuel = "Diesel") |> Seq.map (fun (_,consumption, emission) -> (consumption, emission))

Chart.Combine(
   [ Chart.Line(IMPGPetrolCO2Emission,"PetrolIMPG","CO2 emission (IMPG data)")
     Chart.Line(IMPGDieselCO2Emission,"DieselIMPG") ])
     |> Chart.WithLegend(InsideArea = false) 
     |> Chart.WithYAxis(true,"CO2 g/km") |> Chart.WithXAxis(true,"L/100 km",Min=3.0)

let USMPGPetrolCO2Emission = convertedUSMPGData |> Seq.filter (fun (fuel,_,_) -> fuel = "Petrol") |> Seq.map (fun (_,consumption, emission) -> (consumption, emission))
let USMPGDieselCO2Emission = convertedUSMPGData |> Seq.filter (fun (fuel,_,_) -> fuel = "Diesel") |> Seq.map (fun (_,consumption, emission) -> (consumption, emission))

Chart.Combine(
   [ Chart.Line(USMPGPetrolCO2Emission,"PetrolUSMPG","CO2 emission (USMPG data)")
     Chart.Line(USMPGDieselCO2Emission,"DieselUSMPG") ])
     |> Chart.WithLegend(InsideArea = false) 
     |> Chart.WithYAxis(true,"CO2 g/km") |> Chart.WithXAxis(true,"L/100 km",Min=5.0)



 // 6) Combine the data from 2 data sources into a single chart. Add appropriate titles and
//    legends. 
let combinedPetrolCO2Emission = Seq.concat([IMPGPetrolCO2Emission;USMPGPetrolCO2Emission])
let combinedDieselCO2Emission = Seq.concat([IMPGDieselCO2Emission;USMPGDieselCO2Emission])

Chart.Combine(
   [ Chart.Line(combinedPetrolCO2Emission,"Petrol","CO2 emission (combined data)")
     Chart.Line(combinedDieselCO2Emission,"Diesel") ]) 
     |> Chart.WithLegend(InsideArea = false) 
     |> Chart.WithYAxis(true,"CO2 g/km") |> Chart.WithXAxis(true,"L/100 km",Min=3.0)

