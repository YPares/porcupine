{-# LANGUAGE OverloadedStrings #-}

module Plotting where

import Graphics.Vega.VegaLite
import Data.Text (Text)


barPlot :: Text -> VLSpec
barPlot xName = 
    let enc = encoding
            . position X [PName xName, PmType Nominal, PAxis [AxGrid True, AxTitle xName]]
            . position Y [PName "binnedData", PAggregate Count, PmType Quantitative, PAxis [AxGrid False, AxTitle "count"]]
    in asSpec $ [mark Bar [MOpacity 1.0, MColor "#a3c6de"], enc []]
    
barPlot2 :: Text -> Text -> VLSpec
barPlot2 xName yName = 
    let enc = encoding
            . position X [PName xName, PmType Nominal, PAxis [AxGrid True, AxTitle xName]]
            . position Y [PName yName, PmType Quantitative, PAxis [AxGrid False, AxTitle yName]]
    in asSpec $ [mark Bar [MOpacity 1.0, MColor "#a3c6de"], enc []]
    
linePlot :: Text -> Text -> VLSpec
linePlot xName yName = 
  let enc = encoding
            . position X [PName xName, PmType Quantitative, PAxis [AxGrid True, AxTitle xName]]
            . position Y [PName yName, PmType Quantitative, PAxis [AxGrid False, AxTitle yName]]
  in asSpec $ [mark Line [MColor "green"], enc []]

density2DPlot :: Text -> Text -> (Double, Double) -> (Double, Double) -> VLSpec
density2DPlot xName yName (xmin, xmax) (ymin, ymax) = 
  let enc = encoding
            . position X [PName xName, PScale [SDomain (DNumbers [xmin, xmax])], PBin [MaxBins 30], PmType Quantitative, PAxis [AxGrid True, AxTitle xName]]
            . position Y [PName yName, PScale [SDomain (DNumbers [ymin, ymax])], PBin [MaxBins 30], PmType Quantitative, PAxis [AxGrid True, AxTitle yName]]
            . color [ MAggregate Count, MName "col", MmType Quantitative, MScale [{-SReverse False,-} SScheme "blues" [0.0, 1.0]]]
  in asSpec $ [mark Rect [MClip True], enc []]
  
scatter2 :: Text -> Text -> (Double, Double) -> VLSpec
scatter2 xName yName (ymin, ymax) = 
  let enc = encoding
            . position X [PName xName, PmType Nominal, PAxis [AxGrid True, AxTitle xName]]
            . position Y [PName yName, PScale [SDomain (DNumbers [ymin, ymax])], PmType Quantitative, PAxis [AxGrid True, AxTitle yName]]
  in asSpec $ [mark Tick [MClip True], enc []]
  
plot :: (Double, Double) -> [VLSpec] -> [(Text, DataValues)] -> VegaLite
plot (figw,figh) layers samples =
    let desc = description "Plot"
        dataColumns = map (\(x,y) -> dataColumn x y) samples
        dat =  foldl (.) (dataFromColumns []) dataColumns
        conf = configure
            . configuration (Axis [ DomainWidth 1 ])
            . configuration (SelectionStyle [ ( Single, [ On "dblclick" ] ) ])
            . configuration (View [ViewStroke (Just "transparent")])
    in toVegaLite [width figw, height figh, conf [], desc, dat [], layer layers]
