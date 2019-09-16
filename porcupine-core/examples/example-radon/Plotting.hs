{-# LANGUAGE OverloadedStrings #-}

module Plotting where

import qualified Graphics.Vega.VegaLite as VL
import Data.Text (Text, pack, unpack)


barPlot :: Text -> VL.VLSpec
barPlot xName = 
    let enc = VL.encoding
            . VL.position VL.X [VL.PName xName, VL.PmType VL.Nominal, VL.PAxis [VL.AxGrid True, VL.AxTitle xName]]
            . VL.position VL.Y [VL.PName "binnedData", VL.PAggregate VL.Count, VL.PmType VL.Quantitative, VL.PAxis [VL.AxGrid False, VL.AxTitle "count"]]
    in VL.asSpec $ [VL.mark VL.Bar [VL.MOpacity 1.0, VL.MColor "#a3c6de"], enc []]
    
barPlot2 :: Text -> Text -> VL.VLSpec
barPlot2 xName yName = 
    let enc = VL.encoding
            . VL.position VL.X [VL.PName xName, VL.PmType VL.Nominal, VL.PAxis [VL.AxGrid True, VL.AxTitle xName]]
            . VL.position VL.Y [VL.PName yName, VL.PmType VL.Quantitative, VL.PAxis [VL.AxGrid False, VL.AxTitle yName]]
    in VL.asSpec $ [VL.mark VL.Bar [VL.MOpacity 1.0, VL.MColor "#a3c6de"], enc []]
    
linePlot :: Text -> Text -> VL.VLSpec
linePlot xName yName = 
  let enc = VL.encoding
            . VL.position VL.X [VL.PName xName, VL.PmType VL.Quantitative, VL.PAxis [VL.AxGrid True, VL.AxTitle xName]]
            . VL.position VL.Y [VL.PName yName, VL.PmType VL.Quantitative, VL.PAxis [VL.AxGrid False, VL.AxTitle yName]]
  in VL.asSpec $ [VL.mark VL.Line [VL.MColor "green"], enc []]

density2DPlot :: Text -> Text -> (Double, Double) -> (Double, Double) -> VL.VLSpec
density2DPlot xName yName (xmin, xmax) (ymin, ymax) = 
  let enc = VL.encoding
            . VL.position VL.X [VL.PName xName, VL.PScale [VL.SDomain (VL.DNumbers [xmin, xmax])], VL.PBin [VL.MaxBins 30], VL.PmType VL.Quantitative, VL.PAxis [VL.AxGrid True, VL.AxTitle xName]]
            . VL.position VL.Y [VL.PName yName, VL.PScale [VL.SDomain (VL.DNumbers [ymin, ymax])], VL.PBin [VL.MaxBins 30], VL.PmType VL.Quantitative, VL.PAxis [VL.AxGrid True, VL.AxTitle yName]]
            . VL.color [ VL.MAggregate VL.Count, VL.MName "col", VL.MmType VL.Quantitative, VL.MScale [{-VL.SReverse False,-} VL.SScheme "blues" [0.0, 1.0]]]
  in VL.asSpec $ [VL.mark VL.Rect [VL.MClip True], enc []]
  
scatter2 :: Text -> Text -> (Double, Double) -> VL.VLSpec
scatter2 xName yName (ymin, ymax) = 
  let enc = VL.encoding
            . VL.position VL.X [VL.PName xName, VL.PmType VL.Nominal, VL.PAxis [VL.AxGrid True, VL.AxTitle xName]]
            . VL.position VL.Y [VL.PName yName, VL.PScale [VL.SDomain (VL.DNumbers [ymin, ymax])], VL.PmType VL.Quantitative, VL.PAxis [VL.AxGrid True, VL.AxTitle yName]]
  in VL.asSpec $ [VL.mark VL.Tick [VL.MClip True], enc []]
  
plot :: (Double, Double) -> [VL.VLSpec] -> [(Text, VL.DataValues)] -> VL.VegaLite
plot (figw,figh) layers samples =
    let desc = VL.description "Plot"
        dataColumns = map (\(x,y) -> VL.dataColumn x y) samples
        dat =  foldl (.) (VL.dataFromColumns []) dataColumns
        conf = VL.configure
            . VL.configuration (VL.Axis [ VL.DomainWidth 1 ])
            . VL.configuration (VL.SelectionStyle [ ( VL.Single, [ VL.On "dblclick" ] ) ])
            . VL.configuration (VL.View [VL.ViewStroke (Just "transparent")])
    in VL.toVegaLite [VL.width figw, VL.height figh, conf [], desc, dat [], VL.layer layers]
