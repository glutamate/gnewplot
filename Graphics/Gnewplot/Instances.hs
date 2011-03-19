{- |

Predefined instances of PlotWithGnuplot for

* @[(Double,Double)]@ Plot as points or lines, according to Graphics.Gnewplot.Style

* @[((Double,Double),Double)]@ Plot durations as disconnected horizontal lines

* @(Double->Double, (Double,Double))@ Plot arbitrary functions with maximum and minimum values

* @TimeSeries@ Plot a timeseries with a step size, start time and a StorableVector of values.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, ExistentialQuantification #-}
module Graphics.Gnewplot.Instances where

import Graphics.Gnewplot.Types
import Graphics.Gnewplot.Exec
import Graphics.Gnewplot.Style

import System.Directory
import System.IO
import Control.Monad
import qualified Data.StorableVector as SV


instance PlotWithGnuplot [(Double,Double)] where
    getGnuplotCmd [] = return []
    getGnuplotCmd es = 
        do fnm <- ("/tmp/gnuplotevs"++) `fmap` uniqueIntStr
           writeEvts fnm es
           return [PL (concat ["\"", fnm, "\" using 1:2"]) "" "points" (removeFile fnm)]
        where writeEvts fp evs = do
                   h <- openFile fp WriteMode
                   forM_ evs $ \(t,v)-> hPutStrLn h $ show t++"\t"++show v
                   hClose h

instance PlotWithGnuplot [((Double,Double),Double)] where
    getGnuplotCmd [] = return []
    getGnuplotCmd es = 
        do fnm <- ("/tmp/gnuplotdurs"++) `fmap` uniqueIntStr
           writeEvts fnm es
           return [PL (concat ["\"", fnm, "\" using 1:($2)"]) "" "lines" (removeFile fnm)]
           where writeEvts fp durs = do
                   h <- openFile fp WriteMode
                   forM_ durs $ \((t1,t2),v)-> do 
                          hPutStrLn h $ show t1++"\t"++show v
                          hPutStrLn h $ show t2++"\t"++show v
                          hPutStrLn h $ show t2++"\tNaN"
                   hClose h



instance PlotWithGnuplot (Double->Double, (Double,Double)) where
    getGnuplotCmd (f,(t1,t2)) = 
        let dx = (t2-t1)/1000 
            xs = map (\p-> p*dx+t1) [0..999] 
        in getGnuplotCmd $ Lines [LineStyle 0] $ zip xs $ map f xs

data TimeSeries = TimeSeries { stepSize:: Double,
                               startTime :: Double,
                               tsValues ::(SV.Vector Double) }

instance PlotWithGnuplot TimeSeries where
    getGnuplotCmd (TimeSeries dt t1 vls) = do
           fnm <- ("/tmp/gnuplotsig"++) `fmap` uniqueIntStr
           h <- openBinaryFile fnm WriteMode
           SV.hPut h vls
           hClose h
           return $ [PL (concat ["\"", fnm, "\" binary format=\"%float64\" using ($0*",
                                    show dt, "+", show t1, "):1"] )
                       "" -- (show t1++"->"++show t2) 
                       "lines"
                       (removeFile fnm)]
