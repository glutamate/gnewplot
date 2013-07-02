{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, ExistentialQuantification #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, GADTs, ScopedTypeVariables, DeriveDataTypeable #-}

module Graphics.Gnewplot.Histogram where

import Control.Monad
import Data.Unique
import Data.List
import Data.Array.Unboxed
import System.Random
import System.Directory
import System.IO

import Graphics.Gnewplot.Types
import Graphics.Gnewplot.Exec

import Debug.Trace

{- stolen from gnuplot-0.3.3 (Henning Thieleman) -}

--histArr :: (Int,Int) -> [Double] -> UArray Int Double
histArr :: (Int, Int) -> [Int] -> UArray Int Double
histArr bnds is = accumArray (+) 0 bnds [( i, 1) | i<-is, inRange bnds i]

histValues :: Int -> [Double] -> [(Double,Double)]
histValues nbins vls = 
    let (hArr, lo, hi, binSize) = histList nbins vls
    in zip [lo, lo+binSize..hi] hArr

histList :: Int -> [Double] -> ([Double] , Double, Double, Double)
histList _ [] = ([], 0, 0, 1)
histList nbins vls = let lo = foldl1' min vls
                         hi = foldl1' max vls
                         num = realToFrac $ length vls
                         binSize = (hi-lo)/(realToFrac nbins+1)
                         ixs = map (\v-> floor $! (v-lo)/binSize ) vls
                         hArr = histArr (0,nbins-1) $ ixs
                         empiricalIntegral = sum $ map (*binSize) $ elems hArr
                     in ((/empiricalIntegral) `fmap` elems hArr, lo, hi, binSize)

histLogList :: Int -> [Double] -> [(Double, Double)]
histLogList _ [] = []
histLogList nbins vls = let lo = log $ foldl1' min vls
                            hi = log $ foldl1' max vls
                            num = realToFrac $ length vls
                            binSize = (hi-lo)/(realToFrac nbins+1)
                            ixs = map (\v-> floor $! (log v-lo)/binSize ) vls
                            hArr = histArr (0,nbins-1) $ ixs
                            binloc i = exp $ lo+(hi-lo) *(realToFrac i/realToFrac nbins)
                           
                            empiricalIntegral = sum $ map (\(i,freq) -> ((binloc i - binloc (i-1))*freq)) $ assocs hArr
                            res = map (\(i,freq) -> (binloc i, freq/empiricalIntegral)) $ assocs hArr
                        in trace ("ixs="++show (res)) $ trace ("bnds="++show (lo,hi)) 
                           $ res

{-histListBZ :: Double -> [Double] -> ([Double] , Double, Double, Double)
histListBZ _ [] = ([], 0, 0, 1)
histListBZ bz vls    = let lo = foldl1' min vls
                           hi = foldl1' max vls
                           binSize = bz
                           nbins = round $ (hi-lo)/bz
                           ixs = map (\v-> floor $! (v-lo)/binSize ) vls
                           hArr = histArr (0,nbins) $ ixs
                       in (elems hArr, lo, hi, binSize)

histListFixed :: Double -> Double -> Double -> [Double] -> [Double]
histListFixed t1 t2 dt [] = take (round $ (t2-t1)/dt) $ repeat 0
histListFixed t1 t2 dt vls = let nbins = round $ (t2-t1)/dt
                                 ixs = map (\v-> floor $! (v-t1)/dt ) vls
                                 hArr = histArr (0,nbins-1) $ ixs
                             in elems hArr -}


data Histo = Histo Int [Double]
           | HistoStyle String Int [Double]
           | HistoLog String Int [Double]


                   
instance PlotWithGnuplot Histo where
    getGnuplotCmd (Histo _ []) = return []
    getGnuplotCmd (HistoStyle _ _ []) = return []
    getGnuplotCmd (Histo n vls) = getGnuplotCmd (HistoStyle "boxes" n vls)
    getGnuplotCmd (HistoStyle sty n vls) = do
            fnm <- ("/tmp/gnuplothist"++) `fmap` uniqueIntStr
            writeHist fnm n vls
            return [PL (concat ["\"", fnm, "\" using 1:2"]) 
                       "" 
                       sty 
                       (removeFile fnm)]
        where writeHist fp n vls = do
                   let (counts, lo, hi, binSize) = histList n vls
                   --print n
                   --print (counts, lo, hi, binSize)
                   h <- openFile fp WriteMode
                   let dat = zip [lo, lo+binSize..hi] counts
                   forM_  dat $ \(x,y)-> hPutStrLn h $ show x++"\t"++show y
                   hClose h
    getGnuplotCmd (HistoLog sty n vls) = do
            fnm <- ("/tmp/gnuplothist"++) `fmap` uniqueIntStr
            writeHist fnm n vls
            return [PL (concat ["\"", fnm, "\" using 1:2"]) 
                       "" 
                       sty 
                       (return ())] -- removeFile fnm)]
        where writeHist fp n vls = do
                   let dat = histLogList n vls
                   --print n
                   --print (counts, lo, hi, binSize)
                   h <- openFile fp WriteMode
                   forM_  dat $ \(x,y)-> hPutStrLn h $ show x++"\t"++show y
                   hClose h

