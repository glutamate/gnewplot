{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, ExistentialQuantification #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, GADTs, ScopedTypeVariables, DeriveDataTypeable #-}

module Graphics.Gnewplot.Types where

import Control.Monad
import Data.List


--import EvalM

semiColonConcat = concat . intersperse "; "

quote :: String -> String
quote = show
                 


type GnuplotCmd = [PlotLine]

data PlotLine = PL {plotData :: String,
                    plotTitle :: String,
                    plotWith :: String,
                    cleanUp :: IO () }
              | TopLevelGnuplotCmd String String

plOnly pls = [pl | pl@(PL _ _ _ _) <- pls]
tlOnlyUnset pls = [s2 | pl@(TopLevelGnuplotCmd s1 s2) <- pls]
tlOnly pls = [s1 | pl@(TopLevelGnuplotCmd s1 s2) <- pls]

cleanupCmds :: [GnuplotCmd] -> IO ()
cleanupCmds cmds = forM_ cmds $ \plines -> sequence_ $ map cleanUp $ plOnly plines

setWith :: String -> GnuplotCmd -> GnuplotCmd
setWith sty = map f
    where f pl@(PL _ _ _ _) = pl {plotWith = sty }
          f tlcmd = tlcmd

addData :: String -> GnuplotCmd -> GnuplotCmd
addData s = map f
    where f pl@(PL _ _ _ _) = pl {plotData = plotData pl++s }
          f tlcmd = tlcmd


showPlotCmd :: GnuplotCmd -> String
showPlotCmd [] = ""
showPlotCmd [TopLevelGnuplotCmd s s2] = s ++ "\n"++ s2
showPlotCmd plines = tls++"\nplot "++(intercalate ", " $ map s $ plOnly $ plines)++"\n"++untls
    where s (PL dat tit wth _) = dat++title tit++withStr wth
          title "" = " notitle"
          title tit = " title '"++tit++"'"
          withStr "" = ""
          withStr s = " with "++s 
          tls = unlines $ tlOnly plines
          untls = unlines $ tlOnlyUnset plines

showMultiPlot :: [(Rectangle, GnuplotCmd)] -> String
showMultiPlot rpls = "set multiplot\n" ++ concatMap pl rpls ++"\nunset multiplot\n"
    where pl (r@(Rect (x0,y0) (x1,y1)), plines)=concat ["#"++show r++"\n",
                                                        "set origin ", 
                                                        show x0, ",", show y0, "\n",
                                                        "set size ", show (x1-x0),
                                                        ",", show (y1-y0), "\n",
                                                        showPlotCmd plines]
                                                      

data Rectangle = Rect (Double, Double) (Double,Double) deriving Show
unitRect = Rect (0,0) (1,1)

rectTopLeft (Rect (x1,y1) (x2,y2)) = (x1+0.035,y2-0.010) 

class PlotWithGnuplot a where
    getGnuplotCmd :: a -> IO GnuplotCmd
    getGnuplotCmd a = (snd . head) `fmap` multiPlot unitRect a

    multiPlot :: Rectangle -> a -> IO [(Rectangle, GnuplotCmd)]
    multiPlot r a = (\x->[(r, x)]) `fmap` getGnuplotCmd a

data GnuplotBox = forall a. PlotWithGnuplot a => GnuplotBox a

data Noplot = Noplot

instance PlotWithGnuplot Noplot where
    getGnuplotCmd _ = return [PL "x" "" "lines lc rgb \"white\"" (return () ),
                             TopLevelGnuplotCmd "unset border; unset tics" "set border; set tics"]

instance PlotWithGnuplot GnuplotBox where
    getGnuplotCmd (GnuplotBox x) = getGnuplotCmd x

instance PlotWithGnuplot [GnuplotBox] where
    getGnuplotCmd xs = concat `fmap` mapM getGnuplotCmd xs

data GnuplotTest = GnuplotTest

instance PlotWithGnuplot (GnuplotTest) where
    multiPlot r _ = do
      return $ [(r, [TopLevelGnuplotCmd "test" ""])]


