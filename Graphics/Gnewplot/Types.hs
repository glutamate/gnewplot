{- | 

Definitions of the PlotWithGnuplot class.

-} 

{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, ExistentialQuantification #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, GADTs, ScopedTypeVariables, DeriveDataTypeable #-}

module Graphics.Gnewplot.Types where

import Control.Monad
import Data.List

semiColonConcat = concat . intersperse "; "

quote :: String -> String
quote = show
                 
type GnuplotCmd = [PlotLine]

data PlotLine = PL {plotData :: String,
                    plotTitle :: String,
                    plotWith :: String,
                    cleanUp :: IO () }
              | SPL {plotData :: String,
                    plotTitle :: String,
                    plotWith :: String,
                    cleanUp :: IO () }
              | TopLevelGnuplotCmd String String

instance Show PlotLine where
   show (PL d t w _ ) = "PL "++show d++" "++show t ++" "++show w
   show (TopLevelGnuplotCmd a b) = "TopLevelGnuplotCmd "++ show a ++ " "++show b

plOnly pls = [pl | pl@(PL _ _ _ _) <- pls]
splOnly pls = [pl | pl@(SPL _ _ _ _) <- pls]
tlOnlyUnset pls = [s2 | pl@(TopLevelGnuplotCmd s1 s2) <- pls]
tlOnly pls = [s1 | pl@(TopLevelGnuplotCmd s1 s2) <- pls]

hasPl pls = not $ null $ plOnly pls 
hasSpl pls = not $ null $ splOnly pls 

cleanupCmds :: [GnuplotCmd] -> IO ()
cleanupCmds cmds = forM_ cmds $ \plines -> sequence_ $ map cleanUp $ plOnly plines ++ splOnly plines

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
showPlotCmd plines 
 | hasPl plines = tls++"\nplot "++(intercalate ", " $ map p $ plOnly $ plines)++"\n"++untls
 | hasSpl plines = tls++"\nsplot "++(intercalate ", " $ map s $ splOnly $ plines)++"\n"++untls
    where s (SPL dat tit wth _) = dat++title tit++withStr wth
          p (PL dat tit wth _) = dat++title tit++withStr wth
          tls = unlines $ tlOnly plines
          untls = unlines $ tlOnlyUnset plines
          title "" = " notitle"
          title tit = " title '"++tit++"'"
          withStr "" = ""
          withStr s = " with "++s 

showMultiPlot :: [(Rectangle, GnuplotCmd)] -> String
showMultiPlot rpls = "set multiplot\n" ++ concatMap pl rpls ++"\nunset multiplot\n"
    where pl (r@(Rect (x0,y0) (x1,y1)), plines)=concat ["#"++show r++"\n",
                                                        "set origin ", 
                                                        show x0, ",", show y0, "\n",
                                                        "set size ", show (x1-x0),
                                                        ",", show (y1-y0), "\n",
                                                        showPlotCmd plines]
                                                      
-- | coordinates for a rectangle 
data Rectangle = Rect (Double, Double) (Double,Double) deriving Show
unitRect = Rect (0,0) (1,1)

rectTopLeft (Rect (x1,y1) (x2,y2)) = (x1+0.035,y2-0.010) 

-- | the main class - implement either getGnuplotCmd or muliplot
class PlotWithGnuplot a where
    getGnuplotCmd :: a -> IO GnuplotCmd
    getGnuplotCmd a = (snd . head) `fmap` multiPlot unitRect a

    multiPlot :: Rectangle -> a -> IO [(Rectangle, GnuplotCmd)]
    multiPlot r a = (\x->[(r, x)]) `fmap` getGnuplotCmd a

instance (PlotWithGnuplot a, PlotWithGnuplot b) => PlotWithGnuplot (Either a b) where
    multiPlot r (Left xs) = multiPlot r xs
    multiPlot r (Right xs) = multiPlot r xs

-- | An existential box for anythign plottable
data GnuplotBox = forall a. PlotWithGnuplot a => GnuplotBox a

instance PlotWithGnuplot GnuplotBox where
    multiPlot r (GnuplotBox x) = multiPlot r x

instance PlotWithGnuplot [GnuplotBox] where
    getGnuplotCmd xs = concat `fmap` mapM getGnuplotCmd xs


-- | the empty (blank) plot
data Noplot = Noplot

instance PlotWithGnuplot Noplot where
    getGnuplotCmd _ = return [PL "x" "" "lines lc rgb \"white\"" (return () ),
                             TopLevelGnuplotCmd "unset border; unset tics" "set border; set tics"]

-- | Gnuplot's test plot
data GnuplotTest = GnuplotTest

instance PlotWithGnuplot (GnuplotTest) where
    multiPlot r _ = do
      return $ [(r, [TopLevelGnuplotCmd "test" ""])]


