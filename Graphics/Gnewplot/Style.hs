{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, ExistentialQuantification #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, GADTs, ScopedTypeVariables, DeriveDataTypeable #-}

module Graphics.Gnewplot.Style where


--import Math.Probably.FoldingStats hiding (F)
import Control.Monad
import Data.List

import Graphics.Gnewplot.Types

--newtype Lines a = Lines {unLines :: a }
--newtype Dashed a = Dashed {unDashed :: a }
newtype Boxes a = Boxes {unBoxes :: a }

data Lines a = Lines [StyleOpt] a
data LinesPoints a = LinesPoints [StyleOpt] a
data Points a = Points [StyleOpt] a

data StyleOpt = LineWidth Double 
              | LineType Int
              | LineStyle Int
              | LineColor String
              | PointType Int
              | PointSize Double

styleOptsToString :: [StyleOpt] -> String
styleOptsToString = intercalate " " . map g
    where g (LineType lt) = "lt "++show lt
          g (LineWidth lt) = "lw "++show lt
          g (LineStyle lt) = "ls "++show lt
          g (LineColor lc) = "lc rgb "++show lc
          g (PointType lt) = "pt "++show lt
          g (PointSize lt) = "ps "++show lt



instance PlotWithGnuplot a => PlotWithGnuplot (Lines a) where
    multiPlot r (Lines sos x) = do
      px <- multiPlot r x
      let wstr = styleOptsToString sos
      return $ map (\(r', pls) -> (r', setWith ("lines "++wstr) pls)) px

instance PlotWithGnuplot a => PlotWithGnuplot (LinesPoints a) where
    multiPlot r (LinesPoints sos x) = do
      px <- multiPlot r x
      let wstr = styleOptsToString sos
      return $ map (\(r', pls) -> (r', setWith ("linespoints "++wstr) pls)) px


instance PlotWithGnuplot a => PlotWithGnuplot (Points a) where
    multiPlot r (Points sos x) = do
      px <- multiPlot r x
      let wstr = styleOptsToString sos
      return $ map (\(r', pls) -> (r', setWith ("points "++wstr) pls)) px

instance PlotWithGnuplot a => PlotWithGnuplot (Boxes a) where
    multiPlot r (Boxes x) = do
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', setWith "boxes" pls)) px


data XRange a = XRange Double Double a
data YRange a = YRange Double Double a

data XTics a = XTics [Double] a
data YTics a = YTics [Double] a

data TicFormat a = TicFormat XY String a 

data XY = X | Y | XY

data Noaxis a = Noaxis a
              | NoXaxis a
              | NoYaxis a
              | NoTRaxis a


data Key a = KeyTopLeft Bool a 
           | KeyTopRight Bool a 

instance PlotWithGnuplot a => PlotWithGnuplot (XRange a) where
    multiPlot r m@(XRange lo hi x) = do
      px <- multiPlot r x
      let setit = "set xrange ["++show lo++":"++show hi++"]\n"
      return $ map (\(r', pls) -> (r', (TopLevelGnuplotCmd setit "set xrange [*:*]"):pls)) px

instance PlotWithGnuplot a => PlotWithGnuplot (TicFormat a) where
    multiPlot r m@(TicFormat xy s x) = do
      px <- multiPlot r x
      let whereStr X = "x"
          whereStr Y = "y"
          whereStr XY = "xy"
      let setit = "set format "++whereStr xy++" "++show s++"\n"
      return $ map (\(r', pls) -> (r', (TopLevelGnuplotCmd setit "unset format"):pls)) px


instance PlotWithGnuplot a => PlotWithGnuplot (Key a) where
    multiPlot r m@(KeyTopLeft box x) = do
      px <- multiPlot r x
      let boxs = if box then "box" else ""
      let setit = "set key top left "++boxs++"\n"
      return $ map (\(r', pls) -> (r', (TopLevelGnuplotCmd setit "set key default"):pls)) px

instance PlotWithGnuplot a => PlotWithGnuplot (Noaxis a) where
    multiPlot r m@(Noaxis x) = do
      px <- multiPlot r x
      let cmd = TopLevelGnuplotCmd "unset border; unset tics" "set border; set tics"
      return $ map (\(r', pls) -> (r', cmd:pls)) px
    multiPlot r m@(NoYaxis x) = do
      px <- multiPlot r x
      let cmd = TopLevelGnuplotCmd "set border 1; set tics" "set border; set tics"
      return $ map (\(r', pls) -> (r', cmd:pls)) px
    multiPlot r m@(NoTRaxis x) = do
      px <- multiPlot r x
      let cmd = TopLevelGnuplotCmd "set border 3; set tics; set xtics nomirror; set ytics nomirror" "set border; set tics"
      return $ map (\(r', pls) -> (r', cmd:pls)) px

instance PlotWithGnuplot a => PlotWithGnuplot (XTics a) where
    multiPlot r m@(XTics tics x) = do
      px <- multiPlot r x
      let setit = "set xtics "++ (intercalate ", " $ map show tics) ++"\n"
      return $ map (\(r', pls) -> (r', (TopLevelGnuplotCmd setit "set xtics autofreq"):pls)) px


instance PlotWithGnuplot a => PlotWithGnuplot (YTics a) where
    multiPlot r m@(YTics tics x) = do
      px <- multiPlot r x
      let setit = "set ytics "++ (intercalate ", " $ map show tics) ++"\n"
      return $ map (\(r', pls) -> (r', (TopLevelGnuplotCmd setit "set xtics autofreq"):pls)) px


instance PlotWithGnuplot a => PlotWithGnuplot (YRange a) where
    multiPlot r m@(YRange lo hi x) = do
      px <- multiPlot r x
      let setit = "set yrange ["++show lo++":"++show hi++"]\n"
      return $ map (\(r', pls) -> (r', (TopLevelGnuplotCmd setit "set yrange [*:*]"):pls)) px


lineWidth w = Lines [LineWidth w]
lineType t = Lines [LineType t]
pointSize t = Points [PointSize t]
pointType t = Points [PointType t]

data CustAxis = CustAxis {
      caOrigin :: (Double,Double),
      caLength :: Double,
      caVertical :: Bool,
      caTicLen :: Double,
      caTicOffset :: Double,
      caTics :: [(Double, String)]
}

data WithAxis a = WithAxis CustAxis a

instance PlotWithGnuplot a => PlotWithGnuplot (WithAxis a) where
    multiPlot r (WithAxis (CustAxis (x0,y0) len True tlen toff tics) x) = do
      let ticCmds (y,txt)  = 
              [TopLevelGnuplotCmd ("set arrow from first "++show x0++","++show (y0+y)++
                                   " to first "++show (x0-tlen)++","++show (y0+y)++" nohead front") 
                                  "unset arrow",
               TopLevelGnuplotCmd ("set label "++show txt++" at first "++
                                    show (x0-toff)++","++show (y0+y)++" right front") 
                                   "unset label"
               ]
      let cmds = TopLevelGnuplotCmd ("set arrow from first "++show x0++","++show y0++
                                   " to first "++show x0++","++show (y0+len)++" nohead front") 
                                  "unset arrow" : concatMap ticCmds tics
      px <- multiPlot r $ x
      return $ map (\(r', pls) -> (r', cmds++pls)) px


data ScaleBars a = ScaleBars (Double, Double) (Double,String) (Double,String) a
                 | XScaleBar (Double, Double) (Double,String) Double a
                 | YScaleBar (Double, Double) (Double,String) Double a

data LineAt a = LineAt (Double, Double) (Double, Double) a
data ArrowAt a = ArrowAt (Double, Double) (Double, Double) a

data TextAt a = TextAt (Double, Double) String a
              | TextAtLeft (Double, Double) String a
              | TextAtRot (Double,Double) String a
instance PlotWithGnuplot a => PlotWithGnuplot (TextAt a) where
    multiPlot r (TextAt (x0,y0) s x) = do
      let mklab = TopLevelGnuplotCmd ("set label "++show s++" at first "++show x0++","++show y0++" center front") 
                                     "unset label"
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklab:pls)) px
    multiPlot r (TextAtRot (x0,y0) s x) = do
      let mklab = TopLevelGnuplotCmd ("set label "++show s++" at first "++show x0++","++show y0++" center front rotate") 
                                     "unset label"
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklab:pls)) px
    multiPlot r (TextAtLeft (x0,y0) s x) = do
      let mklab = TopLevelGnuplotCmd ("set label "++show s++" at first "++show x0++","++show y0++" left front") 
                                     "unset label"
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklab:pls)) px


instance PlotWithGnuplot a => PlotWithGnuplot (LineAt a) where
    multiPlot r (LineAt (x0,y0) (x1, y1) x) = do
      let mklab = TopLevelGnuplotCmd ("set arrow from first "++show x0++","++show y0++" to first "++show x1++","++show y1++" nohead front") 
                                     "unset arrow"
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklab:pls)) px 

instance PlotWithGnuplot a => PlotWithGnuplot (ArrowAt a) where
    multiPlot r (ArrowAt (x0,y0) (x1, y1) x) = do
      let mklab = TopLevelGnuplotCmd ("set arrow from first "++show x0++","++show y0++" to first "++show x1++","++show y1++" heads front") 
                                     "unset arrow"
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklab:pls)) px 


instance PlotWithGnuplot a => PlotWithGnuplot (ScaleBars a) where
    multiPlot r (ScaleBars p0 (xsz, xtxt) (ysz, ytxt) x) = do
      multiPlot r $ XScaleBar p0 (xsz, xtxt) (ysz/4) $ YScaleBar p0 (ysz, ytxt) (xsz/2) x     
    multiPlot r (XScaleBar (x0,y0) (xsz, xtxt) yo x) = do
      let xtxtpos = (x0+xsz/2, y0 - yo)
      multiPlot r $ LineAt (x0, y0) (x0+xsz, y0) 
                  $ TextAt xtxtpos xtxt x
    multiPlot r (YScaleBar (x0,y0)  (ysz, ytxt) yo x) = do
      let ytxtpos = (x0+yo, y0 + ysz/2)                                                             
      multiPlot r $ LineAt (x0, y0) (x0, y0+ysz) 
                  $ TextAt ytxtpos ytxt x

data TicFont a = TicFont String a

instance PlotWithGnuplot a => PlotWithGnuplot (TicFont a) where
    multiPlot r (TicFont str x) = do
      let mklab = TopLevelGnuplotCmd ("set tics font "++show str)
                                     ""
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklab:pls)) px



data CentreLabel = CentreLabel String

instance PlotWithGnuplot CentreLabel where
    multiPlot r (CentreLabel str)  = do      
      let mklab = TopLevelGnuplotCmd ("set label "++show str++" at graph 0.5,0.5 center front") "unset label"
      nop::[GnuplotCmd] <- fmap (map snd) $ multiPlot r Noplot

      return [(r, mklab:concat nop)]


data AxisLabels a = AxisLabels String String a
                  | XLabel String a
                  | YLabel String a

instance PlotWithGnuplot a => PlotWithGnuplot (AxisLabels a) where
    multiPlot r (AxisLabels xlab ylab x) = do
      let mklabs = [TopLevelGnuplotCmd ("set xlabel "++show xlab) "unset xlabel", 
                    TopLevelGnuplotCmd ("set ylabel "++show ylab) "unset ylabel"]
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklabs++pls)) px
    multiPlot r (XLabel xlab x) = do
      let mklabs = [TopLevelGnuplotCmd ("set xlabel "++show xlab) "unset xlabel"]
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklabs++pls)) px
    multiPlot r (YLabel xlab x) = do
      let mklabs = [TopLevelGnuplotCmd ("set ylabel "++show xlab) "unset ylabel"]
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklabs++pls)) px


instance PlotWithGnuplot a => PlotWithGnuplot (String, a) where
    multiPlot r (title, x) = do
      pls <- multiPlot r x
      return $ map (\(r', plines) -> (r' ,map (addTitle title) plines)) pls
      where addTitle title (PL x _ y clean) = PL x title y clean

