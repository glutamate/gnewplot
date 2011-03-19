{- |
Graphics.Gnewplot.Panels defines constructors the allow plots to be composed.

Example: 

@
import Graphics.Gnewplot.Instances
import Graphics.Gnewplot.Exec
import Graphics.Gnewplot.Panels

someData :: [(Double,Double)]
somedata = [(0.0, 1.0),(0.1, 2.0),(0.2, 1.4),(0.3, 1.7),(0.4, 1.0),
            (0.5, 1.8),(0.6, 1.1),(0.7, 1.5),(0.8, 1.2),(0.9, 1.9)]

moreData1 :: [(Double,Double)]
moredata1 = map (\(x,y) -> (x+1.0)) someData

moreData2 :: [(Double,Double)]
moredata2 = map (\(x,y) -> (x-1.0)) someData

main = do 
  -- Two plots besides each other, evenly spaced
  gnuplotOnScreen $ someData :||: moreData1
  -- Two plottable things on the same panel
  gnuplotOnScreen $ someData :+: moreData1
  -- One plot above the other, evenly spaced and labeled
  gnuplotOnScreen $ A someData :==: B moreData1
  -- a more complicated layout that cannot be defined by a grid layout
  -- Panels Ai is above panel Aii and the are both next to panel B
  gnuplotOnScreen $ (Ai someData :==: Aii moreData1) :||: B moreData2
@

-}

{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, ExistentialQuantification #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, GADTs, ScopedTypeVariables, DeriveDataTypeable #-}

module Graphics.Gnewplot.Panels where

import Control.Monad
import Data.List

import Graphics.Gnewplot.Types

infixl 4 %
infixr 3 :+:
infixr 2 :|:
infixr 1 :--:

-- | superimposition
data a :+: b = a :+: b

-- | horizontal composition, evenly divided
data a :||: b = a :||: b

-- | horizontal composition, custom division.
--   For example: @plot $ 33 % xs :--: 66 % ys@.
--   The percentages do not have to add to 100
data a :|: b = PcntDiv a :|: PcntDiv b

-- | vertical composition, even division
data a :--: b = PcntDiv a :--: PcntDiv b

-- | vertical composition, custom division
data a :==: b =  a :==: b

data PcntDiv a = Pcnt Double a

data WithColour a = WithColour String a

x % a = Pcnt x a

instance (PlotWithGnuplot a, PlotWithGnuplot b) => PlotWithGnuplot (a :||: b) where
    multiPlot r (xs :||: ys) = multiPlot r (50% xs :|: 50% ys)

instance (PlotWithGnuplot a, PlotWithGnuplot b) => PlotWithGnuplot (a :==: b) where
    multiPlot r (xs :==: ys) = multiPlot r (50% xs :--: 50% ys)


instance (PlotWithGnuplot a, PlotWithGnuplot b) => PlotWithGnuplot ( a :|: b) where
    multiPlot (Rect (x0, y0) (x1,y1)) (Pcnt pcp p :|: Pcnt pcq q) = do
      let xsep = x0+(pcp/(pcp+pcq))*(x1-x0)
      px <- multiPlot ( Rect (x0,y0) (xsep, y1) ) p
      py <- multiPlot ( Rect (xsep,y0) (x1, y1) ) q
      return $ px++py 

instance (PlotWithGnuplot a, PlotWithGnuplot b) => PlotWithGnuplot ( a :--: b) where
    multiPlot (Rect (x0, y0) (x1,y1)) (Pcnt pcp p :--: Pcnt pcq q) = do
      let ysep = y0+(pcq/(pcp+pcq))*(y1-y0)
      px <- multiPlot ( Rect (x0,y0) (x1, ysep) ) q
      py <- multiPlot ( Rect (x0, ysep) (x1, y1) ) p
      return $ py++px 

instance (PlotWithGnuplot a, PlotWithGnuplot b) => PlotWithGnuplot (a :+: b) where
    multiPlot r (xs :+: ys) = do
      px <- getGnuplotCmd xs
      py <- getGnuplotCmd ys                          
      return $ [(r,px++py)]


-- | Add a label to the panel
data SubLabel a = 
    A a | Ai a | Aii a | Aiii a
  | B a | Bi a | Bii a | Biii a
  | C a | Ci a | Cii a | Ciii a
  | D a | Di a | Dii a | Diii a
  | E a | F a | G a 
  | SubNum Int a

subLabSplit :: SubLabel a -> (String, a)
subLabSplit (A x) = ("A",x)
subLabSplit (Ai x) = ("Ai",x)
subLabSplit (Aii x) = ("Aii",x)
subLabSplit (Aiii x) = ("Aiii",x)
subLabSplit (B x) = ("B",x)
subLabSplit (Bi x) = ("Bi",x)
subLabSplit (Bii x) = ("Bii",x)
subLabSplit (Biii x) = ("Biii",x)
subLabSplit (C x) = ("C",x)
subLabSplit (Ci x) = ("Ci",x)
subLabSplit (Cii x) = ("Cii",x)
subLabSplit (Ciii x) = ("Ciii",x)
subLabSplit (D x) = ("D",x)
subLabSplit (Di x) = ("Di",x)
subLabSplit (Dii x) = ("Dii",x)
subLabSplit (Diii x) = ("Diii",x)
subLabSplit (E x) = ("E",x)
subLabSplit (F x) = ("F",x)
subLabSplit (G x) = ("G",x)
subLabSplit (SubNum n x) = (show n, x)

instance PlotWithGnuplot a => PlotWithGnuplot (SubLabel a) where
    multiPlot r sl = do
      let (lab, x ) = subLabSplit sl
          (xpos, ypos) = rectTopLeft r
      let mklab = TopLevelGnuplotCmd ("set label "++show lab++" at screen "++show xpos++","++show ypos++" front") 
                                     "unset label"
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklab:pls)) px


-- | Set the buttom, top left, and right margins
data Margin a = Margin Double Double Double Double a
                               
instance PlotWithGnuplot a => PlotWithGnuplot (Margin a) where
    multiPlot r m@(Margin _ _ _ _ x) = do
      px <- multiPlot r x
      let setit = setMargin m
      return $ map (\(r', pls) -> (r', (TopLevelGnuplotCmd setit unsetMargin):pls)) px
     where unsetMargin = unlines ["unset bmargin ",
                       "unset lmargin ",
                       "unset rmargin ",
                       "unset tmargin "]
           setMargin (Margin b t l r _) = unlines ["set bmargin "++show b,
                                        "set lmargin "++show l,
                                        "set rmargin "++show r,
                                        "set tmargin "++show t]

-- | Scale the canvas (plot size) in horizontal and vertical dimensions
data CanvasScale a = CanvasScale Double Double a

instance PlotWithGnuplot a => PlotWithGnuplot (CanvasScale a) where
    multiPlot r (CanvasScale xsz ysz x) = do
      let mklab = TopLevelGnuplotCmd ("set size "++show xsz++","++show ysz)
                                     ""
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklab:pls)) px

-- | Add padding to a plot
data Pad a = Pad Double a
           | PadX Double Double a
           | PadY Double Double a

instance (PlotWithGnuplot a) => PlotWithGnuplot (Pad a) where
    multiPlot r (Pad p x) = multiPlot r $ PadX p p $ PadY p p x
    multiPlot (Rect (x0, y0) (x1,y1)) (PadX p1 p2 x) = do
      let xw = (x1 - x0)
      px <- multiPlot (Rect (x0+xw*p1,y0) (x1-xw*p2, y1) ) x
      return $ px
    multiPlot (Rect (x0, y0) (x1,y1)) (PadY p1 p2 x) = do
      let yh = (y1 - y0) 
      px <- multiPlot ( Rect (x0,y0+yh*p1) (x1, y1-yh*p2) ) x
      return $ px


data Hplots a = Hplots [a]
data Vplots a = Vplots [a]

instance (PlotWithGnuplot a) => PlotWithGnuplot (Hplots a) where
    multiPlot (Rect (x0, y0) (x1,y1)) (Hplots ps) = do
      let n = realToFrac $ length ps
      let xeach = (x1-x0)/n
      pls <- forM (zip ps [0..]) $ \(p,i) -> 
               multiPlot ( Rect (x0+(i*xeach),y0) (x0+((i+1)*xeach), y1) ) p
      return $ concat pls

instance (PlotWithGnuplot a) => PlotWithGnuplot (Vplots a) where
    multiPlot (Rect (x0, y0) (x1,y1)) (Vplots ps) = do
      let n = realToFrac $ length ps
      let yeach = (y1-y0)/n
      pls <- forM (zip ps [0..]) $ \(p,i) -> 
               multiPlot ( Rect (x0,y0+(i*yeach)) (x1, y0+((i+1)*yeach)) ) p
      return $ concat pls

newtype LabelConsecutively a = LabelConsecutively a

instance PlotWithGnuplot a => PlotWithGnuplot (LabelConsecutively [a]) where
    getGnuplotCmd (LabelConsecutively xs) = do
      pls::[ GnuplotCmd] <- mapM (getGnuplotCmd) xs
      return $ concatMap (\(rs,i)-> (addTitleMany (show i)) rs) $ zip pls [0..]
      where addTitle title (PL x _ y clean) = PL x title y clean
            addTitleMany :: String -> ( GnuplotCmd) -> ( GnuplotCmd)
            addTitleMany title (cmd) = ( map (addTitle title) cmd)

--tilePlots ::  PlotWithGnuplot a => Int -> [a] -> Vplots (Hplots a)
tilePlots :: Int -> [t] -> Vplots (Hplots (SubLabel (Either t Noplot)))
tilePlots n ps = let nps = (length ps) 
                     nfinal = if nps `mod` n == 0
                                 then nps
                                 else ((nps `div` n)+1)*n
                     allps = ensureLength (nfinal) Noplot ps
                 in Vplots $ map Hplots $ map (map (\(p,i) -> SubNum i p)) $ groupsOf n (zip allps [0..])

gridPlot :: [[GnuplotBox]] -> Vplots (Hplots GnuplotBox)
gridPlot plots = Vplots $ map Hplots plots

groupsOf n [] = []
groupsOf n xs = let (mine, rest) = splitAt n xs
                in mine: groupsOf n rest

ensureLength n filler xs = map Left xs++replicate (n - length xs) (Right filler)

