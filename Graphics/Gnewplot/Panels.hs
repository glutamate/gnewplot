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



data a :+: b = a :+: b

data ManySup a = ManySup [a]

data a :||: b = a :||: b
data a :|: b = PcntDiv a :|: PcntDiv b

data a :--: b = PcntDiv a :--: PcntDiv b
data a :==: b =  a :==: b

data Hplots a = Hplots [a]
data Vplots a = Vplots [a]

data PcntDiv a = Pcnt Double a

data WithColour a = WithColour String a

x % a = Pcnt x a

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



data Margin a = Margin Double Double Double Double a

setMargin (Margin b t l r _) = unlines ["set bmargin "++show b,
                                        "set lmargin "++show l,
                                        "set rmargin "++show r,
                                        "set tmargin "++show t]
                               

unsetMargin = unlines ["unset bmargin ",
                       "unset lmargin ",
                       "unset rmargin ",
                       "unset tmargin "]
instance PlotWithGnuplot a => PlotWithGnuplot (Margin a) where
    multiPlot r m@(Margin _ _ _ _ x) = do
      px <- multiPlot r x
      let setit = setMargin m
      return $ map (\(r', pls) -> (r', (TopLevelGnuplotCmd setit unsetMargin):pls)) px

data CanvasScale a = CanvasScale Double Double a

instance PlotWithGnuplot a => PlotWithGnuplot (CanvasScale a) where
    multiPlot r (CanvasScale xsz ysz x) = do
      let mklab = TopLevelGnuplotCmd ("set size "++show xsz++","++show ysz)
                                     ""
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklab:pls)) px


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

instance (PlotWithGnuplot a) => PlotWithGnuplot (ManySup a) where
    multiPlot r (ManySup ps) = do
      pxs <- mapM getGnuplotCmd ps
      return $ [(r,concat pxs)]

instance (PlotWithGnuplot a, PlotWithGnuplot b) => PlotWithGnuplot (a :+: b) where
    multiPlot r (xs :+: ys) = do
      px <- getGnuplotCmd xs
      py <- getGnuplotCmd ys                          
      return $ [(r,px++py)]

instance (PlotWithGnuplot a, PlotWithGnuplot b) => PlotWithGnuplot (Either a b) where
    multiPlot r (Left xs) = multiPlot r xs
    multiPlot r (Right xs) = multiPlot r xs



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

