-----------------------------------------------------------------------------
{- |
Graphics.Gnewplot.Exec allows you to plot values of types that implement PlotWithGnuplot in 
various ways (to a file or on disk).

Example: 

@
import Graphics.Gnewplot.Instances
import Graphics.Gnewplot.Exec

someData :: [(Double,Double)]
somedata = [(0.0, 1.0),(0.1, 2.0),(0.2, 1.4),(0.3, 1.7),(0.4, 1.0),
            (0.5, 1.8),(0.6, 1.1),(0.7, 1.5),(0.8, 1.2),(0.9, 1.9)]

main = do 
  gnuplotToPS \"foo.ps\" someData
  gnuplotToPNG \"foo.png\" someData
  gnuplotOnScreen someData
@

-}

{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, ExistentialQuantification #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, GADTs, ScopedTypeVariables, DeriveDataTypeable #-}

module Graphics.Gnewplot.Exec(gnuplotOnScreen, gnuplotToPNG, gnuplotToPS, gnuplotToPDF, gnuplotToSparklinePNG, uniqueIntStr, execGP, gnuplotToPNGOpts, gnuplotToPSOpts, TermOpts (FontSpec, SizeSpec)) where
--module Graphics.Gnewplot.Exec where

--import EvalM
import System.IO
import System.Cmd
import System.Exit
--import Math.Probably.FoldingStats hiding (F)
import Control.Monad
import Data.Unique
import Data.List
--import Control.Monad.Trans
import System.Directory
--import System.Posix.Files
import System.Random

import Graphics.Gnewplot.Types
import Debug.Trace
{- stolen from gnuplot-0.3.3 (Henning Thieleman) -}

import qualified System.Process as Proc
import Control.Concurrent
import Control.Exception



myForkIO :: IO () -> IO ()
myForkIO io = do
     mvar <- newEmptyMVar
     forkIO (io `finally` putMVar mvar ())
     takeMVar mvar

interactivePlot ma =  do --putStrLn program
      h@(inp,o,e,pid) <- Proc.runInteractiveCommand "gnuplot" 
      threadDelay $ 100*1000
      myForkIO $ do tellInteractivePlot h "set terminal wxt noraise"
                    ma h
      hClose o
      hClose e
      hClose inp
      Proc.terminateProcess pid
      Proc.waitForProcess pid      
      return ()

tellInteractivePlot (inp,o,e,p) s = do
  hPutStr inp $ s++"\n"
  hFlush inp

execGPPipe ::
      String {-^ The lines of the gnuplot script to be piped into gnuplot -}
   -> IO ExitCode
execGPPipe program =
   do --putStrLn program
      (inp,_out,_err,pid) <-
         Proc.runInteractiveProcess "gnuplot" [""] Nothing Nothing
      hPutStr inp program
      --print pid
      Proc.waitForProcess pid

execGPSh ::
      String {-^ The lines of the gnuplot script to be piped into gnuplot -}
--   -> [String] {-^ Options for gnuplot -}
   -> IO ExitCode
execGPSh program  =
   let cmd =
          "sh -c 'echo " ++ quote ( program) ++
                 " | gnuplot '"
   in  do putStrLn cmd
          system cmd

execGPPersist ::
      String {-^ The lines of the gnuplot script to be piped into gnuplot -}
--   -> [String] {-^ Options for gnuplot -}
   -> IO ()
execGPPersist cmds = do
  x <- randomRIO (0,99999999::Int)
  let fnm = "/tmp/gnuplotCmds"++show x
  writeFile fnm cmds
  system $ "gnuplot -persist "++fnm
  removeFile $ fnm

execGPTmp cmds = do
  x <- randomRIO (0,99999999::Int)
  let fnm = "/tmp/gnuplotCmds"++show x
  writeFile fnm cmds
  system $ "gnuplot "++fnm
  --removeFile $ fnm


execGP = execGPTmp

-- | Generate a unique string
uniqueIntStr :: IO String
uniqueIntStr = (show. hashUnique) `fmap` newUnique

-- | Plot to screen -- will open a new window
gnuplotOnScreen :: PlotWithGnuplot a => a -> IO ()
gnuplotOnScreen x = do
  plines <- multiPlot unitRect x
  let cmdLines = "set datafile missing \"NaN\"\n"++
                  (showMultiPlot plines)
                       
  writeFile "/tmp/gnuplotCmds" cmdLines
  system "gnuplot -persist /tmp/gnuplotCmds"
  --removeFile "/tmp/gnuplotCmds"
  cleanupCmds $ map snd plines
  return ()

-- | Plot to a png file
gnuplotToPNG :: PlotWithGnuplot a => String -> a -> IO ()
gnuplotToPNG fp x = do
  plines <- multiPlot unitRect x
  let cmdLines = "set datafile missing \"NaN\"\n"++
                 "set terminal png enhanced size 1200,900 crop\n"++
                 "set output '"++fp++"'\n"++
                  (showMultiPlot plines)
                       
  --putStrLn cmdLines
  execGP cmdLines
  {- writeFile "/tmp/gnuplotCmds" cmdLines
  system "gnuplot /tmp/gnuplotCmds"
  removeFile "/tmp/gnuplotCmds" -}
  cleanupCmds $ map snd plines
  return ()

data TermOpts = FontSpec String
              | SizeSpec String

gnuplotToPNGOpts :: PlotWithGnuplot a => String -> [TermOpts] -> a -> IO ()
gnuplotToPNGOpts fp opts x = do
  plines <- multiPlot unitRect x
  let fontstr = case [spec | FontSpec spec <- opts ] of
                  [] -> ""
                  s:_ -> "font \""++s++"\" "
  let szstr = case [spec | SizeSpec spec <- opts ] of
                  [] -> ""
                  s:_ -> "size "++s++" "
  let cmdLines = "set datafile missing \"NaN\"\n"++
                 "set terminal png enhanced "++fontstr++szstr ++" crop\n"++
                 "set output '"++fp++"'\n"++
                  (showMultiPlot plines)
                       
  --putStrLn cmdLines
  execGP cmdLines
  {- writeFile "/tmp/gnuplotCmds" cmdLines
  system "gnuplot /tmp/gnuplotCmds"
  removeFile "/tmp/gnuplotCmds" -}
  cleanupCmds $ map snd plines
  return ()


gnuplotToPSOpts:: PlotWithGnuplot a => String-> [TermOpts] -> a -> IO ()
gnuplotToPSOpts fp  opts x = do
  plines <- multiPlot unitRect x
  let fontstr = case [spec | FontSpec spec <- opts ] of
                  [] -> "\"Helvetica\" 16 "
                  s:_ -> s++" "
  let szstr = case [spec | SizeSpec spec <- opts ] of
                  [] -> "size 5.0,3.5"
                  s:_ -> "size "++s++" "
  let cmdLines = "set datafile missing \"NaN\"\n"++
                 "set terminal postscript eps enhanced color "++fontstr++szstr ++"\n"++
                 "set output '"++fp++"'\n"++
                  (showMultiPlot plines)
                       
  execGP cmdLines
{-  writeFile "/tmp/gnuplotCmds" cmdLines
  system "gnuplot /tmp/gnuplotCmds"
  removeFile "/tmp/gnuplotCmds"-}
  cleanupCmds $ map snd plines
  return ()
-- | Plot to a very small (100 by 50 pixels) png file 
gnuplotToSparklinePNG :: PlotWithGnuplot a => String -> a -> IO ()
gnuplotToSparklinePNG fp x = do
  plines <- multiPlot unitRect x
  let cmdLines = "set datafile missing \"NaN\"\n"++
                 "set terminal png size 100,50 crop\n"++
                 "unset xtics\n"++
                 "unset ytics\n"++
                 "set border 0\n"++
                 "set output '"++fp++"'\n"++
                  (showMultiPlot plines)

  execGP cmdLines                       
  {-writeFile "/tmp/gnuplotCmds" cmdLines
  system "gnuplot /tmp/gnuplotCmds 2>/dev/null"
  removeFile "/tmp/gnuplotCmds"-}
  cleanupCmds $ map snd plines
  return ()

-- | Plot to PDF figure fire. Requires ps2pdf in the current path
gnuplotToPDF:: PlotWithGnuplot a => String -> a -> IO ()
gnuplotToPDF fp x = do
  gnuplotToPS fp x
  system $ "ps2pdf "++fp
  return ()

-- | Plot to an encapsulated postscript file
gnuplotToPS:: PlotWithGnuplot a => String-> a -> IO ()
gnuplotToPS fp  x = do
  plines <- multiPlot unitRect x
  let cmdLines = "set datafile missing \"NaN\"\n"++
                 "set terminal postscript eps enhanced color \"Helvetica\" 16 size 5.0,3.5\n"++
                 "set output '"++fp++"'\n"++
                  (showMultiPlot plines)
                       
  execGP $ trace (show plines) $ cmdLines
{-  writeFile "/tmp/gnuplotCmds" cmdLines
  system "gnuplot /tmp/gnuplotCmds"
  removeFile "/tmp/gnuplotCmds"-}
  cleanupCmds $ map snd plines
  return ()


{-gnuplotMany :: [String] -> [(String, GnuplotBox)] -> IO ()
gnuplotMany opts nmbxs = do
  nmcmds <- forM nmbxs $ \(nm, GnuplotBox x) -> do
                      cmd <- multiPlot unitRect x
                      --print2 nm cmd
                      return (nm,cmd)
  let start = "set datafile missing \"NaN\"\n"
  let h = optVal 'h' 480 opts
  let w = optVal 'w' 640 opts
  let term = "set terminal png size "++ show w++","++show h++" crop\n"
  let cmds = start++term ++concatMap plotOne nmcmds
  execGP cmds

  forM_ nmcmds $ \(_,cmd) -> cleanupCmds $ map snd cmd
  return ()
    where plotOne (fp, plines) = "set output '"++fp++"'\n"++
                                 (showMultiPlot plines)

gnuplotManyLatex :: [String] -> [(String, GnuplotBox)] -> IO ()
gnuplotManyLatex opts nmbxs = do
  nmcmds <- forM nmbxs $ \(nm, GnuplotBox x) -> do
                      cmd <- multiPlot unitRect x
                      --print2 nm cmd
                      return (nm,cmd)
  let start = "set datafile missing \"NaN\"\n"
  let h::Double = (/10) $ realToFrac $ optVal 'h' (35::Int) opts
  let w::Double = (/10) $ realToFrac $ optVal 'w' (50::Int) opts
  let fs = optVal 'f' 16 opts
  let term = "set terminal postscript eps enhanced color \"Helvetica\" "++show fs++" size "++ show w++","++show h++"\n"-- crop\n"
  let cmds = start++term ++concatMap plotOne nmcmds
  execGP cmds
  forM_ nmcmds $ \(nm,cmd) -> do
    system $ "epstopdf "++nm++".eps"
    cleanupCmds $ map snd cmd
  return ()
    where plotOne (fp, plines) = "set output '"++fp++".eps'\n"++
                                 (showMultiPlot plines)
 
-}