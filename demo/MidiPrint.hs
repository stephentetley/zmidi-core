{-# OPTIONS -Wall #-}

-- Dump the contents of a MIDI file

-- Note - GHC (Windows at least) appears to throw an error if
-- the copyright symbol is used a Text meta-event.



module Main where

import ZMidi.Core

import System.Console.GetOpt
import System.Environment
import System.Exit



header :: String 
header = unlines $ 
    [ "Usage: MidiPrint --format=... <filename>"
    , ""
    , "Formats are 'csv', 'ascii', 'original'."
    ]

help_message :: String
help_message = unlines $  
    [ "Decode binary MIDI files." ]


data Flag = Usage
          | OutFormat String
  deriving (Eq, Show)


options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]     (NoArg Usage)        help_message
    , Option ['f'] ["format"]   (ReqArg OutFormat "original")  "output format"
    ]

main :: IO ()
main = do 
  args <- getArgs
  let (opts, nonopts, errs) = getOpt Permute options args
  main2 opts nonopts errs


main2 :: [Flag] -> [FilePath] -> [String] -> IO ()
main2 opts            _        _ 
  | Usage       `elem` opts         = midiPrintExit $ usageInfo header options
main2 [OutFormat ss]  [infile] []   = process ss infile
main2 []              [infile] []   = process "original" infile
main2 _               _        errs = 
    midiPrintExitFail 1 (concat errs ++ usageInfo header options)


midiPrintExit :: String -> IO ()
midiPrintExit s = putStrLn s >> exitWith ExitSuccess

midiPrintExitFail :: Int -> String -> IO ()
midiPrintExitFail i s = putStrLn s >> exitWith (ExitFailure i)


process :: String -> FilePath -> IO ()
process fmt filename = do
    ans <- readMidi filename
    case ans of
      Left (ParseErr n msg) ->
          midiPrintExitFail 1 $ "Parse failure at " ++ show n ++ ": " ++ msg
      Right m               -> outputMidi fmt m

 
outputMidi :: String -> (MidiFile -> IO ())
outputMidi "csv"        = putCsv
outputMidi "ascii"      = putAscii
outputMidi "original"   = putMidi
outputMidi _            = const $ putStrLn "Unrecognized output format."