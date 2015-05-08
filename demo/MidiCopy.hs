{-# OPTIONS -Wall #-}

-- Read a MIDI file, build a syntax tree and encode it again.
-- Are the files the same?


module Main where

import ZMidi.Core

import System.Environment


main :: IO ()
main = do 
  args <- getArgs
  case args of
    [path] -> process path
    _      -> mapM_ putStrLn $ 
              [ "Usage: MidiCopy <filename>"
              , "--"
              , "Read the file, building a syntax tree, generate binary output"
              , "from the syntax tree."
              , ""
              , "Tests that read and write are isomorphic." 
              , ""
              ]


process :: FilePath -> IO ()
process filename = do
    ans <- readMidi filename
    case ans of
      Left err -> print err
      Right mfile -> let outfile = filename ++ ".001"
                     in do { mapM_ putStrLn $ printMidiHeader mfile
                           ; putStrLn ""
                           ; writeMidi outfile mfile
                           ; putStrLn $ "Wrote file: " ++ outfile
                           }

 