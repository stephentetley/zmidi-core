#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.Setup ( SDistFlags )
import Distribution.PackageDescription ( HookedBuildInfo, emptyHookedBuildInfo )


main = defaultMainWithHooks sdist_warning_hooks

sdist_warning_hooks :: UserHooks
sdist_warning_hooks = simpleUserHooks { preSDist = sdistVersionWarning }


sdistVersionWarning :: Args -> SDistFlags -> IO HookedBuildInfo
sdistVersionWarning _ _ = 
    mapM_ putStrLn msg >> printVersionNumberFile >> return emptyHookedBuildInfo
  where
    msg = [ "-------------------------------------------------------"
          , "-------------------------------------------------------"
          , ""
          , "WARNING - is ZMidi.Core.VersionNumber correct?"
          , ""
          , "-------------------------------------------------------"
          , "-------------------------------------------------------"
          ]

printVersionNumberFile :: IO ()
printVersionNumberFile = 
  readFile "src/ZMidi/Core/VersionNumber.hs" >>= putStrLn
