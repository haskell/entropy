{-# LANGUAGE CPP #-}
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple.Utils
import Distribution.Simple.Program
import Distribution.Verbosity
import System.Process
import System.Directory
import System.FilePath
import System.Exit

#if __GLASGOW_HASKELL__ < 702
import Control.Monad
import System.IO
#endif

main = defaultMainWithHooks hk
 where
 hk = simpleUserHooks { buildHook = \pd lbi uh bf -> do
                                        -- let ccProg = Program "gcc" undefined undefined undefined
                                        let hcProg = Program "ghc" undefined undefined undefined
                                            mConf  = lookupProgram hcProg (withPrograms lbi)
                                            err    = error "Could not determine C compiler"
                                            cc     = locationPath . programLocation  . maybe err id $ mConf
                                        b <- canUseRDRAND cc
                                        let newWithPrograms1 = userSpecifyArgs "gcc" cArgs (withPrograms lbi)
                                            newWithPrograms  = userSpecifyArgs "ghc" cArgsHC newWithPrograms1
                                            lbiNew = if b then (lbi {withPrograms = newWithPrograms }) else lbi
                                        buildHook simpleUserHooks pd lbiNew uh bf
                      }

cArgs :: [String]
cArgs = ["-DHAVE_RDRAND"]

cArgsHC :: [String]
cArgsHC = cArgs ++ map ("-optc" ++) cArgs




#if __GLASGOW_HASKELL__ < 702
printRawCommandAndArgs :: Verbosity -> FilePath -> [String] -> IO ()
printRawCommandAndArgs verbosity path args
 | verbosity >= deafening = print (path, args)
 | verbosity >= verbose   = putStrLn $ unwords (path : args)
 | otherwise              = return ()

rawSystemExitCode :: Verbosity -> FilePath -> [String] -> IO ExitCode
rawSystemExitCode verbosity path args = do
  printRawCommandAndArgs verbosity path args
  hFlush stdout
  exitcode <- rawSystem path args
  unless (exitcode == ExitSuccess) $ do
    debug verbosity $ path ++ " returned " ++ show exitcode
  return exitcode
#endif



canUseRDRAND :: FilePath -> IO Bool
canUseRDRAND cc = do
        withTempDirectory normal "" "testRDRAND" $ \tmpDir -> do
        writeFile (tmpDir ++ "/testRDRAND.c")
                (unlines        [ "#include <stdint.h>"
                                , "int main() {"
                                , "   uint64_t therand;"
                                , "   unsigned char err;"
                                , "   asm volatile(\"rdrand %0 ; setc %1\""
                                , "     : \"=r\" (therand), \"=qm\" (err));"
                                , "   return (!err);"
                                , "}"
                                ])
        ec <- rawSystemExitCode normal cc [tmpDir </> "testRDRAND.c", "-o", tmpDir ++ "/a.o","-c"]
        notice normal $ "Result of RDRAND Test: " ++ show (ec == ExitSuccess)
        return (ec == ExitSuccess)
