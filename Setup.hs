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
