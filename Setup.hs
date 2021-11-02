{-# LANGUAGE CPP #-}
import Control.Monad
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
import System.IO

main = defaultMainWithHooks hk
 where
 hk = simpleUserHooks { buildHook = \pd lbi uh bf -> do
                                        -- let ccProg = Program "gcc" undefined undefined undefined
                                        let mConf  = lookupProgram ghcProgram (withPrograms lbi)
                                            err    = error "Could not determine C compiler"
                                            cc     = locationPath . programLocation  . maybe err id $ mConf
                                        lbiNew <- checkRDRAND cc lbi >>= checkGetrandom cc >>= checkGetentropy cc
                                        buildHook simpleUserHooks pd lbiNew uh bf
                      }

compileCheck :: FilePath -> String -> String -> String -> IO Bool
compileCheck cc testName message sourceCode = do
    withTempDirectory normal "" testName $ \tmpDir -> do
        writeFile (tmpDir ++ "/" ++ testName ++ ".c") sourceCode
        ec <- myRawSystemExitCode normal cc [tmpDir </> testName ++ ".c", "-o", tmpDir ++ "/a","-no-hs-main"]
        notice normal $ message ++ show (ec == ExitSuccess)
        return (ec == ExitSuccess)

addOptions :: [String] -> [String] -> LocalBuildInfo -> LocalBuildInfo
addOptions cArgs hsArgs lbi = lbi {withPrograms = newWithPrograms }
  where newWithPrograms1 = userSpecifyArgs "gcc" cArgs (withPrograms lbi)
        newWithPrograms  = userSpecifyArgs "ghc" (hsArgs ++ map ("-optc" ++) cArgs) newWithPrograms1

checkRDRAND :: FilePath -> LocalBuildInfo -> IO LocalBuildInfo
checkRDRAND cc lbi = do
        b <- compileCheck cc "testRDRAND" "Result of RDRAND Test: "
                (unlines        [ "#include <stdint.h>"
                                , "int main() {"
                                , "   uint64_t therand;"
                                , "   unsigned char err;"
                                , "   asm volatile(\"rdrand %0 ; setc %1\""
                                , "     : \"=r\" (therand), \"=qm\" (err));"
                                , "   return (!err);"
                                , "}"
                                ])
        return $ if b then addOptions cArgs cArgs lbi else lbi
  where cArgs = ["-DHAVE_RDRAND"]

checkGetrandom :: FilePath -> LocalBuildInfo -> IO LocalBuildInfo
checkGetrandom cc lbi = do
    libcGetrandom <- compileCheck cc "testLibcGetrandom" "Result of libc getrandom() Test: "
                (unlines        [ "#define _GNU_SOURCE"
                                , "#include <errno.h>"
                                , "#include <sys/random.h>"

                                , "int main()"
                                , "{"
                                , "    char tmp;"
                                , "    return getrandom(&tmp, sizeof(tmp), GRND_NONBLOCK) != -1;"
                                , "}"
                                ])
    if libcGetrandom then return $ addOptions cArgsLibc cArgsLibc lbi
    else do
        syscallGetrandom <- compileCheck cc "testSyscallGetrandom" "Result of syscall getrandom() Test: "
                (unlines        [ "#define _GNU_SOURCE"
                                , "#include <errno.h>"
                                , "#include <unistd.h>"
                                , "#include <sys/syscall.h>"
                                , "#include <sys/types.h>"
                                , "#include <linux/random.h>"

                                , "static ssize_t getrandom(void* buf, size_t buflen, unsigned int flags)"
                                , "{"
                                , "    return syscall(SYS_getrandom, buf, buflen, flags);"
                                , "}"

                                , "int main()"
                                , "{"
                                , "    char tmp;"
                                , "    return getrandom(&tmp, sizeof(tmp), GRND_NONBLOCK) != -1;"
                                , "}"
                                ])
        return $ if syscallGetrandom then addOptions cArgs cArgs lbi else lbi
  where cArgs = ["-DHAVE_GETRANDOM"]
        cArgsLibc = cArgs ++ ["-DHAVE_LIBC_GETRANDOM"]

checkGetentropy :: FilePath -> LocalBuildInfo -> IO LocalBuildInfo
checkGetentropy cc lbi = do
        b <- compileCheck cc "testGetentropy" "Result of getentropy() Test: "
                (unlines        [ "#define _GNU_SOURCE"
                                , "#include <unistd.h>"

                                , "int main()"
                                , "{"
                                , "    char tmp;"
                                , "    return getentropy(&tmp, sizeof(tmp));"
                                , "}"
                                ])
        return $ if b then addOptions cArgs cArgs lbi else lbi
  where cArgs = ["-DHAVE_GETENTROPY"]

myRawSystemExitCode :: Verbosity -> FilePath -> [String] -> IO ExitCode
#if __GLASGOW_HASKELL__ >= 704
-- We know for sure, that if GHC >= 7.4 implies Cabal >= 1.14
myRawSystemExitCode = rawSystemExitCode
#else
-- Legacy branch:
-- We implement our own 'rawSystemExitCode', this will even work if
-- the user happens to have Cabal >= 1.14 installed with GHC 7.0 or
-- 7.2
myRawSystemExitCode verbosity path args = do
    printRawCommandAndArgs verbosity path args
    hFlush stdout
    exitcode <- rawSystem path args
    unless (exitcode == ExitSuccess) $ do
        debug verbosity $ path ++ " returned " ++ show exitcode
    return exitcode
  where
    printRawCommandAndArgs :: Verbosity -> FilePath -> [String] -> IO ()
    printRawCommandAndArgs verbosity path args
      | verbosity >= deafening = print (path, args)
      | verbosity >= verbose = putStrLn $ unwords (path : args)
      | otherwise = return ()
#endif
