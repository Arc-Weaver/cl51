import Distribution.Simple
import Distribution.Simple.Utils (notice, warn)
import Distribution.Verbosity    (normal)
import System.Directory          (doesFileExist, findExecutable,
                                  getModificationTime)
import System.FilePath           ((</>))
import System.Process            (callProcess)
import Control.Monad             (when)
import Control.Exception         (try, SomeException)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { preBuild = \args flags -> do
        assembleExampleProgram
        preBuild simpleUserHooks args flags
    }

-- | Assemble example/Example/program.S → program.bin if an 8051 assembler is
--   available and the source is newer than the binary.
--
--   Uses sdcc (SDCC Small Device C Compiler, which includes an 8051 assembler).
--   If sdcc is not available, falls back to a pre-built program.bin when
--   present, or fails loudly when both are missing.
--
--   Silently skips if:
--     - program.S does not exist (nothing to do)
--     - sdcc-as is not on PATH AND program.bin already exists (use stale bin)
--
--   Fails loudly if:
--     - sdcc-as is not on PATH AND program.bin does not exist
assembleExampleProgram :: IO ()
assembleExampleProgram = do
    let dir = "example" </> "Example"
        src = dir </> "program.S"
        rel = dir </> "program.rel"
        bin = dir </> "program.bin"
        ihx = dir </> "program.ihx"

    srcExists <- doesFileExist src
    when srcExists $ do
        masmAs <- findExecutable "sdas8051"
        case masmAs of
            Nothing -> do
                binExists <- doesFileExist bin
                if binExists
                    then notice normal
                             "sdas8051 not found; using pre-built program.bin"
                    else fail $ unlines
                             [ "sdas8051 not found and example/Example/program.bin is missing."
                             , "Install SDCC (e.g. apt install sdcc) and re-run the build, or"
                             , "commit a pre-built program.bin."
                             ]
            Just _ -> do
                stale <- isStale src bin
                when stale $ do
                    notice normal "Assembling example/Example/program.S ..."
                    callProcess "sdas8051"
                        ["-lo", rel, src]
                    callProcess "sdld"
                        [ "-n", "-i", ihx
                        , "-b", "CODE=0x0000"
                        , rel
                        ]
                    callProcess "objcopy"
                        ["-I", "ihex", "-O", "binary", ihx, bin]
                    notice normal "Assembly done."

-- | True if dst is missing or older than src.
isStale :: FilePath -> FilePath -> IO Bool
isStale src dst = do
    dstExists <- doesFileExist dst
    if not dstExists
        then return True
        else do
            ts <- getModificationTime src
            td <- getModificationTime dst
            return (ts > td)
