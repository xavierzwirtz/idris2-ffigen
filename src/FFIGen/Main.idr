module FFIGen.Main

import Language.JSON
import Data.List
import Data.List1
import Data.String
import Data.SortedMap
import System
import System.File
import Control.App
import Control.App.Console
import Control.App.FileIO
import FFIGen.Compiler
import FFIGen.CompileSettings
import FFIGen.ClangParser
import FFIGen.CAST
import JSON.Derive

data AppError
  = ParseError
  | InvalidArgs
  | CompilerParseError CompilerParseError
  | CompileSettingsParseError DecodingErr

Show AppError where
  show ParseError = "error parsing json"
  show InvalidArgs = "invalid args"
  show (CompilerParseError x) = "parse error: \{show x}"
  show (CompileSettingsParseError x) = "compile settings parse error: \{show x}"

-- wrap
--   : (Exception AppError es)
--   => (SortedMap String
--                 FunctionSettings -> List ClangTopLevel -> App (err :: es) (String, String)) ->
--        (err -> AppError) ->
--        SortedMap String FunctionSettings ->
--        List ClangTopLevel ->
--        App es (String, String)
-- wrap compiler onerr functions json = handle (compiler functions json) pure (\x => throw $ onerr x)

-- pickCompileSettings : (Has [Exception AppError] es)
--                    => (fileName : String)
--                    -> App es CompileSettings
-- pickCompileSettings "raylib.h.json" = pure Compilers.RaylibCompiler.settings
-- pickCompileSettings x = throw $ UnsupportedFile x

parseCompileSettings
  : Exception AppError es
  => String
  -> App es CompileSettings
parseCompileSettings str =
  case decode {a = CompileSettings} str of
    Left err => throw $ CompileSettingsParseError err
    Right x => pure x

parseArgs
  : (Has [Exception AppError] es, Console es)
  => List String
  -> App es (String, String, CompileSettings)
parseArgs [_, file, compileSettingsRaw] = do
  compileSettings <- parseCompileSettings compileSettingsRaw
  pure (file, last $ split (== '/') file, compileSettings)
parseArgs _ = throw InvalidArgs

writeFile : FileIO e =>
            (filepath : String) -> (contents : String) ->
            App e ()
writeFile file contents
  = withFile file WriteTruncate throw $
      (flip Control.App.FileIO.fPutStr contents)

app : (Has [ Exception AppError
           , Exception IOError
           , Exception ClangParseError
           ]
           es
      , FileIO es
      , Console es
      )
   => List String
   -> App es ()
app args = do
  (file, fileName, compileSettings) <- parseArgs args
  contents <- readFile file
  case parse contents of
    Nothing => throw ParseError
    Just json => do
      parsed <- moduleParser
        (map structName compileSettings.structsToParse)
        (map functionName (functionsToParse compileSettings))
        json
      (c, idris) <- handle (Compiler.compile (libName compileSettings)
                                     (moduleName compileSettings)
                                     (getStructSettings compileSettings.structsToParse)
                                     (getFunctionSettings compileSettings.functionsToParse)
                                     parsed) pure (\x => throw $ CompilerParseError x)
      x <- writeFile "idris.idr" idris
      x <- writeFile "c.c" c
      pure ()

interface System e where
  exitWith : System.ExitCode -> App {l} e ()

export
PrimIO e => System e where
  exitWith i = primIO $ System.exitWith i

handleErr : System es => Console es => Show a => a -> App es ()
handleErr x = do
  putStrLn $ show x
  exitWith $ ExitFailure 1

handleAppErr : System es => Console es => AppError -> App es ()
handleAppErr x = handleErr x

handleFileIOErr : System es => Console es => IOError -> App es ()
handleFileIOErr x = handleErr x

handleClangParseError : System es => Console es => ClangParseError -> App es ()
handleClangParseError x = handleErr x

main : IO ()
main = do
  args <- getArgs
  run (handle (handle (handle (app args) pure handleFileIOErr) pure handleAppErr) pure handleClangParseError)
