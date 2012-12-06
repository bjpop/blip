module Compile (compileFile) where

-- import Language.Python.Version3.Parser (parseModule)
-- import Language.Python.Common.AST (ModuleSpan)
import System.FilePath ((<.>), takeBaseName)
import System.Directory (doesFileExist)
import System.IO (openFile, IOMode(..), Handle, hClose)
import Blip.Marshal (writePyc, PycFile (..), PyObject (..))

compileFile :: FilePath -> IO ()
compileFile path = do
   fileExists <- doesFileExist path
   if not fileExists
      then error $ "Python source file not found: " ++ path
      else do
         fileContents <- readFile path
         -- pyModule <- parseAndCheckErrors fileContents path
         -- pyc <- compileModule pyModule
         let pyc = simplePyc
         let pycFilePath = takeBaseName path <.> ".pyc"
         handle <- openFile pycFilePath WriteMode 
         writePyc handle pyc
         hClose handle

simplePyc =
   PycFile
   { magic = 12
   , modified_time = 12345
   , size = 10
   , object = None }
    
{-
parseAndCheckErrors :: String -> FilePath -> IO ModuleSpan
parseAndCheckErrors fileContents sourceName =
   case parseModule fileContents sourceName of
      Left e -> error $ show e
      Right (pyModule, _comments) -> return pyModule

compileModule :: ModuleSpan -> IO PycFile
compileModule module = error "not implemented yet"
-}
