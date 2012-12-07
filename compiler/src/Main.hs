module Main where

import System.Exit (ExitCode (..), exitWith, exitFailure)
import System.Environment (getArgs)
import System.IO (openFile, IOMode(..), Handle, hClose)
import Control.Monad (when)
import System.Console.ParseArgs
   (Argtype (..), argDataOptional, argDataRequired, Arg (..)
   , gotArg, getArg, parseArgsIO, ArgsComplete (..), Args(..))
import Blip.Version (versionString)
import Compile (compileFile)
import Blip.Marshal (writePyc)
import Blip.Pretty (prettyString)

progName = "blip"

data ArgIndex
   = Help
   | InputFile
   | Version
   deriving (Eq, Ord, Show)

help :: Arg ArgIndex
help =
   Arg
   { argIndex = Help
   , argAbbr = Just 'h'
   , argName = Just "help"
   , argData = Nothing
   , argDesc = "Display a help message."
   }

inputFile :: Arg ArgIndex
inputFile =
   Arg
   { argIndex = InputFile
   , argAbbr = Nothing
   , argName = Nothing
   , argData = argDataOptional "input file" ArgtypeString
   , argDesc = "Name of the input Python file."
   }

version :: Arg ArgIndex
version =
   Arg
   { argIndex = Version
   , argAbbr = Nothing
   , argName = Just "version"
   , argData = Nothing
   , argDesc = "Show the version number of " ++ progName ++ "."
   }

getInputFile :: Args ArgIndex -> Maybe FilePath
getInputFile argMap = getArg argMap InputFile 

main :: IO ()
main = do
   let args = [version, help, inputFile]
   argMap <- parseArgsIO ArgsComplete args
   when (gotArg argMap Help) $ do
      putStrLn $ argsUsage argMap
      exitWith ExitSuccess
   when (gotArg argMap Version) $ do
      putStrLn $ progName ++ " version " ++ versionString
      exitWith ExitSuccess
   -- XXX might as well support multiple input Python files.
   -- They can be compiled in any order anyway.
   case getInputFile argMap of
      Nothing -> return ()
      Just inFile -> compileFile inFile
