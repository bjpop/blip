-----------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The Main module of Blip. Contains the entry point of the compiler, and
-- handles command line argument parsing.
--
-----------------------------------------------------------------------------

module Main where

import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import System.IO (openFile, IOMode(..), Handle, hClose)
import Control.Monad (when, forM_)
import System.Console.ParseArgs
   (Argtype (..), argDataOptional, argDataRequired, Arg (..)
   , gotArg, getArg, parseArgsIO, ArgsComplete (..), Args(..))
import Blip.Version (versionString)
import Compile (compileFile, CompileConfig (..))
import Blip.Marshal (writePyc)
import Blip.Pretty (prettyString)
import ProgName (progName)

data ArgIndex
   = Help
   | InputFile
   | Version
   | MagicNumber
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

-- this works for CPython 3.3.0
defaultMagicNumber :: Int
defaultMagicNumber = 168627358

magicNumberArg :: Arg ArgIndex
magicNumberArg =
   Arg
   { argIndex = MagicNumber 
   , argAbbr = Nothing
   , argName = Just "magic"
   , argData = argDataOptional "magic number" ArgtypeInt
   , argDesc = "Magic number to include in pyc file header."
   }

getInputFile :: Args ArgIndex -> Maybe FilePath
getInputFile args = getArg args InputFile 

getMagicNumber :: Args ArgIndex -> Int
getMagicNumber args = 
   maybe defaultMagicNumber id $ getArg args MagicNumber

initCompileConfig :: CompileConfig
initCompileConfig =
   CompileConfig { compileConfig_magic = 0 }

main :: IO ()
main = do
   let argDescriptions = [version, help, magicNumberArg]
   args <- parseArgsIO (ArgsTrailing "PYTHON_FILES") argDescriptions
   when (gotArg args Help) $ do
      putStrLn $ argsUsage args
      exitSuccess
   when (gotArg args Version) $ do
      putStrLn $ progName ++ " version " ++ versionString
      exitSuccess
   let pythonFiles = argsRest args
   when (null pythonFiles) $ do
      putStrLn $ progName ++ ": no Python input files specified"
      putStrLn $ argsUsage args
      exitFailure 
   forM_ pythonFiles $ \pyFile -> do
      let magicNumber = getMagicNumber args
          config = initCompileConfig 
                        { compileConfig_magic = fromIntegral magicNumber }
      compileFile config pyFile 
