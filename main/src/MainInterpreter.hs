-----------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The Main module of Blipi. Contains the entry point of the interpreter, and
-- handles command line argument parsing.
--
-----------------------------------------------------------------------------

module Main where

import System.Exit (exitSuccess)
import Control.Monad (when)
import System.Console.ParseArgs
   (Argtype (..), argDataRequired, Arg (..)
   , gotArg, getArg, parseArgsIO, ArgsComplete (..), Args(..), getRequiredArg)
import Blip.Version (versionString)
import ProgNameInterpreter (progName)
import Blip.Interpreter.Interpret (interpretFile)

main :: IO ()
main = do
   let argDescriptions = [version, help, inputFile]
   args <- parseArgsIO ArgsComplete argDescriptions
   when (gotArg args Help) $ do
      putStrLn $ argsUsage args
      exitSuccess
   when (gotArg args Version) $ do
      putStrLn $ progName ++ " version " ++ versionString
      exitSuccess
   interpretFile $ getRequiredArg args InputFile

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
   , argData = argDataRequired "input pyc file" ArgtypeString
   , argDesc = "Name of the input pyc file."
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
getInputFile args = getArg args InputFile 
