-----------------------------------------------------------------------------
-- |
-- Module      : Main 
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- A program to read and pretty print a Python bytecode file (.pyc file).
--
-----------------------------------------------------------------------------

module Main where

import System.Exit (ExitCode (..), exitWith, exitFailure)
import System.Environment (getArgs)
import System.IO (openFile, IOMode(..), Handle, hClose)
import Control.Monad (when)
import System.Console.ParseArgs
   (Argtype (..), argDataOptional, argDataRequired, Arg (..)
   , gotArg, getArg, parseArgsIO, ArgsComplete (..), Args(..))
import Blip.Version (versionString)
import Blip.Marshal (readPyc)
import Blip.Pretty (prettyString)

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
   , argDesc = "Show the version number of readpyc."
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
      putStrLn $ "readpyc version " ++ versionString
      exitWith ExitSuccess
   case getInputFile argMap of
      Nothing -> return ()
      Just inFile -> do
         handle <- openFile inFile ReadMode
         pycFile <- readPyc handle
         putStrLn $ prettyString pycFile
         hClose handle
