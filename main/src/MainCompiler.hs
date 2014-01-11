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
import Control.Exception (try)
import Control.Monad (when)
import System.Console.ParseArgs
   ( Argtype (..), argDataOptional, Arg (..)
   , gotArg, getArg, parseArgsIO, ArgsComplete (..), Args(..))
import ProgNameCompiler (progName)
import Data.Set as Set (Set, empty, singleton, union)
import Blip.Version (versionString)
import Blip.Compiler.Types (Dumpable (..), CompileConfig (..))
import Blip.Compiler.Compile (compileFile, writePycFile)

main :: IO ()
main = do
   let argDescriptions = [version, help, magicNumberArg, dumpScopeArg, dumpASTArg]
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
   let magicNumber = getMagicNumber args
       dumps = getDumps args
       config = initCompileConfig 
                   { compileConfig_magic = fromIntegral magicNumber
                   , compileConfig_dumps = dumps }
   mapM_ (compileAndWritePyc config) pythonFiles

compileAndWritePyc :: CompileConfig -> FilePath -> IO ()
compileAndWritePyc config path =
   handleIOErrors $ do
      pyc <- compileFile config path 
      writePycFile pyc path

handleIOErrors :: IO () -> IO ()
handleIOErrors comp = do
   r <- try comp
   case r of
      -- XXX maybe we want more customised error messages for different kinds of
      -- IOErrors?
      Left e -> putStrLn $ progName ++ ": " ++ show (e :: IOError)
      Right () -> return ()

data ArgIndex
   = Help
   | InputFile
   | Version
   | MagicNumber
   | Dump Dumpable
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

dumpScopeArg :: Arg ArgIndex
dumpScopeArg =
   Arg
   { argIndex = Dump DumpScope
   , argAbbr = Nothing
   , argName = Just "dumpScope"
   , argData = Nothing
   , argDesc = "Dump the variable scope."
   }

dumpASTArg :: Arg ArgIndex
dumpASTArg =
   Arg
   { argIndex = Dump DumpAST
   , argAbbr = Nothing
   , argName = Just "dumpAST"
   , argData = Nothing
   , argDesc = "Dump the abstract syntax tree."
   }

getInputFile :: Args ArgIndex -> Maybe FilePath
getInputFile args = getArg args InputFile 

getMagicNumber :: Args ArgIndex -> Int
getMagicNumber args = 
   maybe defaultMagicNumber id $ getArg args MagicNumber

getDumps :: Args ArgIndex -> Set.Set Dumpable 
getDumps args
   = getDump DumpScope args `Set.union` 
     getDump DumpAST args
   where
   getDump :: Dumpable -> Args ArgIndex -> Set.Set Dumpable
   getDump dumpable args
      | gotArg args (Dump dumpable) = Set.singleton dumpable
      | otherwise = Set.empty

initCompileConfig :: CompileConfig
initCompileConfig =
   CompileConfig { compileConfig_magic = 0, compileConfig_dumps = Set.empty }
