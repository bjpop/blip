-----------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The Main module of Blip. Contains the entry point of the compiler and
-- the interpreter and handles command line argument parsing.
--
-----------------------------------------------------------------------------

module Main where

import System.Exit (exitSuccess)
import Control.Exception (try)
import Control.Monad (when, unless)
import System.Console.ParseArgs
   ( Argtype (..), argDataOptional, Arg (..)
   , gotArg, getArg, parseArgsIO, ArgsComplete (..), Args(..))
import ProgName (progName)
import Data.Set as Set (Set, empty, singleton, union)
import System.FilePath (splitExtension)
import Blip.Version (versionString)
import Blip.Compiler.Types (Dumpable (..), CompileConfig (..))
import Blip.Compiler.Compile (compileFile, writePycFile)
import Blip.Interpreter.Interpret (interpretFile)   
import Repl (repl)

argDescriptions :: [Arg ArgIndex]
argDescriptions =
   [ version, help, magicNumberArg
   , dumpScopeArg, dumpASTArg, compileOnly, inputFile
   ]

main :: IO ()
main = do
   args <- parseArgsIO ArgsComplete argDescriptions
   when (gotArg args Help) $ do
      putStrLn $ argsUsage args
      exitSuccess
   when (gotArg args Version) $ do
      putStrLn $ progName ++ " version " ++ versionString
      exitSuccess
   when (gotArg args InputFile) $ do
      case getArg args InputFile of
          Nothing -> repl 
          Just filename -> do
             let (prefix, suffix) = splitExtension filename
             if suffix == ".pyc"
                then do
                   interpretFile filename
                   exitSuccess
                else do
                   let magicNumber = getMagicNumber args
                       dumps = getDumps args
                       config = initCompileConfig 
                                { compileConfig_magic = fromIntegral magicNumber
                                , compileConfig_dumps = dumps }
                   compileAndWritePyc config filename
                   unless (gotArg args CompileOnly) $ 
                       interpretFile $ prefix ++ ".pyc"
                   exitSuccess
   repl

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
   | CompileOnly
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

compileOnly :: Arg ArgIndex
compileOnly =
   Arg
   { argIndex = CompileOnly 
   , argAbbr = Just 'c'
   , argName = Just "compile" 
   , argData = Nothing
   , argDesc = "Compile .py to .pyc but do not run the program."
   }

inputFile :: Arg ArgIndex
inputFile =
   Arg
   { argIndex = InputFile
   , argAbbr = Nothing
   , argName = Nothing
   , argData = argDataOptional "input file" ArgtypeString
   , argDesc = "Name of the input Python file, either .py or .pyc"
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

-- this works for CPython 3.8.2
-- see get_python_magic_number/get_python_magic_number.py
defaultMagicNumber :: Int
defaultMagicNumber = 168627541 

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
