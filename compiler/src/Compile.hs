{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies #-}

module Compile (compileFile, CompileConfig (..)) where

import Scope (Scope (..), empty )
import Blip.Marshal (writePyc, PycFile (..), PyObject (..))
import Language.Python.Version3.Parser (parseModule)
import Language.Python.Common.AST (ModuleSpan)
import System.FilePath ((<.>), takeBaseName)
import System.Directory (doesFileExist)
import System.IO (openFile, IOMode(..), Handle, hClose)
import Data.Word (Word32)
import Control.Monad.State.Strict as State hiding (State)
import Control.Monad.State.Class (MonadState (..))
import Control.Applicative (Applicative (..))
import Data.Traversable as Traversable (Traversable (..))

data CompileConfig =
   CompileConfig
   { compileConfig_magic :: Word32
   }
   deriving (Eq, Show)

data CompileState = CompileState
   { compileState_unique :: !Integer
   , compileState_scope :: Scope
   , compileState_config :: CompileConfig
   }

initState :: CompileConfig -> CompileState
initState config = CompileState
   { compileState_unique = 0
   , compileState_scope = Scope.empty
   , compileState_config = config
   }

newtype Compile a
   = Compile (StateT CompileState IO a)
   deriving (Monad, Functor, MonadIO, Applicative)

instance MonadState CompileState Compile where
   get = Compile get
   put s = Compile $ put s

runCompileMonad :: Compile a -> CompileState -> IO a
runCompileMonad (Compile comp) = evalStateT comp

compileFile :: CompileConfig -> FilePath -> IO ()
compileFile config path = do
   fileExists <- doesFileExist path
   if not fileExists
      then error $ "Python source file not found: " ++ path
      else do
         fileContents <- readFile path
         pyModule <- parseAndCheckErrors fileContents path
         pyc <- compileModule config pyModule
         let pycFilePath = takeBaseName path <.> ".pyc"
         handle <- openFile pycFilePath WriteMode 
         writePyc handle pyc
         hClose handle

simplePyc magic =
   PycFile
   { magic = magic
   , modified_time = 12345
   , size = 10
   , object = None }
    
parseAndCheckErrors :: String -> FilePath -> IO ModuleSpan
parseAndCheckErrors fileContents sourceName =
   case parseModule fileContents sourceName of
      Left e -> error $ show e
      Right (pyModule, _comments) -> return pyModule

compileModule :: CompileConfig -> ModuleSpan -> IO PycFile
compileModule config mod = return $ simplePyc $ compileConfig_magic config

compiler :: Compilable a => a -> CompileState -> IO (CompileResult a)
compiler = runCompileMonad . compile

class Compilable a where
   type CompileResult a :: *
   compile :: a -> Compile (CompileResult a)

instance Compilable a => Compilable [a] where
   type CompileResult [a] = [CompileResult a]
   compile = Traversable.mapM compile

instance Compilable a => Compilable (Maybe a) where
   type CompileResult (Maybe a) = Maybe (CompileResult a)
   compile = Traversable.mapM compile
