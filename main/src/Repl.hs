{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Repl
-- Copyright   : (c) 2010, 2014 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The Read Eval Print Loop (REPL) of the interpreter.
--
-----------------------------------------------------------------------------

module Repl (repl) where

import Data.Set as Set (empty)
import Control.Monad.Trans as Trans (lift, liftIO)
import Control.Monad.State.Strict as State (StateT (..), evalStateT, gets)
import Control.Monad.CatchIO as CatchIO (MonadCatchIO (..))
import System.Console.Haskeline as Haskeline (getInputLine, defaultSettings)
import System.Console.Haskeline.IO (queryInput, initializeInput, InputState)
import Control.Monad (when)
import System.IO (hSetBuffering, stdout, BufferMode (..))
import Language.Python.Common.Token (Token (..))
import Language.Python.Version3.Lexer (lexer, initLexState)
import Language.Python.Common.ParserMonad
   (runParser, ParseState (..))
import Language.Python.Common.Pretty (prettyText)
import Language.Python.Common.AST (StatementSpan)
import Language.Python.Common.PrettyParseError ()
import Blip.Version (versionString)
import Blip.Compiler.Compile (compileReplInput)
import Blip.Compiler.Types (CompileConfig (..))
import Blip.Interpreter.Interpret as Interpreter (runTopObjectEval, initGlobals)
import Blip.Interpreter.Types as Interpreter (Eval)
import Blip.Interpreter.State as Interpreter (runEvalMonad, initState)

repl :: IO ()
repl = do
    hSetBuffering stdout NoBuffering
    runRepl $ do
       lift Interpreter.initGlobals
       greeting
       replLoop

greeting :: Repl ()
greeting = 
   liftIO $ putStrLn $ "Berp version " ++ versionString ++ ", type control-d to exit."

replLoop :: Repl ()
replLoop = do
   maybeInput <- getInputLines
   case maybeInput of
      Nothing -> return ()
      Just input -> evalInput input >> replLoop


initCompileConfig :: CompileConfig
initCompileConfig =
   CompileConfig { compileConfig_magic = 0
                 , compileConfig_dumps = Set.empty }

evalInput :: String -> Repl ()
evalInput input =
   when (not $ null input) $ do
      pyObject <- liftIO $ compileReplInput initCompileConfig (input ++ "\n")
      lift $ runTopObjectEval pyObject

type Repl a = StateT ReplState Interpreter.Eval a

data ReplState = ReplState { repl_inputState :: !InputState }

runRepl :: Repl a -> IO a
runRepl comp = do
   initInputState <- initializeInput defaultSettings
   let initReplState = ReplState { repl_inputState = initInputState }
   runEvalMonad (evalStateT comp initReplState) Interpreter.initState

withInputState :: (InputState -> Repl a) -> Repl a
withInputState f = f =<< (gets repl_inputState)

lexState :: String -> ParseState
lexState input = initLexState input "<stdin>"

getInputLines :: Repl (Maybe String)
getInputLines = do
   maybeInput <- prompt ">>> "
   case maybeInput of
      Nothing -> return Nothing
      Just line
         | Right (tokens, _state) <- lexResult,
           null $ noComments tokens -> return $ Just []
         | Right (tokens, state) <- lexResult,
           lastTokenIsColon $ noComments tokens -> do
             restLines <- getIndentContinueLines state []
             return $ Just $ unlines (line:restLines)
         | Right (_tokens, state) <- lexResult,
           nonEmptyParenStack state -> do
             restLines <- getParenContinueLines state []
             return $ Just $ unlines (line:restLines)
         | otherwise -> return $ Just line
         where
         lexResult = runParser lexer $ lexState line
         noComments = filter (not . isComment)

isComment :: Token -> Bool
isComment (CommentToken {}) = True
isComment _other = False

lastTokenIsColon :: [Token] -> Bool
lastTokenIsColon [] = False
lastTokenIsColon tokens =
   isColon $ last tokens
   where
   isColon :: Token -> Bool
   isColon (ColonToken {}) = True
   isColon _other = False

nonEmptyParenStack :: ParseState -> Bool
nonEmptyParenStack = not . null . parenStack

getIndentContinueLines :: ParseState -> [String] -> Repl [String]
getIndentContinueLines state acc = do
   maybeInput <- prompt "... "
   case maybeInput of
      Nothing -> return $ reverse acc
      Just line
         | Right (_tokens, newState) <- lexResult,
           nonEmptyParenStack newState -> do
              getIndentContinueLines newState (line:acc)
         | Right (_tokens, newState) <- lexResult,
           length line > 0 -> do
              getIndentContinueLines newState (line:acc)
         | otherwise -> return $ reverse (line:acc)
         where
         lexResult = runParser lexer $ stateWithLine
         stateWithLine = state { input = '\n':line }

getParenContinueLines :: ParseState -> [String] -> Repl [String]
getParenContinueLines state acc = do
   maybeInput <- prompt "... "
   case maybeInput of
      Nothing -> return $ reverse acc
      Just line
         | Right (_tokens, newState) <- lexResult,
           nonEmptyParenStack newState ->
              getParenContinueLines newState (line:acc)
         | otherwise -> return $ reverse (line:acc)
         where
         lexResult = runParser lexer $ stateWithLine
         stateWithLine = state { input = '\n':line }

prompt :: String -> Repl (Maybe String)
prompt str =
   withInputState prompter
   where
   prompter state = liftIO $ queryInput state $ getInputLine str
