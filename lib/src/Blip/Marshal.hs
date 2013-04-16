{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Blip.Marshal
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Reading, writing and representation of Python bytecode files.
--
-----------------------------------------------------------------------------

module Blip.Marshal (readPyc, writePyc, PycFile (..), PyObject (..)) where

import Blip.MarshalDouble (bytesToDouble, doubleToBytes)
import Blip.Bytecode (decode, BytecodeSeq (..))
import Blip.Pretty (Pretty (..), prettyList, prettyTuple)
import Control.Applicative ((<$>), (<*>))
import Data.Map as Map hiding (map, size)
import Data.Word (Word8, Word32)
import Control.Monad.Error (ErrorT (..), lift, replicateM)
import System.IO (Handle)
import qualified Data.ByteString.Lazy as B 
   (ByteString, hGetContents, unpack, hPutStr, length)
import Data.ByteString.Lazy.UTF8 as UTF8 (toString, fromString) 
import Data.Binary.Get (Get, runGet, getLazyByteString, getWord32le, getWord8)
import Data.Binary.Put (PutM, putWord32le, putLazyByteString, runPutM, putWord8)
import Data.Int (Int64)
import Data.Char (chr, ord)
import Text.PrettyPrint
   (text, (<+>), ($$), (<>), Doc , vcat, int, equals, doubleQuotes)

data PycFile =
   PycFile
   { magic :: Word32
   , modified_time :: Word32
   , size :: Word32           -- the size in bytes of the original Python source
   , object :: PyObject       -- a code object
   }
   deriving Show

instance Pretty PycFile where
   pretty pycFile = 
      text "magic =" <+> pretty (magic pycFile) $$
      text "modified time =" <+> pretty (modified_time pycFile) $$
      text "size =" <+> pretty (size pycFile) $$
      text "object =" <+> pretty (object pycFile)

data PyObject
   = Code
        { argcount :: Word32 -- #arguments, except *args
        , kwonlyargcount :: Word32 -- #keyword only arguments 
        , nlocals :: Word32 -- #local variables
        , stacksize :: Word32 -- #entries needed for evaluation stack
        , flags :: Word32 -- CO_..., see below
        , code :: PyObject -- instruction opcodes (a string)
        , consts :: PyObject -- list (constants used) XXX seems to be a tuple
        , names :: PyObject -- list of strings (names used)
        , varnames :: PyObject -- tuple of strings (local variable names)
        , freevars :: PyObject -- tuple of strings (free variable names)
        , cellvars :: PyObject -- tuple of strings (cell variable names)
        , filename :: PyObject -- unicode (where it was loaded from)
        , name :: PyObject -- unicode (name, for reference)
        , firstlineno :: Word32 -- first source line number
        , lnotab :: PyObject -- string (encoding addr<->lineno mapping)
        }
   | String { string :: B.ByteString }
   | Tuple { elements :: [PyObject] }
   | Int { int_value :: Word32 }
   | Float { float_value :: Double }
   | None
   | Ellipsis
   | Unicode { unicode :: String } -- should be decoded into a String
   | TrueObj
   | FalseObj
   | Complex { real :: Double, imaginary :: Double }
   deriving (Eq, Ord, Show)

instance Pretty PyObject where
   pretty (String {..}) = doubleQuotes $ pretty string
   pretty (Tuple {..}) = prettyTuple $ map pretty elements
   pretty (Int {..}) = pretty int_value
   pretty (Float {..}) = pretty float_value
   pretty None = text "None"
   pretty Ellipsis = text "..."
   pretty TrueObj = text "True"
   pretty FalseObj = text "False"
   pretty (Unicode {..}) = doubleQuotes $ text unicode
   pretty (Code {..}) =
      text "argcount =" <+> pretty argcount $$
      text "kwonlyargcount =" <+> pretty kwonlyargcount $$
      text "nlocals =" <+> pretty nlocals $$
      text "stacksize =" <+> pretty stacksize $$
      text "flags =" <+> pretty flags $$
      text "varnames =" <+> pretty varnames $$
      text "freevars =" <+> pretty freevars $$
      text "cellvars =" <+> pretty cellvars $$
      text "filename =" <+> pretty filename $$
      text "name =" <+> pretty name $$
      text "firstlineno =" <+> pretty firstlineno $$
      text "lnotab =" <+> prettyLnotab lnotab $$
      text "names =" <+> pretty names $$
      prettyConsts consts $$ 
      text "code =" <+> pretty (BytecodeSeq $ decode $ string code)
   pretty (Complex {..}) = pretty real <+> text "+" <+> pretty imaginary <> text "j"

prettyConsts :: PyObject -> Doc
prettyConsts obj =
   case obj of
      Tuple {..} -> 
         vcat $ map prettyConst $ zip [0..] elements
      _other -> text ("consts not a tuple: " ++ show obj)
   where
   prettyConst :: (Int, PyObject) -> Doc
   prettyConst (i, obj) = text "const" <+> int i <+> equals <+> pretty obj 

prettyLnotab :: PyObject -> Doc
prettyLnotab obj =
   case obj of
      String {..} -> prettyList $ map pretty $ B.unpack string
      _other -> text ("lnotab not a string: " ++ show obj)

readPyc :: Handle -> IO PycFile
readPyc handle = do
   bytes <- B.hGetContents handle
   runGetDataCheck getPycFile bytes

writePyc :: Handle -> PycFile -> IO ()
writePyc handle pycFile = do
   bytes <- runPutDataCheck $ putPycFile pycFile
   B.hPutStr handle bytes

getPycFile :: GetData PycFile
getPycFile = PycFile <$> getU32 <*> getU32 <*> getU32 <*> readObject

putPycFile :: PycFile -> PutData
putPycFile pycFile = do
   putU32 $ magic pycFile
   putU32 $ modified_time pycFile
   putU32 $ size pycFile
   writeObject $ object pycFile

readObject :: GetData PyObject
readObject = do
   object_type <- decodeObjectType <$> getU8
   case object_type of
       CODE -> readCodeObject
       STRING -> readStringObject
       TUPLE -> readTupleObject
       INT -> readIntObject
       NONE -> return None 
       ELLIPSIS -> return Ellipsis
       TRUE -> return TrueObj
       FALSE -> return FalseObj
       UNICODE -> readUnicodeObject
       BINARY_FLOAT -> readFloatObject
       BINARY_COMPLEX -> readComplexObject
       _other -> error ("readObject: unsupported object type: " ++ show object_type)

writeObject :: PyObject -> PutData
writeObject object =
   case object of
      Code {..} -> writeCodeObject object
      String {..} -> writeStringObject object
      Tuple {..} -> writeTupleObject object
      Int {..} -> writeIntObject object
      None -> putU8 $ encodeObjectType NONE
      Ellipsis -> putU8 $ encodeObjectType ELLIPSIS
      Unicode {..} -> writeUnicodeObject object
      TrueObj -> putU8 $ encodeObjectType TRUE
      FalseObj -> putU8 $ encodeObjectType FALSE
      Float {..} -> writeFloatObject object
      Complex {..} -> writeComplexObject object

writeObjectType :: ObjectType -> PutData
writeObjectType = putU8 . encodeObjectType

readCodeObject :: GetData PyObject
readCodeObject =
   Code <$> getU32 <*> getU32 <*> getU32 <*> getU32 <*> getU32 <*> 
      readObject <*> readObject <*> readObject <*> readObject <*>
      readObject <*> readObject <*> readObject <*> readObject <*>
      getU32 <*> readObject

writeCodeObject :: PyObject -> PutData
writeCodeObject (Code {..}) =
   writeObjectType CODE >>
   mapM_ putU32 [argcount, kwonlyargcount, nlocals, stacksize, flags] >>
   mapM_ writeObject [code, consts, names, varnames, freevars, cellvars,
      filename, name] >>
   putU32 firstlineno >>
   writeObject lnotab
writeCodeObject other = error $ "writeCodeObject called on non code object: " ++ show other

readStringObject :: GetData PyObject
readStringObject = do
   len <- getU32
   String <$> (getBS $ fromIntegral len)

writeStringObject :: PyObject -> PutData
writeStringObject (String {..}) = 
   writeObjectType STRING >>
   putU32 (fromIntegral $ B.length string) >>
   putBS string
writeStringObject other = error $ "writStringObject called on non string object: " ++ show other

readTupleObject :: GetData PyObject
readTupleObject = do
   len <- getU32
   Tuple <$> replicateM (fromIntegral len) readObject

writeTupleObject :: PyObject -> PutData
writeTupleObject (Tuple {..}) =
   writeObjectType TUPLE >>
   putU32 (fromIntegral $ length elements) >>
   mapM_ writeObject elements
writeTupleObject other = error $ "writeTupleObject called on non tuple object: " ++ show other

readIntObject :: GetData PyObject
readIntObject = Int <$> getU32

writeIntObject :: PyObject -> PutData
writeIntObject (Int {..}) = 
   writeObjectType INT >> putU32 int_value
writeIntObject other = error $ "writeIntObject called on non int object: " ++ show other

readFloatObject :: GetData PyObject
readFloatObject = Float <$> getDouble

readComplexObject :: GetData PyObject
readComplexObject = Complex <$> getDouble <*> getDouble

writeFloatObject :: PyObject -> PutData
writeFloatObject (Float {..}) = 
   writeObjectType BINARY_FLOAT >> putDouble float_value
writeFloatObject other = error $ "writeFloatObject called on non float object: " ++ show other

writeComplexObject :: PyObject -> PutData
writeComplexObject (Complex {..}) =
   writeObjectType BINARY_COMPLEX >> putDouble real >> putDouble imaginary
writeComplexObject other = error $ "writeComplexObject called on non complex object: " ++ show other

readUnicodeObject :: GetData PyObject
readUnicodeObject = do
   len <- getU32
   bs <- getBS $ fromIntegral len
   return $ Unicode $ UTF8.toString bs

writeUnicodeObject :: PyObject -> PutData
writeUnicodeObject (Unicode {..}) = do
   writeObjectType UNICODE
   let uc = UTF8.fromString unicode 
   putU32 (fromIntegral $ B.length uc)
   putBS uc
writeUnicodeObject other = error $ "writeUnicodeObject called on non unicode object: " ++ show other

data ObjectType
   = NULL               -- '0'
   | NONE               -- 'N'
   | FALSE              -- 'F'
   | TRUE               -- 'T'
   | STOPITER           -- 'S'
   | ELLIPSIS           -- '.'
   | INT                -- 'i'

   | INT64              -- 'I' INT64 is deprecated. It is not,
                        -- generated anymore, and support for reading it
                        -- will be removed in Python 3.4.

   | FLOAT              -- 'f'
   | BINARY_FLOAT       -- 'g'
   | COMPLEX            -- 'x'
   | BINARY_COMPLEX     -- 'y'
   | LONG               -- 'l'
   | STRING             -- 's'
   | TUPLE              -- '('
   | LIST               -- '['
   | DICT               -- '{'
   | CODE               -- 'c'
   | UNICODE            -- 'u'
   | UNKNOWN            -- '?'
   | SET                -- '<'
   | FROZENSET          -- '>'
   deriving (Eq, Ord, Show)

charToObjectType :: Map.Map Char ObjectType
charToObjectType = Map.fromList objectTypeList 

objectTypeToChar :: Map.Map ObjectType Char
objectTypeToChar = Map.fromList [ (y, x) | (x, y) <- objectTypeList ]

objectTypeList :: [(Char, ObjectType)]
objectTypeList = [
   ('0', NULL), 
   ('N', NONE),
   ('F', FALSE), 
   ('T', TRUE),
   ('S', STOPITER),
   ('.', ELLIPSIS), 
   ('i', INT),
   ('I', INT64),
   ('f', FLOAT),
   ('g', BINARY_FLOAT),
   ('x', COMPLEX),
   ('y', BINARY_COMPLEX),
   ('l', LONG),
   ('s', STRING),
   ('(', TUPLE),
   ('[', LIST),
   ('{', DICT),
   ('c', CODE),
   ('u', UNICODE),
   ('?', UNKNOWN),
   ('<', SET),
   ('>', FROZENSET) ]

encodeObjectType :: ObjectType -> Word8
encodeObjectType objectType =
   case Map.lookup objectType objectTypeToChar of
      Nothing -> error $ "bad object type: " ++ show objectType
      Just chr -> fromIntegral $ ord chr 

decodeObjectType :: Word8 -> ObjectType
decodeObjectType byte =
   case Map.lookup byteChar charToObjectType of
      Nothing -> error $ "bad object type: " ++ show byteChar
      Just t -> t
   where
   byteChar = chr $ fromIntegral byte

-- utilities for reading binary data from a sequence of bytes
type GetData a = ErrorT String Get a

getDouble :: GetData Double
getDouble = do
   bs <- replicateM 8 getU8
   return $ bytesToDouble bs

getBS :: Int64 -> GetData B.ByteString
getBS = lift . getLazyByteString

-- read an unsigned 8 bit word
getU8 :: GetData Word8
getU8 = lift getWord8

-- XXX is it always little endian?
-- read an unsigned 32 bit word
getU32 :: GetData Word32
getU32 = lift getWord32le

runGetData :: GetData a -> B.ByteString -> Either String a
runGetData = runGet . runErrorT

runGetDataCheck  :: GetData a -> B.ByteString -> IO a
runGetDataCheck g b =
   case runGetData g b of
      Left e -> fail e
      Right v -> return v

-- utilities for writing binary data to a sequence of bytes
type PutData = ErrorT String PutM ()

putDouble :: Double -> PutData
putDouble d = mapM_ putU8 $ doubleToBytes d

-- write a bytestring
putBS :: B.ByteString -> PutData
putBS = lift . putLazyByteString

-- write an unsigned 8 bit word
putU8 :: Word8 -> PutData
putU8 = lift . putWord8

-- XXX is it always little endian?
-- write an unsigned 32 bit word
putU32 :: Word32 -> PutData
putU32 = lift . putWord32le

runPutData :: PutData -> Either String B.ByteString
runPutData comp = 
   case runPutM (runErrorT comp) of
      (Left err, _) -> Left err
      (Right (), bs) -> Right bs 

runPutDataCheck  :: PutData -> IO B.ByteString
runPutDataCheck comp =
   case runPutData comp of
      Left e -> fail e
      Right bs -> return bs
