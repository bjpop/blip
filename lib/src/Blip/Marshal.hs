{-# LANGUAGE RecordWildCards #-}

module Blip.Marshal (readPyc, writePyc, PycFile (..), PyObject (..)) where

import Data.Map as Map hiding (map, size)
import Data.Word (Word8, Word32)
import Data.Binary (Binary (..))
import Control.Monad.Error (ErrorT (..), lift, replicateM)
import System.IO (Handle)
import qualified Data.ByteString.Lazy as B 
   (ByteString, hGetContents, empty, unpack, hPutStr, length)
import Data.Binary.Get (Get, runGet, getLazyByteString, getWord32le, getWord8)
import Data.Binary.Put (Put, PutM, putWord32le, putLazyByteString, runPutM, putWord8)
import Data.Int (Int64)
import Data.Char (chr, ord)
import Blip.Bytecode (decode, encode, BytecodeSeq (..))
import Blip.Pretty (Pretty (..), prettyList, prettyTuple)
import Text.PrettyPrint
   (text, (<+>), ($$), render, empty, integer, (<>), hsep, Doc, parens
   , comma, hcat, vcat, rparen, int, equals, doubleQuotes, brackets, punctuate)
import Control.Applicative ((<$>), (<*>))

data PycFile =
   PycFile
   { magic :: Word32
   , modified_time :: Word32
   , size :: Word32           -- XXX what does this measure?
   , object :: PyObject
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
   | Int { value :: Word32 }
   | None
   | Unicode { unicode :: B.ByteString } -- should be decoded into a String
   | TrueObj
   | FalseObj
   deriving Show

instance Pretty PyObject where
   pretty (String {..}) = doubleQuotes $ pretty string
   pretty (Tuple {..}) = prettyTuple $ map pretty elements
   pretty (Int {..}) = pretty value
   pretty None = text "None"
   pretty TrueObj = text "True"
   pretty FalseObj = text "False"
   pretty (Unicode {..}) = doubleQuotes $ pretty unicode
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

prettyConsts :: PyObject -> Doc
prettyConsts obj =
   case obj of
      Tuple {..} -> 
         vcat $ map prettyConst $ zip [0..] elements
      other -> text ("consts not a tuple: " ++ show obj)
   where
   prettyConst :: (Int, PyObject) -> Doc
   prettyConst (i, obj) = text "const" <+> int i <+> equals <+> pretty obj 

prettyLnotab :: PyObject -> Doc
prettyLnotab obj =
   case obj of
      String {..} -> prettyList $ map pretty $ B.unpack string
      other -> text ("lnotab not a string: " ++ show obj)

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
       TRUE -> return TrueObj
       FALSE -> return FalseObj
       UNICODE -> readUnicodeObject
       _other -> error ("unsupported object " ++ show object_type)

writeObject :: PyObject -> PutData
writeObject object =
   case object of
      Code {..} -> writeCodeObject object
      String {..} -> writeStringObject object
      Tuple {..} -> writeTupleObject object
      Int {..} -> writeIntObject object
      None -> putU8 $ encodeObjectType NONE
      Unicode {..} -> writeUnicodeObject object
      TrueObj -> putU8 $ encodeObjectType TRUE
      FalseObj -> putU8 $ encodeObjectType FALSE

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

readStringObject :: GetData PyObject
readStringObject = do
   len <- getU32
   String <$> (getBS $ fromIntegral len)

writeStringObject :: PyObject -> PutData
writeStringObject (String {..}) = 
   writeObjectType STRING >>
   putU32 (fromIntegral $ B.length string) >>
   putBS string

readTupleObject :: GetData PyObject
readTupleObject = do
   len <- getU32
   Tuple <$> replicateM (fromIntegral len) readObject

writeTupleObject :: PyObject -> PutData
writeTupleObject (Tuple {..}) =
   writeObjectType TUPLE >>
   putU32 (fromIntegral $ length elements) >>
   mapM_ writeObject elements

readIntObject :: GetData PyObject
readIntObject = Int <$> getU32

writeIntObject :: PyObject -> PutData
writeIntObject (Int {..}) = 
   writeObjectType INT >> putU32 value

readUnicodeObject :: GetData PyObject
readUnicodeObject = do
   len <- getU32
   Unicode <$> (getBS $ fromIntegral len) -- XXX should be decoded into a String

writeUnicodeObject :: PyObject -> PutData
writeUnicodeObject (Unicode {..}) =
   writeObjectType UNICODE >>
   putU32 (fromIntegral $ B.length unicode) >>
   putBS unicode

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

getE :: Binary a => GetData a
getE = lift get

getBS :: Int64 -> GetData B.ByteString
getBS = lift . getLazyByteString

-- read an unsigned 8 bit word
getU8 :: GetData Word8
getU8 = lift getWord8

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

-- write a bytestring
putBS :: B.ByteString -> PutData
putBS = lift . putLazyByteString

-- write an unsigned 8 bit word
putU8 :: Word8 -> PutData
putU8 = lift . putWord8

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
