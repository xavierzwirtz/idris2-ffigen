module FFIGen.ClangParser

import Data.List
import Data.List1
import Data.String
import Data.String.Extra
import Text.Lexer
import Text.Parser
import Control.App
import Control.App.Console
import Language.JSON
import FFIGen.CAST

%default covering

public export
data ClangTopLevel
  = Function CNameDef (List CNameDef)
  | Struct String (List1 CNameDef)
  | Synonym String String

export
data ClangParseError
= UndefinedKey String (List (String, JSON))
| CantParseType String
| WrongJsonType
| ZeroMemberStruct String

export
Show ClangParseError where
  show (UndefinedKey x json) = "undefined key '\{x}' in '\{show json}'"
  show (CantParseType ty) = "can't parse type '\{ty}'"
  show WrongJsonType = "wrong json type"
  show (ZeroMemberStruct struct) = "zero member struct '\{struct}'"

getKey : Exception ClangParseError es => String -> List (String, JSON) -> App es (JSON)
getKey name xs =
  case find (\(x, y) => x == name) xs of
    Just (x, y) => pure y
    Nothing => throw (UndefinedKey name xs)

getKeyMaybe : String -> List (String, JSON) -> Maybe JSON
getKeyMaybe name xs =
  case find (\(x, y) => x == name) xs of
    Just (x, y) => Just y
    Nothing => Nothing

getTypeName : List String -> Maybe (String, List String)
getTypeName [] = Nothing
getTypeName ("struct" :: name :: rest) = Just (name, rest)
getTypeName (name :: rest) = Just (name, rest)

parsePointerCount : List String -> Maybe Nat
parsePointerCount [] = Just 0
parsePointerCount [x] = do
  let unpacked = unpack x
  let filtered = filter ((==) '*') unpacked
  if unpacked == filtered then
      Just $ length unpacked
    else Nothing
parsePointerCount _ = Nothing


data CTypeToken
= Const
| Unsigned

Eq CTypeToken where
  Const    == Const      = True
  Unsigned == Unsigned   = True
  _        == _          = False

tokenizeCType : List String -> (List CTypeToken, List String)
tokenizeCType [] = ([], [])
tokenizeCType (x :: xs) =
  let
    token : Maybe CTypeToken
    token =
      case x of
        "const" => Just Const
        "unsigned" => Just Unsigned
        _ => Nothing
  in
  case token of
    Nothing => ([], x::xs)
    Just token' =>
      let
        (tokens, rest) = tokenizeCType xs
      in
      (token'::tokens, rest)

parseCType
  : Exception ClangParseError es
  => Console es
  => (name : String)
  -> (ty : String)
  -> App es CNameDef
parseCType name ty = do
  let ty = trim ty
  let rest = words ty
  let (tokens, rest) = tokenizeCType rest
  (typeName, rest) <- case getTypeName rest of
                        Nothing => throw $ CantParseType ty
                        Just x => pure x
  -- rest should just be * now
  pointerCount <- case parsePointerCount rest of
                    Nothing => throw $ CantParseType ty
                    Just x => pure x
  pure $ MkNameDef typeName name (elem Const tokens) (elem Unsigned tokens) pointerCount

parseQualTypeToNameDef
  : Exception ClangParseError es
  => Console es
  => String
  -> JSON
  -> App es CNameDef
parseQualTypeToNameDef name qualType =
  case qualType of
    (JString qualType) => do
      let (ty, _) = span (/= '(') qualType
      parseCType name ty
    _ => throw WrongJsonType

filterM : Monad m => (a -> m Bool) -> List a -> m (List a)
filterM p [] = pure []
filterM p (x :: xs)
    = if !(p x)
         then do xs' <- filterM p xs
                 pure (x :: xs')
         else filterM p xs

mapM : Monad m => (a -> m b) -> List a -> m (List b)
mapM p [] = pure []
mapM p (x :: xs)
     = do
    x' <- p x
    xs' <- mapM p xs
    pure (x' :: xs')

parseInner
  : Exception ClangParseError es
  => Console es
  => String
  -> List (String, JSON)
  -> App es (List CNameDef)
parseInner kind xs = do
  let inner = getKeyMaybe "inner" xs
  case inner of
    Nothing => pure []
    Just (JArray inner) => do
      argsJson <- mapM
        (\x => do
          case x of
            JObject x => pure x
            _ => throw WrongJsonType)
        inner
      argsJson <- filterM (\x => do
             kind' <- getKey "kind" x
             case kind' of
               JString x =>
                 if x == kind then pure True else pure False
               _ => throw WrongJsonType
       ) argsJson
      args <- mapM (\arg => do
        name <- getKey "name" arg
        case name of
            JString name => do
                type <- getKey "type" arg
                case type of
                    JObject type => do
                        qualType <- getKey "qualType" type
                        parseQualTypeToNameDef name qualType
                    _ => throw WrongJsonType
            _ => throw WrongJsonType
        ) argsJson
      pure args
    _ => throw WrongJsonType

parseFunctionDecl
  : Exception ClangParseError es
  => Console es
  => List String
  -> List (String, JSON)
  -> App es (Maybe ClangTopLevel)
parseFunctionDecl functionsToParse xs = do
  name <- getKey "name" xs
  case name of
    (JString name) => do
      if not $ elem name functionsToParse then pure Nothing
       else do
            nameDef <- do
                type <- getKey "type" xs
                case type of
                    (JObject type) => do
                      qualType <- getKey "qualType" type
                      parseQualTypeToNameDef name qualType
                    _ => throw WrongJsonType
            args <- parseInner "ParmVarDecl" xs
            pure $ Just $ Function nameDef args
    _ => throw WrongJsonType

parseRecordDecl
  : Exception ClangParseError es
  => Console es
  => List String
  -> List (String, JSON)
  -> App es (Maybe ClangTopLevel)
parseRecordDecl structsToParse xs = do
  case getKeyMaybe "name" xs of
    (Just (JString name)) => do
      if not $ elem name structsToParse then pure Nothing
       else do
            args <- parseInner "FieldDecl" xs
            case args of
              [] => throw $ ZeroMemberStruct name
              arg::args => pure $ Just $ Struct name (arg:::args)
    (Just _) => throw WrongJsonType
    (Nothing) => pure Nothing

parseTypedefDecl : Exception ClangParseError es => List String -> List (String, JSON) -> App es (Maybe ClangTopLevel)
parseTypedefDecl structsToParse xs = do
  JString name <- getKey "name" xs
          | _ => throw WrongJsonType
  if not $ elem name structsToParse then pure Nothing
    else do
      JObject ty <- getKey "type" xs
              | _ => throw WrongJsonType
      JString qualType <- getKey "qualType" ty
              | _ => throw WrongJsonType
      if isPrefixOf "struct " qualType then pure $ Nothing
        else pure $ Just $ Synonym name qualType
    --         qualType <- getKey "qualType" ty
    --         if startsWith "struct " qualType then []
    --           else
    --         args <- parseInner "FieldDecl" xs
    --         pure $ Just $ Struct name args

topLevel
  : (Exception ClangParseError es)
  => Console es
  => List String
  -> List String
  -> JSON
  -> (App es (List ClangTopLevel))
topLevel structsToParse functionsToParse (JObject xs) = do
  kind <- getKey "kind" xs
  case kind of
   (JString "FunctionDecl") => do
     x <- parseFunctionDecl functionsToParse xs
     pure $ case x of
       Nothing => []
       Just x => [x]
   (JString "RecordDecl") => do
     x <- parseRecordDecl structsToParse xs
     pure $ case x of
       Nothing => []
       Just x => [x]
   (JString "TypedefDecl") => do
     x <- parseTypedefDecl structsToParse xs
     pure $ case x of
       Nothing => []
       Just x => [x]
   (JString _) => pure []
   _ => throw WrongJsonType
topLevel _ _ _ = throw WrongJsonType

topLevelMany
  : Exception ClangParseError es
  => Console es
  => List String
  -> List String
  -> List JSON
  -> App es (List ClangTopLevel)
topLevelMany structsToParse functionsToParse [] = pure []
topLevelMany structsToParse functionsToParse (x :: xs) = do
  x <- topLevel structsToParse functionsToParse x
  xs <- topLevelMany structsToParse functionsToParse xs
  pure (x ++ xs)

export
moduleParser
  : (Exception ClangParseError es)
  => Console es
  => List String
  -> List String
  -> JSON
  -> (App es (List ClangTopLevel))
moduleParser structsToParse functionsToParse (JObject xs) = do
  inner <- getKey "inner" xs
  case inner of
    JArray inner => topLevelMany structsToParse functionsToParse inner
    _ => throw WrongJsonType
moduleParser _ _ _ = throw WrongJsonType
