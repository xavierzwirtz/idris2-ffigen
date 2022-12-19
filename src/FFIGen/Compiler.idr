module FFIGen.Compiler

import Data.SortedMap
import Data.List
import Data.List1
import Data.String
import Data.String.Extra
import Data.Maybe
import Control.App
import Language.JSON
import FFIGen.IdrisAST
import FFIGen.CAST
import FFIGen.ClangParser
import FFIGen.MonadExt

%default covering

public export
data CompilerParseError
= UnhandledArgumentType String
| UnhandledReturnType String
| UnexpectedFunction String

public export
record StructMemberSettings where
  constructor MkStructMemberSettings
  createGet : Bool
  createSet : Bool

public export
record StructSettings where
  constructor MkStructSettings
  createConstructor : Bool
  members : SortedMap String StructMemberSettings

defaultStructSettings : Maybe StructSettings -> StructSettings
defaultStructSettings (Just x) = x
defaultStructSettings Nothing =
  MkStructSettings
    { createConstructor = True
    , members = empty
    }

defaultStructMemberSettings : Maybe StructMemberSettings -> StructMemberSettings
defaultStructMemberSettings (Just x) = x
defaultStructMemberSettings Nothing =
  MkStructMemberSettings
    { createGet = True
    , createSet = True
    }

public export
data FunctionArgMapping
  = AsPtrString

public export
FunctionArgMappings : Type
FunctionArgMappings = SortedMap String FunctionArgMapping

public export
record FunctionSettings where
  constructor MkFunctionSettings
  hasIO : Bool
  functionArgMappings : FunctionArgMappings

export
Show CompilerParseError where
  show (UnhandledArgumentType x) = "unhandled argument type '\{x}'"
  show (UnhandledReturnType x) = "unhandled return type '\{x}'"
  show (UnexpectedFunction x) = "unexpected function '\{x}'"

shimLibName : String -> String
shimLibName x = "libidris2-ffigen-\{x}"

buildIdrisFunctionType : List1 (String, ASTType) -> ASTType -> ASTType
buildIdrisFunctionType ((head_n, head_ty) ::: []) returnType = ASTTypeFunNamed head_n head_ty returnType
buildIdrisFunctionType ((head_n, head_ty) ::: (tail :: tails)) returnType =
  let x = tail ::: tails in
  ASTTypeFunNamed head_n head_ty (buildIdrisFunctionType x returnType)

buildPrimitiveFunctionApply : (args : List (String, ASTType)) -> Expression -> Expression
buildPrimitiveFunctionApply args expr =
  case args of
    [] => expr
    (arg, _)::args => buildPrimitiveFunctionApply args $ Apply expr (ExprName arg)

buildIdrisFunctionArgs : (args : List (String, ASTType)) -> Expression -> Expression
buildIdrisFunctionArgs args expr =
  case args of
    [] => expr
    (arg, _)::args => Lambda arg (buildIdrisFunctionArgs args expr)

buildForeignFunction : (libName : String)
                    -> (primName : String)
                    -> (name : String)
                    -> (args : List (String, ASTType))
                    -> (returnType : ASTType)
                    -> (needsShim : Bool)
                    -> (isPrimIO : Bool)
                    -> TopLevel
buildForeignFunction libName
                     primName
                     name
                     args
                     returnType
                     needsShim
                     isPrimIO = do
  let returnType = case returnType of
                       ASTTypeName "Bool" => ASTTypeName "Int"
                       x => x
  let returnType =
    if isPrimIO == True then ASTTypeApply (ASTTypeName "PrimIO") returnType
                        else returnType

  let idrisTy =
    case args of
       [] => returnType
       x::xs => buildIdrisFunctionType (x:::xs) returnType

  let libName' : String = if needsShim then shimLibName libName else libName

  TopMember $ MkMember primName
                       idrisTy
                       Export
                       (Foreign "C:\{name},\{libName'}")

buildIdrisFunction : (functionSettings : FunctionSettings)
                  -> (primName : String)
                  -> (name : String)
                  -> (args : List (String, ASTType))
                  -> (returnType : ASTType)
                  -> TopLevel
buildIdrisFunction functionSettings primName name args returnType = do
  let ioReturnType =
    if hasIO functionSettings then (ASTTypeApply (ASTTypeName "io") returnType)
                              else returnType
  let idrisTy =
    case args of
       [] => ioReturnType
       x::xs => buildIdrisFunctionType (x:::xs) ioReturnType
  let idrisTy =
    if hasIO functionSettings then ASTTypeConstraint (MkConstraint "HasIO" "io") idrisTy
                              else idrisTy
  let idrisName =
    pack $ case unpack name of
                [] => Prelude.Nil
                x::xs => (toLower x) :: xs
  let apply = (buildPrimitiveFunctionApply args
                                           (ExprName primName))
  let body =
    if hasIO functionSettings then (Apply (ExprName "primIO") apply)
                              else apply

  let compare = Apply (Apply (ExprName "(==)") (ExprName "x")) (ExprName "1")

  let body =
    case returnType of
            ASTTypeName "Bool" => Apply (Apply (ExprName "(>>=)") body) (Lambda "x" (Apply (ExprName "pure") compare))
            _ => body

  let idrisExpr =
      buildIdrisFunctionArgs
        args
        body
  TopMember $ MkMember idrisName
                       idrisTy
                       Export
                       (Value idrisExpr)

compileNameDef : (nameDef : CNameDef) -> (Maybe String, ASTType)
compileNameDef nameDef =
  case (CAST.CNameDef.const nameDef, typeName nameDef, pointerCount nameDef) of
    (False, "_Bool", 0) => (Nothing, ASTTypeName "Bool")
    (False, "int", 0) => (Nothing, ASTTypeName "Int")
    (False, "int32_t", 0) => (Nothing, ASTTypeName "Int")
    (False, "float", 0) => (Nothing, ASTTypeName "Double")
    (False, "double", 0) => (Nothing, ASTTypeName "Double")
    (False, "void", 0) => (Nothing, ASTTypeName "()")
    (False, "void", 1) => (Nothing, ASTTypeName "AnyPtr")
    (False, "uintptr_t", 0) => (Nothing, ASTTypeName "AnyPtr") -- TODO untested
    (True, "char", 1) => (Nothing, ASTTypeName "String")
    (False, "char", 0) => (Nothing, ASTTypeName "Char")
    (_, n, _) => (Just n, ASTTypeName n)

compileCShimApplyArg : (arg : CNameDef)
                    -> ((List CNameDef), CExpression)
compileCShimApplyArg arg =
  ([ { typeName := case typeName arg of
                     "float" => "double"
                     x => x } arg ], CExprName $ name $ arg)

compileCShimBody : Bool
                -> CNameDef
                -> CExpression
                -> List CStatement
compileCShimBody needsPointerWrapper nameDef expr =
  if needsPointerWrapper then
      [ CVarDefSet ({ name := "retVal" } nameDef) (CExprName "malloc(sizeof (\{typeName nameDef}))")
      , CVarSet (pointerCount nameDef) "retVal" expr
      , CReturn $ CExprName "retVal"
      ]
    else [CReturn expr]

compileCShim : (functionSettings : FunctionSettings)
            -> (structs : SortedMap String (List1 CNameDef))
            -> (nameDef : CNameDef)
            -> (shimName : String)
            -> (args : List CNameDef)
            -> (needsPointerWrapper : Bool)
            -> CTopLevel
compileCShim functionSettings
             structs
             nameDef
             shimName
             args
             needsPointerWrapper =
  let
    retBool = typeName nameDef == "_Bool"
    args_expr =
      (map (\arg =>
        case lookup (name arg) (functionArgMappings functionSettings) of
          (Just AsPtrString) => (compileCShimApplyArg arg)
          Nothing =>
            case compileNameDef arg of
              (Just n, _) =>
                ( [ ({ pointerCount := (pointerCount arg) + 1 } arg) ]
                , (CExprDeref $ CExprName $ name arg)
                )
              (_, _) => (compileCShimApplyArg arg))
        args)

    args = concat $ map fst args_expr
    expr = CApply (name nameDef) (map snd args_expr)
    expr = if retBool then CTernary expr (CValueInt 1) (CValueInt 0) else expr
    nameDef =
      let
        nameDef' : CNameDef
        nameDef' = if retBool then { typeName := "int" } nameDef else nameDef
        nameDef'' : CNameDef
        nameDef'' = { name := shimName } nameDef'
        retNameDef : CNameDef
        retNameDef = if needsPointerWrapper then ({ pointerCount := (pointerCount nameDef') + 1} nameDef'') else nameDef''
      in retNameDef
  in
  CTopLevelFunction $ MkCFunction nameDef args (compileCShimBody needsPointerWrapper nameDef expr)

compileFunction : (Exception CompilerParseError es)
               => (libName : String)
               -> (functionSettings : FunctionSettings)
               -> (structs : SortedMap String (List1 CNameDef, StructSettings))
               -> (nameDef : CNameDef)
               -> (args : List CNameDef)
               -> App es (List TopLevel, List CTopLevel, List String)
compileFunction libName functionSettings structs nameDef args = do
  let primName = "prim__\{name nameDef}"
  let shimmedArgTypes = ["_Bool", "float"] ++ (keys structs)
  let idrisArgs_argPointerTypes =
    map
        (\arg => case lookup (name arg) (functionArgMappings functionSettings) of
                 Nothing =>
                   let (ptrTypeName, ty) = compileNameDef arg in
                   ( ( name arg, ty), ptrTypeName)
                 (Just AsPtrString) =>
                   ( ( name arg
                     , ASTTypeApply (ASTTypeName "Ptr") (ASTTypeName "String"))
                   , Nothing ))
        args
  let idrisArgs = map fst idrisArgs_argPointerTypes
  let argPointerTypes = concat $ map (toList . snd) idrisArgs_argPointerTypes
  let (returnTypePointerType, returnType) = compileNameDef nameDef
  let needsPointerWrapper = isJust returnTypePointerType
  let isShimmed = (\x => elem (typeName x) shimmedArgTypes)
  let needsShim =
    any isShimmed args ||
    isShimmed nameDef ||
    needsPointerWrapper ||
    any (\arg => case lookup (name arg) (functionArgMappings functionSettings) of
                 Nothing => False
                 Just _ => True) args ||
    any (\arg => case compileNameDef arg of
                 (Nothing, _) => False
                 (Just _, _) => True) args
  let name = (name nameDef)
  let cName = if needsShim then "idris_\{name}" else name
  let idris = buildIdrisFunction functionSettings
                                 primName
                                 name
                                 idrisArgs
                                 returnType
  let foreign = buildForeignFunction libName
                                     primName
                                     cName
                                     idrisArgs
                                     returnType
                                     needsShim
                                     (hasIO functionSettings)
  let c = if needsShim then [compileCShim functionSettings
                                          (map fst structs)
                                          nameDef
                                          cName
                                          args
                                          needsPointerWrapper] else []
  pure ([ foreign, idris ], c, argPointerTypes ++ toList returnTypePointerType)

compileStructMemberGet
  : (libName : String)
  -> (structName : String)
  -> (member : CNameDef)
  -> (TopLevel, CTopLevel)
compileStructMemberGet libName structName member =
  let
    n = ("get_" ++ structName ++ "_" ++ name member)
    (returnTypePointerType, returnType) = compileNameDef member
    box = isJust $ returnTypePointerType
    idr = buildForeignFunction libName
                               n
                               n
                               [(structName, ASTTypeName structName)]
                               returnType
                               True
                               False
    typeName' = case typeName member of
                  "float" => "double"
                  x => x
    c = CTopLevelFunction $ MkCFunction
      (MkNameDef { typeName = typeName'
                 , name = n
                 , const = False
                 , unsigned = False
                 , pointerCount = if box then (pointerCount member) + 1
                                         else pointerCount member
                 }) --pointerCount needs to change when we are wrapping a struct with a pointer
      [MkNameDef { typeName = structName
                 , name = "val"
                 , const = False
                 , unsigned = False
                 , pointerCount = 1
                 }]
      (if not box
        then [CReturn $ CArrowOperator (CExprName "val") (name member)]
        else [ CVarDefSet ({ name := "retVal", pointerCount := pointerCount member + 1 } member) (CExprName "malloc(sizeof (\{typeName member}))")
             , CVarSet ((pointerCount member)+1) "retVal" $ CArrowOperator (CExprName "val")
                                                                       (name member)
             , CReturn $ CExprName "retVal"
             ])
  in
  (idr, c)


buildStructMkFunType : List1 CNameDef
                    -> ASTType
                    -> ASTType
buildStructMkFunType (memb ::: []) returnType =
  ASTTypeFunNamed (name memb) (snd $ compileNameDef memb) returnType
buildStructMkFunType (memb ::: (tail :: tails)) returnType =
  let x = tail ::: tails in
  ASTTypeFunNamed (name memb) (snd $ compileNameDef memb) (buildStructMkFunType x returnType)

compileStructMkFunCArg : CNameDef -> CNameDef
compileStructMkFunCArg arg =
  { typeName := case typeName arg of
                  "float" => "double"
                  x => x
  , pointerCount := case compileNameDef arg of
                     (Nothing, _) => pointerCount arg
                     (Just _, _) => pointerCount arg + 1
  } arg

compileStructMkFunSetField : CNameDef -> CStatement
compileStructMkFunSetField arg =
  let
    expr = (CExprName $ name arg)
  in
  CStructMembSet (CExprDeref $ CExprName "inst")
                 (name arg)
                 (case compileNameDef arg of
                   (Nothing, _) => expr
                   (Just _, _) => CExprDeref expr)

compileStructMkFun : (libName : String)
                  -> (structName : String)
                  -> (members : List1 CNameDef)
                  -> (TopLevel, CTopLevel)
compileStructMkFun libName structName members =
  let
    primName = "Mk\{structName}"
    idrisTy = buildStructMkFunType members $ ASTTypeName structName
    mk =
      TopMember $ MkMember primName
        idrisTy
        Export
        (Foreign "C:\{primName},\{shimLibName libName}")
    nameDef = MkNameDef structName "Mk\{structName}" False False 1
    mkC = CTopLevelFunction
      $ MkCFunction
        nameDef
        (map compileStructMkFunCArg $ toList members)
        ( [ CVarDefSet (MkNameDef structName "inst" False False 1) (CExprName "malloc(sizeof (\{typeName nameDef}))") ]
        ++ (toList $ map compileStructMkFunSetField members)
        ++ [ CReturn (CExprName "inst")]
        )
  in
  (mk, mkC)


compileStruct
  : (Exception CompilerParseError es)
  => (libName : String)
  -> (structName : String)
  -> (createConstructor : Bool)
  -> (members : List1 (CNameDef, StructMemberSettings))
  -> App es (List TopLevel, List CTopLevel, List String)
compileStruct libName structName createConstructor members = do
  let (mkFun, mkFunC) =
    if createConstructor == True
        then case compileStructMkFun libName structName (map fst members) of
               (x, y) => (Just x, Just y)
        else (Nothing, Nothing)

  let memberFuncs =
      map
        (\(name, settings) =>
          if settings.createGet
            then Just $ compileStructMemberGet libName structName name
            else Nothing)
        (members)

  pure
    ( (toList mkFun) ++ (concat $ forget $ map toList $ map (map fst) memberFuncs)
    , (toList mkFunC) ++ (concat $ forget $ map toList $ map (map snd) memberFuncs)
    , [structName]
    )

rewriteSynonymsNameDef : (synonyms : SortedMap String String)
                      -> (nameDef : CNameDef)
                      -> CNameDef
rewriteSynonymsNameDef synonyms nameDef =
  case lookup (typeName nameDef) synonyms of
    Nothing => nameDef
    Just typeName' => { typeName := typeName' } nameDef

rewriteSynonymsTopLevel : (synonyms : SortedMap String String)
                       ->  (topLevel : ClangTopLevel)
                       ->  ClangTopLevel
rewriteSynonymsTopLevel synonyms (Function nameDef args) =
  Function (rewriteSynonymsNameDef synonyms nameDef)
           (map (rewriteSynonymsNameDef synonyms) args)
rewriteSynonymsTopLevel synonyms (Struct x xs) = Struct x $ map (rewriteSynonymsNameDef synonyms) xs
rewriteSynonymsTopLevel synonyms (Synonym x y) = Synonym x y

compileTopLevel
  : (Exception CompilerParseError es)
  => (libName : String)
  -> (functions : SortedMap String FunctionSettings)
  -> (structs : SortedMap String (List1 CNameDef, StructSettings))
  -> (topLevel : ClangTopLevel)
  -> App es (List TopLevel, List CTopLevel, List String)
compileTopLevel libName functions structs (Function nameDef args) =
  case (lookup (name nameDef) functions) of
    (Just functionSettings) => compileFunction libName functionSettings structs nameDef args
    (Nothing) => throw $ UnexpectedFunction $ name nameDef
compileTopLevel libName _ structs (Struct name members) =
  let
    settings = defaultStructSettings $ lookup name (map snd structs)
    members' = map (\x => (x, defaultStructMemberSettings $ lookup x.name settings.members)) members
  in
  compileStruct libName name settings.createConstructor members'
compileTopLevel _ _ _ (Synonym from to) = pure ([], [], [])

export
compile
  : (Exception CompilerParseError es)
  => (libName : String)
  -> (moduleName : String)
  -> (structs : SortedMap String StructSettings)
  -> (functions : SortedMap String FunctionSettings)
  -> (List ClangTopLevel)
  -> (App es (String, String))
compile libName moduleName structs functions topLevel = do
  let structs' =
      mapMaybe
        (\x => case x of
                 Struct name args =>
                   let
                     settings = defaultStructSettings $ lookup name structs
                   in
                   Just (name, args, settings)
                 _ => Nothing)
        topLevel

  let synonyms =
      fromList $ mapMaybe
        (\x => case x of
                 Synonym from to => Just (from, to)
                 _ => Nothing)
        topLevel

  idris_c <-
     traverse
       (\topLevel =>
         compileTopLevel
           libName
           functions
           (fromList structs')
           $ rewriteSynonymsTopLevel synonyms topLevel)
    topLevel
  let pointerTypes =
      map
        (\pointerType =>
          TopMember $
            MkMember
              pointerType
              (ASTTypeName "Type")
              PublicExport
              (Value $ ExprName "AnyPtr"))
        $ nub
        $ concat
        $ map (\(_, _, x) => x) idris_c

  let idris = pointerTypes ++ (concat $ map (\(x, _, _) => x) idris_c)
  let c = concat $ map (\(_, x, _) => x) idris_c
  let idrisModule = printModule moduleName idris
  let cCode = showTopLevels c
  pure (cCode, idrisModule)
