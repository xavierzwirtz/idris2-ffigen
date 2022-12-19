module FFIGen.CompileSettings

import Data.SortedMap
import FFIGen.Compiler
import JSON.Derive

%language ElabReflection

record FunctionArgSettings where
  functionArgName : String
  functionArgMapping : FunctionArgMapping

public export
record CompilerFunctionSettings where
  constructor MkCompilerFunctionSettings
  functionName : String
  hasIO : Bool
  functionArgSettings : List FunctionArgSettings

Cast CompilerFunctionSettings FunctionSettings where
  cast x =
    MkFunctionSettings
      { hasIO = x.hasIO
      , functionArgMappings =
        fromList $ map
          (\y => (y.functionArgName, y.functionArgMapping))
          x.functionArgSettings
      }

public export
getFunctionSettings
  : List CompilerFunctionSettings
  -> SortedMap String FunctionSettings
getFunctionSettings xs =
  fromList $ map (\x => (x.functionName, cast x)) xs

public export
record CompilerStructMemberSettings where
  constructor MkCompilerStructMemberSettings
  memberName : String
  createSet : Bool
  createGet : Bool

Cast CompilerStructMemberSettings StructMemberSettings where
  cast x =
    MkStructMemberSettings
      { createSet = x.createSet
      , createGet = x.createGet
      }

public export
record CompilerStructSettings where
  constructor MkCompilerStructSettings
  structName : String
  createConstructor : Bool
  members : List CompilerStructMemberSettings

Cast CompilerStructSettings StructSettings where
  cast x =
    MkStructSettings
      { createConstructor = x.createConstructor
      , members =
        fromList $ map
          (\y => (y.memberName, cast y))
          x.members
      }

public export
getStructSettings
  : List CompilerStructSettings
  -> SortedMap String StructSettings
getStructSettings xs =
  fromList $ map (\x => (x.structName, cast x)) xs

public export
record CompileSettings where
  constructor MkCompileSettings
  libName : String
  moduleName : String
  structsToParse : List CompilerStructSettings
  functionsToParse : List CompilerFunctionSettings

%runElab derive "FunctionArgMapping" [Show,Eq,ToJSON,FromJSON]
%runElab derive "FunctionArgSettings" [Show,Eq,ToJSON,FromJSON]
%runElab derive "CompilerFunctionSettings" [Show,Eq,ToJSON,FromJSON]
%runElab derive "CompilerStructMemberSettings" [Show,Eq,ToJSON,FromJSON]
%runElab derive "CompilerStructSettings" [Show,Eq,ToJSON,FromJSON]
%runElab derive "CompileSettings" [Show,Eq,ToJSON,FromJSON]
