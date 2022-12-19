module FFIGen.IdrisAST

import Data.String.Extra

public export
record Constraint where
  constructor MkConstraint
  typeName : String
  typeVariable : String

public export
Show Constraint where
  show x = "\{typeName x} \{typeVariable x}"

public export
data ASTType
  = ASTTypeFun ASTType ASTType
  | ASTTypeFunNamed String ASTType ASTType
  | ASTTypeName String
  | ASTTypeConstraint Constraint ASTType
  | ASTTypeApply ASTType ASTType

export
Show ASTType where
  show (ASTTypeFun x y) = "(\{show x} -> \{show y})"
  show (ASTTypeFunNamed x y z) = "((\{x} : \{show y}) -> \{show z})"
  show (ASTTypeName name) = name
  show (ASTTypeConstraint constraint ty) = "(\{show constraint} => \{show ty})"
  show (ASTTypeApply x y) = "(\{show x} \{show y})"

public export
data Expression
  = ExprName String
  | Apply Expression Expression
  | Lambda String Expression

export
Show Expression where
  show (ExprName name) = name
  show (Apply e1 e2) = "(\{show e1}) (\{show e2})"
  show (Lambda arg body) = "(\\\{arg} => (\{show body}))"

public export
data ForeignOrValue
  = Foreign String
  | Value Expression

public export
data Visibility
  = Private
  | Export
  | PublicExport

public export
record Member where
  constructor MkMember
  name : String
  astType : ASTType
  visibility : Visibility
  foreignOrValue : ForeignOrValue

public export
Show Member where
  show member =
    let exportVal = case visibility member of
                      Private => ""
                      Export => "export "
                      PublicExport => "public export "
    in
    let def = "\{exportVal}\{name member} : \{show $ astType member}" in
    case foreignOrValue member of
      Foreign foreign =>
        """
        %foreign "\{foreign}"
        \{def}
        """
      Value value =>
        """
        \{def}
        \{name member} = \{show $ value}
        """

public export
data TopLevel
  = TopData
  | TopMember Member

public export
Show TopLevel where
  show TopData = "data"
  show (TopMember member) = show member

export
printModule : String -> List TopLevel -> String
printModule x xs =
  "module \{x}\n\{join "\n" $ map show xs}"
