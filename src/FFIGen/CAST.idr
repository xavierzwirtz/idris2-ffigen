module FFIGen.CAST

import Data.String
import Data.String.Extra

public export
record CNameDef where
  constructor MkNameDef
  typeName : String
  name : String
  const : Bool
  unsigned : Bool
  pointerCount : Nat


export
mkPointer : Nat -> String
mkPointer i = replicate i '*'

export
Show CNameDef where
  show ctype = "\{(if unsigned ctype then "unsigned " else "")}\{(if const ctype then "const " else "")}\{typeName ctype} \{mkPointer (pointerCount ctype) }\{name ctype}"

public export
data CExpression
  = CExprName String
  | CTernary CExpression CExpression CExpression
  | CApply String (List CExpression)
  | CApplyOperator CExpression String CExpression
  | CArrowOperator CExpression String
  | CValueInt Int
  | CValueBool Bool
  | CExprDeref CExpression

export
Show CExpression where
  show (CExprName x) = x
  show (CTernary x y z) = assert_total "(\{show x} ? \{show y} : \{show z})"
  show (CApply name args) = assert_total "\{name}(\{join ", " $ map show args})"
  show (CApplyOperator x op y) = assert_total "(\{show x}) \{op} (\{show y})"
  show (CArrowOperator x y) = assert_total "(\{show x})->\{y}"
  show (CValueInt x) = assert_total $ show x
  show (CValueBool x) = assert_total $ if x then "true" else "false"
  show (CExprDeref expr) = assert_total "*\{show expr}"

public export
data CStatement
  = CReturn CExpression
  | CVarDef CNameDef
  | CVarDefSet CNameDef CExpression
  | CVarSet Nat String CExpression
  | CStructDefSet CNameDef CExpression
  | CStructMembSet CExpression String CExpression

export
Show CStatement where
  show (CReturn expr)                 = "return (\{show expr});"
  show (CVarDef nameDef)              = "\{show nameDef};"
  show (CVarDefSet nameDef expr)   = "\{show nameDef} = \{show expr};"
  show (CVarSet pointerCount n expr)  = "\{mkPointer pointerCount}\{n} = \{show expr};"
  show (CStructDefSet nameDef expr)   = "struct \{show nameDef} = \{show expr};"
  show (CStructMembSet var memb val) =
    "(\{show var}).\{memb} = \{show val};"

public export
record CFunction where
  constructor MkCFunction
  nameDef : CNameDef
  args : List CNameDef
  body : List CStatement

export
Show CFunction where
  show function =
    """
      \{show $ nameDef function}(\{join ", " $ map show $ args function}) {
    """ ++
    (join "\n" $ map (\statement => "    \{show statement}") $ body function) ++
    """
      }
    """

public export
data CTopLevel
  = CTopLevelFunction CFunction

export
Show CTopLevel where
  show (CTopLevelFunction x) = show x

export
showTopLevels : List CTopLevel -> String
showTopLevels xs = join "\n" $ map show xs
