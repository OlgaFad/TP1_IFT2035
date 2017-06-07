---------------------------------------------------------------------------
-- Fichier principal pour le TP 1
-- Vous avez à modifier / compléter les fonctions de ce fichier
---------------------------------------------------------------------------

module Eval where

import Parseur


---------------------------------------------------------------------------
-- Le datatype des types
---------------------------------------------------------------------------
data Type = TInt
          | TArrow Type Type
          deriving (Eq)

instance Show Type where
  show TInt = "Int"
  show (TArrow t1 t2) = showParen' t1 ++ " -> " ++ show t2
    where showParen' x@(TArrow _ _) = "(" ++ show x ++ ")"
          showParen' x = show x

---------------------------------------------------------------------------
-- Le datatype des expressions et valeurs
---------------------------------------------------------------------------
data Exp = EInt Int
         | EVar Symbol
         | EApp Exp Exp
         | ELam Symbol Type Exp
         deriving (Eq)

data Value = VInt Int
           | VLam Symbol Exp Env
           | VPrim (Value -> Value)

instance Show Value where
  show (VInt n) = show n
  show _ = "<function>"

instance Eq Value where
  (VInt n1) == (VInt n2) = n1 == n2
  -- Impossible de comparer fonctions et primitives
  _ == _ = False

---------------------------------------------------------------------------
-- Pour ce TP, une erreur est simplement un chaîne de caractères
-- expliquant le problème
---------------------------------------------------------------------------
type Error = String


---------------------------------------------------------------------------
-- L'environnement d'exécution
-- Une simple liste qui contient des identifiants associés à des valeurs
---------------------------------------------------------------------------
type Env = [(Symbol, Value)]

env0 :: Env
env0 = [("+", prim (+)),
        ("-", prim (-)),
        ("*", prim (*))]
  where prim op =
          VPrim (\ (VInt x) -> VPrim (\ (VInt y) -> VInt (x `op` y)))



---------------------------------------------------------------------------
-- Fonction de converstion des Sexp en Expressions (Exp)
-- Vous allez devoir modifier sexp2type et sexp2Exp
---------------------------------------------------------------------------
sexp2type :: Sexp -> Either Error Type
sexp2type (SSym "Int") = Right TInt
sexp2type (SList (x : xs)) = do
  type1 <- sexp2type x
  type2 <- sexp2type (SList xs)
  return $ TArrow type1 type2
sexp2type _ = Left "Ill formed type"

reservedKeywords :: [Symbol]
reservedKeywords = ["lambda", "let", "case", "data", "Erreur"]

sexp2Exp :: Sexp -> Either Error Exp
sexp2Exp (SNum x) = Right $ EInt x
sexp2Exp (SSym ident) | ident `elem` reservedKeywords
  = Left $ ident ++ " is a reserved keyword"
sexp2Exp (SSym ident) = Right $ EVar ident
sexp2Exp (SList ((SSym "lambda") : (SList ((SList ((SSym var) : t : [])) : [])) : body : [])) = do
  body' <- sexp2Exp body
  t' <- sexp2type t
  return $ ELam var t' body'
sexp2Exp (SList ((SSym "lambda") :
                 (SList []) :
                 _ :
                 [])) = Left "Syntax Error : No parameter"

sexp2Exp (SList (func : arg : [])) = do
  func' <- sexp2Exp func
  arg' <- sexp2Exp arg
  return $ EApp func' arg'

sexp2Exp _ = Left "Syntax Error : Ill formed Sexp"


---------------------------------------------------------------------------
-- Fonction d'évaluation
-- Vous allez devoir modifier eval
---------------------------------------------------------------------------

lookupVar :: [(Symbol, Value)] -> Symbol -> Value
lookupVar [] sym = error "Error: empty Env"
lookupVar ((s,v) : _) sym | s == sym = v --que signifie | s == sym
lookupVar (_ : xs) sym = lookupVar xs sym

eval :: Env -> Exp -> Value
eval _ (EInt x) = VInt x
eval env (EVar sym) = lookupVar env sym
eval env (EApp exp1 exp2) =
  case eval env exp1 of
    VInt -> error "Error: an expression cant start with a number"
    VLam -> eval env exp1
    VPrim ->
  -- do
  --   return eval env0 exp1
  --   return eval env0 exp2  --il faut que sa evalue la valeur des deux exp: exp1 et exp2
eval env (ELam sym t ex) = VLam sym ex env

eval _ _ = error "Error: eval not possible"


---------------------------------------------------------------------------
-- Fonction pour la vérification de type
-- Vous allez devoir modifier typeCheck
---------------------------------------------------------------------------
type Tenv = [(Symbol, Type)]
tenv0 :: Tenv
tenv0 = [("+", TArrow TInt (TArrow TInt TInt)),
         ("-", TArrow TInt (TArrow TInt TInt)),
         ("*", TArrow TInt (TArrow TInt TInt))]

lookupType :: [(Symbol, Type)] -> Symbol -> Either Error Type
lookupType [] sym = Left $ "Not in scope variable : " ++ show sym
lookupType ((s,v) : _) sym | s == sym = Right v
lookupType (_ : xs) sym = lookupType xs sym

typeCheck :: Tenv -> Exp -> Either Error Type
typeCheck _ (EInt x) = Right TInt
typeCheck env (EVar sym) = lookupType env sym
typeCheck _ _ = error "Oups ..."
