module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP


type VEnv = E.Env Value

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           | C VEnv String [String] Exp
           -- Others as needed
           deriving (Show)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined -- should not ever be used

evaluate :: Program -> Value
evaluate [Bind _ _ _ e] = evalE E.empty e
evaluate bs = evalE E.empty (Let bs (Var "main"))


evalE :: VEnv -> Exp -> Value

-- Ints
evalE g (Num n) = I n

-- Bools
evalE g (Con "True") = B True
evalE g (Con "False") = B False

-- Lists
evalE g (Con "Nil") = Nil
evalE g (App (App (Con "Cons") e1) e2) = Cons n (evalE g e2)
  where
    I n = evalE g e1

evalE g (App (Prim Head) e) = I h
  where
    Cons h _ = evalE g e

evalE g (App (Prim Tail) e) = t
  where
    Cons _ t = evalE g e

evalE g (App (Prim Null) e) = case (evalE g e) of
  (Nil)     -> B True
  otherwise -> B False


-- Primops + NestedExpr
evalE g (App (App (Prim Gt) e1) e2) = B (m > n)
  where (I m) = evalE g e1
        (I n) = evalE g e2

evalE g (App (App (Prim Ge) e1) e2) = B (m >= n)
  where (I m) = evalE g e1
        (I n) = evalE g e2         

evalE g (App (App (Prim Lt) e1) e2) = B (m < n)
  where (I m) = evalE g e1
        (I n) = evalE g e2

evalE g (App (App (Prim Le) e1) e2) = B (m <= n)
  where (I m) = evalE g e1
        (I n) = evalE g e2

evalE g (App (App (Prim Eq) e1) e2) = B (n == m)
  where (I m) = evalE g e1
        (I n) = evalE g e2

evalE g (App (App (Prim Ne) e1) e2) = B (n /= m)
  where (I m) = evalE g e1
        (I n) = evalE g e2   

evalE g (App (Prim Neg) e) = I (-n)
  where (I n) = evalE g e

evalE g (App (App (Prim Add) e1) e2) = I (m + n)
  where
    (I m) = evalE g e1
    (I n) = evalE g e2

evalE g (App (App (Prim Mul) e1) e2) = I (m * n)
  where
    (I m) = evalE g e1
    (I n) = evalE g e2

evalE g (App (App (Prim Sub) e1) e2) = I (m - n)
  where
    (I m) = evalE g e1
    (I n) = evalE g e2

evalE g (App (App (Prim Quot) e1) e2) = I (m `quot` n)
  where
    (I m) = evalE g e1
    (I n) = evalE g e2

-- IfThenElse
evalE g (If e1 e2 e3) = case (evalE g e1) of
  (B True)  -> evalE g e2
  otherwise -> evalE g e3

-- Variables
evalE g (Var vname) = case E.lookup g vname of
  Just v  -> v
  Nothing -> error ("in evalE var: " ++ vname)

-- Variable Bidings with Let
-- evalE g (Let [Bind v_name _ _ e2] e1) = evalE g' e1
--   where
--     g' = E.add g (v_name, (evalE g e2))

evalE g (Let ((Bind f_name t [x] f_body):bs) e1) = evalE g' e1
  where
    g' = E.add g (f_name, (C g' f_name [x] f_body))


evalE g (Let ((Bind vname t [] e2):bs) e1) = evalE g' e1
  where
    helper :: [Bind] -> [(String, Value)]
    helper (b:bs)
      | (length bs) > 0 = (vname, (evalE g e2)):(helper bs)
      | otherwise       = [(vname, (evalE g e2))]
        where
          Bind vname _ _ e2 = b
    pairs = helper ((Bind vname t [] e2):bs)
    g' = E.addAll g pairs
-- 
-- LetFun
evalE g (Letfun (Bind f_name _ [] (App (Prim op) e))) = C g f_name [""] (App (App (Prim op) e) (Var ""))

evalE g (Letfun (Bind f_name t x (App e (Var vname))))
 | (length x) > 1 = C g f_name [head x] (Letfun (Bind (f_name ++ "'") t (tail x) (App e (Var vname))))




evalE g (Letfun (Bind f_name t x f_body))
  | (null x) = v
  | otherwise = C g f_name x f_body
      where
        v  = evalE g' f_body
        g' = E.add g (f_name, v)

-- Function application
evalE g (App e1 e2) = evalE g'' f_body
  where
    C g' f_name x f_body = evalE g e1
    v   = evalE g e2
    g'' = E.addAll g' [((head x), v), (f_name, C g' f_name x f_body)]








