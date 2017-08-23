module Wiz.Core (
    equal,
    car,
    cdr,
    cons,
    list,
    nil,
    Wiz.Core.not,
    Wiz.Core.or,
    Wiz.Core.and,
    pair,
    evalBool
  ) where

import Wiz.Types
import Wiz.Environment

equal :: Value -> Value -> Expression
equal x y | x == y = Boolean True
          | otherwise = Boolean False

car :: Value -> Expression
car (E (List (x:_))) = x
car (E (List [])) = List []
car _ = error "car applied to non list expression!"

cdr :: Value -> Expression
cdr (E (List (_:xs))) = List xs
cdr (E (List [])) = List []
cdr _ = error "cdr applied to non list expression!"


-- (cons ’a ’()) ⇒ (a)
-- (cons ’(a) ’(b c d)) ⇒ ((a) b c d)
-- (cons "a" ’(b c)) ⇒ ("a" b c)
-- (cons ’a 3) ⇒ (a . 3)
-- (cons ’(a b) ’c) ⇒ ((a b) . c)

cons :: Value -> Value -> Expression
-- cons v1 v2
--   | trace ("-> cons v1:" ++ show v1 ++ ", v2:" ++ show v2) False = undefined
cons (E x) (E (List ys)) = List $ x:ys
cons (E x) (E e) = List [x,e]

list :: [Expression] -> Expression
list = List

nil :: Value -> Expression
nil (E (List [])) = Boolean True
nil _ = Boolean False

not :: Value -> Expression
not (E (Boolean False)) = Boolean True
not _ = Boolean False

evalBool :: Value -> Bool
-- evalBool v | trace ("evalBool " ++ show v) False = undefined
evalBool (E (Boolean b)) = b
evalBool e = error $ "evalBool " ++ show e

or :: [Value] -> Expression
or = Boolean . any evalBool

and :: [Value] -> Expression
and = Boolean . all evalBool

pair :: Value -> Expression
pair (E (List (x:_))) = Boolean True
pair _ = Boolean False
