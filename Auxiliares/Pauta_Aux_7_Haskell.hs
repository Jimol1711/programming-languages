--ghc 8.0.2

type Identifier = String
type Value = Int

type Env = [(Identifier, Value)]

data WAE = Num Int
         | Add WAE WAE
         | Id Identifier
         | With Identifier WAE WAE

envlookup :: Identifier -> Env -> Value
envlookup var ((i,v):r)
    | (var == i) = v
    | otherwise = envlookup var r

extend :: Identifier -> Value -> Env -> Env
extend i v env = (i,v):env

interp :: WAE -> Env -> Value
interp (Num n) env = n
interp (Add l r) env =  (interp l env) + (interp r env)
interp (Id x) env = envlookup x env
interp (With x named_expr body) env = interp body (extend x (interp named_expr env) env)  

main = do 
    print (interp (Add (Num 3) (Num 5)) [])
    print (interp (With "x" (Add (Num 3) (Num 5)) (Add (Id "x") (Id "x"))) [])
    print (interp (With "x" (Num 3) (With "y" (Add (Num 1) (Id "x")) (Add (Id "x") (Id "y")))) [])