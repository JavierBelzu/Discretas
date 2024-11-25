data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)

data Formula = Atom Var
               |Neg Formula
               |Formula :&: Formula
               |Formula :|: Formula
               |Formula :=>: Formula
               |Formula :<=>: Formula deriving (Show, Eq, Ord)

infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:

-------------------- EJERCICIO 1 --------------------
variables :: Formula -> [Var]
variables (Atom a) = [a]
variables (Neg f) = conjunto(variables f)
variables (f1 :&: f2) = conjunto(variables f1 ++ variables f2)
variables (f1 :|: f2) = conjunto(variables f1 ++ variables f2)
variables (f1 :=>: f2) = conjunto(variables f1 ++ variables f2)
variables (f1 :<=>: f2) = conjunto(variables f1 ++ variables f2)


-----------------------------------------------------

---Conjunto
conjunto :: [Var] -> [Var]
conjunto [] = []
conjunto (x:xs) = if estaContenido xs x then conjunto xs
                  else x:conjunto xs


estaContenido :: [Var] -> Var -> Bool
estaContenido [] _ = False
estaContenido (x:xs) y = if x == y then True 
                        else estaContenido xs y


-------------------- EJERCICIO 2 --------------------
negacion :: Formula -> Formula
negacion (Atom a) = Neg (Atom a)
negacion (Neg f) = f
negacion (f1 :&: f2) = (negacion f1) :|: (negacion f2)
negacion (f1 :|: f2) = (negacion f1) :&: (negacion f2)
negacion (f1 :=>: f2) = f1 :&: (negacion f2)
negacion (f1 :<=>: f2) = (f1 :&: (negacion f2)) :|: ((negacion f1) :&: f2)

-----------------------------------------------------

-------------------- EJERCICIO 3 --------------------
equivalencia :: Formula -> Formula
equivalencia (Atom a) = Atom a
equivalencia (Neg f) = Neg (equivalencia f)
equivalencia (f1 :&: f2) = (equivalencia f1) :&: (equivalencia f2)
equivalencia (f1 :|: f2) = (equivalencia f1) :|: (equivalencia f2)
equivalencia (f1 :=>: f2) = (equivalencia (Neg f1)) :|: (equivalencia f2)
equivalencia (f1 :<=>: f2) = ((equivalencia f1) :&: (equivalencia f2)) :|: ((equivalencia (Neg f1)) :&: (equivalencia (Neg f2)))

-----------------------------------------------------

-------------------- EJERCICIO 4 --------------------
lookupVar :: Var -> [(Var, Bool)] -> Bool
lookupVar v [] = error "No todas las variables están definidas"
lookupVar v ((x, val):xs) = if v == x then val 
                                    else lookupVar v xs

interpretacion :: Formula -> [(Var, Bool)] -> Bool
interpretacion (Atom v) vals = lookupVar v vals
interpretacion (Neg f) vals = not (interpretacion f vals)
interpretacion (f1 :&: f2) vals = (interpretacion f1 vals) && (interpretacion f2 vals)
interpretacion (f1 :|: f2) vals = (interpretacion f1 vals) || (interpretacion f2 vals)
interpretacion (f1 :=>: f2) vals = not (interpretacion f1 vals) || (interpretacion f2 vals)
interpretacion (f1 :<=>: f2) vals = (interpretacion f1 vals) == (interpretacion f2 vals)

-----------------------------------------------------


-------------------- EJERCICIO 5 --------------------
combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones f = aux2 (aux (variables f))
-----------------------------------------------------

-------------------- EJERCICIO 6 --------------------

tablaDeVerdad :: Formula -> [([(Var,Bool)],Bool)]
tablaDeVerdad f = [(x, interpretacion f x) | x <- combinaciones f]
-----------------------------------------------------


--Funciones auxiliares
aux :: [Var] -> [[(Var, Bool)]]
aux [] = [[]]
aux (x:xs) = [(x, False):ys | ys <- aux xs] ++ [(x, True):ys | ys <- aux xs]

aux2 :: [[(Var, Bool)]] -> [[(Var, Bool)]]
aux2 [] = []
aux2 (x:xs) = x : aux2 xs





