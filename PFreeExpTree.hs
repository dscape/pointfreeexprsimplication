----------------------------------------------------------------------------------
--                                                                              --
--  Module: PFreeExpTree                                                        --
--                                                                              --
--  By: Nuno Pinto     ( nunojobpinto@gmail.com )                               --
--                                                                              --
--  This module defines a library to be used in the simplification of           --
--  point-free expression trees.                                                --
--                                                                              --
-----------------------------------------------------------------------(01/2006)--
module PFreeExpTree where

-- (01) imports ---------------------------------------------------------------~--
import Mpi
import ExpTree
import Control.Monad.State
import List ( nub )

-- (02) data ------------------------------------------------------------------~--
data PF      = Id     | Comp    |                     -- id and composition
               Prod   | Split   | Fst   | Snd    |    -- product
               CoProd | Either  | I1    | I2     |    -- co-product
               Expon  | Curry   | Ap    |             -- exponentials
               Cata   | In      | T     | B      |    -- catamorphism
               F      | G       |                     -- functorial
               McCart |                               -- mccarthy conditionals
               CompM  | Bind    | Seq                 -- monad
               deriving (Eq, Ord)

-- (03) type ------------------------------------------------------------------~--
type Subst v c     = [(v, Exp v c)]

-- (04) show ------------------------------------------------------------------~--
instance Show PF where
  show Comp    = "."
  show Id      = "id"
  show Prod    = "><"
  show Split   = "split"
  show Fst     = "fst"
  show Snd     = "snd"
  show CoProd  = "-|-"
  show Either  = "either"
  show I1      = "i1"
  show I2      = "i2"
  show Expon   = "exponential"
  show Curry   = "curry"
  show Ap      = "ap"
  show Cata    = "cata"
  show In      = "in"
  show T       = "T"
  show B       = "B"
  show F       = "F"
  show G       = "G"
  show CompM   = " o "
  show McCart  = "?"
  show Bind    = ">>="
  show Seq     = ">>"

instance (Show v, Show c) => Show (Exp v c) where 
  showsPrec p (Term c l) | (length l) == 2 = case (show c) of
                                                  "split"  -> showString $ "<" ++ (show $ l !! 0) ++ "," ++ (show $ l !! 1) ++ ">"
                                                  "either" -> showString $ "[" ++ (show $ l !! 0) ++ "," ++ (show $ l !! 1) ++ "]"
                                                  _        -> showString $ "(" ++ (show $ l !! 0) ++ (show c) ++ (show $ l !! 1) ++ ")"
                         | (length l) == 1 = case (show c) of
                                                  "curry" -> showString $ "_" ++ (show $ l !! 0) ++ "_"
                                                  "cata"  -> showString $ "(|" ++ (show $ l !! 0) ++ "|)"
                                                  "?"     -> showString $ "(" ++ (show $ l !! 0) ++ ")" ++ (show c)
                                                  _       -> showString $ (show c) ++ " (" ++ (show $ l !! 0) ++ ")"
                         | otherwise = showString $ show c
  showsPrec p (Var v) = showString $ (\(_:a:_) -> [a]) (show v)

-- (05) const -----------------------------------------------------------------~--
start :: [Int]
start = [1]

lawNames :: [(Int, String)]
lawNames = [ ( 1, "natural-id")
            ,( 2, "associatividade")
-- composicao
            ,( 3, "universal-x")
            ,( 4, "cancelamento-x")
            ,( 5, "reflexao-x")
            ,( 6, "fusao-x")
            ,( 7, "absorcao-x")
            ,( 8, "functor-x")
            ,( 9, "functor-id-x")
-- produto
            ,(10, "universal-+")
            ,(11, "cancelamento-+")
            ,(12, "reflexao-+")
            ,(13, "fusao-+")
            ,(14, "absorcao-+")
            ,(15, "functor-+")
            ,(16, "functor-id-+")
-- coproduto
            ,(17, "universal")
            ,(18, "cancelamento")
            ,(19, "reflexao")
            ,(20, "fusao")
            ,(21, "absorcao")
            ,(22, "functor")
            ,(23, "functor-id")
-- exponenciacao
            ,(24, "universal-cata")
            ,(25, "cancelamento-cata")
            ,(26, "reflexao-cata")
            ,(27, "fusao-cata")
            ,(28, "absorcao-cata")
-- inducao
            ,(29, "functor-F")
            ,(30, "functor-id-F")
            ,(31, "teorema gratis de g")
-- functores
            ,(32, "lei da troca")
            ,(33, "fusao de predicado guardado")
            ,(34, "1a lei de fusao do condicional")
            ,(35, "2a lei de fusao do condicional")
-- misc
            ,(36, "multiplicacao")
            ,(37, "unidade")
            ,(38, "composicao monadica")
            ,(39, "associatividade-m")
            ,(40, "identidade-m")
            ,(41, "associatividade-m/.")
            ,(42, "associatividade-./m")
            ,(43, "u versus m")
            ,(44, "binding")
            ,(45, "sequenciacao")
-- monadicas
           ]

-- composicao
-- natural-id (1)
natural_id1 :: (Int, (Exp Char PF , Exp Char PF))
natural_id1 = (1, (Term Comp [Var 'f', Term Id []],
                   Var 'f'))

natural_id2 :: (Int, (Exp Char PF , Exp Char PF))
natural_id2 = (1, (Term Comp [Term Id [], Var 'f'],
                   Var 'f'))

-- associatividade (2)
associatividade1 :: (Int, (Exp Char PF , Exp Char PF))
associatividade1 = (2, (Term Comp [Term Comp [Var 'f', Var 'g'], Var 'h'],
                        Term Comp [Var 'f', Term Comp [Var 'g', Var 'h']]))

associatividade2 :: (Int, (Exp Char PF , Exp Char PF))
associatividade2 = (2, (Term Comp [Var 'f', Term Comp [Var 'g', Var 'h']],
                        Term Comp [Term Comp [Var 'f', Var 'g'], Var 'h']))

-- produto
-- cancelamento-x (4)
cancelamento_x1 :: (Int, (Exp Char PF , Exp Char PF))
cancelamento_x1 = (4, (Term Comp [Term Fst [], Term Split [Var 'f', Var 'g']],
                       Var 'f'))

cancelamento_x2 :: (Int, (Exp Char PF , Exp Char PF))
cancelamento_x2 = (4, (Term Comp [Term Snd [], Term Split [Var 'f', Var 'g']],
                       Var 'g'))

-- fusao-x (6)
fusao_x1 :: (Int, (Exp Char PF , Exp Char PF))
fusao_x1 = (6, (Term Comp [Term Split [Var 'g',Var 'h'],Var 'f'],
                Term Split [Term Comp [Var 'g',Var 'f'],Term Comp [Var 'h',Var 'f']]))

fusao_x2 :: (Int, (Exp Char PF , Exp Char PF))
fusao_x2 = (6, (Term Split [Term Comp [Var 'g',Var 'f'],Term Comp [Var 'h',Var 'f']],
                Term Comp [Term Split [Var 'g',Var 'h'],Var 'f']))

-- absorcao-x (7)
absorcao_x1 :: (Int, (Exp Char PF , Exp Char PF))
absorcao_x1 = (7, (Term Comp [Term Prod [Var 'i', Var 'j'], Term Split [Var 'g', Var 'h']],
                   Term Split [Term Comp [Var 'i', Var 'g'], Term Comp [Var 'j', Var 'h']]))

absorcao_x2 :: (Int, (Exp Char PF , Exp Char PF))
absorcao_x2 = (7, (Term Split [Term Comp [Var 'i', Var 'g'], Term Comp [Var 'j', Var 'h']],
                   Term Comp [Term Prod [Var 'i', Var 'j'], Term Split [Var 'g', Var 'h']]))

-- functor-x (8)
functor_x1 :: (Int, (Exp Char PF , Exp Char PF))
functor_x1 = (8, (Term Prod [Term Comp [Var 'g', Var 'h'], Term Comp [Var 'i', Var 'j']],
                  Term Comp [Term Prod [Var 'g', Var 'i'], Term Prod [Var 'h', Var 'j']]))

functor_x2 :: (Int, (Exp Char PF , Exp Char PF))
functor_x2 = (8, (Term Comp [Term Prod [Var 'g', Var 'i'], Term Prod [Var 'h', Var 'j']],
                  Term Prod [Term Comp [Var 'g', Var 'h'], Term Comp [Var 'i', Var 'j']]))

laws :: [(Int, (Exp Char PF , Exp Char PF))]
laws = [natural_id1,natural_id2,associatividade1,associatividade2,cancelamento_x1,cancelamento_x2,fusao_x1,fusao_x2,absorcao_x1,absorcao_x2,functor_x1,functor_x2]

-- (06) functions -------------------------------------------------------------~--
aridade pf = case pf of
                  Id     -> 0
                  Fst    -> 0
                  Snd    -> 0
                  I1     -> 0
                  I2     -> 0
                  Ap     -> 0
                  In     -> 0
                  Expon  -> 1
                  Curry  -> 1
                  Cata   -> 1
                  T      -> 1
                  B      -> 1
                  F      -> 1
                  G      -> 1
                  McCart -> 1
                  _      -> 2

---

valid = cataExpTree $ either (\x -> (&&) (uncurry (==) (split (length . snd) (aridade . fst) x)) ((and . snd) x)) (const True)

vars :: (Eq v) => Exp v c -> [v]
vars = nub . (cataExpTree $ either (concat . snd) (:[]))

pfs :: Exp Char PF -> [PF]
pfs = cataExpTree $ either (\(c,l) -> c:[] ++ (concat l)) (const [])

extensao :: Exp Char PF -> Int
extensao = length . pfs

---

bestSolution :: [Exp Char PF] -> Maybe (Exp Char PF)
bestSolution []   = Nothing
bestSolution exps = Just (exps !! ((\(Just x) -> x) (lookup (minimum (map (\ex -> (extensao ex)) exps)) (zip (map (\ex -> (extensao ex)) exps) [0..]))))

---

doTrans condicao f x ns = if (condicao x) then (f x ns) else error "invalid transformation."

---

validToSub = (all (valid . snd)) . (flip toSub start)

toSub (Term c l) ns = if (null l) then [(ns, Term c [])]
                                  else [(ns, Term c l)] ++ auxSub l (ns ++ start)
toSub (Var v) ns    = [(ns, Var v)]

auxSub [] _ = []
auxSub ((Term c l):xs) ns = [(ns,Term c l)] ++ (auxSub l (ns ++ start)) ++ (auxSub xs (order ns))
auxSub ((Var v):xs) ns    = [(ns, Var v)] ++ (auxSub xs (order ns))

order []     = []
order (l:[]) = [l+1]
order (l:ls) = l : order ls

---

validFromSub = valid . (flip fromSub start)

fromSub l i = make l i (fromSubAux l i)

fromSubAux ((i,ex):xs) pos | (pos == i) = ex
                           | otherwise  = fromSub xs pos

make l pos (Term c _) = Term c (map (\(a,b) -> fromSub l (a++[b])) (zip (repeat pos) [1..(aridade c)]))
make l pos (Var v)    = Var v

---

replace :: Exp Char PF -> ([Int], Exp Char PF) -> [([Int], Exp Char PF)]
replace pt (pos,ex) = replaceAux (toSub pt start) (toSub ex pos)

replaceAux l [] = l
replaceAux l ((pos2,exp2):xs) = replaceAux  ((map (\(pos,exp) -> if pos == pos2 then (pos,exp2) else (pos,exp)) l) ++ [(pos2,exp2)]) xs

---

match :: (Eq v, Eq c) => Exp v c -> Exp v c -> Maybe (Subst v c)
match l r | null (vars r) = execStateT (aux (l,r)) []
   where aux (Var v, e) = do s <- get
                             z <- lift (update s v e)
                             put z
         aux (Term c l, Term d m) = do unless (c == d) $ fail "Expressions dont match."
                                       sequence_ $ map aux (zip l m)
match _ _ = fail "The second expression cannot contain any variables."

---

update :: (Eq v, Eq c) => Subst v c -> v -> Exp v c -> Maybe (Subst v c)
update listasub c ex = if ((notElem) c (map (fst) listasub)) then Just ((c,ex) : listasub)
                                                             else Nothing

---

reescreve law ex | (all (\x -> x `elem` (vars (fst law))) (vars (snd law))) = doSubs (doSubsList law ex) (snd law) ex
                 | otherwise = []

doSubs listsubs law2 originalexp = filter (valid) $ map (\(i, Just sub) -> doSubsAc law2 (transSub i sub originalexp)) listsubs

transSub i subs originalexp = map (\(ch,ex) -> (ch, fromSub (replace originalexp (i,ex)) start)) subs

doSubsList (l1,l2) ex = filter (\(a,b) -> b /= Nothing) $ map (\(fs,sn) -> if (match l1 sn /= Nothing) then (fs,(match l1 sn)) else (fs,Nothing))$ toSub ex start

doSubsAc law2 [] = law2
doSubsAc law2 (sub:xs) = doSubsAc (doLaw law2 sub) xs

doLaw l2 sub = fromSub (replace l2 (start, doLawAux l2 sub)) start

doLawAux (Var v) (ch,pf)    = if (v == ch) then pf else (Var v)
doLawAux (Term c l) (ch,pf) = Term c (map (flip doLawAux (ch,pf)) l)

{-- to do:

      1 -> adicionar novos PF's.
      2 -> adicionar regras.
      3 -> modificar show.
      4 -> traduzir nomes leis.
      5 -> modificar os numeros de leis de forma a que cada lei seja unica.

--}