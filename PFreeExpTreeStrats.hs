----------------------------------------------------------------------------------
--                                                                              --
--  Module: PFreeExpTreeStrats                                                  --
--                                                                              --
--  By: Nuno Pinto     ( nunojobpinto@gmail.com )                               --
--                                                                              --
-- strat definition:                                                            --
-- strat :: [(Int, (Exp Char PF, Exp Char PF))]             -- law list         --
--          -> Exp Char PF                                  -- the expression   --
--          -> Maybe (Exp Char PF, [Int])                   -- maybe a result   --
--                                                                              --
--  Strategies can and should be redefined in order to achieve better results.  --
--                                                                              --
-----------------------------------------------------------------------(01/2006)--
module PFreeExpTreeStrats where

-- (01) imports ---------------------------------------------------------------~--
import Mpi
import ExpTree
import PFreeExpTree
import Control.Monad.State
import List

-- (02) functions -------------------------------------------------------------~--
-- (*1) strat -----------------------------------------------------------------~--
--
-- unfolding the tree to its maximum size, then making the choice.
strat1 :: [(Int, (Exp Char PF, Exp Char PF))]
           -> Exp Char PF
           -> Maybe (Exp Char PF, [Int])

strat1 leis ex = do  a <- evalStateT f1 (leis,ex,[])
                     if null (snd a)
                        then Nothing
                        else Just a

-- where
f1 :: StateT ([(Int, (Exp Char PF, Exp Char PF))],
              Exp Char PF,
              [(Exp Char PF, [Int])])
              Maybe (Exp Char PF, [Int])

f1 = do  sols <- getSolutions
         laws <- getLaws
         ex   <- getExp
         noth <- return (Var 'v', [])
         if (null sols)
            then do  put (laws,ex,(allPossible laws ex []))
                     sols <- getSolutions
                     if (null sols)
                        then return noth
                        else f1
            else do  sols  <- getSolutions
                     sols_ <- return $ filter (\(e,i) -> 3*(extensao ex) > (extensao e) ) $ allAux sols laws
                     if (sols /= sols_)
                        then do put(laws,ex,sols_)
                                f1
                        else do  sol <- return $ head $ sortBy (\(_,ia) (_,ib) -> compare (length ia) (length ib)) $ filter (\(ex,_) -> ex == fromJust(bestSolution (map fst sols))) sols
                                 if (extensao ex > extensao (fst sol))
                                    then return sol
                                    else return noth

--
allPossible :: [(Int, (Exp Char PF, Exp Char PF))]
               -> Exp Char PF
               -> [Int]
               -> [(Exp Char PF, [Int])]

allPossible laws ex n = do  applaws <- return $ (filter (\(_,law) -> reescreve law ex /= []) laws)
                            if (null applaws)
                               then []
                               else norep . breakExps $ map (\(i,lei) -> (reescreve lei ex, n ++ [i])) applaws

--
allAux :: [(Exp Char PF, [Int])]
           -> [(Int, (Exp Char PF, Exp Char PF))]
           -> [(Exp Char PF, [Int])]

allAux [] _               = []
allAux a@((ex,i):xs) laws = norep $ a ++ allPossible laws ex i ++ allAux xs laws

-- (*2) strat -----------------------------------------------------------------~--
--

-- (*)  aux -------------------------------------------------------------------~--
getLaws      = gets $ \(a,_,_) -> a
getExp       = gets $ \(_,a,_) -> a
getSolutions = gets $ \(_,_,a) -> a

--
breakExps [] = []
breakExps ((l,i):xs) | (length l == 1) = (head l,i) : breakExps xs
                     | otherwise       = (head l,i) : breakExps ((tail l,i):xs)

norep = (nubBy  (\(_,na) (_,nb) -> na == nb))
        . (sortBy (\(exa,_) (exb,_) -> compare (extensao exa) (extensao exb)))
        . (nubBy  (\(exa,_) (exb,_) -> exa == exb))
        . (sortBy (\(_,na) (_, nb) -> compare (length na) (length nb)))

--
outMaybe = map (fromJust) . filter isJust

isJust (Just x)  = True
isJust (Nothing) = False

fromJust (Just a) = a