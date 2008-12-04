----------------------------------------------------------------------------------
--                                                                              --
--  Module: PFreeExpTreePrint                                                   --
--                                                                              --
--  By: Nuno Job     ( nunojobpinto@gmail.com )                                 --
--                                                                              --
-----------------------------------------------------------------------(01/2006)--
module PFreeExpTreePrint where

-- (01) imports ---------------------------------------------------------------~--
import HtmlTree
import ExpTree
import PFreeExpTree
import List

-- (02) functions -------------------------------------------------------------~--
-- (*1) prompt ----------------------------------------------------------------~--
showSimplification :: Exp Char PF -> (Exp Char PF, [Int]) -> IO ()
showSimplification originalexp steps  = putStrLn "\n"

-- (*2) html ------------------------------------------------------------------~--
makeHtml :: Exp Char PF -> (Exp Char PF, [Int]) -> IO ()
makeHtml originalexp steps  = putStrLn "\n"

-- (*3) aux -------------------------------------------------------------------~--
putLawName :: Int -> Char -> String
putLawName i c = (concat $ map (\(n,nome) -> if (n == i) then (nome) else []) lawNames)  ++ " " ++
                 (if (c == 'a') then " -> " else " <- ") ++ 
                 (fill (length (concat $ map (\(n,nome) -> if (n == i) then (nome) else []) lawNames)) 15 ' ' (-)) ++ "\t(" ++ 
                 (show i) ++ ")"

fill :: Int -> Int -> Char -> (Int -> Int -> Int) -> String
fill chars ac c f |(ac == chars) = ""
                  | otherwise    = [c] ++ (fill chars (f ac 1) c f)

printlaws :: [(Int, Char, (Exp Char PF, Exp Char PF))] -> String
printlaws leis = concat $ nub $ map (\(i,c,_) -> (putLawName i c) ++ " ->\t" ++ "" ++ "\n") leis

{-- to do:

    1 -> imprimir em html (falta de clareza na prompt).
    2 -> completar a showSimplification de forma a:
          ser inteligivel na prompt
          ser facil de implementar em html
    3 -> code que le de um ficheiro txt e transforma a informacao numa
         PFreeExpTree.
         exemplo:
         <a,(id-|-(fst><curry (id.snd)))>
    4 -> completar a showHtml

--}