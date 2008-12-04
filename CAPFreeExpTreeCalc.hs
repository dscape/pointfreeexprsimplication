----------------------------------------------------------------------------------
--                                                                              --
--  Module: CAPFreeExpTreeCalc                                                  --
--                                                                              --
--  By: Nuno Pinto     ( nunojobpinto@gmail.com )                               --
--                                                                              --
--  This defines a computer-aided point-free expression trees simplification    --
--  module.                                                                     --
--                                                                              --
-----------------------------------------------------------------------(01/2006)--
module CAPFreeExpTreeCalc where

-- (01) imports ---------------------------------------------------------------~--
import ExpTree
import PFreeExpTree
import PFreeExpTreeStrats
import PFreeExpTreePrint

-- (02) calculator ------------------------------------------------------------~--
calc :: [([(Int, (Exp Char PF, Exp Char PF))] -> Exp Char PF -> Maybe (Exp Char PF, [Int]))] -> [(Int, (Exp Char PF, Exp Char PF))] -> Exp Char PF -> IO ()
calc fs laws ex = do  if (valid ex)
                         then do  solutions <- return . outMaybe $ map (\f -> f laws ex) fs
                                  bestsol   <- return . bestSolution $ map fst solutions
                                  results   <- return . outMaybe $ map (\sol -> if (isJust(bestsol) && (fst sol) == fromJust (bestsol)) then (Just sol) else Nothing) solutions
                                  if (results /= [])
                                     then do  lsizes   <- return $ map (length . snd) results
                                              cardinal <- return . (fromJust) . lookup (minimum lsizes) $ zip (lsizes) [0..]
                                              showSimplification ex (results !! cardinal)
                                              makeHtml ex (results !! cardinal)
                                     else return ()
                         else return()