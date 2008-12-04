----------------------------------------------------------------------------------
--                                                                              --
--  Module: ExpTree                                                             --
--                                                                              --
--  By: Nuno Pinto     ( nunojobpinto@gmail.com )                               --
--                                                                              --
--  This module defines a library of expression trees.                          --
--                                                                              --
-----------------------------------------------------------------------(01/2006)--
module ExpTree where

-- (01) imports ---------------------------------------------------------------~--
import Mpi

-- (02) data ------------------------------------------------------------------~--
data Exp v c = Term c [Exp v c] | Var v deriving (Eq)

inExpTree             = either (uncurry Term) $ Var

outExpTree (Var v)    = Right v
outExpTree (Term c l) = Left (c, l)

-- (03) functions -------------------------------------------------------------~--
cataExpTree g         = g . (recExpTree (map (cataExpTree g))) . outExpTree

anaExpTree g          = inExpTree . (recExpTree (map (anaExpTree g))) . g

hyloExpTree g h       = (cataExpTree g) . (anaExpTree h)

baseExpTree g f       = g >< f -|- id

recExpTree f          = baseExpTree id f

{-- instance Functor Exp
             where fmap f = 

    to do:

    1 -> fmap

--}