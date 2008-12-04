----------------------------------------------------------------------------------
--                                                                              --
--  Module: HtmlTree                                                            --
--                                                                              --
--  By: Nuno Pinto     ( nunojobpinto@gmail.com )                               --
--                                                                              --
--  This module defines a library of HTML syntax trees.                         --
--                                                                              --
-----------------------------------------------------------------------(01/2006)--
module HtmlTree where

-- (01) imports ---------------------------------------------------------------~--
import Mpi

-- (02) data ------------------------------------------------------------------~--
data HtmlTree = T String [(String,String)] [HtmlTree] | S String      deriving(Eq)

inHtmlTree = either (uncurry3 T) $ S
                     where uncurry3 f (a,b,c) = f a b c

outHtmlTree (T s l htm) =  Left (s,l,htm)
outHtmlTree (S s)       =  Right s

-- (03) show ------------------------------------------------------------------~--
instance Show HtmlTree where 
  showsPrec p (S s) = showString $ s
  showsPrec p (T s l l2) = showString $ "\n<" ++ s ++ showAux l ++ ">" ++ (concatMap show l2) ++ "\n</" ++ s ++ ">"
            where  showAux l  = concatMap (\(a,b) -> " " ++ a ++ "=\"" ++ b ++ "\"" ) l

-- (04) functions -------------------------------------------------------------~--
cataHtmlTree f     = f . (recHtmlTree (map (cataHtmlTree f))) . outHtmlTree

anaHtmlTree g      = inHtmlTree . (recHtmlTree (map (anaHtmlTree g))) . g

hyloHtmlTree g h   = (cataHtmlTree g) . (anaHtmlTree h)

baseHtmlTree g f   = (coProd3 id g f) -|- id

recHtmlTree f      = baseHtmlTree id f 

-- where 
coProd3 a b c = split3 (a . fst3) (b . snd3) (c . thd3)

split3 f g h x = (f x, g x, h x)

fst3 (a,_,_) = a
snd3 (_,a,_) = a
thd3 (_,_,a) = a

{-- to-do:

    1 -> fmap



  instance Functor HtmlTree
           where fmap f =

   exemplos (apagar)

htmsample :: HtmlTree
htmsample = T "HTML" [] [T "HEAD" [] [T "TITLE" [] [S "teste"]], T "BODY" [("BGCOLOR","#F4EFD8")] [S "Texto do body"]]

strings = cataHtmlTree $ either (\(a,b,c) -> concat c) (\s -> s:[])

tags    = cataHtmlTree $ either (\(a,b,c) -> a:[] ++ concat c) (const [])
--}