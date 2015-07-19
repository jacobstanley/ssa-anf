module Util.Pretty where

import Text.PrettyPrint.ANSI.Leijen

------------------------------------------------------------------------

name :: Pretty n => n -> Doc
name x = white (pretty x)

num :: Pretty n => n -> Doc
num x = dullred (pretty x)

var :: Pretty a => a -> Doc
var x = cyan (pretty x)

kw :: String -> Doc
kw xs = dullblue (text xs)

op :: String -> Doc
op xs = dullyellow (text xs)

eos :: Doc
eos = dullblack semi

letName :: Pretty a => a -> Doc
letName x = magenta (pretty x)

tuple :: [Doc] -> Doc
tuple xs  = dullblack (parens . hcat $ punctuate (comma <> space) xs)
