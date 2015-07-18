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
kw name = dullblue (text name)

op :: String -> Doc
op name = dullyellow (text name)

eos :: Doc
eos = dullblack semi

letName :: Pretty a => a -> Doc
letName x = magenta (pretty x)

tuple :: [Doc] -> Doc
tuple xs  = dullblack (parens . hcat $ punctuate (comma <> space) xs)
