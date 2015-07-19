module ANF.Pretty (printExpr) where

import Prelude hiding ((<$>))
import Text.PrettyPrint.ANSI.Leijen

import ANF.Syntax
import Util.Pretty

------------------------------------------------------------------------

printExpr :: Pretty n => Expr n -> IO ()
printExpr expr = putDoc (ppExpr expr) >> putStrLn ""

------------------------------------------------------------------------

ppAtom :: Pretty n => Atom n -> Doc
ppAtom (Var   n) = var n
ppAtom (Const x) = num x

ppAtoms :: Pretty n => [Atom n] -> Doc
ppAtoms = tuple . map ppAtom

ppTail :: Pretty n => Tail n -> Doc
ppTail expr = case expr of
    Copy   x  -> ppAtom x
    Call f xs -> ppAtom f <+> ppAtoms xs

ppBinding :: Pretty n => Binding n -> Doc
ppBinding (Bind n ns e) = letName n <+> tuple (map name ns) <+> op "=" <$> text "  " <> align (ppExpr e)

ppBindings :: Pretty n => [Binding n] -> Doc
ppBindings [] = tuple []
ppBindings bs = vcat (map ppBinding bs)

ppExpr :: Pretty n => Expr n -> Doc
ppExpr expr = case expr of
    Return   tl    -> ppTail tl
    Let    n tl e  -> kw "let" <+> letName n <+> op "=" <+> ppTail tl <$> ppExpr e
    LetRec   bs e  -> kw "letrec" <+> ppBindings bs <$> kw "in" <$> text "  " <> align (ppExpr e)
    If     i th el -> kw "if" <+> ppAtom i <+> kw "then" <$> text "  " <> align (ppExpr th)
                                           <$> kw "else" <$> text "  " <> align (ppExpr el)
