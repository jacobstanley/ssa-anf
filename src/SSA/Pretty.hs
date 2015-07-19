module SSA.Pretty (printProgram) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Prelude hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

import           SSA.Syntax
import           Util.Pretty

------------------------------------------------------------------------

printProgram :: Pretty n => Program n -> IO ()
printProgram prog = putDoc (ppProgram prog) >> putStrLn ""

------------------------------------------------------------------------

ppAtom :: Pretty n => Atom n -> Doc
ppAtom (Var   n) = var n
ppAtom (Const x) = num x

ppAtoms :: Pretty n => [Atom n] -> Doc
ppAtoms = tuple . map ppAtom

ppLabel :: Pretty n => Label n -> Doc
ppLabel Start     = var ("start" :: String)
ppLabel (Label n) = var n

ppPhiArg :: Pretty n => Label n -> Atom n -> Doc
ppPhiArg l x = ppLabel l <+> op ":" <+> ppAtom x

ppPhiArgs :: Pretty n => Map (Label n) (Atom n) -> Doc
ppPhiArgs = tuple . map (uncurry ppPhiArg) . Map.toList

ppBlock :: Pretty n => Block n -> Doc
ppBlock block = case block of
    Phi     n   gs b -> letName n <+> op "←" <+> op "φ" <> ppPhiArgs gs <> eos <$> ppBlock b
    Copy    n   x  b -> letName n <+> op "←" <+> ppAtom x               <> eos <$> ppBlock b
    Call    n f xs b -> letName n <+> op "←" <+> ppAtom f <> ppAtoms xs <> eos <$> ppBlock b
    Goto    n        -> kw "goto" <+> var n                  <> eos
    RetCopy     x    -> kw "ret"  <+> ppAtom x               <> eos
    RetCall   f xs   -> kw "ret"  <+> ppAtom f <> ppAtoms xs <> eos
    If      i t e    -> kw "if" <+> ppAtom i <+> kw "then" <$> text "  " <> align (ppBlock t)
                                             <$> kw "else" <$> text "  " <> align (ppBlock e)

ppBlocks :: Pretty n => Blocks n -> Doc
ppBlocks blocks = case blocks of
    BlockStart b          -> text "  " <> align (ppBlock b)
    BlockLabel bs (n, b)  -> ppBlocks bs <$> letName n <> op ":"
                                         <$> text "  " <> align (ppBlock b)
    BlockScope b1 (n, b2) -> ppBlocks b1 <$> letName n <> op ":" <+> op "{"
                                         <$> text "  " <> align (ppBlocks b2)
                                         <$> op "}"

ppProgram :: Pretty n => Program n -> Doc
ppProgram prog = case prog of
    Entry     b    -> ppBlock b
    Proc n ns bs p -> kw "proc" <+> letName n <> tuple (map name ns) <+> op "{"
                                <$> ppBlocks bs
                                <$> op "}"
                                <$> empty
                                <$> ppProgram p
