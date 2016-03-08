module IRTS.CodegenRust(codegenRust) where

import IRTS.CodegenCommon
import IRTS.Lang
import Idris.Core.TT
import Data.Char (ord)

codegenRust :: CodeGenerator
codegenRust ci = do putStrLn $ codegenRust' (liftDecls ci)

asciiHex :: String -> String
asciiHex x = "n" ++ x

asciiHex' :: String -> String
asciiHex' [] = ""
asciiHex' (x:xs) = convertChar x ++ asciiHex' xs where 
  convertChar :: Char -> String
  convertChar '.' = ".n"
  convertChar x = show $ ord x


tab :: Int -> String
tab n = concat $ replicate (n*4) " "

addSpace :: String -> String -> String
addSpace a b = a ++ " " ++ b

addNewLine :: Int -> String -> String -> String
addNewLine n a b = a ++ "\n" ++ tab n ++ b

showFirst :: (Name, LDecl) -> String
showFirst x = (asciiHex $ show $ fst x)

codegenRust' :: [(Name, LDecl)] -> String
codegenRust' x = concat (map genLDecl x)


genLDecl :: (Name, LDecl) -> String
genLDecl (n, LConstructor n2 tag arity) = "LDecl " ++ show n ++ " LConstructor " ++ show tag ++" " ++ show arity ++ "\n"
genLDecl (n, LFun opts n2 args e) = "LDecl " ++ show n ++ " LFun " ++"[" ++ (foldr addSpace "" $ map show opts) ++ "]" ++  " [" ++ (foldr addSpace "" $ map show args) ++ "] " ++ "\n" ++ tab 1 ++ genLExp 2 e ++ "\n"

genConst :: Const -> String
genConst (I x) = "I " ++ show x
genConst (BI x) = "BI " ++ show x
genConst (Fl x) = "Fl " ++ show x
genConst (Ch x) = "Ch " ++ show x
genConst (Str x) = "Str " ++ show x
genConst (B8 x) = "B8 " ++ show x
genConst (B16 x) = "B16 " ++ show x
genConst (B32 x) = "B32 " ++ show x
genConst (B64 x) = "B64 " ++ show x
genConst a = show a

genAlt :: Int -> LAlt -> String
genAlt t (LConCase i n args e) = "LConCase " ++ show i ++ " " ++ show n ++ " [" ++ (foldr addSpace "" $ map show args) ++ "] " ++ "\n" ++ tab t ++ genLExp (t+1) e
genAlt t (LConstCase const e) = "LConstCase " ++ genConst const ++ " " ++ "\n" ++ tab t ++ genLExp (t+1) e
genAlt t (LDefaultCase e) = "LDefaultCase " ++ "\n" ++ tab t ++ genLExp (t+1) e

genLExp :: Int -> LExp -> String
genLExp t (LV v) = "LV " ++ show v
genLExp t (LApp b e1 e2) = "LApp " ++ show b ++ " " ++ "\n" ++ tab t ++ genLExp (t+1) e1 ++ " " ++ "[ \n" ++ tab t ++  (foldr (addNewLine t) "" $ map (genLExp (t+1)) e2) ++ " ]"
genLExp t (LLazyApp n e1) = "LLazyApp " ++ show n ++ " " ++ "[ \n" ++ tab t ++ (foldr (addNewLine t) "" $ map (genLExp (t+1)) e1) ++ " ]"
genLExp t (LLazyExp e1) = "LLazyExp " ++ "\n" ++ tab t ++ genLExp (t+1) e1
genLExp t (LForce e1) = "LForce " ++ "\n" ++ tab t ++ genLExp (t+1) e1
genLExp t (LLet n e1 e2) = "LLet " ++ show n ++ " " ++ "\n" ++ tab t ++ genLExp (t+1) e1 ++ " " ++ "\n" ++ tab t ++ genLExp (t+1) e2
genLExp t (LLam args e1 ) = "LLam " ++ " [ " ++ (foldr addSpace "" $ map show args) ++ " ] " ++ "\n" ++ tab t ++ genLExp (t+1) e1
genLExp t (LProj e1 pr) = "LProj " ++ "\n" ++ tab t ++ genLExp (t+1) e1 ++ " " ++ show pr
genLExp t (LCon Nothing i n e) = "LCon " ++ "(Rel: Nothing )" ++ show i ++ " " ++ show n ++ " " ++ "[ \n" ++ tab t ++ (foldr (addNewLine t) "" $ map (genLExp (t+1)) e) ++ " ]"
genLExp t (LCon (Just a) i n e) = "Lcon " ++ "(Rel: Just " ++ show a ++ ") " ++ show i ++ " " ++ show n ++ " " ++ "[ " ++ (foldr addSpace "" $ map (genLExp (t+1)) e) ++ " ]"
genLExp t (LCase c e alt) = "LCase " ++ show c ++ " " ++ "\n" ++ tab t ++ genLExp (t+1) e ++ " " ++ "[ \n" ++ tab t ++ (foldr (addNewLine t) "" $ map (genAlt t) alt) ++ " ]"
genLExp t (LConst c) = "LConst " ++ genConst c
genLExp t (LForeign fd rd e) = "LForeign " ++ show fd ++ " " ++ show rd ++ "[ \n" ++ tab t ++ (foldr (addNewLine t) "" $ map fo e) ++ " ]" where
  fo :: (FDesc,LExp) -> String
  fo (fd, e) = show fd ++ " " ++ "\n" ++ tab t ++ genLExp (t+1) e
genLExp t (LOp pr e) = "LOp " ++ show pr ++ " " ++ "[ \n" ++ tab t ++ (foldr (addNewLine t) "" $ map (genLExp (t+1)) e) ++ " ]"
genLExp t LNothing = "LNothing"
genLExp t (LError str) = "LError " ++ "'" ++ str ++ "'"

