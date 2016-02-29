module IRTS.CodegenRust(codegenRust) where

import IRTS.CodegenCommon
import IRTS.Lang
import Idris.Core.TT
import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- codegenRust :: CodeGenerator
-- codegenRust ci = do putStrLn $ codegenRust' (liftDecls ci)
-- 
-- codegenRust' :: [(Name, LDecl)] -> String
-- codegenRust' x = concat (map genLDecl x)
-- 
tab :: Int -> String
tab n = concat $ replicate (n*4) " "

addSpace :: String -> String -> String
addSpace a b = a ++ " " ++ b

addNewLine :: Int -> String -> String -> String
addNewLine n a b = a ++ "\n" ++ tab n ++ b



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

-------------------------------------------------------

codegenRust :: CodeGenerator
codegenRust ci = do putStrLn $ codegenRust' $ Map.fromList (liftDecls ci)

codegenRust' :: Map Name LDecl -> String
codegenRust' m = let nm = (sMN 0 "runMain") 
                 in let Just decl = Map.lookup nm m 
		    in let (LFun _ _ _ fn) = decl
                       in  concat (map genLDecl $ Map.toList $ eraseVarFun $ Map.fromList $ (nm,decl) : (fst $ findLDecl fn m Set.empty))




findMain :: Map Name LDecl -> ((Name,LDecl),Name)
findMain m = let mf = Map.lookup (sMN 0 "runMain") m
             in case mf of
                  Just f -> let LFun _ _ args (LForce (LApp _ (LV (Glob n)) _)) = f
		            in ((sMN 0 "runMain",f), n)


amap :: (a -> Set Name -> ([b],Set Name)) -> [a] -> Set Name -> ([b],Set Name)
amap f (y : ys) q = let (r,nq) = f y q
                    in let res = amap f ys  nq
                       in (r++ fst res, snd res)
amap f [] q = ([],q)

findLDecl :: LExp -> Map Name LDecl -> Set Name -> ([(Name,LDecl)], Set Name)
findLDecl (LV (Glob n)) m q =  let e = Set.member n q
	                       in case (e) of
			           False -> let mldec = Map.lookup n m
                                            in case (mldec) of
				                 Just ldec -> let (LFun _ _ _ fn) = ldec
				                              in let res1 = findLDecl fn m (Set.insert n q)
							         in ([(n,ldec)] ++ fst res1 , snd res1)
			                         Nothing   -> ([],q)
			           True -> ([],q)
findLDecl (LV (Loc n)) m q = ([],q) 
findLDecl (LApp _ vr lexps) m q= case (vr) of 
                                            (LV (Glob n)) -> let e = Set.member n q
					                     in case (e) of
							          False -> let mldec = Map.lookup n m
                                                                            in case (mldec) of
				                                                 Just ldec -> let (LFun _ _ _ fn) = ldec
										              in let res1 = findLDecl fn m (Set.insert n q)
											      in let res2 = amap (\x q -> findLDecl x m q) lexps (snd res1)
								                                 in ([(n,ldec)] ++ fst res1 ++ fst res2, snd res2)
			                                                         Nothing   -> amap (\x q -> findLDecl x m q) lexps q
			                                          True -> amap (\x q -> findLDecl x m q) lexps q
					    _             -> let res1 = findLDecl vr m q
							     in let res2 = amap (\x q -> findLDecl x m q) lexps (snd res1)
                                                             in (fst res1 ++ fst res2, snd res2)

findLDecl (LLazyApp n lexps) m q = let e = Set.member n q 
                                 in case (e) of 
				    False -> let mldec = Map.lookup n m
                                            in case (mldec) of
		  	                         Just ldec -> let (LFun _ _ _ fn) = ldec
							      in let res1 = findLDecl fn m (Set.insert n q)
							      in let res2 = amap (\x q -> findLDecl x m q) lexps (snd res1)
							         in ([(n,ldec)] ++ fst res1 ++ fst res2, snd res2)
			                         Nothing              -> amap (\x q -> findLDecl x m q) lexps q
				    True  -> amap (\x q -> findLDecl x m q) lexps q
findLDecl (LLazyExp lexp) m q       = findLDecl lexp m q
findLDecl (LForce lexp) m q       = findLDecl lexp m q
findLDecl (LLet _ lexp1 lexp2) m q = let res1 = findLDecl lexp1 m q
                                     in let res2 = findLDecl lexp2 m (snd res1)
				     in (fst res1 ++ fst res2, snd res2)
findLDecl (LLam _ lexp) m q       = findLDecl lexp m q
findLDecl (LProj lexp _) m q      = findLDecl lexp m q
findLDecl (LCon _ _ n lexps) m q  = let e = Set.member n q
                                in case (e) of
				     False -> let mldec = Map.lookup n m
                                             in case (mldec) of
				                  Just ldec -> let res1 = amap (\x q -> findLDecl x m q) lexps (Set.insert n q)
							       in ([(n,ldec)] ++ fst res1 , snd res1)
			                          Nothing   -> amap (\x q -> findLDecl x m q) lexps q
				     True  -> amap (\x q -> findLDecl x m q) lexps q
findLDecl (LCase _ lexp lalts) m q = let res1 = findLDecl lexp m q
                                     in let res2 = amap (\lalt q -> case (lalt) of
                                                                      LDefaultCase lexp        -> findLDecl lexp m q
                                                                      LConstCase _ lexp    -> findLDecl lexp m q
						         	      LConCase _ _ _ lexp      -> findLDecl lexp m q  ) lalts (snd res1)
				        in (fst res1 ++ fst res2, snd res2)
findLDecl (LOp _ lexps) m q  = amap (\x q -> findLDecl x m q) lexps q
findLDecl (LForeign fd1 fd2 fds) m q = amap (\x q -> findLDecl x m q) (map snd fds) q
findLDecl LNothing _ q            = ([],q)
findLDecl _ _ q                   = ([],q)



-------------------------------------------------------

filterLN :: [LExp] -> ([LExp], [Int])
filterLN x = filterLN' 0 x where
  filterLN' p (y:ys) = let (nle, q) = filterLN' (p+1) ys 
                       in case (y) of
                         LNothing -> (nle, p:q)
                         _        -> (y:nle, q)
  filterLN' p [] = ([],[])

removeLNothing :: LExp -> (LExp, Map Name [Int])
removeLNothing (LApp j1 vr lexps) = let (nlexps,nq) = foldl (\(lle, lq) (le,q) -> (lle ++ [le],  Map.unions [lq, q])) ([],Map.empty) (map removeLNothing lexps)
                                        in case (vr) of
                                             LV (Glob n) -> let (rm, li) = filterLN nlexps
                                                            in ((LApp j1 vr rm),Map.unions [nq, case (li) of
                                                                                                     [] -> Map.empty 
                                                                                                     _  -> Map.insert n li Map.empty])
                                             _           -> ((LApp j1 vr nlexps),nq)
removeLNothing (LLazyApp n lexps) = let (nlexps,nq) = foldl (\(lle, lq) (le,q) -> (lle ++ [le],  Map.unions [lq, q])) ([],Map.empty) (map removeLNothing lexps)
                                    in let (rm, li) = filterLN nlexps
                                       in ((LLazyApp n rm),Map.unions [nq, case (li) of
                                                                            [] -> Map.empty 
                                                                            _  -> Map.insert n li Map.empty])
removeLNothing (LLazyExp lexp)       = let (le,q) =  removeLNothing lexp
                                       in ((LLazyExp le), q)
removeLNothing (LForce lexp)       = let (le,q) =  removeLNothing lexp
                                     in ((LForce le), q)
removeLNothing (LLet j1 lexp1 lexp2) = let (le1,q1) =  removeLNothing lexp1
                                       in let (le2,q2) =  removeLNothing lexp2
                                          in ((LLet j1 le1 le2), Map.unions [q1,q2])
removeLNothing (LLam j1 lexp)      = let (le,q) =  removeLNothing lexp
                                     in ((LLam j1 lexp), q)
removeLNothing (LProj lexp j1)      = let (le,q) =  removeLNothing lexp
                                      in ((LProj le j1), q)
removeLNothing (LCon j1 j2 n lexps)  = let (nlexps,nq) = foldl (\(lle, lq) (le,q) -> (lle ++ [le],  Map.unions [lq, q])) ([],Map.empty) (map removeLNothing lexps)
                                       in let (rm, li) = filterLN nlexps
                                          in ((LCon j1 j2 n rm),Map.unions [nq, case (li) of
                                                                                  [] -> Map.empty 
                                                                                  _  -> Map.insert n li Map.empty])
removeLNothing (LCase j1 lexp lalts) = let (nlalts,nq) = foldl (\(lle, lq) (le,q) -> (lle ++ [le],  Map.unions [lq, q])) ([],Map.empty) (map (\x -> case (x) of 
                                                                                                                                             LDefaultCase lexp        -> let (le,q) = removeLNothing lexp
                                                                                                                                                                         in (LDefaultCase le,q)
                                                                                                                                             LConstCase j2 lexp    -> let (le,q) = removeLNothing lexp
                                                                                                                                                                      in (LConstCase j2 le,q)
                                             	                                                                                             LConCase j3 j4 j5 lexp    -> let (le,q) = removeLNothing lexp
                                                                                                                                                                          in (LConCase j3 j4 j5 le,q)) lalts)
                                       in let (le,q) =  removeLNothing lexp
                                          in ((LCase j1 le nlalts), Map.unions [q,nq])
removeLNothing (LOp j1 lexps)  = let (nlexps,nq) = foldl (\(lle, lq) (le,q) -> (lle ++ [le],  Map.unions [lq, q])) ([],Map.empty) (map removeLNothing lexps)
                                 in ((LOp j1 nlexps),nq)
removeLNothing (LForeign fd1 fd2 fds) = let (nfds,nq) = foldl (\(lle, lq) (le,q) -> (lle ++ [le],  Map.unions [lq, q])) ([],Map.empty) (map (\x -> let (le,q) = removeLNothing (snd x) 
                                                                                                                                          in ((fst x,le),q)) fds)
                                        in ((LForeign fd1 fd2 nfds),nq)
removeLNothing le = (le,Map.empty)

cleanFun :: [(Name, [Int])] -> Map Name LDecl -> Map Name LDecl
cleanFun ((n, li):xs) m = let Just (LFun j1 j2 nms j4) = Map.lookup n m
                          in cleanFun xs (Map.insert n ((LFun j1 j2 (apFilter li nms) j4)) m ) where
                                                               apFilter li nms = apFilter' 0 li nms where
                                                                         apFilter' p (i:ls) (n:ns) = let nnms = apFilter' (p+1) ls ns
                                                                                                     in case (i==p) of
                                                                                                          True -> nnms
                                                                                                          False -> (n:nnms)
                                                                         apFilter' p [] ns = ns 
                                                                         apFilter' p li [] = []  -- why this is needed is beyond me.
cleanFun [] m = m 


eraseVarFun :: Map Name LDecl -> Map Name LDecl
eraseVarFun m = let (nm, q) = foldl (\(nm,nq) (n,(ldec,q)) -> (Map.insert n ldec nm, Map.unions [nq,q])) (Map.empty, Map.empty) (Map.toList (Map.map (\ldec -> case (ldec) of
                                                                                                                                                                LFun j1 j2 j3 le -> let (nle,q) = removeLNothing le
                                                                                                                                                                                    in (LFun j1 j2 j3 nle, q)
                                                                                                                                                                o                -> (o, Map.empty)                   )  m))   
                in cleanFun (Map.toList q) nm
