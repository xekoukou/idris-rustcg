module IRTS.CodegenRust(codegenRust) where

import IRTS.CodegenCommon
import IRTS.Lang
import Idris.Core.TT
import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace


-- This is the main function

codegenRust :: CodeGenerator
codegenRust ci = do putStrLn $ codegenRust' $ Map.fromList (liftDecls ci)

codegenRust' :: Map Name LDecl -> String
codegenRust' m = let nm = (sMN 0 "runMain") 
                 in let Just decl = Map.lookup nm m 
		    in let (LFun _ _ _ fn) = decl
                       in  concat (map genLDecl $ Map.toList $ eraseVarFun $ Map.fromList $ (nm,decl) : (fst $ findLDecl fn m Set.empty))



-- This section provides a method to print the LIR so as to inspect it.

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
genLExp t (LForce e1) = "LForce " ++ "\n" ++ tab t ++ genLExp (t+1) e1
genLExp t (LLet n e1 e2) = "LLet " ++ show n ++ " " ++ "\n" ++ tab t ++ genLExp (t+1) e1 ++ " " ++ "\n" ++ tab t ++ genLExp (t+1) e2
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

-- This sections removes any unnecessary declarations LDecl

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
-- findLDecl (LV (Loc n)) m q = ([],q) 
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
findLDecl (LForce lexp) m q       = findLDecl lexp m q
findLDecl (LLet _ lexp1 lexp2) m q = let res1 = findLDecl lexp1 m q
                                     in let res2 = findLDecl lexp2 m (snd res1)
				     in (fst res1 ++ fst res2, snd res2)
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

-- This section removes the LNothing arguments. TODO It needs to be fixed. It cannot clean functions that are passed as arguments. ??

filterLN :: LExp -> [LExp] -> ([LExp], [Int])
filterLN arg x = filterLN' 0 arg x where
  filterLN' p arg (y:ys) = let (nle, q) = filterLN' (p+1) arg ys 
                       in case (y==arg) of
                           True   ->   (nle, p:q)
                           False  -> (y:nle, q)
  filterLN' p arg [] = ([],[])

removeArg :: LExp -> LExp -> (LExp, Map Name [Int])
removeArg arg (LApp j1 vr lexps) = let (nlexps,nq) = foldl (\(lle, lq) (le,q) -> (lle ++ [le],  Map.unions [lq, q])) ([],Map.empty) (map (removeArg arg) lexps)
                                        in case (vr) of
                                             LV (Glob n) -> let (rm, li) = filterLN arg nlexps
                                                            in ((LApp j1 vr rm),Map.unions [nq, case (li) of
                                                                                                     [] -> Map.empty 
                                                                                                     _  -> Map.insert n li Map.empty])
                                             _           -> ((LApp j1 vr nlexps),nq)
removeArg arg (LLazyApp n lexps) = let (nlexps,nq) = foldl (\(lle, lq) (le,q) -> (lle ++ [le],  Map.unions [lq, q])) ([],Map.empty) (map (removeArg arg) lexps)
                                    in let (rm, li) = filterLN arg nlexps
                                       in ((LLazyApp n rm),Map.unions [nq, case (li) of
                                                                            [] -> Map.empty 
                                                                            _  -> Map.insert n li Map.empty])
removeArg arg (LForce lexp)       = let (le,q) =  removeArg arg lexp
                                     in ((LForce le), q)
removeArg arg (LLet j1 lexp1 lexp2) = let (le1,q1) =  removeArg arg lexp1
                                       in let (le2,q2) =  removeArg arg lexp2
                                          in ((LLet j1 le1 le2), Map.unions [q1,q2])
removeArg arg (LProj lexp j1)      = let (le,q) =  removeArg arg lexp
                                      in ((LProj le j1), q)
removeArg arg (LCon j1 j2 n lexps)  = let (nlexps,nq) = foldl (\(lle, lq) (le,q) -> (lle ++ [le],  Map.unions [lq, q])) ([],Map.empty) (map (removeArg arg) lexps)
                                       in let (rm, li) = filterLN arg nlexps
                                          in ((LCon j1 j2 n rm),Map.unions [nq, case (li) of
                                                                                  [] -> Map.empty 
                                                                                  _  -> Map.insert n li Map.empty])
removeArg arg (LCase j1 lexp lalts) = let (nlalts,nq) = foldl (\(lle, lq) (le,q) -> (lle ++ [le],  Map.unions [lq, q])) ([],Map.empty) (map (\x -> case (x) of 
                                                                                                                                             LDefaultCase lexp        -> let (le,q) = removeArg arg lexp
                                                                                                                                                                         in (LDefaultCase le,q)
                                                                                                                                             LConstCase j2 lexp    -> let (le,q) = removeArg arg lexp
                                                                                                                                                                      in (LConstCase j2 le,q)
                                             	                                                                                             LConCase j3 j4 j5 lexp    -> let (le,q) = removeArg arg lexp
                                                                                                                                                                          in (LConCase j3 j4 j5 le,q)) lalts)
                                       in let (le,q) =  removeArg arg lexp
                                          in ((LCase j1 le nlalts), Map.unions [q,nq])
removeArg arg (LOp j1 lexps)  = let (nlexps,nq) = foldl (\(lle, lq) (le,q) -> (lle ++ [le],  Map.unions [lq, q])) ([],Map.empty) (map (removeArg arg) lexps)
                                 in ((LOp j1 nlexps),nq)
removeArg arg (LForeign fd1 fd2 fds) = let (nfds,nq) = foldl (\(lle, lq) (le,q) -> (lle ++ [le],  Map.unions [lq, q])) ([],Map.empty) (map (\x -> let (le,q) = removeArg arg (snd x) 
                                                                                                                                          in ((fst x,le),q)) fds)
                                        in ((LForeign fd1 fd2 nfds),nq)
removeArg arg le = (le,Map.empty)

cleanFun :: [(Name, [Int])] -> Map Name LDecl -> ([(Name,[Name])],Map Name LDecl)
cleanFun ((n, li):xs) m = let lp = Map.lookup n m
                          in case (lp) of
                              Just (LFun j1 j2 nms j4)  ->     let (rnms,nnms) = apFilter li nms
                                                               in let ldec = ((LFun j1 j2 nnms j4))
                                                                  in let (lrnms,nm) = cleanFun xs (Map.insert n ldec m) 
                                                                     in ((n,rnms):lrnms,nm)  where
                                                                                               apFilter li nms = apFilter' 0 li nms where
                                                                                                        apFilter' p (i:ls) (n:ns) = let (rnms,nnms) = apFilter' (p+1) ls ns
                                                                                                                                    in case (i==p) of
                                                                                                                                         True -> (n:rnms,nnms)
                                                                                                                                         False -> (rnms,n:nnms)
                                                                                                        apFilter' p [] ns = ([],ns) 
                                                                                                        apFilter' p li [] = ([],[])  -- This is only needed because runMain has an LNothing but main does not have a variable.
                              Just (LConstructor _ a t )   -> cleanFun xs (Map.insert n (LConstructor n (a - length li) t) m)
                              _    -> trace (show n) ([],m)
cleanFun [] m = ([],m) 


eraseVarFun :: Map Name LDecl -> Map Name LDecl
eraseVarFun m = let (nm, q) = foldl (\(nm,nq) (n,(ldec,q)) -> (Map.insert n ldec nm, Map.unions [nq,q])) (Map.empty, Map.empty) (Map.toList (Map.map (\ldec -> case (ldec) of
                                                                                                                                                                LFun j1 j2 j3 le -> let (nle,q) = removeArg LNothing le
                                                                                                                                                                                    in (LFun j1 j2 j3 nle, q)
                                                                                                                                                                o                -> (o, Map.empty)                   )  m))   
                in let (ner,nnm) = cleanFun (Map.toList q) nm
                   in eraseVarFun' nnm ner where
                         eraseVarFun' :: Map Name LDecl -> [(Name,[Name])] -> Map Name LDecl
                         eraseVarFun' m ((n,rnms):xs) = let Just ldec = Map.lookup n m
                                                        in let (nldec, lnq) = foldl (\(ldec,pq) rt -> case (ldec) of  
                                                                                                        LFun j1 j2 j3 le -> let (nle,q) = removeArg (LV (Glob rt)) le
                                                                                                                            in (LFun j1 j2 j3 nle, pq ++ [q])
                                                                                                        o                ->  (o, pq)                                   ) (ldec,[]) rnms
                                                           in let (nner, nnnm) =foldl (\(ner,nm) nq -> let (ner',nm') = cleanFun (Map.toList nq) nm
                                                                                                       in  (ner ++ ner',nm')                         ) ([],(Map.insert n nldec m)) lnq
                                                              in eraseVarFun' nnnm (nner ++ xs)
                         eraseVarFun' m []         = m

                                                             

---------------------------------------------------------

-- It creates the tree of dependencies of variables and finds their type.

newtype UniqueId = UnId Int

ui_inc :: UniqueId -> UniqueId
ui_inc (UnId x) = UnId (x+1)

-- CaseR UniqueId is used so as to get the result of the Case application, mostly for the cases that it is a LDefaultCase or LConstCase 
data OperInfo = Con UniqueId Name Int Int | SApp UniqueId Name Int | LzApp UniqueId Name Int | OLet UniqueId Name | CaseCon UniqueId Name Int [Name] | PrimOp UniqueId PrimFn Int | OLForce UniqueId

data VarRel = Leaf (OperInfo, Const) | Edge (OperInfo, Name) | EdgeR (OperInfo, UniqueId) | CaseR UniqueId

data FunCall = Fun Name [VarRel] | FunCase Name UniqueId [[VarRel]]


findVarel :: UniqueId -> LExp -> (([VarRel],UniqueId),[LExp])
findVarel un (LApp j1 vr lexps) = case (vr) of
                               LV (Glob n) -> fst $ foldl (\(((ns,unl),rls),p) lexp -> case (lexp) of
                                                                            LV (Glob nl) -> (((ns ++ [Edge (SApp un n p,nl)],unl),rls), p+1)
                                                                            LConst c     -> (((ns ++ [Leaf (SApp un n p,c)], unl),rls), p+1)
                                                                            LNothing     -> (((ns,unl),rls),p+1)
                                                                            _            -> let ((res,nun),nrls) = findVarel unl lexp    -- The order of execution here is important for EdgeR (un+1)
                                                                                            in (((ns ++ [EdgeR (SApp un n p,ui_inc unl )] ++ res,nun),nrls ++ rls), p+1)   ) ((([],ui_inc un),[]),0) lexps
                               _           -> (([],un),[])  -- ?
findVarel un (LLazyApp n lexps) = fst $ foldl (\(((ns,unl),rls),p) lexp -> case (lexp) of
                                                                            LV (Glob nl) -> (((ns ++ [Edge (LzApp un n p,nl)],unl),rls), p+1)
                                                                            LConst c     -> (((ns ++ [Leaf (LzApp un n p,c)], unl),rls), p+1)
                                                                            LNothing     -> (((ns,unl),rls),p+1)
                                                                            _            -> let ((res,nun),nrls) = findVarel unl lexp
                                                                                            in (((ns ++ [EdgeR (LzApp un n p,ui_inc unl)] ++ res,nun),rls ++ nrls), p+1)   ) ((([],ui_inc un),[]),0) lexps
findVarel un (LForce lexp) = case (lexp) of
                                 LV (Glob nl) -> (([Edge (OLForce un,nl)],ui_inc un),[])
                                 LConst c     -> (([Leaf (OLForce un,c)], ui_inc un),[])
                                 _            -> let ((res,nun),nrls) = findVarel (ui_inc un) lexp 
                                                 in (( EdgeR (OLForce un, ui_inc un) : res,nun),nrls)
findVarel un (LLet j1 lexp1 lexp2)  = let ((r1,nun1),rls1) = case (lexp1) of
                                                               LV (Glob nl) -> (([Edge (OLet un j1,nl)],ui_inc un),[])
                                                               LConst c     -> (([Leaf (OLet un j1,c)], ui_inc un),[])
                                                               _            -> let ((res,nun),nrls) = findVarel (ui_inc un) lexp1 
                                                                               in (( EdgeR (OLet un j1, ui_inc un) : res,nun),nrls)
                                      in let ((r2,nun2),rls2) =  findVarel nun1 lexp2
                                         in ((r1 ++ r2,nun2),rls1 ++ rls2)
findVarel un (LProj lexp j1)               = findVarel un lexp   -- What is Projection? probably lexp is a constructor.
findVarel un (LCon j1 tag n lexps)  = fst $ foldl (\(((ns,unl),rls),p) lexp -> case (lexp) of
                                                                        LV (Glob nl) -> (((ns ++ [Edge (Con un n tag p,nl)],unl),rls),p+1)
                                                                        LConst c     -> (((ns ++ [Leaf (Con un n tag p,c)], unl),rls),p+1)
                                                                        LNothing     -> (((ns,unl),rls),p+1)
                                                                        _            -> let ((res,nun),nrls) = findVarel unl lexp 
                                                                                        in (((ns ++ [EdgeR (Con nun n tag p, ui_inc unl)] ++ res,nun),nrls ++ rls),p+1)   ) ((([],ui_inc un),[]),0) lexps
findVarel un (LCase j1 lexp lalts) = let ((r',nun'),rls') = findVarel (ui_inc un) lexp
                                     in let ((r,nun),rls) = ((r' ++ [CaseR un] ,nun'),rls' ++ [LCase j1 lexp lalts])
                                        in foldl (\((ns,unl),rls) x -> case (x) of 
                                                                     LDefaultCase _        -> ((ns,unl),rls)
                                                                     LConstCase _ _        -> ((ns,unl),rls)
                                        	                     LConCase tag nm args clexp  -> ((ns ++ [EdgeR (CaseCon un nm tag args,ui_inc un)],unl),rls) ) ((r,nun),rls) lalts
findVarel un (LOp j1 lexps) = fst $ foldl (\(((ns,unl),rls),p) lexp -> case (lexp) of
                                                                          LV (Glob nl) -> (((ns ++ [Edge (PrimOp un j1 p,nl)],unl),rls), p+1)
                                                                          LConst c     -> (((ns ++ [Leaf (PrimOp un j1 p,c)], unl),rls), p+1)
                                                                          _            -> let ((res,nun),nrls) = findVarel unl lexp -- The order of execution here is important for EdgeR (un+1)
                                                                                          in (((ns ++ [EdgeR (PrimOp un j1 p,ui_inc unl )] ++ res,nun),nrls ++ rls), p+1)   ) ((([],ui_inc un),[]),0) lexps
findVarel un (LForeign fd1 fd2 fds) = foldl (\((ns,unl),rls) x -> let ((res,nun),nrls) = findVarel unl (snd x) 
                                                                   in ((ns ++ res,nun),nrls ++ rls)  ) (([],un),[]) fds 
findVarel un _ = (([],un),[])


---------------------------

findFunCalls :: Name -> LExp -> [FunCall]
findFunCalls n z = findFunCalls' n (UnId 1) z where
  findFunCalls' n un (LCase j1 _ lalts) = let ((vrs,nun),nlexps) = foldl (\((vrs,unl),rls) x -> case (x) of 
                                                                     LDefaultCase clexp        -> let ((res,nun),nrls) = findVarel unl clexp
                                                                                                  in ((res : vrs ,nun),rls ++ nrls)
                                                                     LConstCase cnst clexp    -> let ((res,nun),nrls) = findVarel unl clexp
                                                                                              in ((res : vrs,nun),rls ++ nrls)
                                        	                     LConCase tag nm args clexp  -> let ((res,nun),nrls) = findVarel unl clexp 
                                                                                                    in ((res : vrs,nun),rls ++ nrls) ) (([],ui_inc un),[]) lalts
                                          in foldl (\fcs lexp -> fcs ++ (findFunCalls' n nun lexp) ) [FunCase n un vrs] nlexps
  findFunCalls' n un lexp =  let ((vr,nun),nlexps) = findVarel un lexp
                             in foldl (\fcs lexp -> fcs ++ (findFunCalls' n nun lexp) ) [Fun n vr] nlexps



findAllFunCalls :: Map Name LDecl -> [FunCall]
findAllFunCalls  m = Map.foldlWithKey (\fcs n ldecl -> case (ldecl) of
                                        LFun j1 j2 j3 lexp -> fcs ++ (findFunCalls n lexp)
                                        _                  -> fcs                         ) [] m


--------

positionToNames :: Map Name LDecl -> Map Name (Map Int Name)
positionToNames m = Map.foldlWithKey (\nm k ldecl -> case (ldecl) of 
        LFun _ _ args _  -> Map.insert k ( fst $ foldl (\(nm,p) n ->  (Map.insert p n nm, p + 1)) (Map.empty,0) args ) nm 
        _                -> nm ) Map.empty m



data FNode = FCon Name Int Int | FFun Name Name



