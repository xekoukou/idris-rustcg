module Definitions 

-- | Names are hierarchies of strings, describing scope (so no danger of
-- duplicate names, but need to be careful on lookup).
data Name = UN String -- ^ User-provided name
          | NS Name (List String) -- ^ Root, namespaces
          | MN Int String -- ^ Machine chosen names
          | SN String -- ^ Decorated function names TODO SpecialName
          | SymRef Int -- ^ Reference to IBC file symbol table (used during serialisation)

data CaseType = Updatable | Shared

data LVar = Loc Int | Glob Name

mutual

  -- ASSUMPTION: All variable bindings have unique names here
  data LExp = LV LVar
            | LApp Bool LExp (List LExp) -- True = tail call
            | LLazyApp Name (List LExp) -- True = tail call
            | LLazyExp LExp
            | LForce LExp -- make sure Exp is evaluted
            | LLet Name LExp LExp -- name just for pretty printing
            | LLam (List Name) LExp -- lambda, lifted out before compiling
            | LProj LExp Int -- projection
            | LCon (Maybe LVar) -- Location to reallocate, if available
                   Int Name (List LExp)
            | LCase CaseType LExp (List LAlt)
            | LConst Const
            | LForeign FDesc -- Function descriptor (usually name as string)
                       FDesc -- Return type descriptor
                       (List (FDesc, LExp)) -- first LExp is the FFI type description
            | LOp PrimFn (List LExp)
            | LNothing
            | LError String

  data FDesc = FCon Name
             | FStr String
             | FUnknown
             | FIO FDesc
             | FApp Name (List FDesc)

  -- Primitive operators. Backends are not *required* to implement all
  -- of these, but should report an error if they are unable
  
  data PrimFn = LPlus ArithTy | LMinus ArithTy | LTimes ArithTy
              | LUDiv IntTy | LSDiv ArithTy | LURem IntTy | LSRem ArithTy
              | LAnd IntTy | LOr IntTy | LXOr IntTy | LCompl IntTy
              | LSHL IntTy | LLSHR IntTy | LASHR IntTy
              | LEq ArithTy | LLt IntTy | LLe IntTy | LGt IntTy | LGe IntTy
              | LSLt ArithTy | LSLe ArithTy | LSGt ArithTy | LSGe ArithTy
              | LSExt IntTy IntTy | LZExt IntTy IntTy | LTrunc IntTy IntTy
              | LStrConcat | LStrLt | LStrEq | LStrLen
              | LIntFloat IntTy | LFloatInt IntTy | LIntStr IntTy | LStrInt IntTy
              | LFloatStr | LStrFloat | LChInt IntTy | LIntCh IntTy
              | LBitCast ArithTy ArithTy -- Only for values of equal width
  
              | LFExp | LFLog | LFSin | LFCos | LFTan | LFASin | LFACos | LFATan
              | LFSqrt | LFFloor | LFCeil | LFNegate
  
              | LStrHead | LStrTail | LStrCons | LStrIndex | LStrRev | LStrSubstr
              | LReadStr | LWriteStr
  
              -- system info
              | LSystemInfo
  
              | LFork
              | LPar -- evaluate argument anywhere, possibly on another
                     -- core or another machine. 'id' is a valid implementation
              | LExternal Name
              | LNoOp

-- FIXME: Why not use this for all the IRs now?
  data LAlt = LConCase Int Name (List Name) LExp
               | LConstCase Const LExp
               | LDefaultCase LExp


  data LOpt = Inline | NoInline

data LDecl = LFun (List LOpt) Name (List Name) LExp -- options, name, arg names, def
           | LConstructor Name Int Int -- constructor name, tag, arity

