{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
--
-- Stg to C-- code generation: the binding environment
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------
module GHC.StgToCmm.Env (
        CgIdInfo,

        litIdInfo, lneIdInfo, rhsIdInfo, mkRhsInit,
        idInfoToAmode,

        addBindC, addBindsC,

        bindArgsToRegs, bindToReg, rebindToReg,
        bindArgToReg, idToReg,
        getArgAmode, getNonVoidArgAmodes,
        getCgIdInfo,
        maybeLetNoEscape,
    ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Core.TyCon
import GHC.Platform
import GHC.StgToCmm.Monad
import GHC.StgToCmm.Utils
import GHC.StgToCmm.Closure

import GHC.Cmm.CLabel

import GHC.Cmm.BlockId
import GHC.Cmm.Expr
import GHC.Cmm.Utils
import GHC.Driver.Session
import GHC.Types.Id
import GHC.Cmm.Graph
import GHC.Types.Name
import Outputable
import GHC.Stg.Syntax
import GHC.Core.Type
import TysPrim
import GHC.Types.Unique.FM
import Util
import GHC.Types.Var.Env

-------------------------------------
--        Manipulating CgIdInfo
-------------------------------------

mkCgIdInfo :: Id -> LambdaFormInfo -> CmmExpr -> CgIdInfo
mkCgIdInfo id lf expr
  = CgIdInfo { cg_id = id, cg_lf = lf
             , cg_loc = CmmLoc expr }

litIdInfo :: DynFlags -> Id -> LambdaFormInfo -> CmmLit -> CgIdInfo
litIdInfo dflags id lf lit
  = pprTrace "litIdInfo" (ppr id) $ CgIdInfo { cg_id = id, cg_lf = lf
             , cg_loc = CmmLoc (addDynTag platform (CmmLit lit) tag) }
  where
    tag = lfDynTag dflags lf
    platform = targetPlatform dflags

lneIdInfo :: Platform -> Id -> [NonVoid Id] -> CgIdInfo
lneIdInfo platform id regs
  = CgIdInfo { cg_id = id, cg_lf = lf
             , cg_loc = LneLoc blk_id (map (idToReg platform) regs) }
  where
    lf     = mkLFLetNoEscape
    blk_id = mkBlockId (idUnique id)


rhsIdInfo :: Id -> LambdaFormInfo -> FCode (CgIdInfo, LocalReg)
rhsIdInfo id lf_info
  = pprTrace "rhsIdInfo" (ppr id) $
    do platform <- getPlatform
       reg <- newTemp (gcWord platform)
       return (mkCgIdInfo id lf_info (CmmReg (CmmLocal reg)), reg)

mkRhsInit :: DynFlags -> LocalReg -> LambdaFormInfo -> CmmExpr -> CmmAGraph
mkRhsInit dflags reg lf_info expr
  = mkAssign (CmmLocal reg) (addDynTag platform expr (lfDynTag dflags lf_info))
  where platform = targetPlatform dflags

idInfoToAmode :: CgIdInfo -> CmmExpr
-- Returns a CmmExpr for the *tagged* pointer
idInfoToAmode (CgIdInfo { cg_loc = CmmLoc e }) = e
idInfoToAmode cg_info
  = pprPanic "idInfoToAmode" (ppr (cg_id cg_info))        -- LneLoc

-- | A tag adds a byte offset to the pointer
addDynTag :: Platform -> CmmExpr -> DynTag -> CmmExpr
addDynTag platform expr tag = cmmOffsetB platform expr tag

maybeLetNoEscape :: CgIdInfo -> Maybe (BlockId, [LocalReg])
maybeLetNoEscape (CgIdInfo { cg_loc = LneLoc blk_id args}) = Just (blk_id, args)
maybeLetNoEscape _other                                      = Nothing



---------------------------------------------------------
--        The binding environment
--
-- There are three basic routines, for adding (addBindC),
-- modifying(modifyBindC) and looking up (getCgIdInfo) bindings.
---------------------------------------------------------

addBindC :: CgIdInfo -> FCode ()
addBindC stuff_to_bind = do
        binds <- getBinds
        setBinds $ extendVarEnv binds (cg_id stuff_to_bind) stuff_to_bind

addBindsC :: [CgIdInfo] -> FCode ()
addBindsC new_bindings = do
        binds <- getBinds
        let new_binds = foldl' (\ binds info -> extendVarEnv binds (cg_id info) info)
                               binds
                               new_bindings
        setBinds new_binds

getCgIdInfo :: Id -> FCode CgIdInfo
getCgIdInfo id
  = do  { dflags <- getDynFlags
        ; local_binds <- getBinds -- Try local bindings first
        ; case lookupVarEnv local_binds id of {
            Just info -> return info ;
            Nothing   -> do {

                -- Should be imported; make up a CgIdInfo for it
          let name = idName id
        ; if isExternalName name then
              let ext_lbl
                      | isUnliftedType (idType id) =
                          -- An unlifted external Id must refer to a top-level
                          -- string literal. See Note [Bytes label] in CLabel.
                          ASSERT( idType id `eqType` addrPrimTy )
                          mkBytesLabel name
                      | otherwise = mkClosureLabel name $ idCafInfo id
              in return $
                  litIdInfo dflags id (mkLFImported id) (CmmLabel ext_lbl)
          else
              cgLookupPanic id -- Bug
        }}}

cgLookupPanic :: Id -> FCode a
cgLookupPanic id
  = do  local_binds <- getBinds
        pprPanic "GHC.StgToCmm.Env: variable not found"
                (vcat [ppr id,
                text "local binds for:",
                pprUFM local_binds $ \infos ->
                  vcat [ ppr (cg_id info) | info <- infos ]
              ])


--------------------
getArgAmode :: NonVoid StgArg -> FCode CmmExpr
getArgAmode (NonVoid (StgVarArg var)) = idInfoToAmode <$> getCgIdInfo var
getArgAmode (NonVoid (StgLitArg lit)) = CmmLit <$> cgLit lit

getNonVoidArgAmodes :: [StgArg] -> FCode [CmmExpr]
-- NB: Filters out void args,
--     so the result list may be shorter than the argument list
getNonVoidArgAmodes [] = return []
getNonVoidArgAmodes (arg:args)
  | isVoidRep (argPrimRep arg) = getNonVoidArgAmodes args
  | otherwise = do { amode  <- getArgAmode (NonVoid arg)
                   ; amodes <- getNonVoidArgAmodes args
                   ; return ( amode : amodes ) }


------------------------------------------------------------------------
--        Interface functions for binding and re-binding names
------------------------------------------------------------------------

bindToReg :: HasCallStack => NonVoid Id -> LambdaFormInfo -> FCode LocalReg
-- Bind an Id to a fresh LocalReg
bindToReg nvid@(NonVoid id) lf_info
  = do platform <- getPlatform
       let reg = idToReg platform nvid
       pprTrace "bindToReg" (ppr id $$ ppr lf_info $$ callStackDoc)
         $ addBindC (mkCgIdInfo id lf_info (CmmReg (CmmLocal reg)))
       return reg

rebindToReg :: HasCallStack => NonVoid Id -> FCode LocalReg
-- Like bindToReg, but the Id is already in scope, so
-- get its LF info from the envt
rebindToReg nvid@(NonVoid id)
  = do  { info <- getCgIdInfo id
        ; bindToReg nvid (cg_lf info) }

bindArgToReg :: HasCallStack => NonVoid Id -> FCode LocalReg
bindArgToReg nvid@(NonVoid id) = bindToReg nvid (mkLFArgument id)

bindArgsToRegs :: HasCallStack => [NonVoid Id] -> FCode [LocalReg]
bindArgsToRegs args = mapM bindArgToReg args

idToReg :: Platform -> NonVoid Id -> LocalReg
-- Make a register from an Id, typically a function argument,
-- free variable, or case binder
--
-- We re-use the Unique from the Id to make it easier to see what is going on
--
-- By now the Ids should be uniquely named; else one would worry
-- about accidental collision
idToReg platform (NonVoid id)
             = LocalReg (idUnique id)
                        (primRepCmmType platform (idPrimRep id))
