module Language.Haskell.Meta.SubstQQ where

import Language.Haskell.Meta.ExtractQQ
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Generics

import qualified Data.IntMap as M
import Data.IntMap (IntMap)
import Text.Read

import Data.List


lookupN :: Name -> IntMap e -> Maybe e
lookupN (Name (OccName s) NameS) m
  | Just n <- readMaybe =<< stripPrefix uniqueStr s = M.lookup n m
lookupN _ _ = Nothing

expUpdate :: QQResult -> Exp -> Exp
expUpdate (es, _, _, _) =
    let m :: IntMap Exp
        m = M.fromList $ zip [0 .. ] (reverse es)
    in \x -> case x of
               VarE n | Just e' <- lookupN n m -> e'
               _ -> x

patUpdate :: QQResult -> Pat -> Pat
patUpdate (_, ps, _, _) =
    let m :: IntMap Pat
        m = M.fromList $ zip [0 .. ] (reverse ps)
    in \x -> case x of
               VarP n | Just e' <- lookupN n m -> e'
               _ -> x

decUpdate :: QQResult -> Dec -> Dec
decUpdate (_, _, _, ds) =
    let m :: IntMap Dec
        m = M.fromList $ zip [0 .. ] (reverse ds)
  in \x -> case x of
             ValD (VarP n) _ _ | Just e' <- lookupN n m -> e'
             _ -> x


substQQ :: Data a => QQResult -> a -> a
substQQ qqr =  everywhere (mkT (patUpdate qqr) `extT` expUpdate qqr)
            .  everywhere (mkT (decUpdate qqr))
-- two traversals because the dec overlaps with the others
