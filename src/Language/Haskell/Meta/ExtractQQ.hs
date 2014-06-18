module Language.Haskell.Meta.ExtractQQ where

import Data.Generics
import Language.Haskell.Exts
import Control.Monad.State
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Data.Map (Map)
import qualified Data.Map as M

uniqueStr = "fns6LODHO5Czm0KSz4Do"
uniqueVar ls = Ident (uniqueStr ++ show (length ls))


type QQResult = ([TH.Exp], [TH.Pat], [TH.Type], [TH.Dec])

initQQResult :: QQResult
initQQResult = ([],[],[],[])


extractQQ :: Data a => Map String QuasiQuoter -> a -> TH.Q (a, QQResult)
extractQQ qqs ast = runStateT (everywhereM quasiT =<< everywhereM decT ast) 
                        initQQResult
 where

  getQQ :: (MonadTrans t, Monad m)
      => (QuasiQuoter -> a -> m b)
      -> String
      -> a
      -> Maybe (t m b)
  getQQ extractQ q body = do
      quoter <- M.lookup q qqs
      Just (lift (extractQ quoter body))

  -- decT done first because of overlap with QuasiQuote
  quasiT, decT :: GenericM (StateT QQResult TH.Q)
  quasiT = mkM expT `extM` patT

  -- quasiquotes in types cannot be parsed yet:
  -- https://github.com/haskell-suite/haskell-src-exts/issues/117

  decT = mkM $ \ xs -> fmap concat $ mapM (\x -> case x of
      SpliceDecl loc (QuasiQuote q body)
        | Just runQQ <- getQQ quoteDec q body -> do
            bodies <- runQQ
            forM bodies $ \ body' -> do
              (e,p,t,d) <- get
              put (e,p,t,body' : d)
              return (PatBind loc
                            (PVar (uniqueVar d))
                            Nothing
                            (UnGuardedRhs
                              (Var (UnQual (uniqueVar d))))
                              (BDecls []))
      _ -> return [x])
    xs

  patT x = case x of
      PQuasiQuote q body
        | Just runQQ <- getQQ quotePat q body -> do
            body' <- runQQ
            (e,p,t,d) <- get
            put (e,body' : p,t,d)
            return (PVar (uniqueVar p))
      _ -> return x

  expT x = case x of
      QuasiQuote q body
        | Just runQQ <- getQQ quoteExp q body -> do
            body' <- runQQ
            (e,p,t,d) <- get
            put (body' : e, p, t, d)
            return (Var (UnQual (uniqueVar e)))
      _ -> return x

