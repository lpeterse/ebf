{-# LANGUAGE TemplateHaskell #-}
module Data.Ebf.Derive where

import Data.Word
import Data.Monoid
import Control.Monad
import Control.Applicative
import Language.Haskell.TH hiding (Kind(..))

import Blaze.ByteString.Builder.Word
import Data.Iteratee.Binary
import qualified Data.Iteratee.ListLike as I

import Data.Ebf.Ebf

deriveEbf  :: Name -> Q [Dec]
deriveEbf n = reify n >>= \i-> case i of
                TyConI (DataD    context _ tvs cons _) -> workerData    context tvs (zip [1..] cons)
                TyConI (NewtypeD context _ tvs con  _) -> workerNewtype context tvs con
                _                                      -> fail $ "Can't derive Ebf instance for: " ++ show (n, i)
              where workerData    context tyvars cons
                      = let ty      =  foldl
                                         appT
                                         (conT n)
                                         [ varT v | PlainTV v <- tyvars ]
                        in  return <$> instanceD
                                         (cxt $ [classP ''Ebf [varT v] | PlainTV v <- tyvars] ++ map return context)
                                         (conT ''Ebf `appT` ty)
                                         [ mkEncode n cons
                                         , mkDecode n cons
                                         ]
                    workerNewtype context tyvars con
                      = let ty      =  foldl
                                         appT
                                         (conT n)
                                         [ varT v | PlainTV v <- tyvars ]
                        in  return <$> instanceD
                                         (cxt $ [classP ''Ebf [varT v] | PlainTV v <- tyvars] ++ map return context)
                                         (conT ''Ebf `appT` ty)
                                         [ funD
                                             'encode
                                             [ do x <- newName "x"
                                                  clause
                                                    [ conP
                                                        (conName con)
                                                        [varP x]
                                                    ] 
                                                    ( normalB $ appE
                                                                  (varE 'encode)
                                                                  (varE x)
                                                    )
                                                    []
                                             ]
                                         , valD 
                                             (varP 'decode)
                                             (normalB $ infixApp
                                                          (varE 'decode)
                                                          (varE '(>>=))
                                                          (infixApp
                                                            (varE 'return)
                                                            (varE '(.))
                                                            (conE $ conName con)
                                                          )
                                             )
                                             []
                                         ]

mkEncode :: Name -> [(Integer, Con)] -> DecQ
mkEncode tyName cons = funD 'encode $ map mkPutClause cons
    where
      putter = if length cons <= 255
                 then 'fromWord8
                 else 'fromWord16be
      mkPutClause (conNumber, con)
          = do putVars        <- replicateM (conSize con) (newName "arg") -- prepare constructor fields
               let putClause   = conP (conName con) (map varP putVars)    -- pattern
                   putCopyBody = foldl                                    -- body
                                  (\x y-> infixApp x (varE 'mappend) y)
                                  ( varE putter  `appE` litE (IntegerL conNumber) )
                                  [ varE 'encode `appE` varE var
                                  | var <- putVars
                                  ]
               clause [putClause] (normalB putCopyBody) []

mkDecode :: Name -> [(Integer, Con)] -> DecQ
mkDecode tyName cons = valD (varP 'decode) (normalB getCopyBody) []
    where
      getter = if length cons <=255
                 then varE 'I.head
                 else appE
                        (appE
                          (appE
                            (varE 'liftM2)
                            (do a <- newName "high"
                                b <- newName "low"
                                lamE
                                  [ varP a, varP b ]
                                  (infixApp
                                    (infixApp
                                      (sigE 
                                        (appE
                                          (varE 'fromIntegral)
                                          (varE a)
                                        )
                                        (conT ''Word16) -- Type annotation !
                                      )
                                      (varE '(*))
                                      (litE $ IntegerL 256)
                                    )
                                    (varE '(+))
                                    (appE
                                      (varE 'fromIntegral)
                                      (varE b)
                                    )
                                  )
                            )
                          )
                          (varE 'I.head)
                        )
                        (varE 'I.head)

      getCopyBody = infixApp
                      getter
                      (varE '(>>=))
                      (do x <- newName "x"
                          lam1E
                           (varP x)
                           (caseE
                             (varE x)
                             ([ match
                                  (litP $ IntegerL i)
                                  (normalB $ do args <- mapM (const $ newName "arg") (conTypes con)
                                                doE ( map
                                                        (\arg-> bindS (varP arg) (varE 'decode))
                                                        args
                                                      ++
                                                      [ noBindS $ foldr -- strictness!
                                                                    (\m n-> infixApp
                                                                              m
                                                                              (varE 'seq)
                                                                              n
                                                                    )
                                                                    (appE
                                                                      (varE 'return)
                                                                      (foldl
                                                                        appE
                                                                        (conE $ conName con)
                                                                        (map varE args)
                                                                      )
                                                                    )
                                                                    (map varE args)
                                                      ]
                                                    )
                                  )
                                  []
                              | (i,con) <- cons
                              ]
                              ++
                              [ do i <- newName "i"
                                   match
                                     (varP i)
                                     (normalB $ appE
                                                  (varE 'fail)
                                                  (infixApp
                                                    (  litE 
                                                    $  StringL
                                                    $  "Error while decoding a value of '"
                                                    ++ show tyName
                                                    ++ "'. Read constructor index "
                                                    )
                                                    (varE '(++))
                                                    (infixApp
                                                      (appE
                                                         (varE 'show)
                                                         (varE i)
                                                      )
                                                      (varE '(++))
                                                      (  litE
                                                      $  StringL
                                                      $  ", but the type has only "
                                                      ++ show (length cons)
                                                      ++ " (beginning with 1)."
                                                      )
                                                    )
                                                  )
                                     )
                                     []
                              ])
                           )
                      )


-- | Reifies a type and recursively shortcircuits synonyms and variables 
followSynonyms :: Type -> Q Type
followSynonyms t@(ConT name)  = let x = reify name >>= \i-> case i of
                                          TyVarI _ ty            -> return $ Just ty
                                          TyConI (TySynD _ _ ty) -> return $ Just ty
                                          _                      -> return $ Nothing
                                in  recover (return Nothing) x >>= maybe (return t) followSynonyms
followSynonyms (AppT ty1 ty2) = liftM2 AppT (followSynonyms ty1) (followSynonyms ty2)
followSynonyms (SigT ty k)    = liftM (flip SigT k) (followSynonyms ty)
followSynonyms t              = return t

-- | Determines the number of fields of a constructor
conSize :: Con -> Int
conSize (NormalC _name args) = length args
conSize (RecC _name recs)    = length recs
conSize InfixC{}             = 2
conSize ForallC{}            = error "Found complex constructor. Cannot derive SafeCopy for it."

-- | Determines a constructor's name
conName :: Con -> Name
conName (NormalC name _args) = name
conName (RecC name _recs)    = name
conName (InfixC _ name _)    = name
conName _                    = error "conName: 2842"

-- | Determines the types of a constructor's fields
conTypes :: Con -> [Type]
conTypes (NormalC _name args)       = [t | (_, t)    <- args]
conTypes (RecC _name args)          = [t | (_, _, t) <- args]
conTypes (InfixC (_, t1) _ (_, t2)) = [t1, t2]
conTypes _                          = error "conTypes: 3749"

-- | Renders a complex type's designation
typeName :: Type -> String
typeName (VarT name) = nameBase name
typeName (ConT name) = nameBase name
typeName (TupleT n)  = '(' : replicate (n-1) ',' ++ ")"
typeName ArrowT      = "Arrow"
typeName ListT       = "List"
typeName (AppT t u)  = typeName t ++ typeName u
typeName (SigT t _k) = typeName t
typeName _           = "_"

