
{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

{- |

Adapted from
QuickCheck-2.14.2, module Test.QuickCheck.All.

Sample usage:

@
{-# LANGUAGE TemplateHaskell #-}

module SomeMod where

import Test.Hspec
import Test.ExtractHspecTests (allSpecs)

some_spec :: Spec
some_spec =
  describe "that function over there" $
    it "should do a thing" $
      pendingWith "TODO"

return []

spec :: Spec
spec = 
    forM_ $( allSpecs ) $ uncurry describe

main :: IO ()
main = hspec spec

@

-}


module Test.ExtractHspecTests
  where

import Control.Monad

import Data.Char                      (isUpper, isAlphaNum, isSpace)
import Data.List                      (nubBy, isSuffixOf, concat)

import            Language.Haskell.TH  (Name, Exp, ExpQ, Q, Loc(loc_filename, Loc) )
import qualified  Language.Haskell.TH  as T
import qualified  System.IO            as S
import            Test.Hspec

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Eta reduce" #-}


-- See section 2.4 of the Haskell 2010 Language Report, plus support for "[]"
isVar :: Name -> Bool
isVar = let isVar' (c:_) = not (isUpper c || c `elem` ":[")
            isVar' _     = True
        in isVar' . T.nameBase

expName :: Name -> Exp
expName n = if isVar n then T.VarE n else T.ConE n

-- Deal with UTF-8 input and output.
set_utf8_io_enc :: S.Handle -> IO S.Handle
#if __GLASGOW_HASKELL__ > 611
-- possibly if MIN_VERSION_base(4,2,0)
set_utf8_io_enc h = do S.hSetEncoding h S.utf8; return h
#else
set_utf8_io_enc h = return h
#endif

readUTF8File name = S.openFile name S.ReadMode >>=
                    set_utf8_io_enc >>=
                    S.hGetContents


-- | List all _spec functions in the current module.
--
-- @$'allSpecs'@ has type @[('String', 'Spec')]@.
--
-- 'allSpecs' has the same issue with scoping as quickCheckAll:
-- see the note at <https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck-All.html#v:quickCheckAll> about @return []@.
--
-- (viz.: you need to insert an odd @return []@ in the .hs source
-- file before you call @allSpecs@.)
allSpecs :: Q Exp
allSpecs = do
  Loc { loc_filename = filename } <- T.location
  when (filename == "<interactive>") $ error "don't run this interactively"
  ls <- T.runIO (fmap lines (readUTF8File filename))
  let prefixes = map (takeWhile (\c -> isAlphaNum c || c == '_' || c == '\'') . dropWhile (\c -> isSpace c || c == '>')) ls
      idents = nubBy (\x y -> snd x == snd y) (filter (("_spec" `isSuffixOf`) . snd) (zip [1..] prefixes))
#if MIN_VERSION_template_haskell(2,8,0)
      warning x = T.reportWarning ("Name " ++ x ++ " found in source file but was not in scope")
#else
      warning x = T.report False  ("Name " ++ x ++ " found in source file but was not in scope")
#endif
      oneSpec :: (Int, String) -> Q [Exp]
      oneSpec (lineNum, x) = do
        exists <- (warning x >> return False) `T.recover` (T.reify (T.mkName x) >> return True)
        if exists then sequence [ [| ($(T.stringE $ x ++ " from " ++ filename ++ ":" ++ show lineNum),
                                     $( return $ expName $ T.mkName x)) |] ]
         else return []
  [| $(fmap (T.ListE . concat) (mapM oneSpec idents)) :: [(String, Spec)] |]

