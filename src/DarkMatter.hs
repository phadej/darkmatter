{-# LANGUAGE OverloadedStrings #-}
module DarkMatter (main) where

import Data.Bifunctor                  (first)
import Data.Maybe                      (catMaybes)
import Data.String                     (IsString (..))
import Data.Traversable                (for)
import Distribution.Compiler           (CompilerFlavor (..))
import Distribution.PackageDescription (ConfVar (..))
import Distribution.Parsec.Common      (Position, showPError, zeroPos)
import Distribution.Parsec.ConfVar     (parseConditionConfVar)
import Distribution.Parsec.Parser
       (Field (..), FieldLine (..), Name (..), readFields)
import Data.List (sortOn)
import Distribution.Parsec.ParseResult (runParseResult)
import Distribution.Pretty             (prettyShow)
import Distribution.Types.Condition    (Condition (..), simplifyCondition)
import Distribution.Version

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Makefile         as MF
import qualified Data.Makefile.Render  as MF
import qualified Data.Text             as T
import qualified Data.Text.Lazy.IO     as TL
import qualified Text.PrettyPrint      as PP

-- import qualified Options.Applicative as O

import Debug.Trace

ghcVersions :: [Version]
ghcVersions = map mkVersion
    [ [7,0,4]
    , [7,2,2]
    , [7,4,2]
    , [7,6,3]
    , [7,8,4]
    , [7,10,3]
    , [8,0,2]
    , [8,2,2]
    , [8,4,4]
    , [8,6,4]
    ]

makeMakefile :: [Version] -> MF.Makefile
makeMakefile vs = MF.Makefile
    $ pfEntry
    : buildEntry
    : testEntry
    : depsEntry
    : sortOn entryTarget (concatMap go vs)
  where
    pfEntry :: MF.Entry
    pfEntry = MF.Rule "project-files" (map projectFileName vs) []

    buildEntry :: MF.Entry
    buildEntry = MF.Rule "build" (map (\v -> fromString $ "build-" ++ prettyShow v) vs) []

    testEntry :: MF.Entry
    testEntry = MF.Rule "test" (map (\v -> fromString $ "test-" ++ prettyShow v) vs) []

    depsEntry :: MF.Entry
    depsEntry = MF.Rule "deps" (map (\v -> fromString $ "deps." ++ prettyShow v ++ ".png") vs) []

    go :: Version -> [MF.Entry]
    go v =
        [ MF.Rule (projectFileName v) ["cabal.darkmatter"]
            [ "darkmatter"
            ]
        , MF.Rule (MF.Target depsPng) [projectFileName v]
            [ MF.Command $ "cabal new-build --builddir=" <> bdir <> " --project-file " <> projectFileName v <> " -w ghc-" <> vt <> " --disable-tests --disable-benchmarks all --dry-run"
            , MF.Command $ "cabal-plan --builddir=" <> bdir <> " --hide-builtin --hide-global dot --tred --tred-weights | dot -Tpng -o" <> depsPng
            ]
        , MF.Rule (MF.Target $ "build-" <> vt) [projectFileName v]
            [ MF.Command $ "cabal new-build --builddir=" <> bdir <> " --project-file " <> projectFileName v <> " -w ghc-" <> vt <> " --disable-tests --disable-benchmarks all"
            ]
        , MF.Rule (MF.Target $ "test-" <> vt) [projectFileName v]
            [ MF.Command $ "cabal new-build --builddir=" <> bdir <> " --project-file " <> projectFileName v <> " -w ghc-" <> vt <> " --enable-tests --disable-benchmarks all"
            , MF.Command $ "cabal new-test --builddir=" <> bdir <> " --project-file " <> projectFileName v <> " -w ghc-" <> vt <> " --enable-tests --disable-benchmarks all"
            ]
        ]
      where
        depsPng = "deps." <> vt <> ".png"
        bdir = "dist-newstyle-" <> vt

        vt = fromString (prettyShow v)

    entryTarget :: MF.Entry -> Maybe T.Text
    entryTarget (MF.Rule (MF.Target t) _ _) = Just t
    entryTarget _                           = Nothing

{-
git-submodule-update :
  git submodule foreach git checkout master
  git submodule foreach git pull
-}

projectFileName :: IsString s => Version -> s
projectFileName  v =  fromString $ "cabal." ++ prettyShow v ++ ".project"

projectFiles :: IO ()
projectFiles = do
    contents <- BS.readFile "cabal.darkmatter"
    cfg <- either (fail . show) pure $ readFields contents

    vs <- for ghcVersions $ \v -> do
        cfg' <- either fail pure $ projectFile v cfg
        if null cfg' then return Nothing else do
            let fn = projectFileName v
            let cfg'' = cfg' ++ [ Field (Name zeroPos "with-compiler") [FieldLine zeroPos $ "ghc-" <> fromString (prettyShow v) ]
                        ]
            let bs = PP.render (prettyFields cfg'') ++ "\n"
            writeFile fn bs
            return (Just v)

    let mf  = makeMakefile (catMaybes vs)
    let mf' = MF.encodeMakefile mf

    TL.putStr mf'

prettyFields :: [Field ann] -> PP.Doc
prettyFields = PP.vcat . map go where
    go :: Field ann -> PP.Doc
    go (Field (Name _ann name) fls) =
        PP.text (BS8.unpack name) PP.<> PP.colon PP.<+> goFls fls
    go (Section _ _ _) = PP.empty -- TODO

    goFls :: [FieldLine ann] -> PP.Doc
    goFls = PP.vcat . map goFls'

    goFls' :: FieldLine ann -> PP.Doc
    goFls' (FieldLine _ann bs) = PP.text (BS8.unpack bs)

projectFile :: Version -> [Field Position] -> Either String [Field Position]
projectFile v [] = Right []
projectFile v (f@Field {} : fs) = (f :) <$> projectFile v fs
projectFile v (Section n@(Name _ann name) args gs : fs)
    | name == "if" = do
        c <- toEither $ runParseResult $ parseConditionConfVar args
        case simplifyCondition c simpl of
            (Lit True, _)  -> projectFile v (gs ++ fs)
            (Lit False, _) -> projectFile v fs
            (_, ds)        -> error $ show ds

    | otherwise = do
        gs' <- projectFile v gs
        fs' <- projectFile v fs
        return $ Section n args gs' : fs'
  where
    toEither = first (unlines . map (showPError "-") . snd) . snd

    simpl :: ConfVar -> Either ConfVar Bool
    simpl (Impl GHC vr) = Right $ withinRange v vr
    simpl cv            = Left cv

main :: IO ()
main = projectFiles
