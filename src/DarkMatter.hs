{-# LANGUAGE OverloadedStrings #-}
module DarkMatter (main) where

import Data.Bifunctor                  (first)
import Data.Char                       (isSpace)
import Data.List                       (foldl', sortOn)
import Data.Maybe                      (catMaybes)
import Data.String                     (IsString (..))
import Data.Traversable                (for)
import Distribution.Compiler           (CompilerFlavor (..))
import Distribution.Fields
       (Field (..), FieldLine (..), Name (..), SectionArg, fromParsecFields,
       readFields, runParseResult, showFields)
import Distribution.Fields.ConfVar     (parseConditionConfVar)
import Distribution.Fields.Field       (fieldLineAnn)
import Distribution.PackageDescription (ConfVar (..))
import Distribution.Parsec             (Position, showPError, zeroPos)
import Distribution.Parsec.Position
import Distribution.Pretty             (prettyShow)
import Distribution.Simple.Utils       (fromUTF8BS)
import Distribution.Types.Condition    (Condition (..), simplifyCondition)
import Distribution.Version

import qualified Data.ByteString      as BS
import qualified Data.Makefile        as MF
import qualified Data.Makefile.Render as MF
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL

-- import qualified Options.Applicative as O

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
    , [8,6,5]
    , [8,8,4]
    , [8,10,7]
    , [9,0,2]
    , [9,2,7]
    , [9,4,6]
    , [9,6,2]
    , [9,8,1]
    ]

makeMakefile :: [Version] -> MF.Makefile
makeMakefile vs = MF.Makefile
    $ pfEntry
    : makefileEntry
    : buildEntry
    : testEntry
    : testBuildEntry
    : testDryEntry
    : depsEntry
    : sortOn entryTarget (concatMap go vs)
  where
    pfEntry :: MF.Entry
    pfEntry = MF.Rule "project-files" (map projectFileName vs) []

    makefileEntry :: MF.Entry
    makefileEntry = MF.Rule "Makefile" ["cabal.darkmatter"]
        [ MF.Command "darkmatter > Makefile"
        ]

    buildEntry :: MF.Entry
    buildEntry = MF.Rule "build" (map (\v -> fromString $ "build-" ++ prettyShow v) vs) []

    testDryEntry :: MF.Entry
    testDryEntry = MF.Rule "test-dry" (map (\v -> fromString $ "test-dry-" ++ prettyShow v) vs) []

    testBuildEntry :: MF.Entry
    testBuildEntry = MF.Rule "test-build" ("test-dry" : map (\v -> fromString $ "test-build-" ++ prettyShow v) vs) []

    testEntry :: MF.Entry
    testEntry = MF.Rule "test" ("test-build" : map (\v -> fromString $ "test-" ++ prettyShow v) vs) []

    depsEntry :: MF.Entry
    depsEntry = MF.Rule "deps" (map (\v -> fromString $ "deps-" ++ prettyShow v ++ ".png") vs) []

    go :: Version -> [MF.Entry]
    go v =
        [ MF.Rule (projectFileName v) ["cabal.darkmatter"]
            [ "darkmatter"
            ]
        , MF.Rule (MF.Target depsPng) [projectFileName v]
            [ MF.Command $ "cabal new-build --builddir=" <> bdir <> " --project-file " <> projectFileName v <> " -w ghc-" <> vt <> " --disable-tests --disable-benchmarks all --dry-run"
            , MF.Command $ "cabal-plan --hide-builtin --hide-global dot --builddir=" <> bdir <> " --tred --tred-weights | dot -Tpng -o" <> depsPng
            ]
        , MF.Rule (MF.Target $ "build-" <> vt) [projectFileName v]
            [ MF.Command $ "cabal new-build --builddir=" <> bdir <> " --project-file " <> projectFileName v <> " -w ghc-" <> vt <> " --disable-tests --disable-benchmarks all"
            ]
        , MF.Rule (MF.Target $ "test-dry-" <> vt) [projectFileName v]
            [ MF.Command $ "cabal new-build --builddir=" <> bdir <> " --project-file " <> projectFileName v <> " -w ghc-" <> vt <> " --enable-tests --enable-benchmarks --dry all"
            ]
        , MF.Rule (MF.Target $ "test-build-" <> vt) [projectFileName v, MF.Dependency $ "test-dry-" <> vt]
            [ MF.Command $ "cabal new-build --builddir=" <> bdir <> " --project-file " <> projectFileName v <> " -w ghc-" <> vt <> " --enable-tests --enable-benchmarks all"
            ]
        , MF.Rule (MF.Target $ "test-" <> vt) [projectFileName v, MF.Dependency $ "test-build-" <> vt]
            [ MF.Command $ "cabal new-test --builddir=" <> bdir <> " --project-file " <> projectFileName v <> " -w ghc-" <> vt <> " --enable-tests --enable-benchmarks all"
            ]
        ]
      where
        depsPng = "deps-" <> vt <> ".png"
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

    ProjectFile _ extraMakefile <- either fail pure $ projectFile (head ghcVersions) cfg

    vs <- for ghcVersions $ \v -> do
        ProjectFile cfg' _ <- either fail pure $ projectFile v cfg
        if null cfg' then return Nothing else do
            let fn = projectFileName v
            let cfg'' = cfg' ++ [ Field (Name zeroPos "with-compiler") [FieldLine zeroPos $ "ghc-" <> fromString (prettyShow v) ]
                        ]
            let bs = showFields (const []) $ fromParsecFields cfg''
            writeFile fn bs
            return (Just v)

    let mf1  = makeMakefile (catMaybes vs)
    let mf2 = MF.encodeMakefile mf1
    let mf3 = TL.unpack mf2

    putStr $ mf3 ++ unlines (map processMakefileLine $ concatMap lines extraMakefile)

processMakefileLine :: String -> String
processMakefileLine (c : cs) | isSpace c = '\t' : dropWhile isSpace cs
processMakefileLine cs = cs

data ProjectFile = ProjectFile
    { pfFields   :: [Field Position]
    , pfMakefile :: [String]
    }

emptyProjectFile :: ProjectFile
emptyProjectFile = ProjectFile [] []

pfAddField :: Field Position -> ProjectFile -> ProjectFile
pfAddField f pf = pf { pfFields = f : pfFields pf }

pfAddMakefile :: String -> ProjectFile -> ProjectFile
pfAddMakefile s pf = pf { pfMakefile = s : pfMakefile pf }

pfSection :: Name Position -> [SectionArg Position] -> ProjectFile -> ProjectFile -> ProjectFile
pfSection n args x y = ProjectFile
    { pfFields   = Section n args (pfFields x) : pfFields y
    , pfMakefile = pfMakefile x ++ pfMakefile y
    }

projectFile :: Version -> [Field Position] -> Either String ProjectFile
projectFile _ [] = Right emptyProjectFile
projectFile v (Field (Name pos name) fls : fs)
    | name == "makefile" =
        pfAddMakefile (fieldlinesToFreeText3 pos fls) <$> projectFile v fs
projectFile v (f@Field {} : fs) = pfAddField f <$> projectFile v fs
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
        return $ pfSection n args gs' fs'
  where
    toEither = first (unlines . map (showPError "-") . snd) . snd

    simpl :: ConfVar -> Either ConfVar Bool
    simpl (Impl GHC vr) = Right $ withinRange v vr
    simpl cv            = Left cv

main :: IO ()
main = projectFiles

-------------------------------------------------------------------------------
-- from Cabal
-------------------------------------------------------------------------------

fieldlinesToFreeText3 :: Position -> [FieldLine Position] -> String
fieldlinesToFreeText3 _   []               = ""
fieldlinesToFreeText3 _   [FieldLine _ bs] = fromUTF8BS bs
fieldlinesToFreeText3 pos (FieldLine pos1 bs1 : fls2@(FieldLine pos2 _ : _))
    -- if first line is on the same line with field name:
    -- the indentation level is either
    -- 1. the indentation of left most line in rest fields
    -- 2. the indentation of the first line
    -- whichever is leftmost
    | positionRow pos == positionRow pos1 = concat
        $ fromUTF8BS bs1
        : mealy (mk mcol1) pos1 fls2

    -- otherwise, also indent the first line
    | otherwise = concat
        $ replicate (positionCol pos1 - mcol2) ' '
        : fromUTF8BS bs1
        : mealy (mk mcol2) pos1 fls2
  where
    mcol1 = foldl' (\a b -> min a $ positionCol $ fieldLineAnn b) (min (positionCol pos1) (positionCol pos2)) fls2
    mcol2 = foldl' (\a b -> min a $ positionCol $ fieldLineAnn b) (positionCol pos1) fls2

    mk :: Int -> Position -> FieldLine Position -> (Position, String)
    mk col p (FieldLine q bs) =
        ( q
        , replicate newlines '\n'
          ++ replicate indent ' '
          ++ fromUTF8BS bs
        )
      where
        newlines = positionRow q - positionRow p
        indent   = positionCol q - col

mealy :: (s -> a -> (s, b)) -> s -> [a] -> [b]
mealy f = go where
    go _ [] = []
    go s (x : xs) = let ~(s', y) = f s x in y : go s' xs
