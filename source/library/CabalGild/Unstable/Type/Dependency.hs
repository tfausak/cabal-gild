{-# LANGUAGE NamedFieldPuns #-}

module CabalGild.Unstable.Type.Dependency where

import qualified Control.Monad as Monad
import qualified Data.List.NonEmpty as NonEmpty
import qualified Distribution.CabalSpecVersion as CabalSpecVersion
import qualified Distribution.Compat.CharParsing as Parse
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Types.PackageName as PackageName
import qualified Distribution.Types.UnqualComponentName as UnqualComponentName
import qualified Distribution.Types.VersionRange as VersionRange
import qualified Text.PrettyPrint as PrettyPrint

data Dependency = MkDependency
  { packageName :: PackageName.PackageName,
    libraryNames :: Maybe (Either UnqualComponentName.UnqualComponentName (NonEmpty.NonEmpty UnqualComponentName.UnqualComponentName)),
    versionRange :: VersionRange.VersionRange
  }
  deriving (Eq, Ord, Show)

instance Parsec.Parsec Dependency where
  parsec = do
    packageName <- Parsec.parsec
    libraryNames <- Parse.optional $ do
      Monad.void $ Parse.char ':'
      csv <- Parsec.askCabalSpecVersion
      Monad.guard $ csv >= CabalSpecVersion.CabalSpecV3_0
      Monad.msum
        [ fmap Left Parsec.parsec,
          Right
            <$> Parse.between
              (Parse.char '{' *> Parse.spaces)
              (Parse.spaces <* Parse.char '}')
              (Parsec.parsecCommaNonEmpty Parsec.parsec)
        ]
    Parse.spaces
    versionRange <-
      Monad.msum
        [ Parsec.parsec,
          pure VersionRange.anyVersion
        ]
    pure
      MkDependency
        { packageName,
          libraryNames,
          versionRange
        }

instance Pretty.Pretty Dependency where
  pretty dependency =
    PrettyPrint.hsep
      [ PrettyPrint.hcat
          [ Pretty.pretty $ packageName dependency,
            case libraryNames dependency of
              Nothing -> mempty
              Just e ->
                PrettyPrint.char ':' <> case e of
                  Left ucn -> Pretty.pretty ucn
                  Right ucns ->
                    PrettyPrint.braces
                      . foldr1
                        ( \ucn doc ->
                            PrettyPrint.hsep
                              [ PrettyPrint.hcat
                                  [ Pretty.pretty ucn,
                                    PrettyPrint.comma
                                  ],
                                doc
                              ]
                        )
                      . fmap Pretty.pretty
                      $ NonEmpty.sort ucns
          ],
        if VersionRange.isAnyVersion $ versionRange dependency
          then mempty
          else Pretty.pretty $ versionRange dependency
      ]
