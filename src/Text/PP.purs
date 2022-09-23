{-
================================================================================
# PP
===============================================================================

The pretty pathetic pretty printing library.

-}
module Text.PP where

import Prelude

import Data.Array (intercalate, uncons, unsnoc)
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

class PP a where
  pp :: a -> Doc

pprint :: forall a. PP a => a -> String 
pprint = show <<< pp  

newtype Doc 
  = Doc { lines :: Array String }

instance Show Doc where 
  show (Doc doc) = intercalate "\n" doc.lines

instance ppString :: PP String where 
  pp str = Doc {lines: [str ]}

words :: Array Doc -> Doc
words = intercalate (pp " ")

paren :: Doc -> Doc
paren doc = pp "(" <> doc <> pp ")"

braks :: Doc -> Doc 
braks doc = pp "[" <> doc <> pp "]"

-- combinators
hcat :: forall f. Foldable f => f Doc -> Doc
hcat = Foldable.foldr (<>) mempty

vcat :: forall f. Foldable f => f Doc -> Doc
vcat = Foldable.foldr (</>) mempty

hsep :: Doc -> Array Doc -> Doc 
hsep sep docs = intercalate sep docs

-- pp utilities

ppArray :: forall a. PP a => Array a -> Doc
ppArray = braks <<< hsep (pp ", ") <<< map pp   

-- instanes

instance semigroupDoc :: Semigroup Doc where
  append (Doc doc1) (Doc doc2) =
    Doc
      { lines:
          case unsnoc doc1.lines /\ uncons doc2.lines of
            Just { init, last } /\ Just { head, tail } -> init <> [ last <> head ] <> tail
            Nothing /\ _ -> doc2.lines
            _ /\ Nothing -> doc1.lines
      }

instance monoidDoc :: Monoid Doc where
  mempty = Doc { lines: [] }

-- vertical append
vappend :: Doc -> Doc -> Doc
vappend (Doc doc1) (Doc doc2) =
  Doc
    { lines: doc1.lines <> doc2.lines
    }

infixr 5 vappend as </>
