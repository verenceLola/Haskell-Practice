module ListyInstances where

import Data.Monoid
import Listy

instance Monoid (Listy a) where
    mempty = Listy []
    mappend mempty x = x
    mappend x mempty = x
    mappend (Listy l) (Listy l') = Listy $ mappend l l'
    mconcat = foldr mappend mempty


type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String

madlibbin' e adv noun adj = e <> "! he said " <> adv <> " as he jumped into his car " <>
    noun <> " and drove off with this " <> adj <> " wife."
