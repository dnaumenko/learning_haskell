module Madlib where

import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbing' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbing' e adv noun adj =
  e <> "! he said " <>
  adv <> " and he jumpted into the car " <>
  noun <> " and drove of with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = mconcat [e, "! he said ", adv, " and he jumpted into the car "
                                      , noun, " and drove of with his ", adj, " wife."]


