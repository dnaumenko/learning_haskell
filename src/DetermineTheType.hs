{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

example = 1 -- with NoMonomorphismRestriction, it will result to Num, not Integer

(* 9) 6