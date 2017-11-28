module CaseExpressions where

funcZ x = case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

pal' xs =
    case y of
        True -> "yes"
        False -> "no"
    where y = xs == reverse xs