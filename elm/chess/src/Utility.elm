module Utility exposing (..)

import Array exposing (Array)


elemIndex : a -> Array a -> Maybe Int
elemIndex x xs =
    Array.map ((==) x) xs
        |> Array.toIndexedList
        |> List.filterMap (\(index, fb) -> if fb then Just index else Nothing)
        |> List.head
