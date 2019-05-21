module NavigationTest exposing (suite)

import Expect
import Model exposing (Navigation(..), navigate)
import Test exposing (..)


suite : Test
suite =
    describe "Palette navigation"
        [ navigateTest ]


navigateTest : Test
navigateTest =
    describe "navigate function"
        [ test "Navigates to next item" <|
            \_ ->
                Expect.equal (navigate 10 3 Next) 4
        , test "Navigates to previous item" <|
            \_ ->
                Expect.equal (navigate 10 3 Previous) 2
        , test "Returns last item when at the end" <|
            \_ ->
                Expect.equal (navigate 10 9 Next) 9
        , test "First item is 0" <|
            \_ ->
                Expect.equal (navigate 10 1 Previous) 0
        , test "Returns first item when at the beginning" <|
            \_ ->
                Expect.equal (navigate 10 0 Previous) 0
        , test "Jumps to item" <|
            \_ ->
                Expect.equal (navigate 10 3 (Jump 7)) 7
        , test "Returns current when item not exists" <|
            \_ ->
                Expect.equal (navigate 10 3 (Jump 13)) 3
        ]
