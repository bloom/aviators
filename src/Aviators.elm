module Aviators exposing (Arrangement(..), Cell, Debounce(..), Flex, OutlineButtonCfg, RowCfg, autoCell, button, buttonCfg, callout, calloutCfg, calloutCfg_error, calloutCfg_ok, column, columnCfg, el, empty, errorText, floatingPage, floatingPageCfg, ghostButton, ghostButtonCfg, headline, headlineCfg, img, imgCfg, input, inputCfg, inputCfg_password, inputCfg_text, link, linkCfg, nav, navCfg, outlineButton, outlineButtonCfg, paragraph, paragraphCfg, pickOneFromFew, pickOneFromFewCfg, ratioCell, root, row, rowCfg, rowWithCells, solidButton, solidButtonCfg, subheadline, subheadlineCfg, wrapWithClasses, wrapWithTailwind)

{-| Aviators! The best UI Library made specifically for Bloom Built in Elm.

@docs Debounce, button, buttonCfg, callout, calloutCfg, calloutCfg_ok, calloutCfg_error, column, columnCfg, empty, errorText, floatingPage, floatingPageCfg, ghostButton, ghostButtonCfg, img, imgCfg, input, inputCfg, inputCfg_password, inputCfg_text, link, linkCfg, nav, navCfg, outlineButton, OutlineButtonCfg, outlineButtonCfg, paragraph, paragraphCfg, pickOneFromFew, pickOneFromFewCfg, root, row, rowCfg, solidButton, solidButtonCfg, headline, headlineCfg, subheadline, subheadlineCfg, wrapWithClasses, el, wrapWithTailwind, Arrangement, RowCfg, autoCell, ratioCell, Cell, rowWithCells, Flex

-}

import Aviators.Svg
import Html exposing (Attribute, Html, div, li, node, text)
import Html.Attributes exposing (attribute, class, href, placeholder, rel, style)
import Html.Events exposing (keyCode, on, onClick, onInput, onMouseDown, onMouseUp, onWithOptions)
import Json.Decode
import SelectList exposing (Position(Selected), SelectList)
import Tailwind exposing (asClasses, tailwind, withClasses)
import Tailwind.Classes exposing (TailwindClass, bg_blue, bg_grey, bg_red, bg_teal, block, border, border_blue, border_grey, border_grey_dark, border_l_0, border_red, border_t, border_t_0, border_transparent, flex, flex_1, flex_auto, flex_col, flex_row, font_bold, h_32, h_full, items_center, items_end, items_start, items_stretch, justify_between, justify_center, justify_end, justify_start, md, p_2, p_4, p_8, rounded_b, rounded_b_none, rounded_bl_none, rounded_l, rounded_r, rounded_t, rounded_t_none, sm, text_grey, text_grey_dark, text_white, w_full)


{-
   ~~~ THE VERY IMPORTANT ROOT FUNCTION ~~~ 👍👍
-}


{-| -}
root : Html msg -> Html msg
root child =
    div [ class "aviators-root" ]
        [ Html.node "link" [ href "https://cdn.jsdelivr.net/npm/tailwindcss/dist/tailwind.min.css", rel "stylesheet" ] []
        , node "style" [] [ Html.text """
        .focus-outline:focus {
            box-shadow: 0px 0px 0px 2px #FFFFFF, 0px 0px 0px 5px #68BDF9;
            outline: none;
        }
        .outline-red {
            box-shadow: 0px 0px 0px 2px #FFFFFF, 0px 0px 0px 5px #d82c2c;
            outline: none;
        }
        .transition {
            transition: 200ms;
        }
        * {
            box-sizing: border-box;
        }
        """ ]
        , child
        ]



{-
   ~~~ COMPLEX VIEW FUNCTIONS ~~~ 👍👍
-}


{-| -}
pickOneFromFew :
    { b
        | label : String
        , labelForOption : a -> String
        , onChange : Maybe (SelectList a -> msg)
        , options : SelectList a
    }
    -> Html msg
pickOneFromFew ({ options } as cfg) =
    -- TODO: This needs a major accessibilty review
    let
        fragments =
            SelectList.mapBy
                (\position option ->
                    { attrs =
                        (if position == Selected then
                            [ tailwind
                                [ bg_blue
                                , text_white
                                , border_blue
                                , h_full
                                ]
                            ]
                         else
                            [ tailwind
                                [ border_grey
                                , text_grey
                                ]
                            ]
                        )
                            -- These attributes apply to all buttons, no matter which position
                            ++ [ tailwind
                                    [ p_2
                                    , w_full
                                    , border
                                    , h_full
                                    , w_full
                                    ]
                               ]
                    , value = option
                    }
                )
                options

        styleByIndex list =
            List.indexedMap
                (\idx { attrs, value } ->
                    if idx == 0 then
                        { attrs =
                            [ tailwind
                                -- Rounding the top when small, and the left when not small
                                [ rounded_t
                                , md rounded_t_none
                                , md rounded_l
                                ]
                            ]
                                ++ attrs
                        , value = value
                        }
                    else if idx == List.length list - 1 then
                        { attrs =
                            [ tailwind
                                [ -- Rounding the bottom when small, and the right when not small
                                  rounded_b
                                , md rounded_b_none
                                , md rounded_r

                                -- Taking off left or top borders on non-first items
                                , border_t_0
                                , md border_t
                                , md border_l_0
                                ]
                            ]
                                ++ attrs
                        , value = value
                        }
                    else
                        { attrs =
                            [ tailwind
                                [ -- Taking off left or top borders on non-first items
                                  border_t_0
                                , md border_t
                                , md border_l_0
                                ]
                            ]
                                ++ attrs
                        , value = value
                        }
                )
                list

        renderFragments fragments =
            List.map
                (\fragment ->
                    Html.button
                        ((case cfg.onChange of
                            Just onChange ->
                                -- If we use "onClick" here instead of "onWithOptions" Safari sends two events, one
                                -- with old message content, and one with the correct message content, but in backward
                                -- order. I don't know if this has something funny to do with DOM structure or event
                                -- listeners, but in any case it can be solved by stopping propagation and preventing
                                -- default.
                                [ onWithOptions "click" { preventDefault = True, stopPropagation = True } (Json.Decode.succeed (onChange (SelectList.select ((==) fragment.value) options))) ]

                            Nothing ->
                                []
                         )
                            ++ fragment.attrs
                        )
                        [ Html.text <| cfg.labelForOption fragment.value ]
                )
                fragments
    in
    Html.label
        [ tailwind
            [ text_grey_dark
            , block
            , font_bold
            ]
        ]
        [ column { columnCfg | spacing = 2 }
            [ text cfg.label
            , fragments
                |> SelectList.toList
                |> styleByIndex
                |> renderFragments
                |> row { rowCfg | spacing = 0, arrangement = Expand, stackWhenSmall = True, crossArrangement = Expand }
            ]
        ]


{-| -}
pickOneFromFewCfg :
    { label : String
    , labelForOption : a -> String
    , onChange : Maybe a1
    , options : SelectList number
    , stackWhenSmall : Bool
    }
pickOneFromFewCfg =
    { options = SelectList.fromLists [ 1, 2 ] 3 [ 4, 5 ]
    , labelForOption = toString
    , onChange = Nothing
    , stackWhenSmall = True
    , label = "A Picker"
    }


pickOneFromFewTest : Html msg
pickOneFromFewTest =
    pickOneFromFew pickOneFromFewCfg


{-| -}
floatingPage : FloatingPageCfg -> Html msg -> Html msg
floatingPage cfg child =
    div
        [ classes
            [ "m-auto"
            , "p-" ++ toString cfg.padding
            ]
        , style
            [ ( "max-width", toString cfg.maxWidth ++ "px" ) ]
        ]
        [ child ]


{-| -}
floatingPageCfg : FloatingPageCfg
floatingPageCfg =
    { maxWidth = 960
    , padding = 2
    }


type alias FloatingPageCfg =
    { maxWidth : Int
    , padding : Int
    }


floatingPageTest : Html msg -> Html msg
floatingPageTest =
    floatingPage floatingPageCfg


{-| -}
nav : NavCfg msg -> Html msg -> Html msg
nav cfg child =
    el
        [ tailwind
            [ w_full
            , sm p_8
            , p_4
            ]
        , style [ ( "background-color", "rgb(249, 251, 251)" ) ]
        ]
    <|
        row
            { rowCfg | arrangement = Isolate, crossArrangement = Middle, stackWhenSmall = True, spacing = 4 }
            [ ghostButton { ghostButtonCfg | onClick = cfg.onClickLogo, ariaLabel = "Navigate to Home" } <|
                img
                    { src = cfg.logo
                    , alt = cfg.logoAltText
                    }
            , child
            ]


{-| -}
navCfg : { logo : String, logoAltText : String, onClickLogo : Maybe a }
navCfg =
    { onClickLogo = Nothing
    , logo = Aviators.Svg.darkLogo
    , logoAltText = "Day One Logo"
    }


type alias NavCfg msg =
    { logo : String, logoAltText : String, onClickLogo : Maybe msg }


testNav : Html msg -> Html msg
testNav =
    nav navCfg


{-| -}
calloutCfg_ok : { textColor : String, backgroundColor : String }
calloutCfg_ok =
    { calloutCfg
        | backgroundColor = "green-lightest"
        , textColor = "green"
    }


{-| -}
calloutCfg_error : { textColor : String, backgroundColor : String }
calloutCfg_error =
    { calloutCfg
        | backgroundColor = "red-lightest"
        , textColor = "red"
    }


{-| -}
callout :
    { a | backgroundColor : String, textColor : String }
    -> Html msg
    -> Html msg
callout cfg child =
    div
        [ classes
            [ -- Colorful background
              "bg-" ++ cfg.backgroundColor
            , "p-3"
            , "rounded"

            -- Text
            , "text-" ++ cfg.textColor
            ]
        ]
        [ child ]


{-| -}
calloutCfg : { backgroundColor : String, textColor : String }
calloutCfg =
    { backgroundColor = "red"
    , textColor = "white"
    }


calloutTest : Html msg -> Html msg
calloutTest =
    callout calloutCfg



{-
   ~~~ INPUTS ~~~ 👍👍
-}


{-| -}
input : InputConfig msg -> Html msg
input cfg =
    let
        -- Here we do some hacky javascripting to fire an event after the user has stopped
        -- changing the input for some interval. Be very very careful here.
        -- Thar be dagrons 🐲. (Yes, I spelled that incorrectly on porpoise 🐬.)
        debounceAttributes =
            case cfg.debounce of
                NoDebounce ->
                    case cfg.onChange of
                        Just fn ->
                            [ onInput fn ]

                        Nothing ->
                            []

                DebounceAfter timeout onDebounce ->
                    let
                        saveContext =
                            "var that = this;"

                        clearPrevTimeout =
                            "if (that.debouncer) {clearTimeout(that.debouncer)}"

                        sendDebounceEvent =
                            "var event = new CustomEvent('custom-debounce'); that.dispatchEvent(event);"

                        onTimeout thing timeout_ =
                            "that.debouncer = setTimeout(function() {" ++ thing ++ "}, " ++ toString timeout_ ++ ");"

                        sendChangeEvent =
                            "var event2 = new CustomEvent('custom-change', {detail: that.value}); that.dispatchEvent(event2);"

                        onInputFn =
                            saveContext
                                ++ clearPrevTimeout
                                ++ onTimeout sendDebounceEvent timeout
                                ++ sendChangeEvent
                    in
                    List.concat
                        [ [ attribute "oninput" onInputFn
                          , on "custom-debounce" <| Json.Decode.succeed onDebounce
                          ]
                        , case cfg.onChange of
                            Just onChange_ ->
                                [ on "custom-change" (Json.Decode.at [ "detail" ] Json.Decode.string |> Json.Decode.map onChange_) ]

                            Nothing ->
                                []
                        ]
    in
    Html.label
        [ tailwind
            [ text_grey_dark
            , block
            ]
        ]
        [ column { columnCfg | spacing = 2 } <|
            List.concat
                [ [ text cfg.label
                  , Html.input
                        ([ Html.Attributes.value cfg.value
                         , Html.Attributes.type_ cfg.type_
                         , Html.Attributes.placeholder cfg.placeholder
                         , tailwind <|
                            withClasses
                                [ "block"
                                , "p-3"

                                -- Fake red border on error state
                                , "w-full"
                                , "rounded"
                                , "focus-outline"
                                , "transition"
                                ]
                                [ if cfg.error /= "" then
                                    border_red
                                  else
                                    border_grey_dark
                                , border
                                ]
                         , on "keydown"
                            (keyCode
                                |> Json.Decode.andThen
                                    (\code ->
                                        case code of
                                            13 ->
                                                case cfg.onEnter of
                                                    Just msg ->
                                                        Json.Decode.succeed msg

                                                    Nothing ->
                                                        Json.Decode.fail ""

                                            27 ->
                                                case cfg.onEscape of
                                                    Just msg ->
                                                        Json.Decode.succeed msg

                                                    Nothing ->
                                                        Json.Decode.fail ""

                                            _ ->
                                                Json.Decode.fail ""
                                    )
                            )
                         ]
                            ++ debounceAttributes
                            ++ cfg.attributes
                        )
                        []
                  ]
                , if cfg.error /= "" then
                    [ wrapWithClasses [ "font-normal" ] <| errorText cfg.error
                    ]
                  else
                    []
                ]
        ]


type alias InputConfig msg =
    { bgColor : String
    , debounce : Debounce msg
    , label : String
    , onChange : Maybe (String -> msg)
    , onEnter : Maybe msg
    , onEscape : Maybe msg
    , placeholder : String
    , type_ : String
    , value : String
    , error : String
    , attributes : List (Html.Attribute msg)
    }


{-| -}
inputCfg : InputConfig msg
inputCfg =
    { type_ = "text"
    , bgColor = "grey-light"
    , debounce = NoDebounce
    , error = ""
    , label = ""
    , onChange = Nothing
    , onEnter = Nothing
    , onEscape = Nothing
    , value = ""
    , placeholder = ""
    , attributes = []
    }


{-| -}
inputCfg_text : InputConfig msg
inputCfg_text =
    { inputCfg | type_ = "text" }


{-| -}
inputCfg_password : InputConfig msg
inputCfg_password =
    { inputCfg | type_ = "password" }


testInput : Html a
testInput =
    input inputCfg



{-
   ~~~ BUTTONS ~~~ 👍👍
-}


{-| Used for passing to inputs to indicate that a message should be fired after a debounce interval
-}
type Debounce msg
    = NoDebounce
    | DebounceAfter Int msg


{-| Intended to just be a transparent button around other things that look like buttons, but aren't (for accessibilty purposes).
For example: logo image
-}
ghostButton : GhostButtonCfg msg -> Html msg -> Html msg
ghostButton cfg child =
    let
        clickMsg =
            case cfg.onClick of
                Just onClick_ ->
                    if cfg.disabled then
                        []
                    else
                        [ onClick onClick_ ]

                Nothing ->
                    []
    in
    Html.button
        ([ classes
            [ "border-0"
            , "p-0"
            , "cursor-pointer"
            , "rounded"
            , "bg-transparent"
            ]
         ]
            ++ cfg.attributes
            ++ clickMsg
        )
        [ child ]


{-| -}
type alias GhostButtonCfg msg =
    { ariaLabel : String
    , bgColor : String
    , border : Maybe { color : String, thickness : Int, disabledColor : String }
    , disabled : Bool
    , disabledBgColor : String
    , disabledTextColor : String
    , htmlNode : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , maxWidth : String
    , onClick : Maybe msg
    , paddingH : Int
    , paddingV : Int
    , textColor : String
    , attributes : List (Html.Attribute msg)
    }


{-| -}
ghostButtonCfg : GhostButtonCfg msg
ghostButtonCfg =
    buttonCfg


testGhostButton : Html msg -> Html msg
testGhostButton =
    ghostButton ghostButtonCfg


{-| -}
solidButton :
    { a
        | color : String
        , disabled : Bool
        , disabledColor : String
        , htmlNode : List (Html.Attribute msg) -> List (Html msg) -> Html msg
        , maxWidth : String
        , onClick : Maybe msg
        , paddingH : Int
        , paddingV : Int
    }
    -> Html msg
    -> Html msg
solidButton cfg child =
    button
        { buttonCfg
            | paddingV = cfg.paddingV
            , paddingH = cfg.paddingH
            , maxWidth = cfg.maxWidth
            , bgColor = cfg.color
            , textColor = "white"
            , disabledBgColor = cfg.disabledColor
            , disabledTextColor = "white"
            , onClick = cfg.onClick
            , htmlNode = cfg.htmlNode
            , disabled = cfg.disabled
            , border = Nothing
        }
        child


{-| -}
solidButtonCfg :
    { color : String
    , disabled : Bool
    , disabledColor : String
    , htmlNode : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , maxWidth : String
    , onClick : Maybe msg
    , paddingH : Int
    , paddingV : Int
    }
solidButtonCfg =
    { paddingV = 3
    , paddingH = 4
    , maxWidth = ""
    , color = "blue"
    , disabledColor = "grey-light"
    , onClick = Nothing
    , htmlNode = Html.button
    , disabled = False
    }


testSolidButton : Html a -> Html a
testSolidButton =
    solidButton solidButtonCfg


{-| -}
outlineButton : OutlineButtonCfg msg -> Html msg -> Html msg
outlineButton cfg child =
    button
        { buttonCfg
            | paddingV = cfg.paddingV
            , paddingH = cfg.paddingH
            , maxWidth = cfg.maxWidth
            , bgColor = "transparent"
            , textColor = cfg.color
            , disabledBgColor = "transparent"
            , disabledTextColor = cfg.disabledColor
            , onClick = cfg.onClick
            , htmlNode = cfg.htmlNode
            , disabled = cfg.disabled
            , border = Just { color = cfg.color, thickness = cfg.outlineThickness, disabledColor = cfg.disabledColor }
            , attributes = cfg.attributes
        }
        child


{-| -}
type alias OutlineButtonCfg msg =
    { paddingV : Int
    , paddingH : Int
    , maxWidth : String
    , color : String
    , disabledColor : String
    , onClick : Maybe msg
    , htmlNode : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , disabled : Bool
    , outlineThickness : Int
    , attributes : List (Html.Attribute msg)
    }


{-| -}
outlineButtonCfg : OutlineButtonCfg msg
outlineButtonCfg =
    { paddingV = 3
    , paddingH = 4
    , maxWidth = ""
    , color = "blue"
    , disabledColor = "white"
    , onClick = Nothing
    , htmlNode = Html.button
    , disabled = False
    , outlineThickness = 1
    , attributes = []
    }


testOutlineButton : Html a -> Html a
testOutlineButton =
    outlineButton outlineButtonCfg


{-| -}
button : ButtonCfg msg -> Html msg -> Html msg
button cfg child =
    let
        borderClasses =
            case cfg.border of
                Nothing ->
                    [ "border"
                    , if cfg.disabled then
                        "border-" ++ cfg.disabledBgColor
                      else
                        "border-" ++ cfg.bgColor
                    ]

                Just { color, thickness, disabledColor } ->
                    [ if cfg.disabled then
                        "border-" ++ disabledColor
                      else
                        "border-" ++ color
                    , if thickness == 1 then
                        "border"
                      else
                        "border-" ++ toString thickness
                    ]

        normalClasses =
            [ "py-" ++ toString cfg.paddingV
            , "px-" ++ toString cfg.paddingH
            , "w-full"
            , if cfg.maxWidth /= "" then
                "max-w-" ++ cfg.maxWidth
              else
                ""
            , "block"

            -- Text color
            , if cfg.disabled then
                "text-" ++ cfg.disabledTextColor
              else
                "text-" ++ cfg.textColor

            -- Background color
            , if cfg.disabled then
                "bg-" ++ cfg.disabledBgColor
              else
                "bg-" ++ cfg.bgColor
            , "rounded"
            , "cursor-pointer"
            , "focus-outline"
            , "transition"
            ]
    in
    cfg.htmlNode
        (List.concat
            [ [ classes (normalClasses ++ borderClasses) ]
            , case cfg.ariaLabel of
                "" ->
                    []

                other ->
                    [ Html.Attributes.attribute "aria-label" other
                    ]
            , case cfg.onClick of
                Just onClick_ ->
                    if cfg.disabled then
                        []
                    else
                        [ onClick onClick_ ]

                Nothing ->
                    []
            , cfg.attributes
            ]
        )
        [ child ]


{-| -}
type alias ButtonCfg msg =
    { ariaLabel : String
    , bgColor : String
    , border : Maybe { color : String, thickness : Int, disabledColor : String }
    , disabled : Bool
    , disabledBgColor : String
    , disabledTextColor : String
    , htmlNode : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , maxWidth : String
    , onClick : Maybe msg
    , paddingH : Int
    , paddingV : Int
    , textColor : String
    , attributes : List (Html.Attribute msg)
    }


{-| -}
buttonCfg : ButtonCfg msg
buttonCfg =
    { paddingV = 3
    , paddingH = 4
    , maxWidth = "full"
    , textColor = "white"
    , bgColor = "blue"
    , disabledBgColor = "grey-light"
    , disabledTextColor = "white"
    , onClick = Nothing
    , htmlNode = Html.button
    , disabled = False
    , border = Nothing
    , ariaLabel = ""
    , attributes = []
    }


testButton : Html a -> Html a
testButton =
    button buttonCfg



{-
   ~~~ BUILDING BLOCKS ~~~ 👍👍
-}


{-| -}
column :
    ColumnCfg msg
    -> List (Html msg)
    -> Html msg
column cfg children =
    let
        spacer =
            div [ classes [ "_Av__spacer", "h-" ++ toString cfg.spacing ] ] []

        filteredChildren =
            List.filter (\a -> a /= text "") children

        interwoven =
            List.intersperse spacer filteredChildren
    in
    div
        ([ tailwind <|
            withClasses
                [ "_Av__column" ]
                [ w_full ]
         ]
            ++ cfg.attributes
        )
        interwoven


{-| -}
columnCfg : ColumnCfg msg
columnCfg =
    { spacing = 1
    , attributes = []
    }


{-| -}
type alias ColumnCfg msg =
    { spacing : Int
    , attributes : List (Html.Attribute msg)
    }


testColumn : List (Html msg) -> Html msg
testColumn =
    column columnCfg


{-| -}
row :
    RowCfg msg
    -> List (Html msg)
    -> Html msg
row cfg children =
    rowWithCells cfg <| List.map autoCell children


{-| -}
rowWithCells :
    RowCfg msg
    -> List (Cell msg)
    -> Html msg
rowWithCells cfg children =
    let
        spacer =
            div
                [ classes <|
                    [ "_Av__spacer"
                    , (if cfg.stackWhenSmall then
                        "md:"
                       else
                        ""
                      )
                        ++ "w-"
                        ++ toString cfg.spacing
                    ]
                        ++ (if cfg.stackWhenSmall then
                                [ "h-" ++ toString cfg.spacing
                                , "md:h-0"
                                ]
                            else
                                []
                           )
                ]
                []

        filteredChildren =
            List.filter (\a -> a.content /= text "") children

        flexifiedChildren =
            List.map
                (\child ->
                    Html.div
                        [ style <|
                            case child.flex of
                                AutoFlex ->
                                    case cfg.arrangement of
                                        Expand ->
                                            [ ( "flex", "1" ) ]

                                        _ ->
                                            []

                                FlexRatio ratio ->
                                    [ ( "flex", toString ratio ) ]
                        ]
                        [ child.content ]
                )
                filteredChildren

        interwoven =
            if cfg.spacing == 0 then
                flexifiedChildren
            else
                List.intersperse spacer flexifiedChildren

        finalInlineStyles =
            [ style <|
                case cfg.arrangement of
                    Start ->
                        []

                    Middle ->
                        []

                    End ->
                        []

                    Expand ->
                        [ ( "justify-content", "stretch" ) ]

                    Isolate ->
                        []
            ]

        finalClasses =
            tailwind <|
                withClasses [ "_Av__row" ]
                    (List.concat
                        [ [ flex, w_full ]
                        , if cfg.stackWhenSmall then
                            [ flex_col, md flex_row ]
                          else
                            [ flex_row ]
                        , case cfg.arrangement of
                            Start ->
                                [ justify_start ]

                            Middle ->
                                [ justify_center ]

                            End ->
                                [ justify_end ]

                            Expand ->
                                [{- There's no tailwind class for justify-stretch, so we've got to
                                    apply that by hand in the "styles" section
                                 -}
                                ]

                            Isolate ->
                                [ justify_between ]
                        , case cfg.crossArrangement of
                            Start ->
                                [ items_start ]

                            Middle ->
                                [ items_center ]

                            End ->
                                [ items_end ]

                            Expand ->
                                [ items_stretch ]

                            Isolate ->
                                [{- This isn't great, but we just ignore this value, 'cause there is no cross isolate. -}]
                        ]
                    )
    in
    div ([ finalClasses ] ++ finalInlineStyles ++ cfg.attributes) interwoven


{-| -}
rowCfg : RowCfg msg
rowCfg =
    { stackWhenSmall = False
    , spacing = 1
    , arrangement = Start
    , crossArrangement = Start
    , attributes = []
    }


{-| -}
type alias RowCfg msg =
    { spacing : Int
    , stackWhenSmall : Bool
    , arrangement : Arrangement
    , crossArrangement : Arrangement
    , attributes : List (Html.Attribute msg)
    }


{-| -}
autoCell : Html msg -> Cell msg
autoCell content =
    { content = content, flex = AutoFlex }


{-| -}
ratioCell : Float -> Html msg -> Cell msg
ratioCell ratio content =
    { content = content, flex = FlexRatio ratio }


{-| -}
type alias Cell msg =
    { content : Html msg, flex : Flex }


{-| -}
type Flex
    = AutoFlex
    | FlexRatio Float


{-| -}
type Arrangement
    = Expand
    | Isolate
    | Start
    | End
    | Middle


{-| -}
el : List (Html.Attribute msg) -> Html msg -> Html msg
el attrs child =
    div attrs [ child ]


testRow : List (Html msg) -> Html msg
testRow =
    row rowCfg


{-| -}
errorText : String -> Html msg
errorText text =
    Html.span [ classes [ "text-red" ] ] <|
        [ Html.text text ]


{-| -}
headline : { a | color : String } -> Html msg -> Html msg
headline cfg child =
    Html.h1
        [ classes
            [ "text-5xl"
            , "sm:text-3xl"
            , "text-" ++ cfg.color
            ]
        ]
        [ child ]


{-| -}
headlineCfg : { color : String }
headlineCfg =
    { color = "grey-dark"
    }


testHeadline : Html msg -> Html msg
testHeadline =
    headline headlineCfg


{-| -}
subheadline : { a | color : String } -> Html msg -> Html msg
subheadline cfg child =
    Html.h1
        [ classes
            [ "text-3xl"
            , "sm:text-xl"
            , "text-" ++ cfg.color
            ]
        ]
        [ child ]


{-| -}
subheadlineCfg : { color : String }
subheadlineCfg =
    { color = "grey-dark"
    }


testSubheadline : Html msg -> Html msg
testSubheadline =
    subheadline subheadlineCfg


{-| -}
paragraph : { a | color : String } -> Html msg -> Html msg
paragraph cfg content =
    Html.p
        [ classes
            [ cfg.color
            , "font-normal"
            , "leading-normal"
            ]
        ]
        [ content ]


{-| -}
paragraphCfg : { color : String }
paragraphCfg =
    { color = "dark-grey" }


testParagraph : Html msg -> Html msg
testParagraph =
    paragraph paragraphCfg


{-| -}
link : { a | color : String, target : String } -> Html msg -> Html msg
link cfg child =
    Html.a
        [ classes
            [ "text-" ++ cfg.color
            , "font-normal"
            ]
        , href cfg.target
        ]
        [ child ]


{-| -}
linkCfg : { color : String, target : String }
linkCfg =
    { color = "blue"
    , target = "#"
    }


testLink : Html msg -> Html msg
testLink =
    link linkCfg


{-| -}
img : { a | alt : String, src : String } -> Html msg
img cfg =
    Html.img [ Html.Attributes.src cfg.src, Html.Attributes.alt cfg.alt ] []


{-| -}
imgCfg : { alt : String, src : String }
imgCfg =
    { src = "A broken image", alt = "Tell this developer to pay attention to accessibilty" }


{-| -}
wrapWithClasses : List String -> Html msg -> Html msg
wrapWithClasses cs child =
    div [ classes ("__Av_class_wrapper" :: cs) ] [ child ]


{-| -}
wrapWithTailwind : List TailwindClass -> Html msg -> Html msg
wrapWithTailwind cs child =
    div [ tailwind cs, class "__Av_tailwind_wrapper" ] [ child ]


{-| -}
empty : Html msg
empty =
    Html.text ""


classes : List String -> Html.Attribute msg
classes list =
    Html.Attributes.class <| String.join " " list
