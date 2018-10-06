module Main exposing (..)

import Html exposing (Html, text, div, ul, li, a)
import Html.Attributes exposing (href)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { stories : List Story
    , message : String
    }


type alias Field =
    { name : String
    , query : Query
    }


type Query
    = Query (List Field)


type alias Story =
    { id : String
    , title : String
    , url : Maybe String
    }


type Msg
    = FetchHNTopStories (Result Http.Error (List Story))


field : String -> List Field -> Field
field name fields =
    Field name (Query fields)


fieldToString : Field -> String
fieldToString { name, query } =
    name ++ " " ++ queryToString query


queryToString : Query -> String
queryToString (Query query) =
    if List.isEmpty query then
        ""
    else
        let
            str =
                List.map fieldToString query
                    |> List.foldr (++) ""
        in
            "{ " ++ str ++ " }"


topStoriesQuery : Query
topStoriesQuery =
    Query
        [ field "hn"
            [ field "topStories"
                [ field "id" []
                , field "title" []
                , field "url" []
                ]
            ]
        ]


storyDecoder : Decode.Decoder Story
storyDecoder =
    Pipeline.decode Story
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "title" Decode.string
        |> Pipeline.optional "url" (Decode.nullable Decode.string) Nothing


request : Http.Request (List Story)
request =
    let
        encoded =
            queryToString topStoriesQuery
                |> Http.encodeUri

        decoder =
            Decode.at [ "data", "hn", "topStories" ] <|
                Decode.list storyDecoder
    in
        Http.get ("https://www.graphqlhub.com/graphql?query=" ++ encoded) decoder


init : ( Model, Cmd Msg )
init =
    { stories = []
    , message = "Waiting for a response... "
    }
        ! [ Http.send FetchHNTopStories request ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchHNTopStories (Ok stories) ->
            { model | stories = stories, message = "Got stories!" } ! []

        FetchHNTopStories (Err res) ->
            { model | message = toString res } ! []


listItem : Story -> Html Msg
listItem { id, title, url } =
    let
        url_ =
            Maybe.withDefault ("https://news.ycombinator.com/item?id=" ++ id) url
    in
        li []
            [ a [ href url_ ] [ text title ]
            ]


view : Model -> Html Msg
view model =
    let
        items =
            List.map listItem model.stories

        storiesList =
            ul [] items
    in
        div [] [ text model.message, storiesList ]