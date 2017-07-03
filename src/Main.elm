module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (href, class, style)
import Http
import Http exposing (get, Error, Response, Error(..))
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Json.Decode as Decode exposing (Decoder)
import BasicAuth exposing (..)
import Auth exposing (..)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Layout as Layout
import Material.Color as Color
import Material.List as Lists
import Material.Icon as Icon


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Ticket =
    { display_id : Int
    , subject : String
    , requester_name : String
    , description : String
    }


type alias Model =
    { query : String
    , tickets : List Ticket
    , error : Maybe String
    , mdl : Material.Model
    , selectedTab : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" [] Nothing Material.model 0, Cmd.none )


decoder : Decoder (List Ticket)
decoder =
    Decode.at [] (Decode.list ticketDecoder)


ticketDecoder : Decoder Ticket
ticketDecoder =
    decode Ticket
        |> required "display_id" Decode.int
        |> required "subject" Decode.string
        |> required "requester_name" Decode.string
        |> required "description" Decode.string


type alias Mdl =
    Material.Model



-- HTTP


getTickets : String -> Cmd Msg
getTickets query =
    let
        url =
            "https://cimat.freshdesk.com/helpdesk/tickets?format=json"

        request =
            Http.request
                { method = "GET"
                , headers =
                    [ Http.header "Content-Type" "application/json"
                    , buildAuthorizationHeader usuario password
                    ]
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson decoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send LoadTickets request



-- UPDATE


type Msg
    = Search
    | LoadTickets (Result Http.Error (List Ticket))
    | Mdl (Material.Msg Msg)
    | SelectTab Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search ->
            ( model, getTickets model.query )

        LoadTickets tickets ->
            case tickets of
                Ok tickets ->
                    ( { model | tickets = tickets }, Cmd.none )

                Err err ->
                    Debug.crash "" err

        Mdl msg_ ->
            Material.update Mdl msg_ model

        SelectTab num ->
            { model | selectedTab = num } ! []



-- VIEW


view : Model -> Html Msg
view model =
    Material.Scheme.topWithScheme
        Color.Teal
        Color.LightGreen
    <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            , Layout.fixedDrawer
              --, Layout.onSelectTab SelectTab
              --, Layout.selectedTab model.selectedTab
            ]
            { header = [ h4 [ style [ ( "padding", "1px" ) ] ] [ text "Helpdesk" ] ]
            , drawer =
                [ Icon.view "account_circle" [ Icon.size48 ]
                , Lists.ul
                    []
                    [ Lists.li []
                        [ Lists.content []
                            [ Lists.icon "label_outline" [ Options.attribute <| Html.Events.onClick (Search) ]
                            , text "Tickets"
                            ]
                        ]
                    , Lists.li []
                        [ Lists.content []
                            [ Lists.icon "add" []
                            , text "Agregar"
                            ]
                        ]
                    ]
                ]
            , tabs = ( [], [] )
            , main = [ viewBody model ]
            }


row : Model -> String -> String -> String -> Html Msg
row model subject requester_name description =
    let
        k =
            7
    in
        Lists.li [ Lists.withBody ]
            [ Lists.content []
                --[ Lists.avatarIcon "photo_camera" []
                [ Icon.view "account_circle" [ Icon.size48 ]
                , text subject
                , Lists.body []
                    [ Options.span [] [ text "From:" ]
                    , Options.span [ css "font-weight" "600" ] [ text requester_name ]
                    , Options.span [] [ text "—" ]
                    , Options.span [] [ text description ]
                    ]
                ]
            , Lists.content2 []
                [ Lists.info2 [] []
                , Button.render Mdl
                    [ k ]
                    model.mdl
                    []
                    [ Icon.i "star_border"
                    ]
                ]
            ]


viewBody : Model -> Html Msg
viewBody model =
    case model.selectedTab of
        0 ->
            div []
                [ Lists.ul [ css "margin" "0", css "padding" "0" ]
                    (List.map
                        (\{ subject, requester_name, description } -> row model subject requester_name description)
                        model.tickets
                    )
                ]

        --[ Lists.ul [ css "margin" "0", css "padding" "0" ] (List.indexedMap (mail model.tickets) model) ]
        1 ->
            text "sw "

        _ ->
            text "404"
