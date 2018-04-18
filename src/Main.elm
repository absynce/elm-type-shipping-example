module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time)
import Time.DateTime as DateTime


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { deliveryBy : String
    , deliveryOn : Time
    , recipientLocation : RecipientLocation
    , currentStatus : Status
    , currentLocation : Location
    , editStatus : Status
    }


type alias Location =
    String


type alias RecipientLocation =
    String


type alias Organization =
    String


type alias Reason =
    String


type alias ReasonCode =
    Int


type Status
    = Processing
    | InTransitToClient Organization
    | Delivered
    | InTransitReturn Reason
    | Faulty Reason ReasonCode


type StatusValidation
    = Valid
      -- | InvalidToState
    | FaultyReasonNotSpecified
    | FaultyReasonCodeNotSpecified
    | FaultyReasonAndReasonCodeNotSpecified


validateStatus : Status -> StatusValidation
validateStatus status =
    case status of
        Processing ->
            Valid

        InTransitToClient organization ->
            -- validateInTransitToClientStatus organization
            Valid

        Delivered ->
            Valid

        -- TODO: Add location.
        InTransitReturn reason ->
            -- validateInTransitReturnStatus reason
            Valid

        Faulty reason reasonCode ->
            validateFaultyStatus reason reasonCode


validateFaultyStatus : Reason -> ReasonCode -> StatusValidation
validateFaultyStatus reason reasonCode =
    case ( reason, reasonCode ) of
        ( "", 0 ) ->
            FaultyReasonAndReasonCodeNotSpecified

        ( "", otherReasonCodes ) ->
            FaultyReasonNotSpecified

        ( otherReasons, 0 ) ->
            FaultyReasonCodeNotSpecified

        ( otherReasons, otherReasonCodes ) ->
            Valid


statusIsValid : Status -> Bool
statusIsValid status =
    case validateStatus status of
        Valid ->
            True

        notValid ->
            False



-- Init


init : ( Model, Cmd Msg )
init =
    ( { deliveryBy = ""
      , deliveryOn = 0
      , recipientLocation = ""
      , currentStatus = Processing
      , currentLocation = "IWT"
      , editStatus = Processing
      }
    , Cmd.none
    )



-- View


view : Model -> Html Msg
view model =
    div []
        [ dl []
            [ dt [] [ text "Delivery on" ]
            , dd []
                [ text
                    (model.deliveryOn
                        |> DateTime.fromTimestamp
                        |> DateTime.toISO8601
                    )
                ]
            , dt [] [ text "Status" ]
            , dd []
                [ text
                    (model.currentStatus |> viewStatusText model)
                ]
            ]
        , viewEditStatusForm model
        ]


viewEditStatusForm : Model -> Html Msg
viewEditStatusForm model =
    div []
        [ h2 [] [ text "Edit status" ]
        , label []
            [ text "Set status"
            , viewStatusOptions model
            ]
        , viewEditStatusFormAdditionalInputs model
        , dl []
            [ dt [] [ text "New status" ]
            , dd []
                [ text
                    (model.editStatus |> viewStatusText model)
                ]
            ]
        , button
            [ disabled <| not (statusIsValid model.editStatus)
            ]
            [ text "Update status" ]
        ]


viewEditStatusFormAdditionalInputs : Model -> Html Msg
viewEditStatusFormAdditionalInputs model =
    case model.editStatus of
        Processing ->
            text ""

        InTransitToClient organization ->
            div []
                [ input
                    [ type_ "text"
                    , value organization
                    ]
                    []
                ]

        Delivered ->
            text ""

        InTransitReturn reason ->
            Debug.crash "TODO: Add InTransitReturn reason check"

        Faulty reason reasonCode ->
            div []
                [ label []
                    [ text "Reason"
                    , input
                        [ type_ "text"
                        , value reason
                        , placeholder "My dog chewed it up"
                        , onInput (\reasonInput -> UpdateEditStatus (Faulty reasonInput reasonCode))
                        ]
                        []
                    ]
                , label []
                    [ text "Reason code"
                    , input
                        [ type_ "number"
                        , value (toString reasonCode)
                        , onInput (UpdateEditStatusFaultyReasonCode reason)
                        ]
                        []
                    ]
                ]


viewStatusText : Model -> Status -> String
viewStatusText model status =
    case status of
        Processing ->
            "Processing"

        InTransitToClient organization ->
            "In transit to " ++ organization

        Delivered ->
            "Delivered to " ++ model.currentLocation

        InTransitReturn reason ->
            "In transit to IWT cuz of " ++ reason

        Faulty reason reasonCode ->
            "Faulty cuz of " ++ reason ++ " (code: " ++ toString reasonCode ++ ")"


viewStatusOptions : Model -> Html Msg
viewStatusOptions model =
    editStatusOptions model
        |> List.map statusOptionWithLabel
        |> selectFromValuesWithLabels Processing UpdateEditStatus


editStatusOptions : Model -> List Status
editStatusOptions model =
    [ Processing
    , InTransitToClient ""
    , Delivered
    , InTransitReturn "[enter reason]"
    , Faulty "" 0
    ]


statusOptionWithLabel : Status -> ( Status, String )
statusOptionWithLabel statusOptionValue =
    ( statusOptionValue, statusOptionToString statusOptionValue )


statusOptionToString : Status -> String
statusOptionToString status =
    case status of
        Processing ->
            "Processing"

        InTransitToClient organization ->
            "In transit to client"

        Delivered ->
            "Delivered"

        InTransitReturn reason ->
            "In transit return " ++ reason

        Faulty reason reasonCode ->
            "Faulty"


{-| From <https://gurdiga.github.io/blog/2017/07/09/select-from-union-type-in-elm/>

Adapted to match pipeline style (moved valuesWithLabels to last argument).

-}
selectFromValuesWithLabels : a -> (a -> msg) -> List ( a, String ) -> Html msg
selectFromValuesWithLabels defaultValue callback valuesWithLabels =
    let
        optionForTuple ( value, label ) =
            option [ selected (defaultValue == value) ] [ text label ]

        options valuesWithLabels defaultValue =
            List.map optionForTuple valuesWithLabels

        maybeValueFromLabel l =
            List.filter (\( value, label ) -> label == l) valuesWithLabels
                |> List.head

        valueFromLabel label =
            case maybeValueFromLabel label of
                Nothing ->
                    defaultValue

                Just ( value, label ) ->
                    value
    in
        select
            [ onInput (callback << valueFromLabel) ]
            (options valuesWithLabels defaultValue)



-- Update


type Msg
    = NoOp
    | UpdateCurrentStatus Status
    | UpdateEditStatus Status
    | UpdateEditStatusFaultyReasonCode Reason String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateCurrentStatus newCurrentStatus ->
            { model | currentStatus = newCurrentStatus } ! []

        UpdateEditStatus newEditStatus ->
            { model | editStatus = newEditStatus } ! []

        UpdateEditStatusFaultyReasonCode reason reasonCodeInput ->
            let
                reasonCodeInputInt =
                    Maybe.withDefault 0 (reasonCodeInput |> String.toInt |> Result.toMaybe)

                newEditStatus =
                    Faulty reason reasonCodeInputInt
            in
                { model | editStatus = newEditStatus } ! []



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
