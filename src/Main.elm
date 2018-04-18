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
    , recipientName : String
    , recipientLocation : RecipientLocation
    , currentStatus : Status
    , currentLocation : Location

    -- Added in order to track selected edit status. Could separate model for shipping order from this form field.
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
    | InTransitToRecipient
    | DeliveredToRecipient
    | InTransitReturn Reason
    | Faulty Reason ReasonCode


type StatusValidation
    = Valid
      -- | InvalidToState
    | InTransitReturnReasonNotSpecified
    | FaultyReasonNotSpecified
    | FaultyReasonCodeNotSpecified
    | FaultyReasonAndReasonCodeNotSpecified


validateStatus : Status -> StatusValidation
validateStatus status =
    case status of
        Processing ->
            Valid

        InTransitToRecipient ->
            -- validateInTransitToClientStatus organization
            Valid

        DeliveredToRecipient ->
            -- TODO: Add location/delivered by.
            Valid

        InTransitReturn reason ->
            validateInTransitReturnStatus reason

        Faulty reason reasonCode ->
            validateFaultyStatus reason reasonCode


validateInTransitReturnStatus : Reason -> StatusValidation
validateInTransitReturnStatus reason =
    case reason of
        "" ->
            InTransitReturnReasonNotSpecified

        otherReasons ->
            Valid


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
      , recipientName = "Harry Potter"
      , recipientLocation = "4 Privet Drive"
      , currentStatus = Processing
      , currentLocation = "Hogwarts warehouse"
      , editStatus = Processing
      }
    , Cmd.none
    )



-- View


view : Model -> Html Msg
view model =
    viewAppContainer []
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
            , dt [] [ text "Current location" ]
            , dd [] [ text model.currentLocation ]
            ]
        , viewEditStatusForm model
        ]


viewAppContainer : List (Attribute msg) -> List (Html msg) -> Html msg
viewAppContainer attributes children =
    div attributes
        [ div [ class "container" ]
            [ div [ class "row main align-items-center justify-content-md-center" ]
                [ div [ class "col-md-auto" ]
                    [ div [ class "card" ]
                        [ div [ class "card-body" ] children
                        ]
                    ]
                ]
            ]
        ]


viewEditStatusForm : Model -> Html Msg
viewEditStatusForm model =
    div []
        [ h2 [] [ text "Edit status" ]
        , label []
            [ text "Set status"
            , viewStatusOptions model
            ]
        , div []
            [ viewEditStatusFormAdditionalInputs model
            ]
        , dl []
            [ dt [] [ text "New status" ]
            , dd []
                [ text
                    (model.editStatus |> viewStatusText model)
                ]
            ]
        , button
            [ class "btn btn-primary"
            , disabled <| not (statusIsValid model.editStatus)
            , onClick UpdateCurrentStatusToEditStatus
            ]
            [ text "Update status" ]
        ]


viewEditStatusFormAdditionalInputs : Model -> Html Msg
viewEditStatusFormAdditionalInputs model =
    case model.editStatus of
        Processing ->
            text ""

        InTransitToRecipient ->
            text ""

        DeliveredToRecipient ->
            text ""

        InTransitReturn reason ->
            label []
                [ text "Reason"

                -- TODO: Replace br with form-group.
                , br [] []
                , input
                    [ type_ "text"
                    , value reason
                    , placeholder "my dog chewed it up"
                    , onInput (\reasonInput -> UpdateEditStatus (InTransitReturn reasonInput))
                    ]
                    []
                ]

        Faulty reason reasonCode ->
            div []
                [ label []
                    [ text "Reason"
                    , br [] []
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
                    , br [] []
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

        InTransitToRecipient ->
            "In transit to " ++ model.recipientName

        DeliveredToRecipient ->
            "Delivered to " ++ model.recipientLocation

        InTransitReturn reason ->
            "In transit to shipper cuz of " ++ reason

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
    , InTransitToRecipient
    , DeliveredToRecipient
    , InTransitReturn ""
    , Faulty "" 0
    ]


statusOptionWithLabel : Status -> ( Status, String )
statusOptionWithLabel statusOptionValue =
    ( statusOptionValue, statusOptionToLabel statusOptionValue )


statusOptionToLabel : Status -> String
statusOptionToLabel status =
    case status of
        Processing ->
            "Processing"

        InTransitToRecipient ->
            "In transit to recipient"

        DeliveredToRecipient ->
            "Delivered"

        InTransitReturn reason ->
            "In transit return"

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
                |> Debug.log "options with labels"

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
    = UpdateCurrentStatusToEditStatus
    | UpdateEditStatus Status
    | UpdateEditStatusFaultyReasonCode Reason String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCurrentStatusToEditStatus ->
            updateCurrentStatus model.editStatus model ! []

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


updateCurrentStatus : Status -> Model -> Model
updateCurrentStatus newStatus model =
    let
        modelWithOtherFieldsUpdatedFromStatus =
            setOtherFieldsFromStatusChange newStatus model
    in
        { modelWithOtherFieldsUpdatedFromStatus | currentStatus = newStatus }


setOtherFieldsFromStatusChange : Status -> Model -> Model
setOtherFieldsFromStatusChange newStatus model =
    case newStatus of
        Processing ->
            { model | currentLocation = "Hogwarts warehouse" }

        InTransitToRecipient ->
            { model | currentLocation = "en route" }

        DeliveredToRecipient ->
            { model | currentLocation = model.recipientLocation }

        InTransitReturn reason ->
            { model | currentLocation = "en route" }

        Faulty reason reasonCode ->
            { model | currentLocation = "Hogwarts warehouse" }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
