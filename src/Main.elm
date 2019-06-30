module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Dict exposing (..)
import Dict.Extra exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Encode exposing (Value)


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MODEL


type Viewing
    = Question
    | Tendacy
    | Failure
    | Loading
    | Result


type alias Model =
    { answers : Array ResultType
    , viewing : Viewing
    , name : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { answers = Array.repeat 30 None
      , viewing = Question
      , name = ""
      }
    , Cmd.none
    )


type ResultType
    = A
    | B
    | C
    | D
    | E
    | None


resultToString : ResultType -> String
resultToString m =
    case m of
        A ->
            "Apostles"

        B ->
            "Prophets"

        C ->
            "Evangelists"

        D ->
            "Shepherds"

        E ->
            "Teachers"

        None ->
            ""


stringToResult : String -> ResultType
stringToResult str =
    case str of
        "Apostles" ->
            A

        "Prophets" ->
            B

        "Evangelists" ->
            C

        "Shepherds" ->
            D

        "Teachers" ->
            E

        _ ->
            None


resultToText : ResultType -> String
resultToText m =
    case m of
        A ->
            "使徒"

        B ->
            "預言者"

        C ->
            "伝道者"

        D ->
            "牧会"

        E ->
            "教師"

        None ->
            "エラーが発生しました。"


getSelectedNum : ResultType -> Array ResultType -> Int
getSelectedNum check a =
    a
        |> Array.foldl
            (\m sum ->
                if m == check then
                    sum + 1

                else
                    sum
            )
            0


findNo1SelectedResultType : Array ResultType -> List ResultType
findNo1SelectedResultType a =
    let
        item =
            [ A, B, C, D, E ]
                |> List.map (\m -> ( getSelectedNum m a, m ))
                |> Dict.Extra.groupBy (\( num, _ ) -> num)
                |> Dict.toList
                |> sortByFirst
                |> List.head
    in
    case item of
        Just ( _, lm ) ->
            List.map (\( _, m ) -> m) lm

        _ ->
            [ None ]


sortByFirst : List ( comparable, b ) -> List ( comparable, b )
sortByFirst =
    List.sortBy Tuple.first >> List.reverse


findNo2SelectedResultType : Array ResultType -> List ResultType
findNo2SelectedResultType a =
    let
        item =
            [ A, B, C, D, E ]
                |> List.map (\m -> ( getSelectedNum m a, m ))
                |> Dict.Extra.groupBy (\( num, _ ) -> num)
                |> Dict.toList
                |> sortByFirst
                |> List.take 2
                |> List.reverse
                |> List.head
    in
    case item of
        Just ( _, lm ) ->
            List.map
                (\( n, m ) ->
                    if n == 0 then
                        None

                    else
                        m
                )
                lm

        _ ->
            [ None ]



-- UPDATE


type Msg
    = Select Int String
    | InputName String
    | Submit
    | NextView Viewing
    | Response (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select index str ->
            ( { model | answers = model.answers |> Array.set index (stringToResult str) }, Cmd.none )

        InputName str ->
            ( { model | name = str }, Cmd.none )

        Submit ->
            if model.name == "" then
                ( { model | viewing = Result }, Cmd.none )

            else
                ( { model | viewing = Loading }, postResult model )

        NextView v ->
            ( { model | viewing = v }, Cmd.none )

        Response r ->
            case r of
                Ok _ ->
                    ( { model | viewing = Result }, Cmd.none )

                Err _ ->
                    ( { model | viewing = Failure }, Cmd.none )



-- HTTP


postResult : Model -> Cmd Msg
postResult model =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Accept" "application/json", Http.header "Content-Type" "application/json" ]
        , url = "/api/"
        , body = Http.jsonBody (makeJsonBody model)
        , expect = Http.expectString Response
        , timeout = Nothing
        , tracker = Nothing
        }


makeJsonBody : Model -> Json.Encode.Value
makeJsonBody model =
    let
        ansNums =
            [ A, B, C, D, E ] |> List.map (\m -> ( resultToString m, Json.Encode.int (getSelectedNum m model.answers) ))
    in
    Json.Encode.object (( "name", Json.Encode.string model.name ) :: ansNums)



-- VIEW


view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ h1 [ class "title" ] [ text "5役者の賜物の査定" ]
            , p [ class "content" ] [ text "下記1~30には、2つの主張(性向)が併記されています。そのうち、自分のことだと思う方を選んでください。" ]
            , div [ class "level" ]
                [ div [ class "level-left" ]
                    [ a [ onClick (NextView Tendacy), class "button level-item" ] [ text "性向一覧確認" ]
                    , a [ class "button level-item", href "https://docs.google.com/spreadsheets/d/e/2PACX-1vQZC30ieu6mK5MD7Lne1DQT9TAWdHxhJHJo_6GaGYdKK8r7gPmX1U_DPwdq6A96g5Bk60udub05jYwq/pubhtml", target "_blank" ] [ text "他の人の結果を見る(HTML)" ]
                    , a [ class "button level-item", href "https://docs.google.com/spreadsheets/d/1Idl-EoBYmy7eSlYtQ0PNwy9-FZXXbWdYW6JjEdxu9d4/edit?usp=sharing", target "_blank" ] [ text "他の人の結果を見る(スプレッドシート)" ]
                    ]
                ]
            , div [ class "level", style "margin-bottom" "0px" ]
                [ div [ class "level-left" ]
                    [ p [ class "level-item" ] [ text "名前" ]
                    , div [ class "level-item" ]
                        [ input [ class "input", placeholder "名前", value model.name, onInput InputName ] []
                        ]
                    ]
                ]
            , p [ class "content is-small", style "margin-bottom" "0px" ] [ text "名前を入力すると結果が保存されます。結果を保存したくない場合は名前を入力しないでください。" ]
            , p [ class "content is-small" ] [ text "結果は１ヶ月経過すると自動的に消去されます。" ]
            , table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "No" ]
                        , th [ colspan 2 ] [ text "主張(性向)" ]
                        ]
                    ]
                , tfoot [] [ tr [] [ td [ colspan 3 ] [] ] ]
                , tbody []
                    (row 1 "私はリーダーの動きに合わせるようにしています。" "私は人と一緒に問題を克服するのが好きです。" A D
                        ++ row 2 "私は具体的な答えを得るために、具体的な質問をします。" "私は学んだことを明確に表現し、人々が理解できるように取り組みをするのが好きです。" B E
                        ++ row 3 "私は人が幸せでいるかどうか知りたいです。" "私は人々が成熟し、成長することにワクワクします。" C A
                        ++ row 4 "よく人から相談とサポートを求められます。" "私はできる限り正確に何かをすることが好きです。" D E
                        ++ row 5 "私はビジョンや夢を定期的に書き留めます。" "私は人に良いことを伝える方法を様々に考えます。" B C
                        ++ row 6 "私は人を見る目があると言われています。" "人生の歩みの中で、軸となる価値観は 、私にとって不可欠です。" B E
                        ++ row 7 "私は意見の一致が状況を改善する主要な方法だと考えています。" "私は意見を聞いて、 他の意見を知ることが状況を改善する方法だと信じています。" A B
                        ++ row 8 "私は物事を黒か白のどちらかだと見なす傾向があります。" "私は大きな絵(ビジョン)を掲げ続ける能力があります。" E A
                        ++ row 9 "私は正しいことを中心に据える傾向があります。" "私は離れてしまった人たちのことを、頻繁に思い出し考えています。" E C
                        ++ row 10 "私は人々が成熟するのを助け、成長することを優先します。" "私は人々の心を癒やし、助けたりするのが大好きです。" A D
                        ++ row 11 "私は利他の心がない人のために時々泣いています。" "私は他の人たちが成長し、仕事ができるように成長させるのが好きです。" C A
                        ++ row 12 "私は人の話を聞いて質問するのが大好きです。" "私は人々がアドバイスを聞けるように助けるのが好きです。" D B
                        ++ row 13 "人々を運命(目的)に導くことは最も重要なことです。" "人を愛することは最も重要なことです。" A D
                        ++ row 14 "私は他の人のためにアドバイスを聞き求めます。" "私は批判的にはなりたくないですが、人々は自分自身に正直でなければならないと思います。" B E
                        ++ row 15 "私はしばしば奇跡が起こるように願い、人々が癒やされるのを見ます。" "私は人々が責任を持ち、正しい情報の中に留まることに情熱を持っています。" A E
                        ++ row 16 "勉強するとき、私は知るということに飢え乾いています。" "私は考えるとき、よく他の人のことについて、思いつきます。" E B
                        ++ row 17 "人は私について、たいへん辛抱強く、人の世話をしていると言います。" "私は好きなことを遠くへ伝えに行くことが大好きです。" D C
                        ++ row 18 "私はネットワーク化と人をつなぐのが好きです。" "私は人々が夢とビジョンを具体的になるよう助けます。" A B
                        ++ row 19 "私はテーマについてすべてを学ぶのが好きです。" "私は平和が好きで、人々が愛し合える方法を見せます。" E D
                        ++ row 20 "私は人々を慰めて励ますのが大好きです。" "私は一貫して利他の心がない人を考えています。" D C
                        ++ row 21 "私はよく他人を励ますために個人的な言葉を語ります。" "私は一度だけ会った人でも、その人のことを心の中で思っています。" B D
                        ++ row 22 "私は好きなことを、できる限りシェアします。" "私は人々が集まるグループに焦点を当て、そのグループに近づく戦略を立てます。" C A
                        ++ row 23 "私は罪ある人達を見ると、すぐに「救いが必要だ」と考えます。" "映画のキャラクターであっても、すぐに感情移入します。" C D
                        ++ row 24 "私は思い付きを言葉にし、人々やグループに宣言します。" "私は私にとって重要なことは、誰にでもシェアするのが大好きです。" B C
                        ++ row 25 "会議やイベントに出席していない人のことを気にかけている自分によく気がつきます。" "私は地上と天国のつながりが非常によくわかります。" D B
                        ++ row 26 "私は基盤となる考え方と原則に焦点を当てます。" "私は新しい人に会い、新しい状況が好きです。" E C
                        ++ row 27 "私は新しいことを始めて、仲間を募集するのが好きです。" "私には、日々、新鮮な情報を受けることが重要です。" A B
                        ++ row 28 "私はタイミングがあれば、熱心に私の好きなことを分かち合います。" "私は物事を秩序に従って整理して、明確にするのが好きです。" C E
                        ++ row 29 "人々は私のところに来て、サポートと助けを求めます。" "私は他の人を訓練して、働きに解き放つのが好きです。" D A
                        ++ row 30 "私は人々が正しい情報を保つのを助けます。" "私はイベントで一緒に集うために、人々を招くのが得意です。" E C
                    )
                ]
            , button
                [ disabled
                    (let
                        canSubmit =
                            Array.toList >> List.all (\m -> m /= None)
                     in
                     not (canSubmit model.answers)
                    )
                , onClick Submit
                , if model.viewing == Loading then
                    class "button is-loading"

                  else
                    class "button"
                ]
                [ text "回答" ]

            {- for debug
               , div [] [ text (String.fromInt (model.answers |> getSelectedNum A)) ]
               , div [] [ text (String.fromInt (model.answers |> getSelectedNum B)) ]
               , div [] [ text (String.fromInt (model.answers |> getSelectedNum C)) ]
               , div [] [ text (String.fromInt (model.answers |> getSelectedNum D)) ]
               , div [] [ text (String.fromInt (model.answers |> getSelectedNum E)) ]
            -}
            ]
        , viewResult model
        , viewTendacy model
        , viewFailure model
        ]


viewResult : Model -> Html Msg
viewResult model =
    div (onClick (NextView Question) :: modal (model.viewing == Result))
        [ div [ class "modal-background" ] []
        , div [ class "modal-content modal-card" ]
            [ Html.header [ class "modal-card-head" ]
                [ p [ class "modal-card-title" ] [ text "診断結果" ]
                , button [ class "modal-button-close delete" ] []
                , br [] []
                ]
            , section [ class "modal-card-body" ]
                [ div [ class "columns" ]
                    [ div [ class "column" ]
                        (p [ class "content is-size-5" ] [ text "あなたの一次的な5役者の賜物は・・・" ]
                            :: create1stResult model.answers
                            ++ create2ndResult model.answers
                            ++ [ createResultGraph model.answers ]
                        )
                    ]
                ]
            , footer [ class "modal-card-foot" ]
                [ button [ class "button modal-card-close" ] [ text "閉じる" ]
                ]
            ]
        ]


viewTendacy : Model -> Html Msg
viewTendacy model =
    div (onClick (NextView Question) :: modal (model.viewing == Tendacy))
        [ div [ class "modal-background" ] []
        , div [ class "modal-content modal-card" ]
            [ Html.header [ class "modal-card-head" ]
                [ p [ class "modal-card-title" ] [ text "性向一覧" ]
                , button [ class "modal-button-close delete" ] []
                , br [] []
                ]
            , section [ class "modal-card-body" ]
                [ div [ class "columns" ]
                    [ div [ class "column" ] ([ A, B, C, D, E ] |> List.map makeResultView)
                    ]
                ]
            , footer [ class "modal-card-foot" ]
                [ button [ class "button modal-card-close" ] [ text "閉じる" ]
                ]
            ]
        ]


viewFailure : Model -> Html Msg
viewFailure model =
    div (onClick (NextView Question) :: modal (model.viewing == Failure))
        [ div [ class "modal-background" ] []
        , div [ class "modal-content modal-card" ]
            [ Html.header [ class "modal-card-head" ]
                [ p [ class "modal-card-title has-text-danger" ] [ text "エラーが発生しました！！" ]
                , button [ class "modal-button-close delete" ] []
                , br [] []
                ]
            , section [ class "modal-card-body" ]
                [ p [ class "content" ] [ text "サーバーとの通信エラーが発生しました。" ]
                , p [ class "content" ] [ text "名前を未入力にして試すか、時間を置いてもう一度試してみてください。" ]
                ]
            , footer [ class "modal-card-foot" ]
                [ button [ class "button modal-card-close" ] [ text "閉じる" ]
                ]
            ]
        ]


row : Int -> String -> String -> ResultType -> ResultType -> List (Html Msg)
row index q1 q2 m1 m2 =
    [ tr []
        [ th [ rowspan 2, style "vertical-align" "middle" ] [ text (String.fromInt index) ]
        , td [] [ input [ type_ "radio", name (String.fromInt index), value (resultToString m1), onInput (Select (index - 1)) ] [] ]
        , td [] [ text q1 ]
        ]
    , tr []
        [ td [] [ input [ type_ "radio", name (String.fromInt index), value (resultToString m2), onInput (Select (index - 1)) ] [] ]
        , td [] [ text q2 ]
        ]
    ]


modal : Bool -> List (Attribute msg)
modal isOpen =
    if isOpen then
        [ class "modal modal-fx-3dFlipHorizontal is-active" ]

    else
        [ class "modal modal-fx-3dFlipHorizontal" ]


makeResultView : ResultType -> Html msg
makeResultView result =
    div [ class "box columns content is-vcentered", style "margin-bottom" "30px" ]
        [ div [ class "column is-one-third" ]
            [ span [ class "level-item subtitle is-3" ] [ text (resultToText result) ]
            , figure [ class "level-item image is-1by1" ]
                [ img [ src ("./img/" ++ resultToString result ++ ".svg"), alt (resultToString result) ] []
                ]
            ]
        , div [ class "column" ]
            [ span [ class "tag" ] [ text "特徴" ]
            , resultProperty result
            , span [ class "tag" ] [ text "弱点" ]
            , resultWeekPoint result
            ]
        ]


create1stResult : Array ResultType -> List (Html msg)
create1stResult a =
    let
        lm =
            findNo1SelectedResultType a
    in
    lm |> List.map makeResultView


create2ndResult : Array ResultType -> List (Html msg)
create2ndResult a =
    let
        lm =
            findNo2SelectedResultType a
    in
    if List.member None lm then
        [ div [ class "columns" ] [] ]

    else
        p [ class "content is-size-5" ] [ text "あなたの二次的な5役者の賜物は・・・" ]
            :: (lm |> List.map makeResultView)


resultProperty : ResultType -> Html msg
resultProperty m =
    case m of
        A ->
            ul []
                [ li [] [ text "人々が協力して効果的に動くことを期待する" ]
                , li [] [ text "組織の一致を願っている" ]
                , li [] [ text "イベントなど誰でも参加できるように配慮する" ]
                , li [] [ text "リーダー的素質で人々が自発的に付いてくる" ]
                , li [] [ text "異なるグループの人に共通の目的を与える" ]
                ]

        B ->
            ul []
                [ li [] [ text "天の思いを識別する" ]
                , li [] [ text "人を励ますのにベストな言葉をかけられる" ]
                , li [] [ text "人の人生の出来事を直感的に感じる" ]
                ]

        C ->
            ul []
                [ li [] [ text "自分が知っていて確信を持っていることを伝えるという責任を感じる" ]
                , li [] [ text "自分が重要だと思うことを熱心に伝える" ]
                , li [] [ text "新しい出会いや環境を好む" ]
                , li [] [ text "重要だと思っていることを伝える機会を求める" ]
                ]

        D ->
            ul []
                [ li [] [ text "人を本気で思い遣る" ]
                , li [] [ text "グループに溶け込めていない人を気遣いコンタクトする" ]
                , li [] [ text "困っている人から頼りにされる" ]
                ]

        E ->
            ul []
                [ li [] [ text "原則をわかりやすく伝えられる" ]
                , li [] [ text "人が人生の目的に忠実であるようにサポートする" ]
                , li [] [ text "強固な土台を構築することに熱中する" ]
                , li [] [ text "真理を愛する" ]
                ]

        None ->
            ul []
                [ li [] [ text "エラー" ]
                ]


resultWeekPoint : ResultType -> Html msg
resultWeekPoint m =
    case m of
        A ->
            ul [] [ li [] [ text "支配的で威圧的になりやすい" ] ]

        B ->
            ul [] [ li [] [ text "変な人だと思われやすい" ] ]

        C ->
            ul [] [ li [] [ text "人から拒絶されているとか誤解されていると感じる" ] ]

        D ->
            ul [] [ li [] [ text "物事を頼まれると「境界線」を引くことが困難であるため疲れてしまう" ] ]

        E ->
            ul [] [ li [] [ text "不正や不実に直面した時に相手を裁いてしまいやすい" ] ]

        None ->
            ul [] [ li [] [ text "エラー" ] ]


createResultGraph : Array ResultType -> Html msg
createResultGraph a =
    let
        content =
            [ A, B, C, D, E ] |> List.map (\m -> ( m, getSelectedNum m a )) |> List.map createResultGraphItem
    in
    nav [ class "level is-mobile" ] content


createResultGraphItem : ( ResultType, Int ) -> Html msg
createResultGraphItem ( m, num ) =
    let
        percentage =
            num * 100 // 12
    in
    div [ class "level-item has-text-centered" ]
        [ div []
            [ p [ class "heading" ] [ text (resultToText m) ]
            , p [ class "title" ] [ text ((percentage |> String.fromInt) ++ "%") ]
            ]
        ]
