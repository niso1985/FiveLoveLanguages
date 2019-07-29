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
            "Words"

        B ->
            "QT"

        C ->
            "Gifts"

        D ->
            "Service"

        E ->
            "Touch"

        None ->
            ""


stringToResult : String -> ResultType
stringToResult str =
    case str of
        "Words" ->
            A

        "QT" ->
            B

        "Gifts" ->
            C

        "Service" ->
            D

        "Touch" ->
            E

        _ ->
            None


resultToText : ResultType -> String
resultToText m =
    case m of
        A ->
            "肯定的な言葉"

        B ->
            "良質な時間"

        C ->
            "プレゼント"

        D ->
            "ヘルプ/手助け"

        E ->
            "スキンシップ(タッチ)"

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
            [ h1 [ class "title" ] [ text "愛を伝える5つの言語(独身者向け)" ]
            , p [ class "content", style "margin-bottom" "0px" ] [ text "下記1~30には、2つの質問が併記されています。そのうち、あなたの希望・要望をよりよく表現していると思う方の文章を選んでください。" ]
            , p [ class "content" ] [ text "少なくとも15分から30分の時間をかけるくらい、なるべくリラックスした環境でテストし、あわてて終わらせることは避けてください。" ]
            , div [ class "level" ]
                [ div [ class "level-left" ]
                    [ a [ onClick (NextView Tendacy), class "button level-item" ] [ text "言語一覧確認" ]
                    , a [ class "button level-item", href "https://docs.google.com/spreadsheets/d/e/2PACX-1vTHVYIK58sd50oKo_gxKDlV_GllLZSfpAYZ0wl1NDVmG8Awm2aH9Omjj2HZ47iRBQ1PK0PUyADEy6Zu/pubhtml", target "_blank" ] [ text "他の人の結果を見る(HTML)" ]
                    , a [ class "button level-item", href "https://docs.google.com/spreadsheets/d/1utvyKJkEkHn1AIYIFiObs_sF-SYLrti6yyjY0aB-zm8/edit?usp=sharing", target "_blank" ] [ text "他の人の結果を見る(スプレッドシート)" ]
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
                        , th [ colspan 2 ] [ text "質問" ]
                        ]
                    ]
                , tfoot [] [ tr [] [ td [ colspan 3 ] [] ] ]
                , tbody []
                    (row 1 "ほめ言葉や励ましのメモをもらう事が好きである。" "抱きしめられたい。" A E
                        ++ row 2 "大切な人と、二人きりで時を過ごすのが好きである。" "適切な助けをしてくれる人に、愛情を感じる。" B D
                        ++ row 3 "プレゼントをもらう事が好きである。" "愛する人や友人と気軽に雑談するのが好きである。" C B
                        ++ row 4 "人が私を行動的に助けてくれることに、愛情を感じる。" "人が私に触れるときに、愛情を感じる。" D E
                        ++ row 5 "愛する人や尊敬する人に肩を抱かれるとき、愛情を感じる。" "愛する人や尊敬する人からプレゼントをもらうとき、愛情を感じる。" E C
                        ++ row 6 "友人や愛する人と、出かけるのが好きである。" "特別な人と握手や、手をつなぐ事が好きである。" B E
                        ++ row 7 "私にとって、目に見える愛情表現(ギフト)が、とても大切である。" "人が私をほめてくれた時、愛情を感じます。" C A
                        ++ row 8 "一緒にいたい人と、隣同士で座るのが好きです。" "人から私が魅力的と言われるのが好きである。" E A
                        ++ row 9 "友人や愛する人と、時間を過ごすことが好きである。" "友人や愛する人から、ちょっとしたプレゼントをもらうのが好きである。" B C
                        ++ row 10 "受け入れを表す言葉は、私に大切である。" "誰かが助けてくれたとき、その人に愛されていると感じる。" A D
                        ++ row 11 "友人や愛する人と、一緒に何かをすることが好きである。" "優しい言葉をかけられることが好きである。" B A
                        ++ row 12 "人の言葉よりも、行動に愛情を感じます。" "ハグは私に親近感と尊重を感じさせます。" D E
                        ++ row 13 "否定的な言葉は極力避け、ほめ言葉を大切にしている。" "一つの大きなプレゼントより、複数の小さなプレゼントが好き。" A C
                        ++ row 14 "人と雑談し行動を共にするとき、親近感をもちます。" "友人や愛する人から触れられた時に、親近感を感じる。" B E
                        ++ row 15 "達成したことを人に褒められるのが好きである。" "好きでない事でも、私のためにしてくれる人に愛情を感じる。" A D
                        ++ row 16 "通りがかりに、友人や愛する人に触れられるのが好きである。" "人が私の話しに耳を傾け、興味を示してくれることを好む。" E B
                        ++ row 17 "友人や愛する人が、仕事や企画を手伝ってくれる時、愛情を感じる。" "友人を愛する人から、プレゼントを受けることがとても好きである。" D C
                        ++ row 18 "人から容姿をほめられることが好きである。" "人が私の気持ちを、時間をかけて理解してくれる時、愛情を感じる。" A B
                        ++ row 19 "特別な人から触れられるとき、安心感を得ることができます。" "助けられるとき愛情を感じます。" E D
                        ++ row 20 "特別な人がしてくれた多くのことに感謝します。" "特別な人が作ってくれたプレゼントを受けることが好きである。" D C
                        ++ row 21 "人が私に全身全霊で接してくれるとき、とても好きである。" "人が私を助けてくれる時の感覚がとても好きである。" B D
                        ++ row 22 "人が私の誕生日にプレゼントで祝ってくれるとき、愛情を感じる。" "心のこもった言葉で誕生日を祝ってくれるとき、愛情を感じる。" C A
                        ++ row 23 "プレゼントをもらうとき、人が私のことを考えていてくれると知ります。" "人が私の雑用を手伝ってくれるとき、愛情を感じる。" C D
                        ++ row 24 "人が私の話を妨げず、我慢強く聞いてくれることに感謝します。" "私の特別な日を、ギフトをもって覚え、祝ってくれること感謝します。" B C
                        ++ row 25 "愛する人が、日々の責任を心配して手伝ってくれることが嬉しい。" "特別な人と遠出をするのが楽しい。" D B
                        ++ row 26 "身近な人とキスしたり、されることに喜びを感じる。" "理由もなくプレゼントを受けるとき、喜びを感じる。" E C
                        ++ row 27 "感謝しているといわれることが好きである。" "話をしているときに、私を見てくれる人が好きである。" A B
                        ++ row 28 "友人または、愛する人からのプレゼントは私にとって常に特別である。" "友人または、愛する人に触れられるとき、気持ちがよい。" C E
                        ++ row 29 "人が要望した責務に情熱を持ってくれるとき、愛情を感じる。" "どれだけ感謝しているかを告げられるとき、愛情を感じる。" D A
                        ++ row 30 "私は毎日触れられていたい。" "私は誉め言葉や励ましが日常必要である。" E A
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
                        (p [ class "subtitle" ] [ text "この結果をパートナーに教えてあげてください。" ]
                            :: p [ class "content is-size-5" ] [ text "あなたの愛の第一言語は・・・" ]
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
                [ p [ class "modal-card-title" ] [ text "言語一覧" ]
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
            [ span [ class "tag" ] [ text "解説" ]
            , resultProperty result
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
        p [ class "content is-size-5" ] [ text "あなたの愛の第二言語は・・・" ]
            :: (lm |> List.map makeResultView)


resultProperty : ResultType -> Html msg
resultProperty m =
    case m of
        A ->
            p [] [ text "行動は必ずしも言葉よりも雄弁ではありません。これがあなたの愛の言語であるならば、想定外の褒め言葉はあなたにとって最高なものでしょう。「あなたを愛しています」という言葉を聞くことはとても重要です。その言葉の背後にある理由を聞くことは、さらにあなたの精神を上向きにします。侮辱はあなたを粉々にし、脳裏から離れないでしょう。あなたは励ましや親切にされる言葉を栄養に活動できます。" ]

        B ->
            p [] [ text "クオリティタイムでは、「愛している」と常に言い続ける必要はありません。このタイプの人は一緒にいることは大事ですが、テレビの電源を切って、食事の手を止めて、すべての雑用と仕事を一旦止める必要があります。そうやって自分だけに注目されているときに、本当に特別で愛されているように感じます。注目されない、予定の延期、傾聴されないことは特に心を痛めます。邪魔されず誰かと話すことや一緒に活動することに時間を費やすように、あなたは時間を共有することによって他人との関係を深めます。" ]

        C ->
            p [] [ text "この愛の言語を物質主義と間違えないでください。贈り物の受け手は、その贈り物の背後にある愛、思慮深さ、努力を大切にします。あなたがこの言語を話すならば、どんな犠牲を払ってでも贈られた完璧な贈り物や行為は、あなたを知り、あなたを気にかけ、あなたを尊んでいるというしるしになります。誕生日を逃したり、気の利かない思いやりのない贈り物は悲惨なことになるでしょう。毎日の行為がないのもそうでしょう。贈り物は、あなたにとって愛の象徴です。" ]

        D ->
            p [] [ text "宿題を手伝うことは本当に愛の表現なのでしょうか？なんと、「サービス行為」の言語を持つ人の負担を軽くすることは大いに意味のあることなのです！この言語を持つ人が最も喜ぶ言葉は、「あなたのために私がやります」です。誰かが義務ではなく愛を持ってあなたに仕えるとき、あなたは本当に大切にされ愛されていると感じます。" ]

        E ->
            p [] [ text "第一言語がボディタッチである人は、驚くことではないが、とても気難しいです。ハグ、背中をたたく、腕への思いやりのあるタッチは、興奮・関心・心配・愛の全てを示す方法でありえます。物理的な存在感や接しやすさは大事なことですが、同時に無視や虐待は許されず破壊的です。適切でタイムリーなタッチは、あなたに暖かさ、安心感、そして愛を伝えます。" ]

        None ->
            p [] [ text "エラー" ]


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
