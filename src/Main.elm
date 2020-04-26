module Main exposing (..)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Http
import Json.Decode as Decode exposing (Decoder, field, float, int, string, list, index)
import Time
import Task
import Url
import XmlParser exposing (Node(..), Xml)

import Styles exposing (..)


-- HN's feed has a bad description
descriptionBlacklist = [ "Hacker News", "Better Programming - Medium" ]

itemCount = 15

hackerMode : Mode
hackerMode =
  { imageCollections =
      [ "9700244"
      , "219941"
      , "312299"
      ]
  , rssLinks =
      [ "https://news.ycombinator.com/rss"
      , "https://rsshub.app/github/trending/daily"
      , "https://medium.com/feed/better-programming"
      , "https://www.wired.com/feed/rss"
      , "https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml"
      ]
  }

---- MODEL ----

type alias RssItem = { title: String, description: Maybe String, url: String }
type alias Feed = { title: String, items: List RssItem }
type alias Mode = { imageCollections: List String, rssLinks: List String}

type alias Model = 
  { mode: Mode
  , feeds: List Feed
  , hour: Maybe Int }

init : ( Model, Cmd Msg )
init = ({mode=hackerMode, feeds=[], hour=Nothing}, Cmd.batch [getHour, getFeeds hackerMode] )


getHour : Cmd Msg
getHour =
  Task.perform GotHour <| Task.map2 Time.toHour Time.here Time.now

getFeeds : Mode -> Cmd Msg
getFeeds mode =
  Cmd.batch ( List.map getFeed mode.rssLinks )

getFeed : String -> Cmd Msg
getFeed url =
  Http.get
    { url = "https://api.rss2json.com/v1/api.json?rss_url=" ++ (Url.percentEncode url)
    , expect = Http.expectJson GotFeed feedDecoder
    }

feedDecoder : Decoder Feed
feedDecoder =
  field "feed" (field "title" string)
    |> Decode.andThen
      (\title ->
        Decode.map (\is -> {title=title, items=is})
          ( field "items" (list (itemDecoder <| not <| List.member title descriptionBlacklist) ) )
      )


itemDecoder : Bool -> Decoder RssItem
itemDecoder withDesc =
  case withDesc of
    True ->
      Decode.map3 (\t d u -> {title=t, description=Just d, url=u})
        ( field "title" string )
        ( field "description" string )
        ( field "link" string )

    False ->
      Decode.map2 (\t u -> {title=t, description=Nothing, url=u})
        ( field "title" string )
        ( field "link" string )


getSummary : String -> Cmd Msg
getSummary url =
  Http.get
    { url = "http://summarizerapi.herokuapp.com/?url=" ++ url
    , expect = Http.expectJson (GotSummary url) summaryDecoder
    }

summaryDecoder : Decoder String
summaryDecoder =
  field "summary" string


---- UPDATE ----

type Msg
  = GotHour Int
  | GotFeed (Result Http.Error Feed)
  | GotSummary String (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotHour hour ->
      ( { model | hour = Just hour }, Cmd.none )
    
    GotFeed res ->
      case res of
        Ok feed ->
          let
            summarizeUrls =
              List.filterMap 
                (\it -> case it.description of
                  Just _  -> Nothing
                  Nothing -> Just it.url
                )
                feed.items
            
            summarizeCmd = Cmd.batch <| List.map getSummary summarizeUrls
          in
            ( { model | feeds = model.feeds ++ [feed] }, summarizeCmd )
        Err e ->
          ( model, Cmd.none )

    GotSummary url res ->
      case res of
        Ok description ->
          let
            updateFeed u desc feed =
              { feed | items = List.map (\i -> if i.url == u then {i | description = Just desc} else i) feed.items }
            newFeeds = List.map (updateFeed url description) model.feeds
          in
            ( { model | feeds = newFeeds }, Cmd.none )
        Err e ->
          ( model, Cmd.none )

---- VIEW ----



view : Model -> Html Msg
view model =
  case model.hour of
    Just hour ->
      let
        headerText =
          if hour <= 7 then
            "Go to bed, Alex"
          else
            if hour <= 11 then
              "Good morning, Alex"
            else
              if hour <= 16 then
                "Good afternoon, Alex"
              else
                "Good evening, Alex"
      
        items = List.take itemCount <| List.map viewRssItem (flatten (List.map .items model.feeds))
        pics = generateImages model.mode (2 * List.length items)
        (pics1, pics2) = (List.take (List.length items) pics, List.drop (List.length items) pics)

      in
        div [css Styles.wrapper]
          [ h1 [ css Styles.mainHeader ] [ text headerText ]
          , div [css Styles.masonry]
            ( flatten [pics1, items, pics2] )
          ]

    Nothing ->
      div [] []


generateImages : Mode -> Int -> List (Html Msg)
generateImages mode n =
  let
    perList = n // List.length mode.imageCollections
    searchTerms = flatten (List.map (List.repeat perList) mode.imageCollections)
  in
    List.indexedMap (\i collection -> img [css Styles.item, src ("https://source.unsplash.com/collection/" ++ collection ++ "?" ++ String.fromInt i)] []) searchTerms

flatten : List (List a) -> List a
flatten ls =
  let
    next = List.map List.head ls
      |> List.foldr (\i l -> l ++ Maybe.withDefault [] (Maybe.map List.singleton i)) []
  in
    case next of
      [] -> []
      _  -> next ++ flatten (List.map (List.tail >> Maybe.withDefault []) ls)


merge : List a -> List a -> List a
merge l1 l2 =
  case (l1, l2) of
    (x1::xs1, x2::xs2) -> [x1, x2] ++ merge xs1 xs2
    ([]     , xs     ) -> xs
    (xs     , []     ) -> xs


viewRssItem : RssItem -> Html Msg
viewRssItem params =
  a [ href params.url ]
    [ div [ css Styles.item ]
      [ h2 [ css Styles.itemHeader ] [ text params.title ]
      , p [ css Styles.itemDescription ] [ text <| Maybe.withDefault "" params.description ]
      ]
    ]


---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


main : Program () Model Msg
main =
  Browser.element
    { view = view >> toUnstyled
    , init = \_ -> init
    , update = update
    , subscriptions = subscriptions
    }
