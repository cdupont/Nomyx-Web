{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Nomyx.Web.Game.Infos where

import Prelude hiding (div)
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.Monoid
import Data.Maybe
import Data.String
import Data.List
import Data.List.Split
import Data.Text (Text)
import Text.Blaze.Html5                    (Html, div, (!), p, table, td, tr, h2, h3, h4, pre, toValue, br, a)
import Text.Blaze.Html5.Attributes as A    (id, class_, href, title)
import Text.Reform.Blaze.String            (inputHidden)
import Text.Reform                         (viewForm, eitherForm)
import Text.Reform.Happstack               (environment)
import Happstack.Server                    (Response, Method(..), seeOther, toResponse, methodM, ok)
import Web.Routes.RouteT                   (showURL, liftRouteT)
import Nomyx.Language
import Nomyx.Web.Common as NWC
import Nomyx.Web.Help as Help
import Nomyx.Web.Types
import Nomyx.Core.Engine (Game, GameName, getVictorious, _gameName, _gameDesc, _players, _desc, _forumURL)
import Nomyx.Core.Session as S
import Nomyx.Core.Profile as Profile

default (Integer, Double, Data.Text.Text)

viewGameDesc :: Game -> Maybe PlayerNumber -> Maybe PlayerNumber -> Bool -> RoutedNomyxServer Html
viewGameDesc g mpn playAs gameAdmin = do
   let gn = _gameName g
   let logged = isJust mpn
   let modJoin = modalJoin gn logged
   let modLeave = modalLeave gn logged
   let modDel = modalDel gn logged
   let isInGame = maybe False (\pn -> pn `elem` (_playerNumber <$> _players g)) mpn
   vp <- viewPlayers (_players g) gn gameAdmin
   ok $ do
      h3 $ fromString $ _gameName g
      when (isJust playAs) $ h4 $ fromString $ "You are playing as player " ++ (show $ fromJust playAs)
      p $ pre $ fromString (_desc $ _gameDesc g)
      p $ h4 $ "This game is discussed in the " >> a "Forum" ! (A.href $ toValue (_forumURL $ _gameDesc g)) >> "."
      p $ h4 "Players in game:"
      when gameAdmin "(click on a player name to \"play as\" this player)"
      vp
      p $ viewVictory g
      if isInGame
         then a "Leave" ! (href $ toValue $ "#openModalLeave" ++ gn) ! A.class_ "button"
         else a "Join"  ! (href $ toValue $ "#openModalJoin" ++ gn)  ! A.class_ "button" ! (title $ toValue Help.joinGame)
      when gameAdmin $ a "Del"   ! (href $ toValue $ "#openModalDel" ++ gn) ! A.class_ "button"
      modJoin >> modLeave >> modDel

modalWindow :: Text -> String -> String -> String -> Html
modalWindow link buttonTitle question modelRef = do
      div ! A.id (toValue $ modelRef) ! A.class_ "modalWindow" $ do
         div $ do
            h2 (fromString question)
            a "Cancel"                 ! (href $ toValue $ showRelURL MainPage) ! A.class_ "modalButton"
            a (fromString buttonTitle) ! (href $ toValue link) ! A.class_ "modalButton"

modalLeave :: GameName -> Bool -> Html
modalLeave gn logged = modalWindow
   (defLink (LeaveGame gn) logged)
   "Leave"
   "Do you really want to leave? You will loose your assets in the game (for example, your bank account)."
   ("openModalLeave" ++ gn)

modalJoin :: GameName -> Bool -> Html
modalJoin gn logged = modalWindow
   (defLink (JoinGame gn) logged)
   "Join"
   "Joining the game. Please register in the forum (see the link) and introduce yourself to the other players! If you do not whish to play, you can just view the game."
   ("openModalJoin" ++ gn)

modalDel :: GameName -> Bool -> Html
modalDel gn logged = modalWindow
   (defLink (DelGame gn) logged)
   "Delete"
   ("Delete the game " ++ gn ++ " ?")
   ("openModalDel" ++ gn)

viewPlayers :: [PlayerInfo] -> GameName -> Bool -> RoutedNomyxServer Html
viewPlayers pis gn gameAdmin = do
   let plChunks = transpose $ chunksOf 15 (sort pis)
   vp <- mapM mkRow plChunks
   ok $ table $ mconcat vp
   where mkRow :: [PlayerInfo] -> RoutedNomyxServer Html
         mkRow row = do
         r <- mapM (viewPlayer gn gameAdmin) row
         ok $ tr $ mconcat r

viewPlayer :: GameName -> Bool -> PlayerInfo -> RoutedNomyxServer Html
viewPlayer gn gameAdmin (PlayerInfo pn name _) = do
   pad <- playAsDiv pn gn
   let inf = fromString name
   ok $ do
    pad
    td $ div ! A.id "playerNumber" $ fromString $ show pn
    td $ div ! A.id "playerName" $ if gameAdmin
       then a inf ! (href $ toValue $ "#openModalPlayAs" ++ show pn)
       else inf

playAsDiv :: PlayerNumber -> GameName -> RoutedNomyxServer Html
playAsDiv pn gn = do
   paf <- liftRouteT $ lift $ viewForm "user" $ playAsForm $ Just pn
   ok $ do
      let cancel = a "Cancel" ! (href $ toValue $ showRelURL MainPage) ! A.class_ "modalButton"
      div ! A.id (toValue $ "openModalPlayAs" ++ show pn) ! A.class_ "modalWindow" $ do
         div $ do
            h2 $ fromString $ "When you are in a private game, you can play instead of any players. This allows you to test " ++
               "the result of their actions."
            blazeForm (h2 (fromString $ "Play as player " ++ show pn ++ "?  ") >> paf) (showRelURL $ SubmitPlayAs gn)
            br
            cancel

playAsForm :: Maybe PlayerNumber -> NomyxForm String
playAsForm pn = inputHidden (show pn)

viewVictory :: Game -> Html
viewVictory g = do
    let vs = _playerName <$> mapMaybe (Profile.getPlayerInfo g) (getVictorious g)
    case vs of
        []   -> br
        a:[] -> h3 $ fromString $ "Player " ++ show a ++ " won the game!"
        a:bs -> h3 $ fromString $ "Players " ++ intercalate ", " bs ++ " and " ++ a ++ " won the game!"

newPlayAs :: GameName -> RoutedNomyxServer Response
newPlayAs gn = toResponse <$> do
   methodM POST
   p <- liftRouteT $ lift $ eitherForm environment "user" $ playAsForm Nothing
   pn <- fromJust <$> getPlayerNumber
   case p of
      Right playAs -> do
         webCommand $ S.playAs (read playAs) pn gn
         seeOther (showRelURL MainPage) "Redirecting..."
      (Left errorForm) -> mainPage  "Admin settings" "Admin settings" (blazeForm errorForm $ showRelURL $ SubmitPlayAs gn) False True
