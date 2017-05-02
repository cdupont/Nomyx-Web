{-# LANGUAGE DoAndIfThenElse      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
--{-# LANGUAGE ApplicativeDo  #-}

module Nomyx.Web.Game.Templates where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Maybe
import           Data.Function (on)
import           Data.String
import           Data.List (sortBy, groupBy)
import           Data.Ord (comparing)
import           Data.Text                   (Text, pack, unpack)
import           Data.Text.Encoding
import           Happstack.Server            (Method (..), Response, methodM,
                                              ok, seeOther, toResponse)
import           Nomyx.Language
import           Nomyx.Core.Engine
import           Nomyx.Core.Session          as S
import           Nomyx.Core.Types            as T
import           Nomyx.Core.Utils
import           Nomyx.Web.Common            as NWC
import qualified Nomyx.Web.Help              as Help
import           Nomyx.Web.Types
import           Prelude                     hiding (div)
import           Text.Blaze.Html5            as H (Html, a, div, h2, h3, h4,
                                                   img, input, label, li, pre,
                                                   toValue, ul, (!), p, br)
import           Text.Blaze.Html5.Attributes as A (class_, disabled, for, href,
                                                   id, placeholder, src, type_,
                                                   value)
import           Text.Reform                 (eitherForm, viewForm, (++>),
                                              (<++))
import           Text.Reform.Blaze.Common    (setAttr)
import           Text.Reform.Blaze.String    (inputHidden, inputSubmit, label,
                                              textarea)
import qualified Text.Reform.Blaze.String    as RB
import           Text.Reform.Happstack       (environment)
import           Web.Routes.RouteT           (liftRouteT)
import           Happstack.Server            (ContentType)
import           Safe
import           Network.HTTP.Base                  (urlEncode)
default (Integer, Double, Data.Text.Text)


-- * Library display

viewLibrary :: Library -> PlayerNumber -> GameName -> Bool -> Bool -> RoutedNomyxServer Html
viewLibrary (Library rts ms) pn gn isGameAdmin isInGame = do
  vrs <- mapM (viewPaneRuleTemplate pn gn isGameAdmin isInGame) rts
  ok $ do
    div ! class_ "ruleList" $ viewRuleTemplateCats rts
    div ! class_ "rules" $ sequence_ vrs

-- * left menu display

viewRuleTemplateCats :: [RuleTemplateInfo] -> Html
viewRuleTemplateCats rts = do
  let cat = (headDef "Not category" . _rCategory . _iRuleTemplate)
  let rts' = groupBy ((==) `on` cat) $ sortBy (comparing cat) rts
  h2 "Library of rules"
  ul $ mapM_  viewRuleTemplateCat rts'

viewRuleTemplateCat :: [RuleTemplateInfo] -> Html
viewRuleTemplateCat rts = li $ do
   fromString $ headDef "No category" $ _rCategory $ _iRuleTemplate $ head rts
   ul $ mapM_  viewRuleTemplateName rts

viewRuleTemplateName :: RuleTemplateInfo -> Html
viewRuleTemplateName (RuleTemplateInfo rt _) = li $ H.a (fromString $ _rName rt) ! A.class_ "ruleName" ! (A.href $ toValue $ "?ruleName=" ++ (idEncode $ _rName rt))


-- * main tab display

viewPaneRuleTemplate :: PlayerNumber -> GameName -> Bool -> Bool -> RuleTemplateInfo -> RoutedNomyxServer Html
viewPaneRuleTemplate pn gn isGameAdmin isInGame rti = do
  com  <- templateCommands     gn rti
  view <- viewRuleTemplate     pn gn rti isGameAdmin isInGame
  edit <- viewRuleTemplateEdit gn rti isInGame
  ok $ div ! A.class_ "rule" ! A.id (toValue $ idEncode $ _rName $ _iRuleTemplate rti) $ do
    com
    view
    edit

-- ** Template commands

templateCommands :: GameName -> RuleTemplateInfo -> RoutedNomyxServer Html
templateCommands gn (RuleTemplateInfo rt _) = do
  let delLink = showRelURL (DelRuleTemplate gn (_rName rt))
  let idrt = idEncode $ _rName rt
  ok $ div ! A.class_ "commandrule" $ do
    p $ H.a "view"   ! (A.href $ toValue $ "?ruleName=" ++ idrt)
    p $ H.a "edit"   ! (A.href $ toValue $ "?ruleName=" ++ idrt ++ "&edit")
    p $ H.a "delete" ! (A.href $ toValue delLink)


delRuleTemplate :: Player -> RuleName -> RoutedNomyxServer Response
delRuleTemplate gn rn = do
  pn <- fromJust <$> getPlayerNumber
  webCommand $ S.delRuleTemplate rn pn
  seeOther (showRelURL $ LibMenu Lib pn) $ toResponse "Redirecting..."


viewRuleTemplate :: PlayerNumber -> GameName -> RuleTemplateInfo -> Bool -> Bool -> RoutedNomyxServer Html
viewRuleTemplate pn (RuleTemplateInfo rt@(RuleTemplate name desc code author picture _ decls) err) isGameAdmin isInGame = do
  lf  <- liftRouteT $ lift $ viewForm "user" (submitRuleTemplatForm (Just rt) isGameAdmin)
  ok $ div ! A.class_ "viewrule" $ do
    viewRuleHead name picture desc author
    viewRuleCode code
    mapM (viewDeclPath pn) decls
    div $ pre $ fromString err
    blazeForm lf $ defLink (SubmitRule gn) isInGame


submitRuleTemplatForm :: (Maybe RuleTemplate) -> Bool -> NomyxForm (String, Maybe String)
submitRuleTemplatForm mrt isGameAdmin = 
   (,) <$> inputHidden (show mrt)
       <*> if isGameAdmin then inputSubmit "Admin submit" else pure Nothing


viewDeclPath :: PlayerNumber -> FilePath -> Html
viewDeclPath pn modPath = do
   let link = showRelURLParams (LibMenu Modules pn) [("modulePath", Just $ pack $ idEncode modPath)]
   H.a (fromString modPath) ! (A.href $ toValue $ link)
   H.br


-- | Submit a template to a given game
submitRuleTemplatePost :: GameName -> RoutedNomyxServer Response
submitRuleTemplatePost gn = toResponse <$> do
   methodM POST
   s <- getSession
   let gi = getGameByName gn s
   admin <- isGameAdmin (fromJust gi)
   r <- liftRouteT $ lift $ eitherForm environment "user" (submitRuleTemplatForm Nothing True)
   pn <- fromJust <$> getPlayerNumber
   rt <- case r of
      Right (srt, Nothing) -> do
         let rt = fromJust $ read srt
         webCommand $ submitRule rt pn gn
         return rt
      Right (srt, Just _)  -> do
         let rt = read srt
         webCommand $ adminSubmitRule rt pn gn
         return rt
      (Left _)            -> error "cannot retrieve form data"
   seeOther (showRelURLParams (LibMenu pn) [("ruleName", Just $ pack $ idEncode $ _rName rt)]) $ "Redirecting..."


-- * Template edit

-- Edit a template
viewRuleTemplateEdit :: GameName -> RuleTemplateInfo -> Bool -> RoutedNomyxServer Html
viewRuleTemplateEdit gn (RuleTemplateInfo rt msg) isInGame = do
  lf  <- liftRouteT $ lift $ viewForm "user" (newRuleTemplateForm (Just rt) True)
  ok $ div ! A.class_ "editRule" $ do
    blazeForm lf $ defLink (NewRuleTemplate gn) isInGame
    pre $ fromString msg

newRuleTemplateForm :: Maybe RuleTemplate -> Bool -> NomyxForm (RuleTemplate, Maybe String)
newRuleTemplateForm sr isGameAdmin = newRuleTemplateForm' (fromMaybe (RuleTemplate "" "" "" "" Nothing [] []) sr) isGameAdmin

newRuleTemplateForm' :: RuleTemplate -> Bool -> NomyxForm (RuleTemplate, Maybe String)
newRuleTemplateForm' rt isGameAdmin =
  (,) <$> newRuleTemplateForm'' rt
      <*> inputSubmit "Check"

newRuleTemplateForm'' :: RuleTemplate -> NomyxForm RuleTemplate
newRuleTemplateForm'' (RuleTemplate name desc code aut pic cat decls) =
  RuleTemplate <$>  RB.label "Name: " ++> RB.inputText name `setAttr` class_ "ruleName" <++ RB.br
               <*> (RB.label "      Short description: " ++> (RB.inputText desc `setAttr` class_ "ruleDescr") <++ RB.br)
               <*>  RB.label "      Code: " ++> textarea 80 15 code `setAttr` class_ "ruleCode" `setAttr` placeholder "Enter here your rule"
               <*>  (inputHidden aut)
               <*>  (read <$> (inputHidden $ show pic))
               <*>  (read <$> (inputHidden $ show cat))
               <*>  (read <$> (inputHidden $ show decls))

newRuleTemplate :: GameName -> RoutedNomyxServer Response
newRuleTemplate gn = toResponse <$> do
  methodM POST
  r <- liftRouteT $ lift $ eitherForm environment "user" (newRuleTemplateForm Nothing False)
  pn <- fromJust <$> getPlayerNumber
  case r of
     Right (rt, Nothing) -> do
       webCommand $ S.newRuleTemplate pn rt
       seeOther (showRelURLParams (LibMenu Lib pn) [("ruleName", Just $ pack $ idEncode $ _rName rt)]) $ "Redirecting..."
     Right (rt, Just _)  -> do
       webCommand $ S.checkRule rt pn gn
       seeOther (showRelURLParams (Menu Lib gn) [("ruleName", Just $ pack $ idEncode $ _rName rt), ("edit", Nothing)]) $ "Redirecting..."
     _ -> error "cannot retrieve form data"

