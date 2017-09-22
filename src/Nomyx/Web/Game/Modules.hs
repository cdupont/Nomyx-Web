{-# LANGUAGE DoAndIfThenElse      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Nomyx.Web.Game.Modules where

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
                                                   toValue, ul, (!), p)
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

viewModules :: Library -> GameName -> Bool -> Bool -> RoutedNomyxServer Html
viewModules (Library _ ms) gn isGameAdmin isInGame = do
  msv <- mapM (viewPaneModule gn isGameAdmin isInGame) ms
  ok $ do
    div ! class_ "modulesList" $ viewModuleNames ms
    div ! class_ "modules" $ sequence_ msv

viewModuleNames :: [ModuleInfo] -> Html
viewModuleNames mods = do
  ul $ mapM_  viewModuleName mods

viewModuleName :: ModuleInfo -> Html
viewModuleName (ModuleInfo mp _) = li $ H.a (fromString $ mp) ! A.class_ "modulePath" ! (A.href $ toValue $ "?modulePath=" ++ (idEncode mp))

  
viewPaneModule :: GameName -> Bool -> Bool -> ModuleInfo -> RoutedNomyxServer Html
viewPaneModule gn isGameAdmin isInGame modi = do
  com <- moduleCommands gn modi
  view <- viewModule gn modi
  edit <- viewModuleEdit gn isInGame modi
  ok $ div ! A.class_ "module" ! A.id (toValue $ idEncode $ _modPath modi) $ do
    com
    view
    edit

-- ** Module commands

moduleCommands :: GameName -> ModuleInfo -> RoutedNomyxServer Html
moduleCommands gn (ModuleInfo path _) = do
  let idmod = idEncode path
  ok $ div ! A.class_ "commandModule" $ do
    p $ H.a "view"   ! (A.href $ toValue $ "?modulePath=" ++ idmod)
    p $ H.a "edit"   ! (A.href $ toValue $ "?modulePath=" ++ idmod ++ "&edit")

-- ** Module view

viewModule :: GameName -> ModuleInfo -> RoutedNomyxServer Html
viewModule gn (ModuleInfo path mod) = do
  ok $ div ! A.class_ "viewModule" $ do
    div $ displayCode $ unpack mod
    
-- * Module edit

-- Edit a template
viewModuleEdit :: GameName -> Bool -> ModuleInfo -> RoutedNomyxServer Html
viewModuleEdit gn isInGame modi = do
  lf  <- liftRouteT $ lift $ viewForm "user" (newModuleForm modi)
  ok $ div ! A.class_ "editModule" $ do
    blazeForm lf $ defLink (NewModule gn) isInGame

newModuleForm :: ModuleInfo -> NomyxForm ModuleInfo
newModuleForm (ModuleInfo path cont) =
  ModuleInfo   <$>  (inputHidden path)
               <*>  (pack <$> (textarea 80 40 (unpack cont) `setAttr` class_ "ruleCode" `setAttr` placeholder "Enter here your module"))

newModule :: GameName -> RoutedNomyxServer Response
newModule gn = toResponse <$> do
  methodM POST
  r <- liftRouteT $ lift $ eitherForm environment "user" (newModuleForm (ModuleInfo "" ""))
  pn <- fromJust <$> getPlayerNumber
  case r of
     Right modi -> do
       webCommand $ S.newModule pn modi
       seeOther (showRelURLParams (Menu Modules gn) [("modulePath", Just $ pack $ idEncode $ _modPath modi)]) $ "Redirecting..."
     _ -> error "cannot retrieve form data"

