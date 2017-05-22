module App (appMain) where

import Control.Arrow (first, second)
import Control.Lens ((^.), preview, _Left, _Right)
import Control.Monad (join, void)
import Data.Bifunctor (bimap)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum((:=>)))
import Data.Functor (($>))
import Data.Functor.Const (Const(Const))
import Data.Functor.Identity (Identity(Identity))
import Data.Functor.Misc (ComposeMaybe(ComposeMaybe), Const2(Const2))
import Data.Monoid ((<>))
import Data.Some (Some(This))
import Data.Text (Text, pack)
import Language.Javascript.JSaddle (eval, jsf, jsg, liftJSM)
import Reflex.Dom -- sorry!
import Reflex.Patch.DMapWithMove
  ( PatchDMapWithMove(PatchDMapWithMove), NodeInfo(NodeInfo), From(From_Insert, From_Delete, From_Move)
  , moveDMapKey, deleteDMapKey )

appMain :: IO ()
appMain = mainWidget appWidget

dumpMount :: forall t m. MonadWidget t m => String -> m ()
dumpMount prefix = do
  mses <- updated <$> getMountStatus
  performEvent_ . ffor mses $ \ ms ->
    liftJSM $ do
      void $ jsg ("console" :: Text) ^. jsf ("log" :: Text) (pack $ prefix <> ": " <> show ms)
      void $ eval ("console.log(document.children[0].children[1].innerHTML)" :: Text)

appWidget :: forall t m. MonadWidget t m => m ()
appWidget = do
  el "div" $ do
    el "h2" $ text "no adjust"
    dumpMount "root"

  el "div" $ do
    el "h2" $ text "runWithReplace (by way of dyn)"
    simpleDyn

  el "div" $ do
    el "h2" $ text "traverseDMapWithKeyWithAdjust"
    dmapWithAdjust

  el "div" $ do
    el "h2" $ text "traverseDMapWithKeyWithAdjustWithMove"
    dmapWithAdjustWithMove

simpleDyn :: forall t m. MonadWidget t m => m ()
simpleDyn = do
  b <- toggle False =<< button "dyn a thing!"

  switched <- dyn $ ffor b $ \ case
    True -> do
      text "yo I am a dyn"
      dumpMount "dyn True"
      Left . fmap Just <$> getMountStatus

    False -> do
      text "yo I am another dyn"
      dumpMount "dyn False"
      Right . fmap Just <$> getMountStatus

  tmes :: Dynamic t (Maybe MountState) <- fmap join . holdDyn (constDyn Nothing) . fmapMaybe (preview _Left) $ switched
  fmes :: Dynamic t (Maybe MountState) <- fmap join . holdDyn (constDyn Nothing) . fmapMaybe (preview _Right) $ switched

  performEvent_ . ffor (updated tmes) $ \ tms ->
    liftJSM $ void $ jsg ("console" :: Text) ^. jsf ("log" :: Text) ("at root, dyn True = " <> (pack . show) tms)
  performEvent_ . ffor (updated fmes) $ \ fms ->
    liftJSM $ void $ jsg ("console" :: Text) ^. jsf ("log" :: Text) ("at root, dyn False = " <> (pack . show) fms)

type DMK = Const2 Int Text
type DMP = PatchDMap DMK Identity
type DMPM = PatchDMapWithMove DMK Identity

dmapConstToList :: DMap k (Const a) -> [a]
dmapConstToList = map (\ (_ :=> Const v) -> v) . DMap.toList

dmapWithAdjust :: forall t m. MonadWidget t m => m ()
dmapWithAdjust = do
  newThing <- button "new thing!"
  index :: Dynamic t Int <- count newThing
  let newThings :: Event t DMP
      newThings =
        ffor (tag (current index) newThing) $ \ i ->
          PatchDMap $ DMap.singleton (Const2 i) (ComposeMaybe (Just (Identity (pack $ show i))))
  let renderThing :: Const2 Int Text a -> Identity a -> m (Const (Event t DMP) a)
      renderThing k@(Const2 i) (Identity t) = do
        dumpMount $ "thing " <> show i
        el "div" $ do
          text $ "I'm thing " <> t
          delete <- button "delete!"
          update <- button "update!"
          pure . Const $ PatchDMap . DMap.singleton (Const2 i) . ComposeMaybe
            <$> leftmost [ delete $> Nothing
                         , update $> (Just . Identity $ t <> "z")
                         ]

  rec
    (_, permuteThingsPatches :: Event t (PatchDMap DMK (Const (Event t DMP)))) <-
      traverseDMapWithKeyWithAdjust renderThing DMap.empty (newThings <> permuteThings)
    permuteThingsDMap :: Dynamic t (DMap DMK (Const (Event t DMP))) <-
      foldDyn applyAlways DMap.empty permuteThingsPatches
    let activePermutations :: Behavior t (Event t DMP)
        activePermutations = leftmost . dmapConstToList <$> current permuteThingsDMap
        permuteThings :: Event t DMP
        permuteThings = switch activePermutations

  pure ()

dmapWithAdjustWithMove :: forall t m. MonadWidget t m => m ()
dmapWithAdjustWithMove = do
  newThing <- button "new thing!"
  backwards <- button "backwards!"
  index :: Dynamic t Int <- count newThing
  let newThings :: Event t DMPM
      newThings =
        ffor (tag (current index) newThing) $ \ i ->
          PatchDMapWithMove . DMap.singleton (Const2 i) $
            NodeInfo (From_Insert . Identity . pack . show $ i) (ComposeMaybe Nothing)
  let renderThing :: Const2 Int Text a -> Identity a -> m (Const (Event t DMPM) a)
      renderThing k@(Const2 i) (Identity t) = do
        dumpMount $ "thing " <> show i
        el "div" $ do
          text $ "I'm thing " <> t
          delete <- button "delete!"
          update <- button "update!"
          pure . Const $ leftmost
            [ delete $> deleteDMapKey k
            , update $> (PatchDMapWithMove . DMap.singleton (Const2 i) $ NodeInfo (From_Insert . Identity $ t <> "z") (ComposeMaybe Nothing))
            ]

  rec
    let reverseThings :: Event t DMPM
        reverseThings =
          ffor (tag (DMap.keys <$> current rowDMap) backwards) $ \ ks ->
            PatchDMapWithMove . DMap.fromList . concat $ ffor (ks `zip` reverse ks) $ \ (This (Const2 kf), This (Const2 kt)) ->
              if kf == kt then [] else
                [ Const2 kt :=> NodeInfo (From_Move $ Const2 kf) (ComposeMaybe . Just $ Const2 kt)
                ]

    (_, rowPatches :: Event t (PatchDMapWithMove DMK (Const (Event t DMPM)))) <-
      traverseDMapWithKeyWithAdjustWithMove renderThing DMap.empty (newThings <> permuteThings <> reverseThings)
    rowDMap :: Dynamic t (DMap DMK (Const (Event t DMPM))) <-
      foldDyn applyAlways DMap.empty rowPatches
    let activePermutations :: Behavior t (Event t DMPM)
        activePermutations = leftmost . dmapConstToList <$> current rowDMap
        permuteThings :: Event t DMPM
        permuteThings = switch activePermutations

  pure ()

