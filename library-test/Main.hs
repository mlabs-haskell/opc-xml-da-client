module Main where

import Data.Default
import qualified Network.HTTP.Client as Hc
import qualified OpcXmlDaClient as Opc
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Prelude
import qualified Data.Vector as V

defBrowse :: Opc.Browse
defBrowse = Opc.Browse
  V.empty
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  10
  Opc.AllBrowseFilter
  Nothing
  Nothing
  True
  True
  True


getItemNames :: Hc.Manager -> IO (V.Vector Text)
getItemNames manager = do
  Opc.browse manager def uri defBrowse >>= \case
    Left e -> error $ show e
    Right r -> pure $ fmap (fromMaybe "" . #itemName) $ V.filter #isItem $ #elements r
      

uri :: Opc.Uri
uri = fromJust . Opc.textUri $
  "http://info.advosol.com/XMLDADemo/XML_Sim/OpcXmlDaServer.asmx"

makeReadItem :: Text -> Opc.ReadRequestItem
makeReadItem n = Opc.ReadRequestItem Nothing Nothing (Just n) Nothing Nothing

makeItemValue :: Text -> Opc.ItemValue
makeItemValue n = Opc.ItemValue
  { Opc._diagnosticInfo = Nothing
  , Opc._value = Nothing
  , Opc._quality = Nothing
  , Opc._valueTypeQualifier = Nothing
  , Opc._itemPath = Nothing
  , Opc._itemName = Just n
  , Opc._clientItemHandle = Nothing
  , Opc._timestamp = Nothing
  , Opc._resultId = Nothing
  }

defRequestOptions :: Opc.RequestOptions
defRequestOptions = Opc.RequestOptions
  True
  True
  True
  True
  True
  Nothing
  Nothing
  Nothing

serverSubHandleRef :: IORef Text
serverSubHandleRef = unsafePerformIO $ newIORef ""
{-# NOINLINE serverSubHandleRef #-}

main = do
  let manager = unsafePerformIO $ Hc.newManager Hc.defaultManagerSettings
      op _op = _op manager def uri
  itemNames <- getItemNames manager
  defaultMain $
    testGroup "Mocking server interactions" $
          [ testCase "GetStatus" $ do
              _res <- op Opc.getStatus $ Opc.GetStatus Nothing Nothing
              assertBool (show _res) $ isRight _res
          , testCase "Browse" $ do
              _res <- op Opc.browse defBrowse
              assertBool (show _res) $ isRight _res
          , testCase "Read" $ do
              _res <- op Opc.read $ Opc.Read
                -- "options" is required
                { Opc._options = Just defRequestOptions
                , Opc._itemList = Just $ Opc.ReadRequestItemList
                    (fmap makeReadItem itemNames)
                    Nothing
                    Nothing
                    Nothing
                }
              assertBool (show _res) $ isRight _res
          , testCase "Write" $ do
              _res <- op Opc.write $ Opc.Write
                -- "options" is required
                { Opc._options = Just defRequestOptions
                , Opc._itemList = Just $ Opc.WriteRequestItemList
                    { Opc._items = fmap makeItemValue itemNames
                    , Opc._itemPath = Nothing
                    }
                , Opc._returnValuesOnReply = False
                }
              assertBool (show _res) $ isRight _res
          , testCase "Subscribe" $ do
              _res <- op Opc.subscribe $ Opc.Subscribe
                { Opc._options = Just defRequestOptions 
                , Opc._returnValuesOnReply = False
                , Opc._itemList = Just $ Opc.SubscribeRequestItemList
                  { Opc._itemPath =  Nothing
                  , Opc._reqType = Nothing
                  , Opc._deadband = Nothing
                  , Opc._requestedSamplingRate = Nothing
                  , Opc._enableBuffering = Nothing
                  , Opc._items = V.fromList
                      [ Opc.SubscribeRequestItem
                          { Opc._itemPath = Nothing
                          , Opc._reqType = Nothing
                          , Opc._itemName = listToMaybe $ V.toList itemNames
                          , Opc._clientItemHandle = Nothing
                          , Opc._deadband = Nothing
                          , Opc._requestedSamplingRate = Nothing
                          , Opc._enableBuffering = Nothing
                          }
                      ]
                  }
                , Opc._subscriptionPingRate = Nothing
                }
              case _res of
                Left err -> assertBool (show err) False
                Right Opc.SubscribeResponse{..} -> case _serverSubHandle of
                  Nothing -> error "SubHandle not found"
                  Just sh -> writeIORef serverSubHandleRef sh
                
          , testCase "Subscription polled refresh" $ do
              sh <- readIORef serverSubHandleRef
              _res <- op Opc.subscriptionPolledRefresh $ Opc.SubscriptionPolledRefresh
                { Opc._options = Just defRequestOptions 
                , Opc._serverSubHandles = V.fromList [sh]
                , Opc._holdTime = Nothing
                , Opc._waitTime = 1
                , Opc._returnAllItems = False
                }
              assertBool (show _res) $ isRight _res
          , testCase "Subscription cancel" $ do
              sh <- readIORef serverSubHandleRef
              _res <- op Opc.subscriptionCancel $ Opc.SubscriptionCancel
                { Opc._serverSubHandle = Just sh
                , Opc._clientRequestHandle = Nothing
                }
              assertBool (show _res) $ isRight _res
          , testCase "Get properties" $ do
              _res <- op Opc.getProperties $ Opc.GetProperties
                { Opc._itemIds = itemNames <&> \n -> Opc.ItemIdentifier
                      { Opc._itemPath = Nothing
                      , Opc._itemName = Just n
                      }
                , Opc._propertyNames = V.empty
                , Opc._localeId = Nothing
                , Opc._clientRequestHandle = Nothing
                , Opc._itemPath = Nothing
                , Opc._returnAllProperties = False
                , Opc._returnPropertyValues = False
                , Opc._returnErrorText = False
                }
              assertBool (show _res) $ isRight _res
          ]
