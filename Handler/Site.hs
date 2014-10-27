{-# LANGUAGE OverloadedStrings #-}
module Handler.Site where

import Import
import Yesod.Form.Bootstrap3
import Data.Time.Calendar (showGregorian)



getHomeR :: Handler Html
getHomeR = do
  entries <- fmap (map entityVal) (runDB (selectList [EntryPublished !=. Nothing] [Desc EntryPublished]))
  defaultLayout $ do
    setTitle "Two Wrongs"
    $(widgetFile "homepage")



getEntryR :: Text -> Handler Html
getEntryR slug = do
  entry <- fmap entityVal (runDB (getBy404 (UniqueSlug slug)))
  author <- isAuthor
  defaultLayout $ do
    setTitle ("Two Wrongs :: " <> toHtml (entryTitle entry))
    $(widgetFile "detail")



entryForm :: Maybe Entry -> Form Entry
entryForm existing = renderBootstrap3 BootstrapBasicForm $
  Entry <$> areq textField (bfs ("Title: " :: Text)) (fmap entryTitle existing)
        <*> areq textField (bfs ("Slug: " :: Text)) (fmap entrySlug existing)
        <*> aopt dayField (bfs ("Published: " :: Text)) (fmap entryPublished existing)
        <*> areq htmlField (bfs ("Content: " :: Text)) (fmap entryContent existing)

getNewR :: Handler Html
getNewR = do
  let next = NewR
  (formFields, enctype) <- generateFormPost (entryForm Nothing)
  defaultLayout $ do
    setTitle "Two Wrongs :: Create New Entry"
    $(widgetFile "entryform")

postNewR :: Handler Html
postNewR = do
  let next = NewR
  ((result, formFields), enctype) <- runFormPost (entryForm Nothing)
  case result of
    FormSuccess entry -> do
      exists <- runDB (insertUnique entry)
      case exists of
        Just _  -> setMessage "Blog entry successfully added" >> redirect (EntryR (entrySlug entry))
        Nothing -> setMessage "That slug already leads to a different entry"
    _ -> setMessage "Invalid form submission, check for errors"
        
  defaultLayout $ do
    setTitle "Two Wrongs :: Create New Entry"
    $(widgetFile "entryform")


getEditR :: Text -> Handler Html
getEditR slug = do
  let next = EditR slug
  entry <- fmap entityVal (runDB (getBy404 (UniqueSlug slug)))
  (formFields, enctype) <- generateFormPost (entryForm (Just entry))
  defaultLayout $ do
    setTitle "Two Wrongs :: Edit Entry"
    $(widgetFile "entryform")

postEditR :: Text -> Handler Html
postEditR slug = do
  let next = EditR slug
  ((result, formFields), enctype) <- runFormPost (entryForm Nothing)
  case result of
    FormSuccess entry -> do
      key <- fmap entityKey (runDB (getBy404 (UniqueSlug slug)))
      runDB (repsert key entry)
      setMessage "Blog entry successfully edited"
      redirect (EntryR slug)
    _ -> setMessage "Invalid form submission, check for errors"
        
  defaultLayout $ do
    setTitle "Two Wrongs :: Create New Entry"
    $(widgetFile "entryform")



authorForm :: Form Authors
authorForm = renderBootstrap3 BootstrapBasicForm $
  Authors <$> areq textField (bfs ("UID: " :: Text)) Nothing

getAuthorsR :: Handler Html
getAuthorsR = do
  (formFields, enctype) <- generateFormPost authorForm

  authors <- fmap (map entityVal) (runDB (selectList [] [Desc AuthorsId]))
  defaultLayout $ do
    setTitle "Two Wrongs :: Authorized Authors"
    $(widgetFile "authors")

postAuthorsR :: Handler Html
postAuthorsR = do
  ((result, formFields), enctype) <- runFormPost authorForm
  formFailure <- case result of
    FormSuccess author -> do
      exists <- runDB (insertUnique author)
      case exists of
        Just _  -> setMessage "Author successfully added!" >> redirect AuthorsR
        Nothing -> setMessage "An author with that UID already exists"
    _ -> setMessage "Invalid form submission, check for errors"
        
  authors <- fmap (map entityVal) (runDB (selectList [] [Desc AuthorsId]))
  defaultLayout $ do
    setTitle "Two Wrongs :: Authorized Authors"
    $(widgetFile "authors")




