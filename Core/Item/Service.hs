module Core.Item.Service where
  
import Core.Item.Types
import qualified Web.Slug as WSlug
import Web.Slug (Slug)

getItems :: (ItemRepo m) => ItemFilter -> m [Item]
getItems = findItems Nothing

getItem :: (ItemRepo m) => Slug -> m (Either ItemError Item)
getItem slug = runExceptT $ do
  result <- lift $ findItems(Just slug) Nothing (ItemFilter Nothing Nothing)
  case result of
    [item] -> return item
    _ -> throwError $ ItemErrorNotFound slug
    
createItem :: (ItemRepo m) => Item -> m (Either ItemError Item)
createItem param = do 
  slug <- genSlug (itemTitle param)
  getItem slug
  
deleteItem :: (ItemRepo m) => Slug -> m (Either ItemError ())
deleteItem slug = runExceptT $ do
  lift $ deleteItemBySlug slug

genSlug :: Text -> Text
genSlug title = 
  maybe "invalidSlug" WSlug.unSlug $ WSlug.mkSlug $ unwords [title]
  
class (Monad m) => ItemRepo m where
  findItems :: Maybe Slug -> ItemFilter -> m [Item]
  deleteItemBySlug :: Slug -> m ()