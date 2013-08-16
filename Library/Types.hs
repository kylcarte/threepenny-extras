
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

import Control.Applicative
import Data.Text

-- Patron {{{

data Patron = Patron
  { patronId    :: Maybe Integer -- only Maybe so we can support ToRow
  , firstName   :: Text
  , lastName    :: Text
  , fstContact  :: Contact -- Text ?
  , sndContact  :: Contact -- Text ?
  , phoneNumber :: Maybe PhoneNumber
  , emailAddr   :: Maybe Text
  , homeAddr    :: Maybe Text
  } deriving (Eq,Show)

instance FromRow Patron where
  fromRow = Patron
        <$> field                     -- patronId
        <*> field <*> field           -- first, last names
        <*> field <*> field           -- preferred means of contact
        <*> field <*> field <*> field -- phone, email, home address

instance ToRow Patron where
  toRow p = toRow
    ( patronId    p
    , firstName   p
    , lastName    p
    , fstContact  p
    , sndContact  p
    , phoneNumber p
    , emailAddr   p
    , homeAddr    p
    )



newtype PhoneNumber = PhoneNumber Integer deriving (Eq,Show)

instance FromRow PhoneNumber where
  fromRow = PhoneNumber <$> field

instance ToRow PhoneNumber where
  toRow (PhoneNumber n) = toRow (Only n)



data Contact
  = Phone
  | Email
  deriving (Eq,Show)

instance FromRow Contact where
  fromRow = do
    pr <- field
    pure $ case (pr :: Integer) of
      0 -> Phone
      1 -> Email

instance ToRow Contact where
  toRow pr = toRow $ Only n
    where
    n :: Integer
    n = case pr of
          Phone -> 0
          Email -> 1

-- }}}

-- Book {{{

data Book = Book
  { bookId :: Maybe Integer -- only Maybe so we can support ToRow
  , title  :: Text
  , author :: Text
  , isbn   :: Maybe Text
  } deriving (Eq,Show)

-- }}}

