module Data.AddressBook where

import Prelude
import Control.Plus (empty)
import Data.List (nubBy, null, List(..), filter, head)
import Data.Maybe (Maybe)

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": "  <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == street

addressBookContainsName :: String -> String -> AddressBook -> Boolean
addressBookContainsName firstName lastName = not null <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: String -> String -> AddressBook -> AddressBook
removeDuplicates firstName lastName = nubBy hasSameName
  where
    hasSameName :: Entry -> Entry -> Boolean
    hasSameName entry1 entry2 =
      entry1.firstName == firstName &&
      entry1.lastName == lastName &&
      entry1.firstName == entry2.firstName &&
      entry1.lastName == entry2.lastName
