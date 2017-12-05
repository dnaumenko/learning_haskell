module ValidatePerson where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty | AgeTooLow deriving Show

type ValidatePerson a = Either [PersonInvalid] a


ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = if age >= 0 then Right age else Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = if name /= "" then Right name else Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' (Right nameOkay) (Right ageOkay) = Right (Person nameOkay ageOkay)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
-- ...


