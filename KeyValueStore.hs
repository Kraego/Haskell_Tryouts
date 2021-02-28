-- Datatype Definition: Generic Key Value
data KeyValStore key val = Empty | KeyValue key val (KeyValStore key val) deriving Show

contains :: Eq key => KeyValStore key val -> key -> Bool
contains Empty _ = False
contains (KeyValue key _ store) id  
    | key == id = True
    | otherwise = contains store id

insert :: Eq key => KeyValStore key val -> key -> val -> KeyValStore key val
insert Empty key val = KeyValue key val Empty
insert (KeyValue key value store) newKey newVal
    | key == newKey = KeyValue key newVal store
    | otherwise = KeyValue key value (insert store newKey newVal)


getValue :: Eq key => KeyValStore key val -> key -> Maybe val
getValue (KeyValue key value store) id 
    | key == id = Just value
    | contains store id = getValue store id
    | otherwise = Nothing

remove :: Eq key => KeyValStore key val -> key -> KeyValStore key val
remove Empty _ = Empty
remove (KeyValue key value store) id
    | key == id = store
    | contains store id = KeyValue key value (remove store id)

-- Testdata
testStore :: KeyValStore Integer String
testStore =  KeyValue 1 "foo" (KeyValue 2 "bar" (KeyValue 3 "krrr" (KeyValue 4 "hmmmmm" (KeyValue 8 "acht" Empty))))

testStoreEmpty :: KeyValStore Integer String
testStoreEmpty = Empty

