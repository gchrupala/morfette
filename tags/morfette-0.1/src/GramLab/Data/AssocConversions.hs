
class AssocC coll1 coll2 key value | coll1 -> key value , coll2 -> key value where
    fromAssoc :: coll1 -> coll2

instance AssocC [(a, b)]            [(a, b)]            a   b where
    fromAssoc = id

instance (Ord a) => AssocC [(a, b)] (Map.Map a b)       a   b where
    fromAssoc = Map.fromList 

instance AssocC [(Int, b)]          (IntMap.IntMap b)   Int b where
    fromAssoc = IntMap.fromList

instance AssocC (IntMap.IntMap b)   (IntMap.IntMap b)   Int b where
    fromAssoc = id

instance AssocC (IntMap.IntMap b)   [(Int, b)]          Int b where
    fromAssoc = IntMap.toAscList

instance AssocC (IntMap.IntMap b)   (Map.Map Int b)     Int b where
    fromAssoc = IntMap.foldWithKey Map.insert Map.empty

instance AssocC (Map.Map a b)       (Map.Map a b)       a   b where
    fromAssoc = id

instance AssocC (Map.Map a b)       [(a, b)]            a   b where
    fromAssoc = Map.toAscList

instance AssocC (Map.Map Int b)     (IntMap.IntMap b)   Int b where
    fromAssoc = Map.foldWithKey IntMap.insert IntMap.empty

