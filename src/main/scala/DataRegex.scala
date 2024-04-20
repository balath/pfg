package common

object DataRegex {
  val dataPath = "data/rawData.txt"
  val chordRegex = "(vii|vi|bVII|bvii|iii|ii|bIII|bII|iv|IV|v|V|bVI|i|II|I)(o|ø|aug)*(64|65|7|6|43|42)*_?(vi|vii|bVII|bvii|iii|ii|bIII|iv|IV|v|V|bVI|i|II|I)*".r
  val metadataRegex = "([0-9]+),([a-g][fs]?),(min|maj),([34]),([1-9][0-9]?),([1-4]),([1-4])".r
  val chordAndMetadataRegex = "(.*[^:]):(.*[^:])".r  
}
