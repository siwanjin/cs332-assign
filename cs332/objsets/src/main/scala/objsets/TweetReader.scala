package objsets

object TweetReader {

  def unparseToData(tws: List[Tweet]): String = {
    val buf = new StringBuffer
    for (tw <- tws) {
      val json = "{ \"user\": \"" + tw.user + "\", \"text\": \"" +
        tw.text.replaceAll("\"", "\\\\\"") + "\", \"retweets\": " +
        tw.retweets + ".0 }"
      buf.append(json + ",\n")
    }
    buf.toString
  }

  def getList[T](s: String): List[T] = s.split(",").toList.asInstanceOf[List[T]]

  def getMap(s: String): Map[String, Any] = Map() // placeholder if needed

  def getTweets(user: String, json: String): List[Tweet] =
    getTweetData(user, json)

  /**
   * JSON 배열 문자열에서 각 Tweet 객체로 파싱하여 리스트로 반환
   */
  def getTweetData(user: String, json: String): List[Tweet] = {
    val tweetBlocks = json
      .split("\\},\\s*\\{")
      .toList
      .map(_.replaceAll("^\\[?\\{?", "").replaceAll("\\}?\\]?$", "")) // 앞/뒤 괄호 제거

    tweetBlocks.flatMap(block => parseOneTweet(user, s"{$block}"))
  }

  def parseOneTweet(user: String, json: String): Option[Tweet] = {
    val userRegex = """"user"\s*:\s*"([^"]+)"""".r
    val textRegex = """"text"\s*:\s*"([^"]+)"""".r
    val retweetRegex = """"retweets"\s*:\s*(\d+)""".r

    for {
      userMatch <- userRegex.findFirstMatchIn(json)
      textMatch <- textRegex.findFirstMatchIn(json)
      retweetMatch <- retweetRegex.findFirstMatchIn(json)
    } yield new Tweet(userMatch.group(1), textMatch.group(1), retweetMatch.group(1).toInt)
  }

  val sites = List("gizmodo", "TechCrunch", "engadget", "amazondeals", "CNET", "gadgetlab", "mashable")

  private val gizmodoTweets = getTweetData("gizmodo", TweetData.gizmodo)
  private val techCrunchTweets = getTweetData("TechCrunch", TweetData.TechCrunch)
  private val engadgetTweets = getTweetData("engadget", TweetData.engadget)
  private val amazondealsTweets = getTweetData("amazondeals", TweetData.amazondeals)
  private val cnetTweets = getTweetData("CNET", TweetData.CNET)
  private val gadgetlabTweets = getTweetData("gadgetlab", TweetData.gadgetlab)
  private val mashableTweets = getTweetData("mashable", TweetData.mashable)

  private val sources = List(gizmodoTweets, techCrunchTweets, engadgetTweets, amazondealsTweets, cnetTweets, gadgetlabTweets, mashableTweets)

  val tweetMap: Map[String, List[Tweet]] = Map() ++ Seq(
    (sites(0) -> gizmodoTweets),
    (sites(1) -> techCrunchTweets),
    (sites(2) -> engadgetTweets),
    (sites(3) -> amazondealsTweets),
    (sites(4) -> cnetTweets),
    (sites(5) -> gadgetlabTweets),
    (sites(6) -> mashableTweets)
  )

  val tweetSets: List[TweetSet] = sources.map(toTweetSet)

  private val siteTweetSetMap: Map[String, TweetSet] =
    Map() ++ (sites zip tweetSets)

  private def unionOfAllTweetSets(curSets: List[TweetSet], acc: TweetSet): TweetSet =
    if (curSets.isEmpty) acc
    else unionOfAllTweetSets(curSets.tail, acc.union(curSets.head))

  val allTweets: TweetSet = tweetSets.foldLeft(new Empty: TweetSet)((acc, ts) => acc.union(ts))

  def toTweetSet(tweets: List[Tweet]): TweetSet = {
    tweets.foldLeft(new Empty: TweetSet)((acc, tweet) => acc.incl(tweet))
  }
}
