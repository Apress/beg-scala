trait TwitterElem

/*
 <status>
 created_at
 id
 text
 source
 truncated
 in_reply_to_status_id
 in_reply_to_user_id
 favorited
 */
case class TwitterStatus(id: Long,
                         createdAt: String,
                         text: String,
                         source: String,
                         truncated: Boolean,
                         inReplyToStatus: Option[Long],
                         inReplyToUser: Option[Long],
                         favorited: Boolean,
                         user: TwitterUser) extends TwitterElem

object TwitterStatus extends SafeMap {
  def apply(in: Any): Option[TwitterStatus] =
  for {m <- in.is[Map[String, Any]]
       id <- m.sGet[String]("id").map(_.toLong)
       createdAt <- m.sGet[String]("created_at")
       text <- m.sGet[String]("text")
       source <- m.sGet[String]("source")
       truncated <- m.sGet[Boolean]("truncated")
       inRepSt = m.sGet[Double]("in_reply_to_status_id").map(_.toLong)
       inRepUsr = m.sGet[Double]("in_reply_to_user_id").map(_.toLong)
       fav = m.sGet[Boolean]("favorited") getOrElse false
       userObj <- m.sGet[Map[String, Any]]("user")
       user <- TwitterUser(userObj)
  } yield new TwitterStatus(id, createdAt, text, source,
                            truncated,
                            inRepSt, inRepUsr, fav, user)

  def fromList(in: Any): List[TwitterStatus] = {
    for {list <- in.is[List[Any]].toList
         item <- list
         st <- apply(item)
    } yield st
  }

}

/*
 <user>
 id
 name
 screen_name
 description
 location
 profile_image_url
 url
 protected
 followers_count
 */
case class TwitterUser(id: Long,
                       name: String,
                       screenName: String,
                       description: Option[String],
                       location: Option[String],
                       image: Option[String],
                       url: Option[String],
                       protectd: Boolean,
                       followerCount: Option[Int]) extends TwitterElem

object TwitterUser extends SafeMap {
  def apply(in: Any): Option[TwitterUser] =
  for {m <- in.is[Map[String, Any]]
       id <- m.sGet[String]("id").map(_.toLong)
       name <- m.sGet[String]("name")
       scrName <- m.sGet[String]("screen_name")
       desc = m.sGet[String]("description")
       loc = m.sGet[String]("location")
       image = m.sGet[String]("profile_image_url")
       url = m.sGet[String]("url")
       prot = m.sGet[Boolean]("protected") getOrElse false
       fc = m.sGet[Double]("followers_count").map(_.toInt)
  } yield new TwitterUser(id, name, scrName,
                          desc, loc, image, url, prot,
                          fc)
}
