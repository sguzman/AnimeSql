// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!
//
// Protofile syntax: PROTO3

package com.github.sguzman.anime.protoc.items

@SerialVersionUID(0L)
final case class AnimeUser(
    anime: scala.Option[com.github.sguzman.anime.protoc.items.Anime] = None,
    user: scala.Option[com.github.sguzman.anime.protoc.items.UserStats] = None
    ) extends scalapb.GeneratedMessage with scalapb.Message[AnimeUser] with scalapb.lenses.Updatable[AnimeUser] {
    @transient
    private[this] var __serializedSizeCachedValue: _root_.scala.Int = 0
    private[this] def __computeSerializedValue(): _root_.scala.Int = {
      var __size = 0
      if (anime.isDefined) { __size += 1 + _root_.com.google.protobuf.CodedOutputStream.computeUInt32SizeNoTag(anime.get.serializedSize) + anime.get.serializedSize }
      if (user.isDefined) { __size += 1 + _root_.com.google.protobuf.CodedOutputStream.computeUInt32SizeNoTag(user.get.serializedSize) + user.get.serializedSize }
      __size
    }
    final override def serializedSize: _root_.scala.Int = {
      var read = __serializedSizeCachedValue
      if (read == 0) {
        read = __computeSerializedValue()
        __serializedSizeCachedValue = read
      }
      read
    }
    def writeTo(`_output__`: _root_.com.google.protobuf.CodedOutputStream): Unit = {
      anime.foreach { __v =>
        _output__.writeTag(1, 2)
        _output__.writeUInt32NoTag(__v.serializedSize)
        __v.writeTo(_output__)
      };
      user.foreach { __v =>
        _output__.writeTag(2, 2)
        _output__.writeUInt32NoTag(__v.serializedSize)
        __v.writeTo(_output__)
      };
    }
    def mergeFrom(`_input__`: _root_.com.google.protobuf.CodedInputStream): com.github.sguzman.anime.protoc.items.AnimeUser = {
      var __anime = this.anime
      var __user = this.user
      var _done__ = false
      while (!_done__) {
        val _tag__ = _input__.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __anime = Option(_root_.scalapb.LiteParser.readMessage(_input__, __anime.getOrElse(com.github.sguzman.anime.protoc.items.Anime.defaultInstance)))
          case 18 =>
            __user = Option(_root_.scalapb.LiteParser.readMessage(_input__, __user.getOrElse(com.github.sguzman.anime.protoc.items.UserStats.defaultInstance)))
          case tag => _input__.skipField(tag)
        }
      }
      com.github.sguzman.anime.protoc.items.AnimeUser(
          anime = __anime,
          user = __user
      )
    }
    def getAnime: com.github.sguzman.anime.protoc.items.Anime = anime.getOrElse(com.github.sguzman.anime.protoc.items.Anime.defaultInstance)
    def clearAnime: AnimeUser = copy(anime = None)
    def withAnime(__v: com.github.sguzman.anime.protoc.items.Anime): AnimeUser = copy(anime = Option(__v))
    def getUser: com.github.sguzman.anime.protoc.items.UserStats = user.getOrElse(com.github.sguzman.anime.protoc.items.UserStats.defaultInstance)
    def clearUser: AnimeUser = copy(user = None)
    def withUser(__v: com.github.sguzman.anime.protoc.items.UserStats): AnimeUser = copy(user = Option(__v))
    def getFieldByNumber(__fieldNumber: _root_.scala.Int): scala.Any = {
      (__fieldNumber: @_root_.scala.unchecked) match {
        case 1 => anime.orNull
        case 2 => user.orNull
      }
    }
    def getField(__field: _root_.scalapb.descriptors.FieldDescriptor): _root_.scalapb.descriptors.PValue = {
      require(__field.containingMessage eq companion.scalaDescriptor)
      (__field.number: @_root_.scala.unchecked) match {
        case 1 => anime.map(_.toPMessage).getOrElse(_root_.scalapb.descriptors.PEmpty)
        case 2 => user.map(_.toPMessage).getOrElse(_root_.scalapb.descriptors.PEmpty)
      }
    }
    def toProtoString: _root_.scala.Predef.String = _root_.scalapb.TextFormat.printToUnicodeString(this)
    def companion = com.github.sguzman.anime.protoc.items.AnimeUser
}

object AnimeUser extends scalapb.GeneratedMessageCompanion[com.github.sguzman.anime.protoc.items.AnimeUser] {
  implicit def messageCompanion: scalapb.GeneratedMessageCompanion[com.github.sguzman.anime.protoc.items.AnimeUser] = this
  def fromFieldsMap(__fieldsMap: scala.collection.immutable.Map[_root_.com.google.protobuf.Descriptors.FieldDescriptor, scala.Any]): com.github.sguzman.anime.protoc.items.AnimeUser = {
    require(__fieldsMap.keys.forall(_.getContainingType() == javaDescriptor), "FieldDescriptor does not match message type.")
    val __fields = javaDescriptor.getFields
    com.github.sguzman.anime.protoc.items.AnimeUser(
      __fieldsMap.get(__fields.get(0)).asInstanceOf[scala.Option[com.github.sguzman.anime.protoc.items.Anime]],
      __fieldsMap.get(__fields.get(1)).asInstanceOf[scala.Option[com.github.sguzman.anime.protoc.items.UserStats]]
    )
  }
  implicit def messageReads: _root_.scalapb.descriptors.Reads[com.github.sguzman.anime.protoc.items.AnimeUser] = _root_.scalapb.descriptors.Reads{
    case _root_.scalapb.descriptors.PMessage(__fieldsMap) =>
      require(__fieldsMap.keys.forall(_.containingMessage == scalaDescriptor), "FieldDescriptor does not match message type.")
      com.github.sguzman.anime.protoc.items.AnimeUser(
        __fieldsMap.get(scalaDescriptor.findFieldByNumber(1).get).flatMap(_.as[scala.Option[com.github.sguzman.anime.protoc.items.Anime]]),
        __fieldsMap.get(scalaDescriptor.findFieldByNumber(2).get).flatMap(_.as[scala.Option[com.github.sguzman.anime.protoc.items.UserStats]])
      )
    case _ => throw new RuntimeException("Expected PMessage")
  }
  def javaDescriptor: _root_.com.google.protobuf.Descriptors.Descriptor = ItemsProto.javaDescriptor.getMessageTypes.get(3)
  def scalaDescriptor: _root_.scalapb.descriptors.Descriptor = ItemsProto.scalaDescriptor.messages(3)
  def messageCompanionForFieldNumber(__number: _root_.scala.Int): _root_.scalapb.GeneratedMessageCompanion[_] = {
    var __out: _root_.scalapb.GeneratedMessageCompanion[_] = null
    (__number: @_root_.scala.unchecked) match {
      case 1 => __out = com.github.sguzman.anime.protoc.items.Anime
      case 2 => __out = com.github.sguzman.anime.protoc.items.UserStats
    }
    __out
  }
  lazy val nestedMessagesCompanions: Seq[_root_.scalapb.GeneratedMessageCompanion[_]] = Seq.empty
  def enumCompanionForFieldNumber(__fieldNumber: _root_.scala.Int): _root_.scalapb.GeneratedEnumCompanion[_] = throw new MatchError(__fieldNumber)
  lazy val defaultInstance = com.github.sguzman.anime.protoc.items.AnimeUser(
  )
  implicit class AnimeUserLens[UpperPB](_l: _root_.scalapb.lenses.Lens[UpperPB, com.github.sguzman.anime.protoc.items.AnimeUser]) extends _root_.scalapb.lenses.ObjectLens[UpperPB, com.github.sguzman.anime.protoc.items.AnimeUser](_l) {
    def anime: _root_.scalapb.lenses.Lens[UpperPB, com.github.sguzman.anime.protoc.items.Anime] = field(_.getAnime)((c_, f_) => c_.copy(anime = Option(f_)))
    def optionalAnime: _root_.scalapb.lenses.Lens[UpperPB, scala.Option[com.github.sguzman.anime.protoc.items.Anime]] = field(_.anime)((c_, f_) => c_.copy(anime = f_))
    def user: _root_.scalapb.lenses.Lens[UpperPB, com.github.sguzman.anime.protoc.items.UserStats] = field(_.getUser)((c_, f_) => c_.copy(user = Option(f_)))
    def optionalUser: _root_.scalapb.lenses.Lens[UpperPB, scala.Option[com.github.sguzman.anime.protoc.items.UserStats]] = field(_.user)((c_, f_) => c_.copy(user = f_))
  }
  final val ANIME_FIELD_NUMBER = 1
  final val USER_FIELD_NUMBER = 2
}
