package stv

import upickle.Js

/**
  * Created by bwbecker on 2016-08-09.
  */
object Pickler extends upickle.AttributeTagged {

  override def OptionW[T: Writer]: Writer[Option[T]] = Writer {
    case None => Js.Null
    case Some(s) => implicitly[Writer[T]].write(s)
  }

  override def OptionR[T: Reader]: Reader[Option[T]] = Reader {
    case Js.Null => None
    case v: Js.Value => Some(implicitly[Reader[T]].read.apply(v))
  }

}