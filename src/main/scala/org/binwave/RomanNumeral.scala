package org.binwave

/**
  * ローマ数字を表すクラス
  *
  * @param intValue 数値
  */
class RomanNumeral(intValue: Int) extends Ordered[RomanNumeral] {
  // ローマ数字は1から3999までしか表すことができない
  require(intValue > 0 && intValue < 4000, s"intValue must be between 1 and 3999. : $intValue")

  /**
    * 数値
    */
  private val value: Int = intValue

  /**
    * Int型の値を返す
    *
    * @return 数値
    */
  def toInt: Int = value

  override def equals(obj: Any): Boolean = {
    obj match {
      case RomanNumeral(n) => n == value
      case _ => false
    }
  }

  override def hashCode(): Int = value.hashCode

  /**
    * 文字列に変換する
    *
    * @return ローマ数字をアルファベット大文字で表した文字列
    */
  override def toString: String = roman(THOUSAND) + roman(HUNDRED) + roman(TEN) + roman(ONE)

  /**
    * 指定された桁のローマ数字を計算する
    *
    * @param ch 対象とする桁の文字セット
    * @return
    */
  private def roman(ch: RomanChars): String = {
    value / ch.unit % 10 match {
      case 9 => ch.one + ch.ten
      case 4 => ch.one + ch.five
      case n: Int => ch.five * (n / 5) + ch.one * (n % 5)
    }
  }

  /**
    * 1桁のローマ数字を表す文字セット
    *
    * @param unit 単位（oneに相当する数値）
    * @param one  1に対応する文字
    * @param five 5に対応する文字
    * @param ten  10に対応する文字
    */
  private abstract case class RomanChars(unit: Int, one: String, five: String, ten: String)

  /**
    * 1000の単位を表す文字セット
    */
  private object THOUSAND extends RomanChars(1000, "M", null, null)

  /**
    * 100の単位を表す文字セット
    */
  private object HUNDRED extends RomanChars(100, "C", "D", "M")

  /**
    * 10の単位を表す文字セット
    */
  private object TEN extends RomanChars(10, "X", "L", "C")

  /**
    * 1の単位を表す文字セット
    */
  private object ONE extends RomanChars(1, "I", "V", "X")

  def +(other: RomanNumeral): RomanNumeral = RomanNumeral(value + other.value)

  def -(other: RomanNumeral): RomanNumeral = RomanNumeral(value - other.value)

  def *(other: RomanNumeral): RomanNumeral = RomanNumeral(value * other.value)

  def /(other: RomanNumeral): RomanNumeral = RomanNumeral(value / other.value)

  override def compare(that: RomanNumeral): Int = value.compare(that.value)
}

object RomanNumeral {
  /**
    * ローマ数字で表すことのできる最大値
    */
  val MIN = RomanNumeral(1)

  /**
    * ローマ数字で表すことのできる最小値
    */
  val MAX = RomanNumeral(3999)

  def apply(intValue: Int): RomanNumeral = new RomanNumeral(intValue)

  def unapply(arg: RomanNumeral): Option[Int] = Some(arg.value)
}
