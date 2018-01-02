package org.binwave

import org.scalatest._

class RomanNumeralTest extends FunSuite with Matchers {

  /**
    * サンプルデータ
    * @see https://ja.wikipedia.org/wiki/%E3%83%AD%E3%83%BC%E3%83%9E%E6%95%B0%E5%AD%97#%E8%A1%A8%E8%A8%98%E6%B3%95
    */
  val samples = List(
    (1, "I"), (2, "II"), (3, "III"), (4, "IV"), (5, "V"),
    (6, "VI"), (7, "VII"), (8, "VIII"), (9, "IX"), (10, "X"),
    (20, "XX"), (30, "XXX"), (40, "XL"), (50, "L"), (60, "LX"),
    (70, "LXX"), (80, "LXXX"), (90, "XC"), (100, "C"), (200, "CC"),
    (300, "CCC"), (400, "CD"), (500, "D"), (600, "DC"), (700, "DCC"),
    (800, "DCCC"), (900, "CM"), (1000, "M"), (2000, "MM"), (3000, "MMM"))

  test("サンプルデータの数値を正しくローマ数字に変換できる") {
    samples.foreach(s => RomanNumeral(s._1).toString shouldBe s._2)
  }

  test("最小値より小さい値のインスタンスを作成すると例外発生") {
    val underMin = RomanNumeral.MIN.toInt - 1
    a [IllegalArgumentException] should be thrownBy RomanNumeral(underMin)
  }

  test("最大値より大きい値のインスタンスを作成すると例外発生") {
    val overMax = RomanNumeral.MAX.toInt + 1
    a [IllegalArgumentException] should be thrownBy RomanNumeral(overMax)
  }

  test("四則演算ができる") {
    RomanNumeral(3) + RomanNumeral(6) shouldBe RomanNumeral(9)
    RomanNumeral(14) - RomanNumeral(8) shouldBe RomanNumeral(6)
    RomanNumeral(3) * RomanNumeral(8) shouldBe RomanNumeral(24)
    RomanNumeral(1024) / RomanNumeral(4) shouldBe RomanNumeral(256)
  }
}

