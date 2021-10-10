package image

import utils._

case class Pixel(val argb: Int) extends AnyVal {  
  def a: Int = argb >> 24 & 0xFF
  def r: Int = argb >> 16 & 0xFF
  def g: Int = argb >> 8 & 0xFF
  def b: Int = argb & 0xFF
  
  def toInt: Int = argb
  def applyByComponent(f: Int => Int): Pixel = Pixel(f(r), f(g), f(b), a)
  
	def +(that: Pixel) = Pixel(r + that.r, g + that.g, b + that.b, a + that.a)
	def -(that: Pixel) = Pixel(r - that.r, g - that.g, b - that.b, a - that.a)
	def *(const: Double) = applyByComponent(x => (x * const).toInt)
	def /(const: Double) = {
		if (const == 0)
			throw new IllegalArgumentException
		applyByComponent(x => (x / const).toInt)
	}
}


object Pixel {
  implicit def intToPixel(const: Int): Pixel = new Pixel(const)
  implicit def doubleToPixel(const: Double) = new Pixel(const.toInt)
  def apply(r: Int, g: Int, b: Int, alpha: Int): Pixel = {
    val int = alpha << 24 | (r & 0xFF) << 16 | (g & 0xFF) << 8 | (b & 0xFF) << 0
    Pixel(int)
  }
  def apply(color: RGBAColor): Pixel = color.toPixel

  def apply(): Pixel = {
    Pixel(RGBAColor.transparent)
  }
}