package image

case class RGBAColor(r: Int, g: Int, b: Int, a: Int = 0) {
  require(0 <= r && r <= 255, "Invalid value for the red component")
  require(0 <= g && g <= 255, "Invalid value for the green component")
  require(0 <= b && b <= 255, "Invalid value for the blue component")
  require(0 <= a && a <= 255, "Invalid value for the alpha component")

  def toPixel: Pixel = Pixel(r, g, b, a)
  def toInt: Int = ((r & 0xFF) << 24) | ((g & 0xFF) << 16) | ((b & 0xFF) << 8) | a & 0xFF
}

object RGBAColor {
  def apply(argb: Int): RGBAColor = RGBAColor((argb >> 24) & 0xFF, (argb >> 16) & 0xFF, (argb >> 8) & 0xFF, argb & 0xFF)
  implicit def intToColor(argb: Int): RGBAColor = apply(argb)
  val transparent: RGBAColor = RGBAColor(255, 255, 255, 0)
}