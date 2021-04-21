
package FFT

trait HasDataConfig {
  val DataWidth = 32
  val BinaryPoint = 16
}

trait HasElaborateConfig {
  val FFTLength = 64
  val useGauss = false
  val supportIFFT = false
}
