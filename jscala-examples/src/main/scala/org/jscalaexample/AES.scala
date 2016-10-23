package org.jscalaexample

import org.jscala._

/**
 * @author Alexander Nemish
 */
object AesExample {

  def main(args: Array[String]) {
    val data = Array(22, 13, 8, 123456789)
    println("Input data " + data.mkString(","))
    val key = Array(1, 1, 1, 1)
    val aes = new Aes(key)
    val encrypted = aes.crypt(data, false)
    println("Encrypted Scala " + encrypted.mkString(","))
    val decrypted = aes.crypt(encrypted, true)
    println("Decrypted Scala " + decrypted.mkString(","))
    val main = javascript {
      val d = inject(data)
      val k = inject(key)
      val aes = new Aes(k)
      val encrypted = aes.crypt(d, false)
      print("Encrypted JS " + encrypted + "\n")
      val decrypted = aes.crypt(encrypted, true)
      print("Decrypted JS " + decrypted + "\n")
    }
    val js = Aes.jscala.javascript ++ main
    js.eval()
  }
}

@Javascript
class Aes(val key: Array[Int]) {
  val encTable = Array(new Array[Int](256), new Array[Int](256), new Array[Int](256), new Array[Int](256), new Array[Int](256))
  val decTable = Array(new Array[Int](256), new Array[Int](256), new Array[Int](256), new Array[Int](256), new Array[Int](256))
  var keys = Array(new Array[Int](44), new Array[Int](44))

  init()

  private def precompute() {
    val sbox = encTable(4)
    val sboxInv = decTable(4)
    val d = new Array[Int](256)
    val th = new Array[Int](256)
    var s = 0
    var tEnc = 0
    var tDec = 0

    // Compute double and third tables
    for (i <- 0 until 256) {
      d(i) = i << 1 ^ (i >> 7) * 283
      th(d(i) ^ i) = i
    }

    var x = 0
    var x2 = 0
    var x4 = 0
    var x8 = 0
    var xInv = 0

    while (sbox(x) == undefined || sbox(x) == 0) {
      // Compute sbox
      s = xInv ^ xInv << 1 ^ xInv << 2 ^ xInv << 3 ^ xInv << 4
      s = s >> 8 ^ s & 255 ^ 99
      sbox(x) = s
      sboxInv(s) = x

      // Compute MixColumns
      x2 = d(x)
      x4 = d(x2)
      x8 = d(x4)
      tDec = x8 * 0x1010101 ^ x4 * 0x10001 ^ x2 * 0x101 ^ x * 0x1010100
      tEnc = d(s) * 0x101 ^ s * 0x1010100

      for (i <- 0 until 4) {
        tEnc = tEnc << 24 ^ tEnc >>> 8
        encTable(i)(x) = tEnc
        tDec = tDec << 24 ^ tDec >>> 8
        decTable(i)(s) = tDec
      }

      x = x ^ (if (x2 != 0 && x2 != undefined) x2 else 1)
      xInv = if (th(xInv) != 0 && th(xInv) != undefined) th(xInv) else 1
    }
  }

  private def init() {
    precompute()

    var tmp = 0
    val encKey = new Array[Int](44)
    val decKey = new Array[Int](44)
    val sbox = this.encTable(4)
    val keyLen = key.length
    var rcon = 1

    if (keyLen != 4 && keyLen != 6 && keyLen != 8) {
      return
    }

    for (l <- 0 until 4) encKey(l) = key(l)
    this.keys = Array(encKey, decKey)

    // schedule encryption keys
    var i = keyLen
    while (i < 4 * keyLen + 28) {
      tmp = encKey(i - 1)

      // apply sbox
      if (i % keyLen == 0 || (keyLen == 8 && i % keyLen == 4)) {
        tmp = sbox(tmp >>> 24) << 24 ^ sbox(tmp >> 16 & 255) << 16 ^ sbox(tmp >> 8 & 255) << 8 ^ sbox(tmp & 255)

        // shift rows and add rcon
        if (i % keyLen == 0) {
          tmp = tmp << 8 ^ tmp >>> 24 ^ rcon << 24
          rcon = rcon << 1 ^ (rcon >> 7) * 283
        }
      }

      encKey(i) = encKey(i - keyLen) ^ tmp
      i += 1
    }
    // schedule decryption keys
    var j = 0
    while (i != 0) {
      tmp = encKey(if ((j & 3) != 0) i else i - 4)
      if (i <= 4 || j < 4) {
        decKey(j) = tmp
      } else {
        decKey(j) = decTable(0)(sbox(tmp >>> 24)) ^
          decTable(1)(sbox(tmp >> 16 & 255)) ^
          decTable(2)(sbox(tmp >> 8 & 255)) ^
          decTable(3)(sbox(tmp & 255))
      }
      j += 1
      i -= 1
    }
  }

  def crypt(input: Array[Int], dir: Boolean): Array[Int] = {
    if (input.length != 4) {
      return Array(0, 0, 0, 0)
    }

    val key = this.keys(if (dir) 1 else 0)
    // state variables a,b,c,d are loaded with pre-whitened data
    var a = input(0) ^ key(0)
    var b = input(if (dir) 3 else 1) ^ key(1)
    var c = input(2) ^ key(2)
    var d = input(if (dir) 1 else 3) ^ key(3)

    val nInnerRounds = key.length / 4 - 2
    var kIndex = 4
    val out = Array(0, 0, 0, 0)

    val table = if (dir) decTable else encTable

    // load up the tables
    val t0 = table(0)
    val t1 = table(1)
    val t2 = table(2)
    val t3 = table(3)
    val sbox = table(4)

    // Inner rounds.  Cribbed from OpenSSL.
    var a2 = 0
    var b2 = 0
    var c2 = 0
    for (i <- 0 until nInnerRounds) {
      a2 = t0(a >>> 24) ^ t1(b >> 16 & 255) ^ t2(c >> 8 & 255) ^ t3(d & 255) ^ key(kIndex)
      b2 = t0(b >>> 24) ^ t1(c >> 16 & 255) ^ t2(d >> 8 & 255) ^ t3(a & 255) ^ key(kIndex + 1)
      c2 = t0(c >>> 24) ^ t1(d >> 16 & 255) ^ t2(a >> 8 & 255) ^ t3(b & 255) ^ key(kIndex + 2)
      d = t0(d >>> 24) ^ t1(a >> 16 & 255) ^ t2(b >> 8 & 255) ^ t3(c & 255) ^ key(kIndex + 3)
      kIndex += 4
      a = a2
      b = b2
      c = c2
    }

    // Last round.
    for (i <- 0 until 4) {
      out(if (dir) 3 & (-i) else i) =
        sbox(a >>> 24) << 24 ^
          sbox(b >> 16 & 255) << 16 ^
          sbox(c >> 8 & 255) << 8 ^
          sbox(d & 255) ^
          key(kIndex)
      kIndex += 1
      a2 = a; a = b; b = c; c = d; d = a2
    }

    out
  }
}