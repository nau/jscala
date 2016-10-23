package org.jscalaexample

import org.jscala._
import org.jscala.JArray
import org.scalajs.dom._
import java.io.{File, FileWriter}

import org.scalajs.dom.raw.HTMLElement

/**
 * Javascript Tetris code originally taken from https://github.com/jakesgordon/javascript-tetris/
 * and rewritten in Scala
 */
object Tetris {
  def tetris = {
    class Canvas(var width: Int, var height: Int, var clientWidth: Int, var clientHeight: Int) extends JsDynamic
    class Stats extends JsDynamic
    javascript {
      //-------------------------------------------------------------------------
      // base helper methods
      //-------------------------------------------------------------------------

      def get(id: String) = document.getElementById(id).as[HTMLElement]
      def hide(id: String) { get(id).style.visibility = "hidden"; }
      def show(id: String) { get(id).style.visibility = null;     }
      def html(id: String, html: String) { get(id).innerHTML = html;            }

      def timestamp() = { new Date().getTime() }
      def random(min: Int, max: Int) = { (min + (Math.random() * (max - min)));            }
      def randomChoice(choices: Array[Int]) = choices(Math.round(random(0, choices.length-1)).as[Int])

      /*if (!window.requestAnimationFrame) {
        val func = (callback: () => Unit, element: Canvas) => {
          window.setTimeout(callback, 1000 / 60)
        }
        window.requestAnimationFrame = window.webkitRequestAnimationFrame ||
          window.mozRequestAnimationFrame    ||
          window.oRequestAnimationFrame      ||
          window.msRequestAnimationFrame     || func
      }*/

      //-------------------------------------------------------------------------
      // game constants
      //-------------------------------------------------------------------------

      val KEY     = new { val ESC = 27; val SPACE = 32; val LEFT = 37; val UP = 38; val RIGHT = 39; val DOWN = 40 }
      val DIR     = new { val UP = 0; val RIGHT = 1; val DOWN = 2; val LEFT = 3; val MIN = 0; val MAX = 3 }
      val stats   = new Stats()
      val canvas  = get("canvas").as[Canvas]
      val ctx     = canvas.getContext("2d")
      val ucanvas = get("upcoming").as[Canvas]
      val uctx    = ucanvas.getContext("2d")
      val speed   = new { val start = 0.6; val decrement = 0.005; val min = 0.1 } // how long before piece drops by 1 row (seconds)
      val nx      = 10 // width of tetris court (in blocks)
      val ny      = 20 // height of tetris court (in blocks)
      val nu      = 5  // width/height of upcoming preview (in blocks)

      //-------------------------------------------------------------------------
      // game variables (initialized during reset)
      //-------------------------------------------------------------------------

      var dx = 0
      var dy = 0        // pixel size of a single tetris block
      var blocks = JArray[JArray[Block]]()        // 2 dimensional array (nx*ny) representing tetris court - either empty block or occupied by a "piece"
      var actions = JArray[Int]()       // queue of user actions (inputs)
      var playing = false       // true|false - game is in progress
      var dt: Long = 0            // time since starting this game
      var current: Piece = null       // the current piece
      var next: Piece = null         // the next piece
      var score = 0         // the current score
      var vscore = 0        // the currently displayed score (it catches up to score in small chunks - like a spinning slot machine)
      var rows = 0          // number of completed rows in the current game
      var step = 0          // how long before current piece drops by 1 row

      //-------------------------------------------------------------------------
      // tetris pieces
      //
      // blocks: each element represents a rotation of the piece (0, 90, 180, 270)
      //         each element is a 16 bit integer where the 16 bits represent
      //         a 4x4 set of blocks, e.g. j.blocks[0] = 0x44C0
      //
      //             0100 = 0x4 << 3 = 0x4000
      //             0100 = 0x4 << 2 = 0x0400
      //             1100 = 0xC << 1 = 0x00C0
      //             0000 = 0x0 << 0 = 0x0000
      //                               ------
      //                               0x44C0
      //
      //-------------------------------------------------------------------------

      class Block(val id: String, val size: Int, val blocks: Seq[Int], val color: String)
      class Piece(val `type`: Block, var dir: Int, var x: Int, var y: Int )

      val i = new Block("i", 4, Seq(0x0F00, 0x2222, 0x00F0, 0x4444), "cyan")
      val j = new Block("j", 3, Seq(0x44C0, 0x8E00, 0x6440, 0x0E20),  "blue"   )
      val l = new Block("l", 3, Seq(0x4460, 0x0E80, 0xC440, 0x2E00), "orange" )
      val o = new Block("o", 2, Seq(0xCC00, 0xCC00, 0xCC00, 0xCC00), "yellow" )
      val s = new Block("s", 3, Seq(0x06C0, 0x8C40, 0x6C00, 0x4620), "green"  )
      val t = new Block("t", 3, Seq(0x0E40, 0x4C40, 0x4E00, 0x4640), "purple" )
      val z = new Block("z", 3, Seq(0x0C60, 0x4C80, 0xC600, 0x2640), "red"    )

      //------------------------------------------------
      // do the bit manipulation and iterate through each
      // occupied block (x,y) for a given piece
      //------------------------------------------------
      def eachblock(`type`: Block, x: Int, y: Int, dir: Int, fn: (Int, Int) => Unit) {
        var row = 0
        var col = 0
        val blocks = `type`.blocks(dir)
        var bit = 0x8000
        while (bit > 0) {
          if ((blocks & bit) != 0) {
            fn(x + col, y + row)
          }
          col += 1
          if (col == 4) {
            col = 0
            row += 1
          }
          bit = bit >> 1
        }
      }

      //-----------------------------------------------------
      // check if a piece can fit into a position in the grid
      //-----------------------------------------------------
      def occupied(`type`: Block, x: Int, y: Int, dir: Int) = {
        var result = false
        eachblock(`type`, x, y, dir, (x, y) => {
          if ((x < 0) || (x >= nx) || (y < 0) || (y >= ny) || (getBlock(x,y) != null)) result = true
        })
        result
      }

      def unoccupied(`type`: Block, x: Int, y: Int, dir: Int) = !occupied(`type`, x, y, dir)

      //-----------------------------------------
      // start with 4 instances of each piece and
      // pick randomly until the "bag is empty"
      //-----------------------------------------
      var pieces = JArray[Block]()
      def randomPiece() = {
        if (pieces.length == 0)
          pieces = JArray(i,i,i,i,j,j,j,j,l,l,l,l,o,o,o,o,s,s,s,s,t,t,t,t,z,z,z,z)
        val `type` = pieces.splice(random(0, pieces.length-1).as[Int], 1)(0)
        new Piece(`type`, DIR.UP, Math.round(random(0, nx - `type`.size)).as[Int], 0)
      }

      //-------------------------------------------------------------------------
      // GAME LOOP
      //-------------------------------------------------------------------------

      def run() {
        showStats()
        addEvents() // attach keydown and resize events
        var last = timestamp()
        var now = timestamp()
        def frame() {
          now = timestamp()
          update(Math.min(1, (now - last) / 1000.0).as[Int]) // using requestAnimationFrame have to be able to handle large delta"s caused when it "hibernates" in a background or non-visible tab
          draw()
          stats.update()
          last = now
          window.as[JsDynamic].requestAnimationFrame(frame _, canvas)
        }
        resize(null) // setup all our sizing information
        reset()  // reset the per-game variables
        frame()  // start the first frame
      }

      def showStats() {
        stats.domElement.id = "stats"
        get("menu").appendChild(stats.domElement.as[Node])
      }

      def addEvents() {
        document.addEventListener("keydown", keydown _, false)
        window.addEventListener("resize", resize _, false)
      }

      def resize(ev: JsDynamic) {
        canvas.width   = canvas.clientWidth  // set canvas logical size equal to its physical size
        canvas.height  = canvas.clientHeight // (ditto)
        ucanvas.width  = ucanvas.clientWidth
        ucanvas.height = ucanvas.clientHeight
        dx = canvas.width / nx // pixel size of a single tetris block
        dy = canvas.height / ny // (ditto)
        invalidate()
        invalidateNext()
      }

      def keydown(ev: JsDynamic) {
        var handled = false
        if (playing) {
          ev.keyCode.as[Int] match {
            case KEY.LEFT =>   actions.push(DIR.LEFT);  handled = true
            case KEY.RIGHT =>  actions.push(DIR.RIGHT); handled = true
            case KEY.UP =>     actions.push(DIR.UP);    handled = true
            case KEY.DOWN =>   actions.push(DIR.DOWN);  handled = true
            case KEY.ESC =>    lose();                  handled = true
          }
        }
        else if (ev.keyCode == KEY.SPACE) {
          play()
          handled = true
        }
        if (handled)
          ev.preventDefault() // prevent arrow keys from scrolling the page (supported in IE9+ and all other browsers)
      }

      //-------------------------------------------------------------------------
      // GAME LOGIC
      //-------------------------------------------------------------------------

      def play() = { hide("start"); reset();          playing = true;  }
      def lose() = { show("start"); setVisualScore(-1); playing = false; }

      def setVisualScore(n: Int) = {
        val s = if (n != -1) n else score
        vscore = s
        invalidateScore()
      }
      def setScore(n: Int) = { score = n; setVisualScore(n);  }
      def addScore(n: Int) = { score = score + n;   }
      def clearScore() = { setScore(0) }
      def clearRows() = { setRows(0) }
      def setRows(n: Int) = { rows = n; step = Math.max(speed.min, speed.start - (speed.decrement*rows)).as[Int]; invalidateRows(); }
      def addRows(n: Int) = { setRows(rows + n); }
      def getBlock(x: Int, y: Int) = {
        val b = if (blocks(x) != null) blocks(x)(y) else null
        b
      }
      def setBlock(x: Int, y: Int, `type`: Block) = {
        val r = if (blocks(x) != null) blocks(x) else JArray[Block]()
        blocks(x) = r
        blocks(x)(y) = `type`
        invalidate()
      }
      def clearBlocks()          { blocks = JArray(); invalidate(); }
      def clearActions() = { actions = JArray() }
      def setCurrentPiece(piece: Piece) = {
        val n = if (piece != null) piece else randomPiece()
        current = n
        invalidate()
      }
      def setNextPiece(piece: Piece = null) = {
        val n = if (piece != null) piece else randomPiece()
        next = n
        invalidateNext()
      }

      def reset() {
        dt = 0
        clearActions()
        clearBlocks()
        clearRows()
        clearScore()
        setCurrentPiece(next)
        setNextPiece()
      }

      def update(idt: Int) {
        if (playing) {
          if (vscore < score)
            setVisualScore(vscore + 1)
          handle(actions.shift())
          dt = dt + idt
          if (dt > step) {
            dt = dt - step
            drop()
          }
        }
      }

      def handle(action: Int) {
        action match {
          case DIR.LEFT =>  move(DIR.LEFT)
          case DIR.RIGHT => move(DIR.RIGHT)
          case DIR.UP =>    rotate()
          case DIR.DOWN =>  drop()
        }
      }

      def move(dir: Int): Boolean = {
        var x = current.x
        var y = current.y
        dir match {
          case DIR.RIGHT => x = x + 1
          case DIR.LEFT =>  x = x - 1
          case DIR.DOWN =>  y = y + 1
        }
        if (unoccupied(current.`type`, x, y, current.dir)) {
          current.x = x
          current.y = y
          invalidate()
          return true
        }
        else {
          return false
        }
      }

      def rotate() {
        val newdir = if (current.dir == DIR.MAX) DIR.MIN else current.dir + 1
        if (unoccupied(current.`type`, current.x, current.y, newdir)) {
          current.dir = newdir
          invalidate()
        }
      }

      def drop() {
        if (!move(DIR.DOWN)) {
          addScore(10)
          dropPiece()
          removeLines()
          setCurrentPiece(next)
          setNextPiece(randomPiece())
          clearActions()
          if (occupied(current.`type`, current.x, current.y, current.dir)) {
            lose()
          }
        }
      }

      def dropPiece() {
        eachblock(current.`type`, current.x, current.y, current.dir, (x, y) => {
          setBlock(x, y, current.`type`)
        })
      }

      def removeLines() {
        var x = 0
        var y = ny
        var complete = false
        var n = 0
        while (y > 0) {
          complete = true
          x = 0
          while (x < nx) {
            if (!(getBlock(x, y).as[Boolean]))
              complete = false
            x += 1
          }
          if (complete) {
            removeLine(y)
            y = y + 1 // recheck same line
            n += 1
          }
          y -= 1
        }
        if (n > 0) {
          addRows(n)
          addScore(100*Math.pow(2,n-1).as[Int]) // 1: 100, 2: 200, 3: 400, 4: 800
        }
      }

      def removeLine(n: Int) {
        var x = 0
        var y = n
        while (y >= 0) {
          x = 0
          while (x < nx) {
            val b = if (y == 0) null else getBlock(x, y-1)
            setBlock(x, y, b)
            x += 1
          }
          y -= 1
        }
      }

      //-------------------------------------------------------------------------
      // RENDERING
      //-------------------------------------------------------------------------

      class Invalid(var court: Boolean, var next: Boolean, var score: Boolean, var rows: Boolean)
      val invalid = new Invalid(false, false, false, false)

      def invalidate()         { invalid.court  = true }
      def invalidateNext()     { invalid.next   = true }
      def invalidateScore()    { invalid.score  = true }
      def invalidateRows()     { invalid.rows   = true }

      def draw() {
        ctx.save()
        ctx.lineWidth = 1
        ctx.translate(0.5, 0.5) // for crisp 1px black lines
        drawCourt()
        drawNext()
        drawScore()
        drawRows()
        ctx.restore()
      }

      def drawCourt() {
        if (invalid.court) {
          ctx.clearRect(0, 0, canvas.width, canvas.height)
          if (playing)
            drawPiece(ctx, current.`type`, current.x, current.y, current.dir)
          var x = 0
          var y = 0
          var block: Block = null
          while (y < ny) {
            x = 0
            while (x < nx) {
              block = getBlock(x,y)
              if (block != null)
                drawBlock(ctx, x, y, block.color)
              x += 1
            }
            y += 1
          }
          ctx.strokeRect(0, 0, nx*dx - 1, ny*dy - 1) // court boundary
          invalid.court = false
        }
      }

      def drawNext() {
        if (invalid.next) {
          val padding = (nu - next.`type`.size) / 2 // half-arsed attempt at centering next piece display
          uctx.save()
          uctx.translate(0.5, 0.5)
          uctx.clearRect(0, 0, nu*dx, nu*dy)
          drawPiece(uctx, next.`type`, padding, padding, next.dir)
          uctx.strokeStyle = "black"
          uctx.strokeRect(0, 0, nu*dx - 1, nu*dy - 1)
          uctx.restore()
          invalid.next = false
        }
      }

      def drawScore() {
        if (invalid.score) {
          val text: JString = "00000" + vscore
          html("score", text.slice(-5).toString())
          invalid.score = false
        }
      }

      def drawRows() {
        if (invalid.rows) {
          html("rows", rows.toString())
          invalid.rows = false
        }
      }

      def drawPiece(ctx: JsDynamic, `type`: Block, x: Int, y: Int, dir: Int) {
        eachblock(`type`, x, y, dir, (x, y) => drawBlock(ctx, x, y, `type`.color))
      }

      def drawBlock(ctx: JsDynamic, x: Int, y: Int, color: String) {
        ctx.fillStyle = color
        ctx.fillRect(x*dx, y*dy, dx, dy)
        ctx.strokeRect(x*dx, y*dy, dx, dy)
      }
      //-------------------------------------------------------------------------
      // FINALLY, lets run the game
      //-------------------------------------------------------------------------

      run()
    }
  }

  def main(args: Array[String]) {
    val path = new File(args(0))
    val ast = tetris
    val fw = new FileWriter(path)
    fw.write(ast.asString)
    fw.close()
  }
}
