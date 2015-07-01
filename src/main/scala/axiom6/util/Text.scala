package axiom6.util

import scala.collection.mutable.{ StringBuilder => StringBuilderText }

object Text
{
  type CS = CharSequence

  // Constructors
  def apply( cap:Int ) : Text = new Text(cap)
//def apply( cs:CS   ) : Text = new Text(cs)  // Works for Text String etc
  def apply( sq:CS*  ) : Text =
  {
    var len = 0
    for( s <- sq )
      len += s.length
    val text = new Text(len)
    for( s <- sq )
      text.app(s)
    text
  }

  // Constants
  val tab   : String = "  "
  val eol   : String = "\n"
  val delim : String = ","

  // Empty and type checks
  val empty : Text = new Text(0)
  def is( text:Text  ) : Boolean = text!=null && text!=empty
  def isCS( cs:CS    ) : Boolean = cs!=null && cs.length > 0

  def toUpper( c:Char ) : Char = java.lang.Character.toUpperCase(c)
  def toLower( c:Char ) : Char = java.lang.Character.toLowerCase(c)


  def equ( a:CS, b:CS )                   : Boolean = { Text.equ( a,  0, a.length, b, 0, b.length ) }
  def equ( a:CS,   ab:Int, al:Int, b:CS ) : Boolean = { Text.equ( a, ab, al,       b, 0, b.length ) }
  def equ( a:Text, ab:Int, al:Int,
           b:Text, bb:Int, bl:Int ) : Boolean = { Text.equ( a.toCS, ab, al,  b.toCS, bb, bl) }
  def equ( a:CS,   ab:Int, al:Int,
           b:CS,   bb:Int, bl:Int ) : Boolean =
  {
    if( a != null && b != null && al == bl )
    {
      var i   = 0
      while( i < al )
      {
        if( a.charAt(ab+i) != b.charAt(bb+i) )
          return false
        i += 1
      }
      return true
    }
    false
  }

}

class Text( _cap:Int ) extends
{
  val sb = new StringBuilderText( _cap )

  type CS    = Text.CS

  def this() = this( 100 )
  def this( cs:CharSequence ) = { this(); app(cs)   }
  def this( text:Text )       = { this(); app(text) }

  override def toString  : String  = { sb.toString() }

  def charAt(i:Int)           : Char = { sb.charAt(i) }
  def setCharAt(i:Int,c:Char) : Unit = { sb.setCharAt(i,c) }
  def len                     : Int  = { sb.size }
  def cap                     : Int  = { sb.capacity }
  def indexOf( c:Char )       : Int  = { sb.indexOf(c) }
  def indexOf( s:String )     : Int  = { sb.indexOf(s) }
  def toCS                    : CS   = { sb.asInstanceOf[CS] }

  def cap( _cap:Int )    : Unit    = { sb.ensureCapacity(_cap) }
  def grow( inc:Int )    : Unit    = { sb.ensureCapacity( cap + inc ) }
  def clear()            : Unit    = { sb.clear() }

  def in( b:Int, e:Int ) : Boolean = in(b)&& in(e-1) && b <= e
  def in( i:Int )        : Boolean = 0 <= i && i < len
  def has(     c:Char )  : Boolean = sb.contains( c )
  def hasTail( c:Char )  : Boolean = sb.charAt(sb.size-1)==c

  def delTail()       : Unit = { sb.setLength(sb.size-1) }
  def delTail(c:Char) : Unit = { if( hasTail(c)) delTail() }

  // ... appends ....
  def app( str:String    ) : Unit = { sb.append( str ) }
  def app( cs:CS         ) : Unit = { sb.append( cs  ) }
  def app( c:Char        ) : Unit = { sb.append( c   ) }
  def app( b:Byte        ) : Unit = { sb.append( b   ) }
  def app( s:Short       ) : Unit = { sb.append( s   ) }
  def app( i:Int         ) : Unit = { sb.append( i   ) }
  def app( n:Long        ) : Unit = { sb.append( n   ) }
  def app( f:Float       ) : Unit = { sb.append( f   ) } // app( f, dec(f) ) }
  def app( d:Double      ) : Unit = { sb.append( d   ) } // app( s, dec(s) ) }
  def app( b:Boolean     ) : Unit = { if(b) sb.append("true") else sb.append("false") }
  def app( a:Array[Char] ) : Unit = { sb.appendAll( a ) }
  def app( u:Unit        ) : Unit = {}
  def app( t:Text        ) : Unit = { if( t!=this ) sb.append( t ) }
  def app( a:Any         ) : Unit = { sb.append( a   ) }
  
  def any( v:Any )
  {
    v match
    {
    //case t:Tok          => app(t)
      case t:Text         => app(t)
      case s:CS           => app(s)
      case c:Char         => app(c)
      case b:Boolean      => app(b)
      case y:Byte         => app(y)
      case i:Int          => app(i)
      case j:Short        => app(j)
      case l:Long         => app(l)
      case f:Float        => app(f)
      case d:Double       => app(d)
      case a:Array[Char]  => app(a)
      case a:Array[Any]   => delim( ",", a )
      case a:List[_]      => delim( ",", a )
    //case e:Enum         => app(e.name)
      case u:Unit         => app(u)
    //case r:Boun         => app(r)
    //case u:Uri          => app(u.text)
      case _              => app(v.toString)
    }

  }

  def all( args:Any*     ) : Unit = { for( arg<-args ) any(arg) }
  def seq( args:Seq[Any] ) : Unit = { for( arg<-args ) any(arg) }

  def replace( a:Char, b:Char ) : Unit =
  {
    for( i <- sb.indices ) if( a == sb.charAt(i) ) sb.setCharAt(i,b)
  }

  def delim( mid:CS, args:Seq[Any] ) : Unit = { delim( "", mid, "", args ) }

  def delim( beg:CS, mid:CS, end:CS, args:Seq[Any] )
  {
    app( beg )
    for( i <- args.indices )
    {
      if(  i < args.length-1 )
        app( args(i), mid )
      else
       any( args(i) )
    }
    app( end )
  }

  def text( args:Any* ) : Text = { seq(args); this }

  def att( name:CS, value:Any ) : Unit =  { app( " ", name, "=\"", value, "\"" ); replace('_','-') }
  def css( name:CS, value:Any ) : Unit =   {app( " ", name, ": ",  value, "; " ); replace('_','-') }
  def css( name:CS, value:Any, quotes:Boolean ) : Unit =
   { app( " ", name, ": \"", value, "\"; " ); replace('_','-') }

  def css( name:CS, value:Any, uom:String ) : Unit =
    {app( " ", name, ": ", value, uom, "; " ); replace('_','-') }

  def space( n:Int ) : Unit =  { for( i <-0 until n ) app(' '     ); }
  def tab(   n:Int ) : Unit =  { for( i <-0 until n ) app(Text.tab); }

  def tab( n:Int, args:Any* ) : Unit = {tab(n); seq(args) }

  def dec( dbl:Double ) : Int  =
  {
    var d:Int      = 0
    var r:Double   = rem(dbl)
    if( r < 0.000000000001 )
      return d

    var d1:Int  = 0
    var d2:Int  = 0
    var d3:Int  = 0

    for( i <- 0 until 15 )  // Console.prIntln( "dbl", dbl, "---------" )
    {                       // Console.prIntln( "r ", r  )
      d1 = dig(r,10);       // Console.prIntln( "d1", d1 )
      d2 = dig(r,100);      // Console.prIntln( "d2", d2 )
      d3 = dig(r,1000);     // Console.prIntln( "d3", d3 )
      if( d1==d2&&d2==d3 )
        return d
      else
        d = d + 1
      r = r * 10
    }
    d
  }

  def rem( dbl:Double )             : Double = dbl - Math.floor(dbl)
  def dig( rem:Double, mul:Int    ) : Int    = (rem * mul).toInt % 10
  
} // End of class Text

