
package axiom6.util

object Test
{
   val  text:Text       = new Text(1000)
   val  hold:Hold[Text] = new Hold[Text]()
   val  sep             = "::"
   type CS              = Text.CS

  def app( args:Any* )
  {
     for( arg<-args )
       text.any( arg )
  }

  def tab( n:Int, args:Any* )
  {
    text.tab( n )
     for( arg<-args )
       text.any( arg )
  }

  def cmp( node:Hode[Text] ) : Boolean =
  {
    val t1 = text
    val t2 = node.data
    val n1 = t1.indexOf(sep)
    val n2 = t2.indexOf(sep)
    Text.equ(t1,0,n1,t2,0,n2)
  }

  def keep( name:CS, args:Any* )
  {
    text.clear()
    text.app( name, sep )
    val node = hold.find( text )
    if( hold.in(node) )
    {
      text.clear()
      text.seq( args )
      node.data.app(text)
    }
    else
    {
      text.seq( args )
      hold.add( new Text(text) )
    }
    text.clear()
  }

  def beg( name:CS )
    { text.clear(); text.app( name, sep ) }

  def test( name:CS, args:Any* )
  {
    beg( name )
    text.seq( args )
    end( name )
  }

  def end( name:CS )
  {
    val n   = text.indexOf(sep)
    if( Text.equ( text.toCS, 0, n, name, 0, name.length ) )
    {
      val node = hold.find( text )
      if( hold.in(node) )
      {
        val keep = node.data
        if( keep.equals(text) )
          { Log.msg("Pass::"); Log.msg(text.toCS);  Log.eol() }
        else
          { Log.msg("Fail::"); Log.msg(keep.toCS); Log.eol()
            Log.msg("    ::"); Log.msg(text.toCS); Log.eol() }
      }
      else
        {  Log.msg("Miss::"); Log.msg(text.toCS);  Log.eol() }
    }
    else
      Log.trace( 2, "Need to call Log.beg(\"", name, "\")" )
  }
}


