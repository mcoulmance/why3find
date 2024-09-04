  $ why3find print bar.mlw | sed 's/<[0-9]*>/<REDACTED>/g'
  (* -------------------------------------------- *)
  (* --- bar.A                                --- *)
  (* -------------------------------------------- *)
  
  module A<REDACTED> {
    use BuiltIn<REDACTED>
    use Bool<REDACTED>
    use Unit<REDACTED>
    type a<REDACTED> = private {..}
  }
  
  (* -------------------------------------------- *)
  (* --- bar.B                                --- *)
  (* -------------------------------------------- *)
  
  module B<REDACTED> {
    use BuiltIn<REDACTED>
    use Bool<REDACTED>
    use Unit<REDACTED>
    type b<REDACTED> = private {..}
  }
  
  (* -------------------------------------------- *)
  (* --- bar.C                                --- *)
  (* -------------------------------------------- *)
  
  module C<REDACTED> {
    use BuiltIn<REDACTED>
    use Bool<REDACTED>
    use Unit<REDACTED>
    scope A {
      use BuiltIn<REDACTED>
      use Bool<REDACTED>
      use Unit<REDACTED>
      type a<REDACTED> = private {..}
      clone A<REDACTED> {
        a<REDACTED> -> a<REDACTED> ;
      }
    }
    use BuiltIn<REDACTED>
    use Bool<REDACTED>
    use Unit<REDACTED>
    type a<REDACTED> = private {..}
    clone A<REDACTED> {
      a<REDACTED> -> a<REDACTED> ;
    }
    scope B {
      use BuiltIn<REDACTED>
      use Bool<REDACTED>
      use Unit<REDACTED>
      type a<REDACTED> = private {..}
      clone A<REDACTED> {
        a<REDACTED> -> a<REDACTED> ;
      }
    }
    scope S {
      use BuiltIn<REDACTED>
      use Bool<REDACTED>
      use Unit<REDACTED>
      type a<REDACTED> = private {..}
      clone A<REDACTED> {
        a<REDACTED> -> a<REDACTED> ;
      }
    }
    scope R {
      scope A {
        use BuiltIn<REDACTED>
        use Bool<REDACTED>
        use Unit<REDACTED>
        type a<REDACTED> = private {..}
        clone A<REDACTED> {
          a<REDACTED> -> a<REDACTED> ;
        }
      }
      use BuiltIn<REDACTED>
      use Bool<REDACTED>
      use Unit<REDACTED>
      type b<REDACTED> = private {..}
      clone B<REDACTED> {
        b<REDACTED> -> b<REDACTED> ;
      }
    }
    use Tuple5<REDACTED>
    type r<REDACTED> = ..
    function p1<REDACTED> = ..
    function p2<REDACTED> = ..
    function p3<REDACTED> = ..
    function p4<REDACTED> = ..
    function p5<REDACTED> = ..
  }
  
