  $ why3find print bar.mlw
  (* -------------------------------------------- *)
  (* --- bar.A                                --- *)
  (* -------------------------------------------- *)
  
  module A<124> {
    use BuiltIn<21>
    use Bool<23>
    use Unit<65>
    type a<125> = private {..}
  }
  
  (* -------------------------------------------- *)
  (* --- bar.B                                --- *)
  (* -------------------------------------------- *)
  
  module B<126> {
    use BuiltIn<21>
    use Bool<23>
    use Unit<65>
    type b<127> = private {..}
  }
  
  (* -------------------------------------------- *)
  (* --- bar.C                                --- *)
  (* -------------------------------------------- *)
  
  module C<128> {
    use BuiltIn<21>
    use Bool<23>
    use Unit<65>
    scope A {
      use BuiltIn<21>
      use Bool<23>
      use Unit<65>
      type a<129> = private {..}
      clone A<124> {
        a<125> -> a<129> ;
      }
    }
    use BuiltIn<21>
    use Bool<23>
    use Unit<65>
    type a<130> = private {..}
    clone A<124> {
      a<125> -> a<130> ;
    }
    scope B {
      use BuiltIn<21>
      use Bool<23>
      use Unit<65>
      type a<131> = private {..}
      clone A<124> {
        a<125> -> a<131> ;
      }
    }
    scope S {
      use BuiltIn<21>
      use Bool<23>
      use Unit<65>
      type a<132> = private {..}
      clone A<124> {
        a<125> -> a<132> ;
      }
    }
    scope R {
      scope A {
        use BuiltIn<21>
        use Bool<23>
        use Unit<65>
        type a<133> = private {..}
        clone A<124> {
          a<125> -> a<133> ;
        }
      }
      use BuiltIn<21>
      use Bool<23>
      use Unit<65>
      type b<134> = private {..}
      clone B<126> {
        b<127> -> b<134> ;
      }
    }
    use Tuple5<149>
    type r<146> = ..
    function p1<156> = ..
    function p2<172> = ..
    function p3<188> = ..
    function p4<204> = ..
    function p5<220> = ..
  }
  
