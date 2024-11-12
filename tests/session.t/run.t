  $ why3find prove -P alt-ergo -r -s foo.mlw
  Theory foo.S: ✔ (-)
  Theory foo.T: ✔ (2)
  $ cat foo/why3session.xml | grep -e "proved=\"true\""
  <file format="whyml" proved="true">
  <theory name="T" proved="true">
   <goal name="Add.com" proved="true">
   <goal name="Mul.com" proved="true">
