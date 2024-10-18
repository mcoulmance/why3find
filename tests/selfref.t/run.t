  $ why3find doc foo.mlw bar.mlw
  Generated $TESTCASE_ROOT/html/index.html
  $ cat html/bar.A.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module bar.A</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library <a href="bar.index.html">bar</a></code> — <code>module A</code></header>
  <pre class="src">
  <span class="keyword">module</span> <span class="scope">A</span><a href="bar.proof.html#A" title="Valid (no goals)" class="icon remark icofont-check"></a>
    <span class="keyword">use</span> foo.<a title="foo.A" href="foo.A.html">A</a>
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
