--------------------------------------------------------------------------
--- Generating Documentation
--------------------------------------------------------------------------
  $ why3find doc duplicated.mlw
  Generated $TESTCASE_ROOT/html/index.html
--------------------------------------------------------------------------
--- Generated Files
--------------------------------------------------------------------------
  $ cat html/duplicated.*.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module duplicated.A</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library <a href="duplicated.index.html">duplicated</a></code> — <code>module A</code></header>
  <pre class="src">
  <span class="keyword">module</span> <span class="scope">A</span><a href="duplicated.proof.html#A" title="2 logic parameters" class="icon small remark icofont-question-circle"></a><a href="duplicated.proof.html#A" title="Valid (no goals)" class="icon remark icofont-check"></a>
    <span class="keyword">type</span> <a id="t">t</a><span title="Parameter" class="icon small remark icofont-question-circle"></span>
    <span class="keyword">val</span> <span class="keyword">function</span> <a id="op">op</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> <a title="duplicated.A.t" href="#t">t</a> <a title="duplicated.A.t" href="#t">t</a> : <a title="duplicated.A.t" href="#t">t</a>
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module duplicated.B</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library <a href="duplicated.index.html">duplicated</a></code> — <code>module B</code></header>
  <pre class="src">
  <span class="keyword">module</span> <span class="scope">B</span><a href="duplicated.proof.html#B" title="1 logic parameter" class="icon small remark icofont-question-circle"></a><a href="duplicated.proof.html#B" title="Valid (no goals)" class="icon remark icofont-check"></a>
    <span class="keyword">clone</span> <a title="duplicated.A" href="duplicated.A.html">A</a><span class="section">{<span class="section-toggle">#</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="comment">(* cloned <a name="clone-2" href="duplicated.A.html">duplicated.A</a> *)</span>
      <span class="comment section-toggle">end</span>
    </span>}</span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">val</span> <span class="attribute">function</span> <a id="A.op" href="duplicated.A.html#op" title="duplicated.B.A.op  (cloned from duplicated.A.op)">op</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> int int : int
      <span class="comment section-toggle">end</span>
    </span>}</span><span title="2 logic parameters" class="icon small remark icofont-question-circle"></span> <span class="keyword">with</span> <span class="keyword">type</span> <a title="duplicated.A.t" href="duplicated.A.html#t">t</a> = int
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Library duplicated</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library duplicated</code></header>
  <pre class="src"><span class="keyword">module</span> <a title="duplicated.A" href="duplicated.A.html">A</a><a href="duplicated.proof.html#A" title="2 logic parameters" class="icon small remark icofont-question-circle"></a><a href="duplicated.proof.html#A" title="Valid (no goals)" class="icon remark icofont-check"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="duplicated.B" href="duplicated.B.html">B</a><a href="duplicated.proof.html#B" title="1 logic parameter" class="icon small remark icofont-question-circle"></a><a href="duplicated.proof.html#B" title="Valid (no goals)" class="icon remark icofont-check"></a></pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Library duplicated</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library <a href="duplicated.index.html">duplicated</a></code> — <code>proofs</code></header>
  <h1>Provers</h1>
  <pre class="src">
  </pre>
  <h1>Proofs</h1>
  <pre class="src"><span class="keyword">module</span> <a id="A" href="duplicated.A.html">duplicated.A</a><span title="2 logic parameters" class="icon small remark icofont-question-circle"></span><span title="Valid (no goals)" class="icon remark icofont-check"></span></pre>
  <pre class="src"><span class="keyword">module</span> <a id="B" href="duplicated.B.html">duplicated.B</a><span title="1 logic parameter" class="icon small remark icofont-question-circle"></span><span title="Valid (no goals)" class="icon remark icofont-check"></span></pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
