  $ why3find prove -T split_vc,inline_goal a.mlw -s
  File "a.mlw", line 5, characters 14-34: proof failed
  Goal T.g: ✘
  Theory a.T: ✘
  Error: 1 unproved file
  [1]

Hammer isn't supposed to backtrack, no inline_goal <transf> tag should appear

  $ cat a/why3session.xml | sed -E 's/ tim(e|elimit)="[0-9.e-]+"//g'
  <?xml version="1.0" encoding="UTF-8"?>
  <!DOCTYPE why3session PUBLIC "-//Why3//proof session v5//EN"
  "https://www.why3.org/why3session.dtd">
  <why3session shape_version="6">
  <prover id="0" name="Alt-Ergo" version="2.4.2" memlimit="1000"/>
  <file format="whyml">
  <path name=".."/><path name="a.mlw"/>
  <theory name="T">
   <goal name="g">
   <proof prover="0"><result status="unknown" steps="0"/></proof>
   <transf name="split_vc" >
    <goal name="g.0">
    <proof prover="0"><result status="unknown" steps="0"/></proof>
    <transf name="inline_goal" >
     <goal name="g.0.0">
     <proof prover="0"><result status="unknown" steps="0"/></proof>
     </goal>
    </transf>
    </goal>
    <goal name="g.1">
    <proof prover="0"><result status="unknown" steps="0"/></proof>
    <transf name="inline_goal" >
     <goal name="g.1.0">
     <proof prover="0"><result status="unknown" steps="0"/></proof>
     </goal>
    </transf>
    </goal>
   </transf>
   </goal>
  </theory>
  </file>
  </why3session>
