/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import {
  commands, languages, debug, window, workspace,
  ExtensionContext, Diagnostic,
  DebugSessionCustomEvent, Uri, Range,
  DebugStackFrame,
  TextEditorDecorationType,
  ThemeColor,
} from 'vscode';
import Lsp from './lsp';
import Dap from './dap';

/* -------------------------------------------------------------------------- */
/* --- Proof Context Helpers                                              --- */
/* -------------------------------------------------------------------------- */

interface DapRange {
  line: number;
  column: number;
  endLine: number;
  endColumn: number;
}

interface Failed extends DapRange { goal: string }
interface Chunk extends DapRange { kind: string }

interface ProofContext {
  source: string ;
  chunks: readonly Chunk[];
}

interface ChunksDecorator {
  goal: TextEditorDecorationType;
  hpos: TextEditorDecorationType;
  hneg: TextEditorDecorationType;
}

const jRange = ({ line, column, endLine, endColumn }: DapRange): Range =>
  new Range(line - 1, column - 1, endLine - 1, endColumn - 1);

const bg = (bg: string) =>
  window.createTextEditorDecorationType({
    backgroundColor: new ThemeColor(bg)
  });

function showProofContext(deco: ChunksDecorator, context: ProofContext) {
  let selection: Range | undefined;
  const goal: Range[] = [];
  const hpos: Range[] = [];
  const hneg: Range[] = [];
  const uri = Uri.file(context.source);
  context.chunks.forEach(ch => {
    const range = jRange(ch);
    switch (ch.kind) {
      case 'goal':
        if (!selection) selection = range;
        goal.push(range);
        break;
      case 'hpos':
        hpos.push(range);
        break;
      case 'hneg':
        hneg.push(range);
        break;
    }
  });
  window.showTextDocument(uri, { selection }).then(editor => {
    editor.setDecorations(deco.goal, goal);
    editor.setDecorations(deco.hpos, hpos);
    editor.setDecorations(deco.hneg, hneg);
  });
}

/* -------------------------------------------------------------------------- */
/* --- Why3 Platform Extension                                            --- */
/* -------------------------------------------------------------------------- */

export function activate(context: ExtensionContext): void {

  const lsp = new Lsp();
  const dap = new Dap();
  const failedproofs: Diagnostic[] = [];
  const diagnostic = languages.createDiagnosticCollection('Why3 Debugger');
  const hneg = bg('diffEditor.removedLineBackground');
  const hpos = bg('diffEditor.insertedLineBackground');
  const goal = bg('diffEditor.removedTextBackground');

  // Clear Failed Proofs Dignostic
  function onDebugClear() {
    failedproofs.length = 0;
    diagnostic.clear();
  }

  // Cleanup Decorators
  function onDebugCleanup() {
    const editor = window.activeTextEditor;
    if (editor) {
      editor.setDecorations(goal, []);
      editor.setDecorations(hpos, []);
      editor.setDecorations(hneg, []);
    }
  }

  // DAP custom event 'why3/failed'
  function onDebugEvent(evt: DebugSessionCustomEvent): void {
    if (evt.session.type === 'why3' && evt.event === 'why3/failed') {
      const uri = Uri.file(evt.body.source.path);
      const failed = evt.body as Failed;
      const message = `Proof Failed (${failed.goal})`;
      failedproofs.push(new Diagnostic(jRange(failed), message));
      diagnostic.set(uri, failedproofs);
    }
  }

  // DAP custom request 'why3/context'
  function onDebugStackFrame(evt: DebugStackFrame | undefined): void {
    if (evt && evt.session.type === 'why3') {
      evt.session.customRequest('why3/context', {
        threadId: evt.threadId,
        frameId: evt.frameId,
      }).then((response) => {
        if (response)
          showProofContext({ goal, hpos, hneg }, response);
        else
          onDebugCleanup();
      });
    }
  }

  // Register commands.
  context.subscriptions.push(
    lsp, dap, diagnostic,
    commands.registerCommand('why3.lsp.start', lsp.start),
    commands.registerCommand('why3.lsp.stop', lsp.stop),
    commands.registerCommand('why3.dap.start', dap.startDebugging),
    commands.registerCommand('why3.diagnostic.clear', onDebugClear),
    debug.registerDebugConfigurationProvider('why3', dap),
    debug.registerDebugAdapterDescriptorFactory('why3', dap),
    debug.onDidReceiveDebugSessionCustomEvent(onDebugEvent),
    debug.onDidStartDebugSession(onDebugClear),
    debug.onDidStartDebugSession(onDebugCleanup),
    debug.onDidTerminateDebugSession(onDebugCleanup),
    debug.onDidChangeActiveStackItem(onDebugStackFrame),
    workspace.onDidSaveTextDocument(onDebugClear),
  );

  // Start the LSP client. This will also launch the server
  lsp.start();
}

export function deactivate(): void { }
