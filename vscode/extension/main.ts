/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import {
  commands, languages, debug, window, workspace,
  ExtensionContext, EventEmitter, Diagnostic,
  DebugSessionCustomEvent, Uri, Range,
  DebugStackFrame, ProviderResult,
  TextEditorDecorationType,
  TextDocumentContentProvider,
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
  source: string;
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
/* --- Why3 Task Provider                                                 --- */
/* -------------------------------------------------------------------------- */


class Why3DocumentProvider implements TextDocumentContentProvider {
  task = Uri.parse('why3:Proof Task');
  emitter = new EventEmitter<Uri>();
  onDidChange = this.emitter.event;
  update() { this.emitter.fire(this.task); }
  provideTextDocumentContent(uri: Uri): ProviderResult<string> {
    const stack = debug.activeStackItem;
    if (stack) {
      const { session, threadId, frameId } = stack as DebugStackFrame;
      return session.customRequest('why3/task', { threadId, frameId });
    } else {
      return undefined;
    }
  }
}

/* -------------------------------------------------------------------------- */
/* --- Why3 Platform Extension                                            --- */
/* -------------------------------------------------------------------------- */

export function activate(context: ExtensionContext): void {

  const lsp = new Lsp();
  const dap = new Dap();
  const failedproofs: Diagnostic[] = [];
  const diagnostic = languages.createDiagnosticCollection('Why3 Debugger');
  const provider = new Why3DocumentProvider();
  const hneg = bg('diffEditor.removedLineBackground');
  const hpos = bg('diffEditor.insertedLineBackground');
  const goal = bg('diffEditor.removedTextBackground');

  function clearDiagnostics() {
    failedproofs.length = 0;
    diagnostic.clear();
  }

  function clearDecorators() {
    window.visibleTextEditors.forEach(editor => {
      if (editor.document.languageId == 'why3') {
        editor.setDecorations(goal, []);
        editor.setDecorations(hpos, []);
        editor.setDecorations(hneg, []);
      }
    });
  }

  // Starting a new debug session
  function onDebugStart() {
    clearDiagnostics();
    clearDecorators();
  }

  // Terminating the debug session
  function onDebugTerminate() {
    clearDecorators();
  }

  // DAP custom event 'why3/failed'
  function onDebugCustomEvent(evt: DebugSessionCustomEvent): void {
    if (evt.session.type === 'why3' && evt.event === 'why3/failed') {
      const uri = Uri.file(evt.body.source.path);
      const failed = evt.body as Failed;
      const message = `Proof Failed (${failed.goal})`;
      failedproofs.push(new Diagnostic(jRange(failed), message));
      diagnostic.set(uri, failedproofs);
    }
  }

  // DAP custom request 'why3/context'
  function onDebugStackFrame(stack: DebugStackFrame | undefined): void {
    if (stack && stack.session.type === 'why3') {
      provider.update();
      const { session, threadId, frameId } = stack;
      session
        .customRequest('why3/context', { threadId, frameId })
        .then((response) => {
          if (response)
            showProofContext({ goal, hpos, hneg }, response);
          else
            onDebugTerminate();
        });
    }
  }

  function onDebugShowTaskPreview(): void {
    const active = debug.activeStackItem as (DebugStackFrame | undefined);
    if (active && active.session.type === 'why3') {
      window.showTextDocument(provider.task, {
        viewColumn: 2,
        preserveFocus: true,
        preview: true,
      }).then(editor => {
        languages.setTextDocumentLanguage(editor.document, 'why3');
      });
    }
  }

  // Register commands.
  context.subscriptions.push(
    lsp, dap, diagnostic,
    commands.registerCommand('why3.lsp.start', lsp.start),
    commands.registerCommand('why3.lsp.stop', lsp.stop),
    commands.registerCommand('why3.dap.start', dap.startDebugging),
    commands.registerCommand('why3.diagnostic.clear', onDebugStart),
    commands.registerCommand('why3.debug.task', onDebugShowTaskPreview),
    debug.registerDebugConfigurationProvider('why3', dap),
    debug.registerDebugAdapterDescriptorFactory('why3', dap),
    debug.onDidReceiveDebugSessionCustomEvent(onDebugCustomEvent),
    debug.onDidStartDebugSession(onDebugStart),
    debug.onDidChangeActiveStackItem(onDebugStackFrame),
    debug.onDidTerminateDebugSession(onDebugTerminate),
    workspace.onDidSaveTextDocument(clearDiagnostics),
    workspace.registerTextDocumentContentProvider('why3', provider),
  );

  // Start the LSP client. This will also launch the server
  lsp.start();
}

export function deactivate(): void { }
