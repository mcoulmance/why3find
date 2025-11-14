/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import {
  commands, languages, debug,
  ExtensionContext, Diagnostic,
  DebugSession, DebugSessionCustomEvent, Uri, Range,
  workspace,
} from 'vscode';
import Lsp from './lsp';
import Dap from './dap';

/* -------------------------------------------------------------------------- */
/* --- Proof Failed Diagnostics                                           --- */
/* -------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------- */
/* --- Why3 Platform Extension                                            --- */
/* -------------------------------------------------------------------------- */

export function activate(context: ExtensionContext): void {

  const lsp = new Lsp();
  const dap = new Dap();
  const failed: Diagnostic[] = [];
  const diagnostic = languages.createDiagnosticCollection('Why3 Debugger');

  function onDebugClear() {
    failed.length = 0;
    diagnostic.clear();
  }

  function onDebugEvent(evt: DebugSessionCustomEvent): void {
    if (evt.session.type === 'why3' && evt.event === 'why3/failed') {
      const uri = Uri.file(evt.body.source.path);
      const { goal, line, endLine, column, endColumn } = evt.body ;
      const message = `Proof Failed (${goal})`;
      const range = new Range(line-1, column-1, endLine-1, endColumn-1);
      failed.push(new Diagnostic(range, message));
      diagnostic.set(uri, failed);
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
    workspace.onDidSaveTextDocument(onDebugClear),
  );

  // Start the LSP client. This will also launch the server
  lsp.start();

}

export function deactivate(): void { }
