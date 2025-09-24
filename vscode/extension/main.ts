/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* -------------------------------------------------------------------------- */
/* --- Why3 Platform Extension                                            --- */
/* -------------------------------------------------------------------------- */

import { commands, debug, ExtensionContext } from 'vscode';
import Lsp from './lsp';
import Dap from './dap';

export function activate(context: ExtensionContext): void {

  const lsp = new Lsp();
  const dap = new Dap();

  // Register commands.
  context.subscriptions.push(
    lsp, dap,
    commands.registerCommand('why3.lsp.start', lsp.start),
    commands.registerCommand('why3.lsp.stop', lsp.stop),
    debug.registerDebugConfigurationProvider('why3', dap),
    debug.registerDebugAdapterDescriptorFactory('why3', dap),
  );

  // Start the LSP client. This will also launch the server
  lsp.start();

}

export function deactivate(): void { }
