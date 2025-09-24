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

import { window, commands, ExtensionContext } from 'vscode';
import * as Client from './client';

export function activate(context: ExtensionContext): void {

  // Register commands.
  context.subscriptions.push(
    commands.registerCommand('why3.lsp.start', Client.start),
    commands.registerCommand('why3.lsp.stop', Client.stop),
  );

  // Start the client. This will also launch the server
  Client.start();

}

export function deactivate(): void {
  Client.deactivate();
}
