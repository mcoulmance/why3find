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
import Lsp from './lsp';

export function activate(context: ExtensionContext): void {

	const lsp = new Lsp();

	// Register commands.
	context.subscriptions.push(
		commands.registerCommand('why3.lsp.start', lsp.start),
		commands.registerCommand('why3.lsp.stop', lsp.stop),
		lsp
	);

	// Start the LSP client. This will also launch the server
	lsp.start();

}

export function deactivate(): void { }
