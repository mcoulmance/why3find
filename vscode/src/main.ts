/* ************************************************************************ */
/*                                                                          */
/*   This file is part of the why3find.                                     */
/*                                                                          */
/*   Copyright (C) 2022-2024                                                */
/*     CEA (Commissariat à l'énergie atomique et aux énergies               */
/*          alternatives)                                                   */
/*                                                                          */
/*   you can redistribute it and/or modify it under the terms of the GNU    */
/*   Lesser General Public License as published by the Free Software        */
/*   Foundation, version 2.1.                                               */
/*                                                                          */
/*   It is distributed in the hope that it will be useful,                  */
/*   but WITHOUT ANY WARRANTY; without even the implied warranty of         */
/*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          */
/*   GNU Lesser General Public License for more details.                    */
/*                                                                          */
/*   See the enclosed LICENSE.md for details.                               */
/*                                                                          */
/* ************************************************************************ */

/* -------------------------------------------------------------------------- */
/* --- Why3 Platform Extension                                            --- */
/* -------------------------------------------------------------------------- */

import { window, commands, ExtensionContext } from 'vscode';
import * as Client from './client';
import * as Status from './status';
import * as Proof from './proof';

export function activate(context: ExtensionContext): void {

	// Register commands.
	context.subscriptions.push(
		commands.registerCommand('why3.lsp.start', Client.start),
		commands.registerCommand('why3.lsp.stop', Client.stop),
		commands.registerCommand('why3.lsp.interrupt', Client.interrupt),
		commands.registerCommand('why3.proof.show', Proof.show),
	);

	// Register Status Bar.
	Status.activate(context);

	// Start the client. This will also launch the server
	Client.start();

	// Create the proof panel.
	Proof.activate(context);

}

export function deactivate(): void {
	Client.deactivate();
	Proof.deactivate();
}
