/* -------------------------------------------------------------------------- */
/* --- Why3 Platform Extension                                            --- */
/* -------------------------------------------------------------------------- */

import { workspace, commands, ExtensionContext } from 'vscode';
import * as Client from './lspclient';

export function activate(context: ExtensionContext): void {

	// Register commands.
	commands.registerCommand('why3lsp.restart', Client.start);

	// Start the client. This will also launch the server
	Client.start();

}

export function deactivate(): void {
	Client.stop();
}
