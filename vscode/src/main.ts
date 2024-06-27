/* -------------------------------------------------------------------------- */
/* --- Why3 Platform Extension                                            --- */
/* -------------------------------------------------------------------------- */

import { workspace, ExtensionContext } from 'vscode';
import * as Client from './client';

export async function activate(context: ExtensionContext): Promise<void> {
	// Start the client. This will also launch the server
	await Client.start();
}

export async function deactivate(): Promise<void> {
	await Client.stop();
}
