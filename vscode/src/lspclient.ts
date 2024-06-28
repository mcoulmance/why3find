/* -------------------------------------------------------------------------- */
/* --- Why3 LSP Client                                                    --- */
/* -------------------------------------------------------------------------- */

import { window, workspace } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	Executable,
	TransportKind,
	SocketTransport,
	ExecutableOptions
} from 'vscode-languageclient/node';

let client: LanguageClient;

export async function stop(): Promise<void> {
	if (client) {
		const toStop = client;
		client = undefined;
		await toStop.stop();
	}
}

export async function start(): Promise<void> {

	// Stop currently running client
	stop();

	// Configure server
	const serverCommand: Executable = {
		command: 'why3find', args: ['lsp'],
		transport: { kind: TransportKind.socket, port: 8989 }
	};
	const serverOptions: ServerOptions = {
		run: serverCommand, debug: serverCommand,
	};

	// Configure client
	const clientOptions: LanguageClientOptions = {
		outputChannel: window.createOutputChannel("WhyCode Server"),
		traceOutputChannel: window.createOutputChannel("WhyCode Server Trace"),
		documentSelector: [{ scheme: 'file', language: 'why3' }],
		synchronize: {
			fileEvents: [
				workspace.createFileSystemWatcher('**/*.mlw'),
				workspace.createFileSystemWatcher('**/why3find.json'),
			]
		}
	};

	// Create LSP
	client = new LanguageClient(
		'languageServerExample',
		'Language Server Example',
		serverOptions,
		clientOptions
	);

	// Start the client & launch server
	await client.start();
}
