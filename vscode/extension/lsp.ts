/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* -------------------------------------------------------------------------- */
/* --- Why3 LSP Client                                                    --- */
/* -------------------------------------------------------------------------- */

import {
	OutputChannel,
	window,
	workspace,
} from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	StreamInfo,
} from 'vscode-languageclient/node';

import Server, { type SocketInfo } from './rpc';

/* -------------------------------------------------------------------------- */
/* --- Child Process Management                                           --- */
/* -------------------------------------------------------------------------- */

export default class Client {
	private readonly output: OutputChannel;
	private readonly trace: OutputChannel;
	private server: Server | undefined;
	private client: LanguageClient | undefined;

	constructor() {
		this.output = window.createOutputChannel("WhyCode Server");
		this.trace = window.createOutputChannel("WhyCode Server Trace");
		this.launchServer = this.launchServer.bind(this);
		this.start = this.start.bind(this);
		this.stop = this.stop.bind(this);
	}

	async dispose(): Promise<void> {
		await this.stop();
		this.output.dispose();
		this.trace.dispose();
	}

	async start(): Promise<void> {

		// Configure client
		const clientOptions: LanguageClientOptions = {
			outputChannel: this.output,
			traceOutputChannel: this.trace,
			documentSelector: [{ scheme: 'file', language: 'why3' }],
			synchronize: {
				fileEvents: [
					workspace.createFileSystemWatcher('**/*.mlw'),
					workspace.createFileSystemWatcher('**/why3find.json'),
				]
			}
		};

		// Create LSP
		this.client = new LanguageClient(
			'why3lsp', 'Why3 Language Server',
			this.launchServer,
			clientOptions
		);

		// Start LSP
		this.output.appendLine('[VSCode] Launch lsp client');
		await this.client.start();
	}

	private async launchServer(): Promise<StreamInfo> {
		this.server = new Server('lsp', this.output);
		await this.server.cleanup();
		const { socket } = await this.server.spawn();
		return { reader: socket, writer: socket };
	}

	async stop(): Promise<void> {
		if (this.client) {
			this.output.appendLine('[VSCode] Stop lsp client');
			const client = this.client;
			this.client = undefined;
			await client.stop();
		}
		if (this.server) {
			this.output.appendLine('[VSCode] Shutdown lsp server');
			const server = this.server;
			this.server = undefined;
			await server.kill();
		}
	}

}

/* -------------------------------------------------------------------------- */
