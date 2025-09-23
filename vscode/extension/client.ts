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

import { rm } from 'fs/promises';
import { Socket, connect } from 'net';
import { ChildProcess, spawn } from 'child_process';

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

/* -------------------------------------------------------------------------- */
/* --- Child Process Management                                           --- */
/* -------------------------------------------------------------------------- */

const RETRIES = 20;
const TIMEOUT = 250;
const SOCKADDR = '/tmp/why3lsp.io';

let output: OutputChannel | undefined;
let trace: OutputChannel | undefined;
let process: ChildProcess | undefined;
let socket: Socket | undefined;
let timer: NodeJS.Timeout | undefined;
let client: LanguageClient | undefined;
let retries: number = 0;

function shutdownServer() {
	if (process) {
		if (!process.killed) {
			output?.appendLine('[VSCode] Shutdown LSP Server');
			process.kill('SIGKILL');
		}
		process = undefined;
	}
}

function disconnectFromServer(): void {
	if (socket) {
		output?.appendLine('[VSCode] Disconnected');
		socket.destroy(); socket = undefined;
	}
	if (timer) { clearTimeout(timer); timer = undefined; }
	retries = 0;
}

function connectToServer(
	resolve: (stream: StreamInfo) => void,
	reject: (err: Error) => void,
) {
	retries++;
	const s = connect(SOCKADDR);
	socket = s;
	s.once('connect', () => {
		output?.appendLine('[VSCode] Connected');
		retries = 0;
		resolve({ reader: s, writer: s });
	});
	s.on('error', (err: Error) => {
		if (retries < RETRIES) {
			output?.appendLine(`[VSCode] Reconnect to LSP #${retries}`);
			socket = undefined;
			timer = setTimeout(() => {
				connectToServer(resolve, reject);
			}, TIMEOUT);
		} else {
			disconnectFromServer();
			reject(err);
		}
	});
}

function launchServer(): Promise<StreamInfo> {
	// Diconnect current
	disconnectFromServer();
	shutdownServer();
	// Span new server
	output?.appendLine('[VSCode] Launch LSP Server');
	const p = spawn('why3find', ['lsp', '--pipe', SOCKADDR]);
	p.stdout.on('data', (msg) => output?.append(`${msg}`));
	p.stderr.on('data', (msg) => output?.append(`${msg}`));
	process = p;
	// Connect to new server
	return new Promise(connectToServer);
}

/* -------------------------------------------------------------------------- */
/* --- LSP Entry Points                                                   --- */
/* -------------------------------------------------------------------------- */

export async function stop(): Promise<void> {
	if (client) {
		output?.appendLine('[VSCode] Stop LSP Client');
		const toStop = client;
		client = undefined;
		await toStop.stop();
		disconnectFromServer();
		shutdownServer();
		await rm(SOCKADDR, { force: true });
	}
}

export async function start(): Promise<void> {

	// Channels
	if (!output) output = window.createOutputChannel("WhyCode Server");
	if (!trace) trace = window.createOutputChannel("WhyCode Server Trace");

    // In case of re-start
	await stop();

    // Remove existing socket
	await rm(SOCKADDR, { force: true });

	// Configure client
	const clientOptions: LanguageClientOptions = {
		outputChannel: output,
		traceOutputChannel: trace,
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
		'why3lsp', 'Why3 Language Server',
		launchServer,
		clientOptions
	);

	// Start LSP
	output?.appendLine('[VSCode] Launch LSP Client');
	await client.start();
}

/* -------------------------------------------------------------------------- */
/* --- Extension Registration                                             --- */
/* -------------------------------------------------------------------------- */

export function deactivate() {
	disconnectFromServer();
	shutdownServer();
};

/* -------------------------------------------------------------------------- */
