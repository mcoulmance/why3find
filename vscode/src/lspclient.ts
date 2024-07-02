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
/* --- Why3 LSP Client                                                    --- */
/* -------------------------------------------------------------------------- */

import { ChildProcess, spawn } from 'child_process';
import { Socket, connect } from 'net';
import { OutputChannel, window, workspace } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	StreamInfo
} from 'vscode-languageclient/node';

/* -------------------------------------------------------------------------- */
/* --- Child Process Management                                           --- */
/* -------------------------------------------------------------------------- */

const RETRIES = 10;
const TIMEOUT = 200;
const SOCKADDR = '/tmp/why3lsp.io';

let output: OutputChannel | undefined;
let trace: OutputChannel | undefined;
let process: ChildProcess | undefined;
let socket: Socket | undefined;
let timer: NodeJS.Timeout | undefined;
let client: LanguageClient | undefined;
let retries: number = 0;

function disconnectFromLsp(): void {
	if (socket) {
		output?.appendLine('[VSCode] Disconnected from LSP');
		socket.destroy(); socket = undefined;
	}
	if (timer) { clearTimeout(timer); timer = undefined; }
	retries = 0;
}

function connectToLsp(
	resolve: (stream: StreamInfo) => void,
	reject: (err: Error) => void,
) {
	if (socket) socket.destroy();
	if (timer) { clearTimeout(timer); timer = undefined; }
	retries++;
	const s = connect(SOCKADDR);
	socket = s ;
	s.once('connect', () => {
		output?.appendLine('[VSCode] Connected to LSP');
		retries = 0;
		resolve({ reader: s, writer: s });
	});
	s.once('error', (err: Error) => {
		if (retries < RETRIES) {
			output?.appendLine(`[VSCode] Reconnect to LSP #${retries}`);
			socket = undefined;
			timer = setTimeout(() => {
				connectToLsp(resolve, reject);
			}, TIMEOUT);
		} else {
			disconnectFromLsp();
			reject(err);
		}
	});
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
	}
	disconnectFromLsp();
	if (process) {
		if (!process.killed) {
			output?.appendLine('[VSCode] Shutdown LSP Server');
			process.kill('SIGKILL');
		}
		process = undefined;
	}
}

export async function start(): Promise<void> {

	// Channels
	if (!output) output = window.createOutputChannel("WhyCode Server");
	if (!trace) trace = window.createOutputChannel("WhyCode Server Trace");

	// Stop currently running client
	stop();

	// Span server
	output.appendLine('[VSCode] Launch LSP Server');
	const p = spawn('why3find', ['lsp', '--pipe', SOCKADDR]);
	p.stdout.on('data', (msg) => output?.append(`${msg}`));
	p.stderr.on('data', (msg) => output?.append(`${msg}`));
	process = p;

	const serverOptions: ServerOptions = () => new Promise(connectToLsp);

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
		serverOptions,
		clientOptions
	);

	// Start LSP
	output?.appendLine('[VSCode] Launch LSP Client');
	await client.start();
}
