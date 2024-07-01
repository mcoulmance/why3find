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
	if (socket) { socket.destroy(); socket = undefined; }
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
	output.appendLine(`Connection Attempt #${retries}`);
	socket = connect(SOCKADDR);
	socket.once('connect', () => {
		retries = 0;
		resolve({ reader: socket, writer: socket });
	});
	socket.once('error', (err: Error) => {
		if (retries < RETRIES) {
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
	disconnectFromLsp();
	if (process) { process.kill('SIGKILL'); process = undefined; }
	if (client) {
		const toStop = client;
		client = undefined;
		await toStop.stop();
	}
}

export async function start(): Promise<void> {

	// Stop currently running client
	stop();

	// Channels
	if (!output) output = window.createOutputChannel("WhyCode Server");
	if (!trace) trace = window.createOutputChannel("WhyCode Server Trace");

	// Span server
	process = spawn('why3find', ['lsp', '--pipe', SOCKADDR]);
	process.stdout.on('data', (msg) => output.append(`${msg}`));
	process.stderr.on('data', (msg) => output.append(`${msg}`));

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

	// Start the client & launch server
	await client.start();
}
