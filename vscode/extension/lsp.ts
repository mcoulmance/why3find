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

import { Socket, connect } from 'net';

import {
  window,
  OutputChannel,
  workspace,
  Disposable,
} from 'vscode';

import {
  LanguageClient,
  LanguageClientOptions,
  StreamInfo,
} from 'vscode-languageclient/node';

import Server from './rpc';

/* -------------------------------------------------------------------------- */
/* --- Child Process Management                                           --- */
/* -------------------------------------------------------------------------- */

const RETRIES = 20;
const TIMEOUT = 250;
const SOCKADDR = '/tmp/why3find.lsp.io';

export default class Client extends Server implements Disposable {
  private socket: Socket | undefined;
  private timer: NodeJS.Timeout | undefined;
  private retries: number = 0;
  private client: LanguageClient | undefined;

  constructor() {
    super(SOCKADDR, 'Why3 LSP');
    this.start = this.start.bind(this);
    this.stop = this.stop.bind(this);
    this.launchServer = this.launchServer.bind(this);
    this.resolveConnection= this.resolveConnection.bind(this);
  }

  async dispose(): Promise<void> {
    await this.client?.stop();
    this.disconnectFromServer();
    await super.dispose();
  }

  private disconnectFromServer(): void {
    if (this.socket) {
      this.log('disconnect');
      this.socket.destroy();
      this.socket = undefined;
    }
    if (this.timer) {
      clearTimeout(this.timer);
      this.timer = undefined;
    }
    this.retries = 0;
  }

  private resolveConnection(
    resolve: (pipe: StreamInfo) => void,
    reject: (err: Error) => void,
  ): void {
    this.retries++;
    const socket = connect(this.sockaddr);
    socket.once('connect', () => {
      this.log('connected');
      this.retries = 0;
      resolve({ reader: socket, writer: socket });
    });
    socket.on('error', (err: Error) => {
      if (this.retries < RETRIES) {
        this.log(`reconnect #${this.retries}`);
        this.socket = undefined;
        this.timer = setTimeout(() => {
          this.resolveConnection(resolve, reject);
        }, TIMEOUT);
      } else {
        this.disconnectFromServer();
        reject(err);
      }
    });
    this.socket = socket;
  }

  private async launchServer(): Promise<StreamInfo> {
    await this.spawn('why3find', 'lsp', '--pipe', SOCKADDR);
    return new Promise(this.resolveConnection);
  }

  async start(): Promise<void> {
    // Configure client
    const clientOptions: LanguageClientOptions = {
      outputChannel: this.output,
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
    this.log('start client');
    await this.client.start();
  }

  async stop(): Promise<void> {
    if (this.client) {
      this.log('stop client');
      await this.client.stop();
      this.client = undefined;
    }
    this.disconnectFromServer();
    await this.shutdown();
  }

}

/* -------------------------------------------------------------------------- */
