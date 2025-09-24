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

const SOCKADDR = '/tmp/why3find.lsp.io';

export default class Client implements Disposable {
  private readonly output: OutputChannel;
  private server: Server | undefined;
  private client: LanguageClient | undefined;

  constructor() {
    this.output = window.createOutputChannel('Why3 LSP');
    this.launchServer = this.launchServer.bind(this);
    this.start = this.start.bind(this);
    this.stop = this.stop.bind(this);
  }

  async dispose(): Promise<void> {
    await this.client?.stop();
    await this.server?.shutdown();
    this.output.dispose();
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
    this.output.appendLine('[VSCode] launch lsp client');
    await this.client.start();
  }

  private async launchServer(): Promise<StreamInfo> {
    this.server = new Server(SOCKADDR, this.output);
    const socket =
      await this.server.spawn('why3find', 'lsp', '--pipe', SOCKADDR);
    return { reader: socket, writer: socket };
  }

  async stop(): Promise<void> {
    if (this.client) {
      this.output.appendLine('[VSCode] Stop lsp client');
      await this.client.stop();
      this.client = undefined;
    }
    if (this.server) {
      this.output.appendLine('[VSCode] Shutdown lsp server');
      await this.server.shutdown();
      this.server = undefined;
    }
  }

}

/* -------------------------------------------------------------------------- */
