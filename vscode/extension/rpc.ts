/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* -------------------------------------------------------------------------- */
/* --- Server Process for DAP & LSP LSP Client                            --- */
/* -------------------------------------------------------------------------- */

import { rm } from 'fs/promises';
import { Socket, connect } from 'net';
import { ChildProcess, spawn } from 'child_process';

import {
  OutputChannel,
} from 'vscode';

/** Remote Process for DAP & LSP */

const RETRIES = 20;
const TIMEOUT = 250;

export default class Server {

  private readonly sockaddr: string;
  private readonly output: OutputChannel;
  private process: ChildProcess | undefined;
  private socket: Socket | undefined;
  private timer: NodeJS.Timeout | undefined;
  private retries: number = 0;

  constructor(sockaddr: string, output: OutputChannel) {
    this.output = output;
    this.sockaddr = sockaddr;
    this.out = this.out.bind(this);
    this.doConnect = this.doConnect.bind(this);
  }

  out(msg: any) { this.output.appendLine(msg.toString()); }
  log(msg: any) { this.output.appendLine(`[VSCode] ${msg.toString()}`); }

  async spawn(program: string, ...args: string[]): Promise<Socket> {
    // Disconnect current
    this.disconnectFromServer();
    this.shutdownServer();
    await rm(this.sockaddr, { force: true });
    // Span new server
    this.log('launch server');
    const process = spawn(program, args);
    process.stdout.on('data', this.out);
    process.stderr.on('data', this.out);
    this.process = process;
    // Connect to new server
    return new Promise(this.doConnect);
  }

  async shutdown(): Promise<void> {
    this.disconnectFromServer();
    this.shutdownServer();
    await rm(this.sockaddr, { force: true });
  }

  private shutdownServer(): void {
    const process = this.process;
    if (process && !process.killed) {
      this.log('shutdown server');
      process.kill('SIGKILL');
    }
    this.process = undefined;
  }

  private disconnectFromServer(): void {
    if (this.socket) {
      this.log('disconnected');
      this.socket.destroy();
      this.socket = undefined;
    }
    if (this.timer) {
      clearTimeout(this.timer);
      this.timer = undefined;
    }
    this.retries = 0;
  }

  private doConnect(
    resolve: (pipe: Socket) => void,
    reject: (err: Error) => void,
  ): void {
    this.retries++;
    const socket = connect(this.sockaddr);
    socket.once('connect', () => {
      this.log('connected');
      this.retries = 0;
      resolve(socket);
    });
    socket.on('error', (err: Error) => {
      if (this.retries < RETRIES) {
        this.log(`reconnect #${this.retries}`);
        this.socket = undefined;
        this.timer = setTimeout(() => {
          this.doConnect(resolve, reject);
        }, TIMEOUT);
      } else {
        this.disconnectFromServer();
        reject(err);
      }
    });
    this.socket = socket;
  }

}
