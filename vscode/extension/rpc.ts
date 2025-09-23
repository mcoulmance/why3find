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
  window,
  workspace,
} from 'vscode';

export type SocketInfo = {
  addr : string ;
  socket : Socket ;
}

/** Remote Process for DAP & LSP */

const RETRIES = 20;
const TIMEOUT = 250;

export default class Server {

  private readonly name: string;
  private readonly addr: string;
  private readonly output: OutputChannel;
  private process: ChildProcess | undefined;
  private socket: Socket | undefined;
  private timer: NodeJS.Timeout | undefined;
  private retries: number = 0;

  constructor(name: string, output : OutputChannel) {
    this.output = output ;
    this.name = name ;
    this.addr = `/tmp/why3${name}.io` ;
    this.doConnect = this.doConnect.bind(this);
  }

  log(msg: string) {
    this.output.appendLine(msg);
  }

  spawn(...args : string[]): Promise<SocketInfo> {
    // Disconnect current
    this.disconnectFromServer();
    this.shutdownServer();
    // Span new server
    this.log(`[VSCode] Launch ${this.name} server`);
    const process = spawn('why3find', [this.name, '--pipe', this.addr, ...args]);
    process.stdout.on('data', (msg) => this.log(msg.toString()));
    process.stderr.on('data', (msg) => this.log(msg.toString()));
    this.process = process;
    // Connect to new server
    return new Promise(this.doConnect);
  }

  cleanup(): Promise<void> {
    return rm(this.addr , { force: true });
  }

  kill(): Promise<void> {
    this.disconnectFromServer();
    this.shutdownServer();
    return rm(this.addr , { force: true });
  }

  private shutdownServer(): void {
    const p = this.process;
    this.process = undefined;
    if (p && !p.killed) {
      this.log('[VSCode] Shutdown ${this.name} server');
      p.kill('SIGKILL');
    }
  }

  private disconnectFromServer(): void {
    if (this.socket) {
      this.log('[VSCode] Disconnected');
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
    resolve: (pipe: SocketInfo) => void,
    reject: (err: Error) => void,
  ): void {
    this.retries++;
    const addr = this.addr ;
    const socket = connect(addr);
    this.socket = socket;
    socket.once('connect', () => {
      this.log('[VSCode] Connected');
      this.retries = 0;
      resolve({ socket, addr });
    });
    socket.on('error', (err: Error) => {
      if (this.retries < RETRIES) {
        this.log(`[VSCode] Reconnect to ${this.name} #${this.retries}`);
        this.socket = undefined;
        this.timer = setTimeout(() => {
          this.doConnect(resolve, reject);
        }, TIMEOUT);
      } else {
        this.disconnectFromServer();
        reject(err);
      }
    });
  }

}
