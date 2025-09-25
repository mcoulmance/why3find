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
import { ChildProcess, spawn } from 'child_process';

import { OutputChannel, Disposable, window } from 'vscode';

/** Remote Process for DAP & LSP */

export default class Server implements Disposable {

  protected readonly sockaddr: string;
  protected readonly output: OutputChannel;
  private process: ChildProcess | undefined;

  constructor(sockaddr: string, name: string) {
    this.output = window.createOutputChannel(name);
    this.sockaddr = sockaddr;
    this.out = this.out.bind(this);
  }

  log(msg: any) { this.output.appendLine(`[VSCode] ${msg.toString()}`); }
  private out(msg: any) { this.output.appendLine(msg.toString()); }

  async spawn(program: string, ...args: string[]): Promise<void> {
    // Disconnect current
    await this.shutdown();
    // Span new server
    this.log('launch server');
    const process = spawn(program, args);
    process.stdout.on('data', this.out);
    process.stderr.on('data', this.out);
    this.process = process;
  }

  async shutdown(): Promise<void> {
    const process = this.process;
    if (process && !process.killed) {
      this.log('shutdown server');
      process.kill('SIGKILL');
    }
    this.process = undefined;
    await rm(this.sockaddr, { force: true });
  }

  async dispose(): Promise<void> {
    await this.shutdown();
    this.output.dispose();
  }

}
