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
  Disposable,
  DebugConfiguration,
  DebugConfigurationProvider,
  ProviderResult,
  WorkspaceFolder,
  DebugAdapterDescriptorFactory,
  DebugAdapterDescriptor,
  DebugAdapterExecutable,
  DebugSession,
  OutputChannel,
  DebugAdapterNamedPipeServer,
} from 'vscode';

import Server from './rpc';

/* -------------------------------------------------------------------------- */
/* --- Debug Adapater Proxy                                               --- */
/* -------------------------------------------------------------------------- */

const SOCKADDR = '/tmp/why3find.dap.io';

export default class Adapter implements
  DebugConfigurationProvider,
  DebugAdapterDescriptorFactory,
  Disposable {

  private output: OutputChannel;
  private server: Server | undefined;

  constructor() {
    this.output = window.createOutputChannel('Why3 DAP');
  }

  async dispose(): Promise<void> {
    await this.server?.shutdown();
    this.output.dispose();
  }

  // Debug Configuration
  resolveDebugConfiguration(
    folder: WorkspaceFolder | undefined,
    config: DebugConfiguration,
  ): ProviderResult<DebugConfiguration> {
    if (!config.type && !config.name && !config.request) {
      const editor = window.activeTextEditor;
      if (editor && editor.document.languageId === 'why3') {
        config.type = 'why3';
        config.name = 'Debug';
        config.request = 'launch';
        config.file = '${file}';
        config.debug = true;
      }
    }
    return config;
  }

  // Adapter Factory
  createDebugAdapterDescriptor(
    session: DebugSession,
    executable: DebugAdapterExecutable | undefined
  ): ProviderResult<DebugAdapterDescriptor> {
    if (session.type !== 'why3') return null;
    if (executable !== undefined) return null;
    this.server?.shutdown();
    const { file, debug } = session.configuration;
    this.output.appendLine(`WHY3 ${debug ? 'DEBUG' : 'PROVE'} "${file}"`);
    this.server = new Server(SOCKADDR, this.output);
    return this.server
      .spawn("why3find", "dap", "--pipe", SOCKADDR, debug && '--debug')
      .then(() => new DebugAdapterNamedPipeServer(SOCKADDR));
  }

}
